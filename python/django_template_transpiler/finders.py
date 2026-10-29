import dataclasses
from functools import cached_property
from hashlib import sha256
from io import StringIO
from logging import getLogger
from pathlib import Path
from typing import Any, Generator, Iterable

from django.apps import apps
from django.contrib.staticfiles.finders import BaseFinder
from django.contrib.staticfiles.storage import StaticFilesStorage
from django.core.checks import Error
from django.core.files import File
from django.core.files.storage import Storage
from django.template.backends.django import Template
from django.template.exceptions import TemplateDoesNotExist
from django.template.loader import get_template as django_get_template
from django.utils.datastructures import MultiValueDict
from django.utils.module_loading import import_string

from django_template_transpiler import transpile

__all__ = ["DjangoTemplateTranspilerFinder", "target", "TRANSPILE_TARGETS_DOTTED_PATH"]

TRANSPILE_TARGETS_DOTTED_PATH = "transpile_targets.targets"


logger = getLogger(__name__)


@dataclasses.dataclass
class target:
    """Customize how a transpile target is generated

    See :ref:`fine-grained-script-generation`.

    :param template: Name of the Django template to transpile, relative to its ``templates`` directory, as
                      would be specified in :templatetag:`{% extends %} <extends>`.

    :param script_name: name and, optionally, directory tree of the generated script.
    :type script_name: str, optional

    :param flat: Whether or not to ignore the directory tree. This parameter is ignored if ``script_name`` is specified.
    :type flat: bool, optional

    """

    template: str
    _: dataclasses.KW_ONLY
    script_name: str = None
    flat: bool = False

    _script_name: str | None = dataclasses.field(default=None, init=False, repr=False)

    @property
    def script_name(self):  # noqa: F811
        if self._script_name is not None:
            return self._script_name
        from django.conf import settings

        suffix = getattr(settings, "DTT_SCRIPT_EXTENSION", ".js").removeprefix(".")
        result = Path(self.template).with_suffix(f".{suffix}")
        return str(result.name if self.flat else result)

    @script_name.setter
    def script_name(self, value):
        if isinstance(value, (str | None)):
            self._script_name = value


def _hash(template: Template):
    return sha256(template.template.source.encode()).hexdigest()


def _get_template(template_name) -> Template:
    return django_get_template(template_name, using="django")


class TranspilerStorage(StaticFilesStorage):
    def __init__(self, template):
        super().__init__(allow_overwrite=True)
        self.template = template

    def open(self, name, *args, **kwargs):
        template = _get_template(self.template)
        return File(StringIO(transpile(template.template.source)), name)


class DjangoTemplateTranspilerFinder(BaseFinder):
    @cached_property
    def storage(self):
        return StaticFilesStorage(allow_overwrite=True)

    def __init__(self):
        super().__init__()
        self._targets = MultiValueDict()
        for app_config in apps.get_app_configs():
            module_qualname = f"{app_config.module.__name__}.{TRANSPILE_TARGETS_DOTTED_PATH}"
            try:
                targets = import_string(module_qualname)
            except ImportError:
                continue

            for item in targets:
                match item:
                    case str():
                        item = target(item)
                    case target():
                        pass
                    case _:
                        raise ValueError(f"{module_qualname}: expected either a str or target(); found {item}")
                self._targets.appendlist(item.script_name, item)

    def get_targets(self) -> Generator[target, None, None]:
        for key in self._targets.keys():
            for _target in self._targets.getlist(key, []):
                yield _target

    def check(self, **kwargs: Any):
        errors = []
        for _target in self.get_targets():
            try:
                _get_template(_target.template)
            except TemplateDoesNotExist as e:
                errors.append(Error(str(e), id="django_template_transpiler.E001"))
        return errors

    def find(self, path, find_all=False, **kwargs):
        try:
            _target: target = self._targets.getlist(path, [])[0]
        except IndexError:
            raise KeyError(f"{path} is not listed in any app's {TRANSPILE_TARGETS_DOTTED_PATH}")

        template = _get_template(_target.template)
        logger.debug(f"Selected {template.origin} while requesting {path}")

        from django.conf import settings

        script_name = _target.script_name
        if settings.DEBUG:
            pathlib_path = Path(_target.script_name)
            script_name = f"{pathlib_path.stem}.{_hash(template)}{pathlib_path.suffix}"

        if not self.storage.exists(script_name):
            self.storage.save(script_name, StringIO(transpile(template.template.source)))
        return self.storage.path(script_name)

    def list(self, ignore_patterns: Iterable[str] | None) -> Generator[tuple[str, Storage], None, None]:
        for _target in self.get_targets():
            storage = TranspilerStorage(_target.template)
            yield _target.script_name, storage
