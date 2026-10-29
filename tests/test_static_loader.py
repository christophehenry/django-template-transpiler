from contextlib import contextmanager
from typing import Collection
from unittest.mock import patch

import pytest
from django.conf import global_settings
from django.contrib.staticfiles.storage import StaticFilesStorage
from django.core.management import call_command
from django.urls import reverse
from django.utils.module_loading import import_string
from playwright.sync_api import Page

from django_template_transpiler import TRANSPILE_TARGETS_DOTTED_PATH, target

DUMMY_TARGET_MODULE = f"dummy_django_app.{TRANSPILE_TARGETS_DOTTED_PATH}"


@contextmanager
def patch_import_string(targets: str | target | Collection[str | target]):
    if isinstance(targets, (str, target)):
        targets = [targets]

    def patched_import_string(dotted_path):
        return targets if dotted_path in DUMMY_TARGET_MODULE else import_string(dotted_path)

    with patch("django_template_transpiler.finders.import_string", patched_import_string):
        yield


@pytest.fixture(autouse=True, scope="function")
def before_each(settings):
    from django.contrib.staticfiles.finders import get_finder

    settings.STATICFILES_FINDERS = [
        "django_template_transpiler.finders.DjangoTemplateTranspilerFinder",
        *global_settings.STATICFILES_FINDERS,
    ]
    get_finder.cache_clear()

    yield


@pytest.mark.parametrize(
    "test_target,expected_script_name,debug",
    [
        ("test_loader.html", "test_loader.0.js", True),
        (target("test_loader-2.html", script_name="test_loader.js"), "test_loader.0.js", True),
        ("test_loader.html", "test_loader.js", False),
        (target("test_loader-2.html", script_name="test_loader.js"), "test_loader.js", False),
    ],
)
def test_static_loader(test_target, expected_script_name, debug, settings, live_server, page: Page):
    storage = StaticFilesStorage()
    storage.delete(expected_script_name)
    assert not storage.exists(expected_script_name), (
        f"{storage.path(expected_script_name)} still exists before test execution"
    )

    settings.DEBUG = debug

    with (
        patch_import_string(test_target),
        patch("django_template_transpiler.finders._hash", return_value="0"),
    ):
        script_name = (test_target if isinstance(test_target, target) else target(test_target)).script_name
        page.goto(f"{live_server.url}{reverse('tag_test', kwargs={'render_script_name': script_name})}")
        assert storage.exists(expected_script_name)


@pytest.mark.parametrize(
    "test_target,generated_file",
    [
        ("test_loader.html", "test_loader.js"),
        ("forms/test_loader-3.html", "forms/test_loader-3.js"),
        (target("test_loader-2.html", script_name="test_loader.js"), "test_loader.js"),
        (target("forms/test_loader-3.html", script_name="bump.js"), "bump.js"),
        (target("forms/test_loader-3.html", flat=True), "test_loader-3.js"),
    ],
)
def test_collectstatic(test_target, generated_file):
    storage = StaticFilesStorage()
    storage.delete(generated_file)
    assert not storage.exists(generated_file), f"{storage.path(generated_file)} still exists before test execution"

    # Test
    with patch_import_string(test_target):
        call_command("collectstatic", interactive=False)

        assert storage.exists(generated_file)
        with storage.open(generated_file) as f:
            assert len(f.read())
