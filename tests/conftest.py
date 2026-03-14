import json
import os
from pathlib import Path
from urllib.parse import urlparse, parse_qsl, urlencode, urlunparse
from uuid import uuid4

import pytest
from django.template.backends.django import Template
from django.template.loader import select_template
from playwright.sync_api import Page, expect

from django_template_transpiler import transpile

# https://github.com/microsoft/playwright-python/issues/439
os.environ.setdefault("DJANGO_ALLOW_ASYNC_UNSAFE", "true")


@pytest.fixture
def transpile_template(settings):
    def func(template_name):
        template: Template = select_template(
            template_name
            if isinstance(template_name, (tuple, list))
            else (template_name,)
        )

        dest = Path(settings.STATIC_ROOT)
        if not dest.is_absolute():
            dest = settings.BASE_DIR / dest
        dest.mkdir(parents=True, exist_ok=True)
        dest = dest / Path(template.origin.name).with_suffix(".js").name

        with open(dest, "w") as f:
            f.write(transpile(template.template.source))

        return dest.name

    return func


@pytest.fixture
def render_with_context(page: Page):
    def func(script_name, context_dict):
        from django.templatetags.static import static

        expect(page.locator("#main")).to_be_empty()

        engine_file = static("core.js")
        script_file = static(script_name)
        json_context = json.dumps(context_dict)

        # language=js
        page.evaluate(
            f"""async () => {{
                const {{Engine}} = await import("{engine_file}")
                const render = (await import("{script_file}")).default

                const engine = new Engine()
                document.querySelector("#main").innerHTML = render(engine, {json_context})
            }}"""
        )
        expect(page.locator("#main")).not_to_be_empty()

    return func


@pytest.fixture
def render_template(live_server, settings, page: Page):
    dest = Path(settings.STATIC_ROOT)
    if not dest.is_absolute():
        dest = settings.BASE_DIR / dest

    dest.mkdir(parents=True, exist_ok=True)
    script_path = dest / f"{uuid4()}.js"

    def func(template, context_dict, module_name):
        with open(script_path, "w") as f:
            f.write(transpile(template))

        url_parts = list(urlparse(live_server.url))
        query = dict(parse_qsl(url_parts[4]))
        query["template"] = script_path.name
        query["module_name"] = module_name
        url_parts[4] = urlencode(query)

        page.goto(f"{urlunparse(url_parts)}")

        expect(page.locator("#main")).to_be_empty()
        json_context = json.dumps(context_dict)

        # language=js
        page.evaluate(
            f"""async () => {{
                const {{Engine}} = await import("core")
                const render = (await import("{module_name}")).default

                const engine = new Engine()
                document.querySelector("#main").innerHTML = render(engine, {json_context})
            }}"""
        )

        expect(page.locator("#main")).not_to_be_empty()

    yield func
    script_path.unlink(missing_ok=True)
