import json
import os
from pathlib import Path

import pytest
from django.template.backends.django import Template
from django.template.loader import select_template
from playwright.sync_api import Page, expect

from django_template_transpiler import transpile

# https://github.com/microsoft/playwright-python/issues/439
os.environ.setdefault("DJANGO_ALLOW_ASYNC_UNSAFE", "true")


@pytest.fixture
def transpile_template():
    def func(template_name):
        from django.conf import settings

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
