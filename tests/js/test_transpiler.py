from pathlib import Path

from django.template import engines, Template
from django.urls import reverse
from playwright.sync_api import Page

from django_rusty_templates import transpile_from_string


def test_transpiler(live_server, page: Page):
    template: Template = engines["django"].engine.select_template(["full_example.html"])

    transpile_from_string(template.source, f"{Path(template.name).stem}.js")

    page.goto(f"{live_server}{reverse('home')}")
    pass
