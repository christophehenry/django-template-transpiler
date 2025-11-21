from pathlib import Path

from django.template import engines, Template
from django.urls import reverse

from django_template_transpiler import transpile_from_string


def test_transpiler(live_server, page):
    template: Template = engines["django"].engine.select_template(["full_example.html"])
    transpile_from_string(template.source, f"{Path(template.name).stem}.js")
    page.goto(f"{live_server}{reverse('home')}")

    assert page.locator("#main").inner_html() == (
        "<p>\n    1\n    1\n    \"bar\"\n    'baz'\n    hello\n    Some text"
        "\n    \n    \n    \n    \n    \n    \n    \n    \n    \n</p>\n"
    )
