from django.urls import reverse
from playwright.sync_api import Page
from pytest_django.asserts import assertHTMLEqual

from utils import setup


def test_variable_tag01(live_server, page: Page):
    with setup(
        script_name="var_001",
        template="""<p>
                {{ foo|default:1 }}
                {{ foo|default:1.0 }}
                {{ foo|default:"bar" }}
                {{ foo|default:'baz' }}
                {{ foo|default:_('hello') }}
                Some text
                {{ bar|lower }}
                {{ bar|cut:"foo" }}
                {{ foo.bar }}
            </p>""",
        js_context='{"bar": "baz"}',
    ) as test_render_script:
        page.goto(
            f"{live_server.url}{reverse('tag_test', kwargs={'render_script_name': test_render_script.name})}"
        )
        assertHTMLEqual(
            page.locator("main").inner_html(),
            "<p> 1 1 bar baz hello Some text baz baz </p>",
        )
