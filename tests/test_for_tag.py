import pytest
from django.urls import reverse
from playwright.sync_api import Page
from pytest_django.asserts import assertHTMLEqual

from utils import setup

"""
`{% for %}` tests adapted from Django's
See: https://github.com/django/django/blob/main/tests/template_tests/syntax_tests/test_for.py
"""


def test_for_tag01(live_server, page: Page):
    with setup(
        script_name="for-tag01",
        template="{% for val in values %}<span>{{ val }}</span>{% endfor %}",
        js_context="{values: [1, 2, 3]}",
    ) as test_render_script:
        page.goto(
            f"{live_server.url}{reverse('tag_test', kwargs={'render_script_name': test_render_script.name})}"
        )
        assertHTMLEqual(
            page.locator("main").inner_html(),
            "<span>1</span><span>2</span><span>3</span>",
        )


def test_for_tag02(live_server, page: Page):
    with setup(
        script_name="for-tag02",
        template="{% for val in values reversed %}<span>{{ val }}</span>{% endfor %}",
        js_context="{values: [1, 2, 3]}",
    ) as test_render_script:
        page.goto(
            f"{live_server.url}{reverse('tag_test', kwargs={'render_script_name': test_render_script.name})}"
        )
        assertHTMLEqual(
            page.locator("main").inner_html(),
            "<span>3</span><span>2</span><span>1</span>",
        )


def test_for_tag_vars01(live_server, page: Page):
    with setup(
        script_name="for-tag-vars01",
        template="{% for val in values %}{{ forloop.counter }}{% endfor %}",
        js_context='{"values": [6, 6, 6]}',
    ) as test_render_script:
        page.goto(
            f"{live_server.url}{reverse('tag_test', kwargs={'render_script_name': test_render_script.name})}"
        )
        assertHTMLEqual(page.locator("main").inner_html(), "123")


def test_for_tag_vars02(live_server, page: Page):
    with setup(
        script_name="for-tag-vars02",
        template="{% for val in values %}{{ forloop.counter0 }}{% endfor %}",
        js_context='{"values": [6, 6, 6]}',
    ) as test_render_script:
        page.goto(
            f"{live_server.url}{reverse('tag_test', kwargs={'render_script_name': test_render_script.name})}"
        )
        assertHTMLEqual(page.locator("main").inner_html(), "012")


def test_for_tag_vars03(live_server, page: Page):
    with setup(
        script_name="for-tag-vars03",
        template="{% for val in values %}{{ forloop.revcounter }}{% endfor %}",
        js_context='{"values": [6, 6, 6]}',
    ) as test_render_script:
        page.goto(
            f"{live_server.url}{reverse('tag_test', kwargs={'render_script_name': test_render_script.name})}"
        )
        assertHTMLEqual(page.locator("main").inner_html(), "321")


def test_for_tag_vars04(live_server, page: Page):
    with setup(
        script_name="for-tag-vars04",
        template="{% for val in values %}{{ forloop.revcounter0 }}{% endfor %}",
        js_context='{"values": [6, 6, 6]}',
    ) as test_render_script:
        page.goto(
            f"{live_server.url}{reverse('tag_test', kwargs={'render_script_name': test_render_script.name})}"
        )
        assertHTMLEqual(page.locator("main").inner_html(), "210")


def test_for_tag_vars05(live_server, page: Page):
    with setup(
        script_name="for-tag-vars05",
        template="{% for val in values %}{% if forloop.first %}f{% else %}x{% endif %}{% endfor %}",
        js_context='{"values": [6, 6, 6]}',
    ) as test_render_script:
        page.goto(
            f"{live_server.url}{reverse('tag_test', kwargs={'render_script_name': test_render_script.name})}"
        )
        assertHTMLEqual(
            page.locator("main").inner_html(),
            "fxx",
        )


def test_for_tag_vars06(live_server, page: Page):
    with setup(
        script_name="for-tag-vars06",
        template="{% for val in values %}{% if forloop.last %}l{% else %}x{% endif %}{% endfor %}",
        js_context='{"values": [6, 6, 6]}',
    ) as test_render_script:
        page.goto(
            f"{live_server.url}{reverse('tag_test', kwargs={'render_script_name': test_render_script.name})}"
        )
        assertHTMLEqual(
            page.locator("main").inner_html(),
            "xxl",
        )


def test_for_tag_unpack01(live_server, page: Page):
    with setup(
        script_name="for-tag-unpack01",
        template="{% for key,value in items %}{{ key }}:{{ value }}/{% endfor %}",
        js_context='{"items": [["one", 1], ["two", 2]]}',
    ) as test_render_script:
        page.goto(
            f"{live_server.url}{reverse('tag_test', kwargs={'render_script_name': test_render_script.name})}"
        )
        assertHTMLEqual(
            page.locator("main").inner_html(),
            "one:1/two:2/",
        )


def test_for_tag_unpack03(live_server, page: Page):
    with setup(
        script_name="for-tag-unpack01",
        template="{% for key, value in items %}{{ key }}:{{ value }}/{% endfor %}",
        js_context='{"items": [["one", 1], ["two", 2]]}',
    ) as test_render_script:
        page.goto(
            f"{live_server.url}{reverse('tag_test', kwargs={'render_script_name': test_render_script.name})}"
        )
        assertHTMLEqual(
            page.locator("main").inner_html(),
            "one:1/two:2/",
        )


@pytest.mark.skip("https://github.com/LilyFirefly/django-rusty-templates/issues/396")
def test_for_tag_unpack04(live_server, page: Page):
    with setup(
        script_name="for-tag-unpack04",
        template="{% for key , value in items %}{{ key }}:{{ value }}/{% endfor %}",
        js_context='{"items": [["one", 1], ["two", 2]]}',
    ) as test_render_script:
        page.goto(
            f"{live_server.url}{reverse('tag_test', kwargs={'render_script_name': test_render_script.name})}"
        )
        assertHTMLEqual(
            page.locator("main").inner_html(),
            "one:1/two:2/",
        )


@pytest.mark.skip("Error handling not implemented yet")
def test_for_tag_unpack06(live_server, page: Page):
    with setup(
        script_name="for-tag-unpack06",
        template="{% for key value in items %}{{ key }}:{{ value }}/{% endfor %}",
        js_context='{"items": [["one", 1], ["two", 2]]}',
    ) as test_render_script:
        page.goto(
            f"{live_server.url}{reverse('tag_test', kwargs={'render_script_name': test_render_script.name})}"
        )


@pytest.mark.skip("Error handling not implemented yet")
def test_for_tag_unpack07(live_server, page: Page):
    with setup(
        script_name="for-tag-unpack07",
        template="{% for key,,value in items %}{{ key }}:{{ value }}/{% endfor %}",
        js_context='{"items": [["one", 1], ["two", 2]]}',
    ) as test_render_script:
        page.goto(
            f"{live_server.url}{reverse('tag_test', kwargs={'render_script_name': test_render_script.name})}"
        )


@pytest.mark.skip("Error handling not implemented yet")
def test_for_tag_unpack08(live_server, page: Page):
    with setup(
        script_name="for-tag-unpack08",
        template="{% for key,value, in items %}{{ key }}:{{ value }}/{% endfor %}",
        js_context='{"items": [["one", 1], ["two", 2]]}',
    ) as test_render_script:
        page.goto(
            f"{live_server.url}{reverse('tag_test', kwargs={'render_script_name': test_render_script.name})}"
        )


@pytest.mark.skip("Error handling not implemented yet")
def test_for_tag_double_quote(live_server, page: Page):
    with setup(
        script_name="for-tag-double-quote",
        template='{% for "k" in items %}{{ "k" }}/{% endfor %}',
        js_context='{"items": [1, 2]}',
    ) as test_render_script:
        page.goto(
            f"{live_server.url}{reverse('tag_test', kwargs={'render_script_name': test_render_script.name})}"
        )


@pytest.mark.skip("Error handling not implemented yet")
def test_for_tag_single_quote(live_server, page: Page):
    with setup(
        script_name="for-tag-single-quote",
        template="{% for 'k' in items %}{{ k }}/{% endfor %}",
        js_context='{"items": [1, 2]}',
    ) as test_render_script:
        page.goto(
            f"{live_server.url}{reverse('tag_test', kwargs={'render_script_name': test_render_script.name})}"
        )


@pytest.mark.skip("Error handling not implemented yet")
def test_for_tag_vertical_bar(live_server, page: Page):
    with setup(
        script_name="for-tag-vertical-bar",
        template="{% for k|upper in items %}{{ k|upper }}/{% endfor %}",
        js_context='{"items": [1, 2]}',
    ) as test_render_script:
        page.goto(
            f"{live_server.url}{reverse('tag_test', kwargs={'render_script_name': test_render_script.name})}"
        )


def test_for_tag_unpack09(live_server, page: Page):
    with setup(
        script_name="for-tag-unpack09",
        template="{% for val in items %}{{ val.0 }}:{{ val.1 }}/{% endfor %}",
        js_context='{"items": [["one", 1], ["two", 2]]}',
    ) as test_render_script:
        page.goto(
            f"{live_server.url}{reverse('tag_test', kwargs={'render_script_name': test_render_script.name})}"
        )
        assertHTMLEqual(
            page.locator("main").inner_html(),
            "one:1/two:2/",
        )


def test_for_tag_unpack13(live_server, page: Page):
    with setup(
        script_name="for-tag-unpack13",
        template="{% for x,y,z in items %}{{ x }}:{{ y }},{{ z }}/{% endfor %}",
        js_context='{"items": [["one", 1, "carrot"], ["two", 2, "cheese"]]}',
    ) as test_render_script:
        page.goto(
            f"{live_server.url}{reverse('tag_test', kwargs={'render_script_name': test_render_script.name})}"
        )
        assertHTMLEqual(
            page.locator("main").inner_html(),
            "one:1,carrot/two:2,cheese/",
        )


def test_for_tag_empty01(live_server, page: Page):
    with setup(
        script_name="for-tag-empty01",
        template="{% for val in values %}{{ val }}{% empty %}empty text{% endfor %}",
        js_context='{"items": [1, 2, 3]}',
    ) as test_render_script:
        page.goto(
            f"{live_server.url}{reverse('tag_test', kwargs={'render_script_name': test_render_script.name})}"
        )
        assertHTMLEqual(
            page.locator("main").inner_html(),
            "empty text",
        )


def test_for_tag_empty02(live_server, page: Page):
    with setup(
        script_name="for-tag-empty02",
        template="{% for val in values %}{{ val }}{% empty %}values array empty{% endfor %}",
        js_context='{"items": []}',
    ) as test_render_script:
        page.goto(
            f"{live_server.url}{reverse('tag_test', kwargs={'render_script_name': test_render_script.name})}"
        )
        assertHTMLEqual(
            page.locator("main").inner_html(),
            "values array empty",
        )


def test_for_tag_empty03(live_server, page: Page):
    with setup(
        script_name="for-tag-empty03",
        template="{% for val in values %}{{ val }}{% empty %}values array not found{% endfor %}",
        js_context="{}",
    ) as test_render_script:
        page.goto(
            f"{live_server.url}{reverse('tag_test', kwargs={'render_script_name': test_render_script.name})}"
        )
        assertHTMLEqual(
            page.locator("main").inner_html(),
            "values array not found",
        )


def test_for_tag_unpack_strs(live_server, page: Page):
    with setup(
        script_name="for-tag-unpack-strs",
        template="{% for x,y in items %}{{ x }}:{{ y }}/{% endfor %}",
        js_context='{"items": ["ab", "ac"]}',
    ) as test_render_script:
        page.goto(
            f"{live_server.url}{reverse('tag_test', kwargs={'render_script_name': test_render_script.name})}"
        )
        assertHTMLEqual(
            page.locator("main").inner_html(),
            "a:b/a:c/",
        )


@pytest.mark.skip("Error handling not implemented yet")
def test_for_tag_unpack10(live_server, page: Page):
    with setup(
        script_name="for-tag-unpack10",
        template="{% for x,y in items %}{{ x }}:{{ y }}/{% endfor %}",
        js_context='{"items": [["one", 1, "carrot"], ["two", 2, "orange"]]}',
    ) as test_render_script:
        page.goto(
            f"{live_server.url}{reverse('tag_test', kwargs={'render_script_name': test_render_script.name})}"
        )


@pytest.mark.skip("Error handling not implemented yet")
def test_for_tag_unpack11(live_server, page: Page):
    with setup(
        script_name="for-tag-unpack11",
        template="{% for x,y,z in items %}{{ x }}:{{ y }},{{ z }}/{% endfor %}",
        js_context='{"items": [["one", 1], ["two", 2]]}',
    ) as test_render_script:
        page.goto(
            f"{live_server.url}{reverse('tag_test', kwargs={'render_script_name': test_render_script.name})}"
        )


@pytest.mark.skip("Error handling not implemented yet")
def test_for_tag_unpack12(live_server, page: Page):
    with setup(
        script_name="for-tag-unpack12",
        template="{% for x,y,z in items %}{{ x }}:{{ y }},{{ z }}/{% endfor %}",
        js_context='{"items": [["one", 1, "carrot"], ["two", 2]]}',
    ) as test_render_script:
        page.goto(
            f"{live_server.url}{reverse('tag_test', kwargs={'render_script_name': test_render_script.name})}"
        )


@pytest.mark.skip("Error handling not implemented yet")
def test_for_tag_unpack14(live_server, page: Page):
    with setup(
        script_name="for-tag-unpack14",
        template="{% for x,y in items %}{{ x }}:{{ y }}/{% endfor %}",
        js_context='{"items": [1, 2]}',
    ) as test_render_script:
        page.goto(
            f"{live_server.url}{reverse('tag_test', kwargs={'render_script_name': test_render_script.name})}"
        )


@pytest.mark.skip("Error handling not implemented yet")
def test_for_tag_invalid_in_keyword(live_server, page: Page):
    with setup(
        script_name="invalid_for_loop",
        template="{% for x from items %}{{ x }}{% endfor %}",
        js_context="{}",
    ) as test_render_script:
        page.goto(
            f"{live_server.url}{reverse('tag_test', kwargs={'render_script_name': test_render_script.name})}"
        )
