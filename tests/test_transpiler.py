from django.urls import reverse


def test_transpiler(live_server, page, transpile_template, render_with_context):
    import_file = transpile_template("full_example.html")
    page.goto(f"{live_server}{reverse('home')}")

    render_with_context(import_file, {"bar": "baz"})

    assert page.locator("#main").inner_html() == (
        "<p>\n    1\n    1\n    bar\n    baz\n    hello\n    Some text"
        "\n    baz\n    baz\n    \n    \n    \n    \n    \n    \n    \n</p>\n"
    )
