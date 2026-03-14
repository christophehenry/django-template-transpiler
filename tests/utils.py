import contextlib
from pathlib import Path

from django.template import Template, Context

from django_template_transpiler import transpile

RENDER_SCRIPT_TPL = """
{% load static %}
import {Engine} from "{% static 'core.js' %}"
import render from "{% static render_script %}"

const engine = new Engine()
document.querySelector("#main").innerHTML = render(engine, {{ js_context|safe }})
"""


@contextlib.contextmanager
def setup(*, script_name: str, template: str, js_context: str):
    from django.conf import settings

    script_name = script_name.removesuffix(".js")

    static_dir = Path(settings.STATIC_ROOT)
    if not static_dir.is_absolute():
        static_dir = settings.BASE_DIR / static_dir
    static_dir.mkdir(parents=True, exist_ok=True)

    script_path = static_dir / f"{script_name}.js"
    with open(script_path, "w") as f:
        f.write(transpile(template))

    test_render_script = static_dir / f"{script_name}_render.js"
    with open(test_render_script, "w") as f:
        render_script_content = Template(RENDER_SCRIPT_TPL).render(
            Context({"render_script": script_path.name, "js_context": js_context})
        )
        f.write(render_script_content)

    yield test_render_script
