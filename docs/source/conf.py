import tomllib

with open("../../Cargo.toml", "rb") as f:
    cargo_toml = tomllib.load(f)

project = "Django Template Transpiler"
version = cargo_toml["package"]["version"]
author = "Christophe Henry"

extensions = ["sphinxcontrib_django", "sphinx.ext.autodoc", "myst_parser", "sphinx_rtd_theme"]

templates_path = ["_templates"]
exclude_patterns = []

html_theme = "sphinx_rtd_theme"
html_static_path = ["_static"]

source_suffix = {
    ".rst": "restructuredtext",
    ".md": "markdown",
}

django_settings = "django_template_transpiler.settings"
