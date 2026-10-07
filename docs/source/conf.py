import tomllib
from sphinx_pyproject import SphinxConfig

with open("../../Cargo.toml", "rb") as f:
    cargo_toml = tomllib.load(f)

config = SphinxConfig(
    "../../pyproject.toml",
    globalns=globals(),
    config_overrides={
        "name": "Django Template Transpiler",
        "version": cargo_toml["package"]["version"],
    },
)

extensions = ["myst_parser", "sphinx_rtd_theme"]

templates_path = ["_templates"]
exclude_patterns = []

html_theme = "sphinx_rtd_theme"
html_static_path = ["_static"]

source_suffix = {
    ".rst": "restructuredtext",
    ".md": "markdown",
}
