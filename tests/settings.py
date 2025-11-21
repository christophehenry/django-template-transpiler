from importlib.resources import files as importlib_resources
from pathlib import Path

import django_template_transpiler

DEBUG = True

BASE_DIR = Path(__file__).parent

SECRET_KEY = "test"

INSTALLED_APPS = [
    "django.contrib.staticfiles",
    "tests.apps.DummyAppConfig",
]

ROOT_URLCONF = "tests.urls"

TEMPLATES = [
    {
        "BACKEND": "django.template.backends.django.DjangoTemplates",
        "DIRS": ["tests/templates"],
        "APP_DIRS": True,
        "OPTIONS": {
            "context_processors": [
                "django.template.context_processors.request",
            ],
        },
    },
]

DATABASES = {
    "default": {
        "ENGINE": "django.db.backends.sqlite3",
        "NAME": BASE_DIR / "db.sqlite3",
    }
}

STATIC_URL = "static/"
STATIC_ROOT = BASE_DIR / "static"
STATICFILES_DIRS = [importlib_resources(django_template_transpiler).joinpath("data")]
