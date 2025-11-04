from importlib.resources import files as importlib_resources
from pathlib import Path

import django_rusty_templates

DEBUG = True

BASE_DIR = Path(__file__).parent

SECRET_KEY = "test"

INSTALLED_APPS = [
    "django.contrib.staticfiles",
    "tests.apps.DummyAppConfig",
]

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

ROOT_URLCONF = "tests.urls"

STATIC_URL = "static/"
STATIC_ROOT = BASE_DIR / "static"
STATICFILES_DIRS = [importlib_resources(django_rusty_templates).joinpath("data")]
