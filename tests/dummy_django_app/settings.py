from pathlib import Path

from django.conf import global_settings

DEBUG = True

BASE_DIR = Path(__file__).parent

SECRET_KEY = "test"

INSTALLED_APPS = [
    "django.contrib.staticfiles",
    "django_template_transpiler",
    "tests.dummy_django_app",
]

ROOT_URLCONF = "tests.dummy_django_app.urls"

TEMPLATES = [
    {
        "BACKEND": "django.template.backends.django.DjangoTemplates",
        "DIRS": ["tests/dummy_django_app/templates"],
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
STATICFILES_FINDERS = [
    *global_settings.STATICFILES_FINDERS,
    "django_template_transpiler.finders.DjangoTemplateTranspilerFinder",
]
