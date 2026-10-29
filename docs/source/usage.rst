Quickstart
==========

Installation & configuration
----------------------------

First, install DTT from PyPI:

.. code-block:: shell

    pip install django-template-transpiler

DTT uses :mod:`Django's staticfiles app <django.contrib.staticfiles>` so you need to make sure it is enabled and
correctly configured in your Django project. You will need to add
:py:class:`django_template_transpiler.finders.DjangoTemplateTranspilerFinder` to :setting:`STATICFILES_FINDERS`.
If it's not already overridden in your ``settings.py``, you can declare like so:

.. code-block:: python

    from django.conf import global_settings

    STATICFILES_FINDERS = [
      *global_settings.STATICFILES_FINDERS,
      "django_template_transpiler.finders.DjangoTemplateTranspilerFinder",
    ]

It is important that you keep ``*global_settings.STATICFILES_FINDERS`` so that you do not remove the ``staticfiles``
app's default finders.

.. _transpiled-target:

Declare your templates
----------------------

DTT will only transpile the template you explicitly declare. You can specify the templates you want to be transpiled
by creating a ``transpile_targets.py`` at the root of any of your django apps, next to your ``urls.py`` or ``apps.py``:

.. code-block::
   :emphasize-lines: 6,17

    <your-django-project>
    ├── <your-django-project>
    │   ├── __init__.py
    │   ├── asgi.py
    │   ├── settings.py
    │   ├── transpile_targets.py
    │   ├── urls.py
    │   └── wsgi.py
    ├── <your-django-app>
    │   ├── __init__.py
    │   ├── apps.py
    │   ├── templates
    │   │   ├── people.html
    │   │   ├── forms
    │   │   │   └── identities.html
    │   │   └── parameters.html
    │   └── transpile_targets.py
    └── manage.py

.. code-block:: python
   :caption: In ``transpile_targets.py``

    targets = [
        "people.html",
        "forms/identities.html",
    ]

In this example, DTT will then generate ``people.js`` and ``forms/identities.js`` in the directory configured by
:setting:`STATIC_ROOT`. Files are generated either when :django-admin:`collectstatic` is called or on-the-fly, if
:setting:`DEBUG` is true.

Advanced usage
--------------

.. _dtt-script-extension:

Customize script extension
^^^^^^^^^^^^^^^^^^^^^^^^^^

By default, generated JS scripts will take ``.js`` extension. If you'd rather use another extension, for instance
``.cjs`` or ``.mjs``, you can do so by specifying the ``DTT_SCRIPT_EXTENSION`` setting:

.. code-block:: python
   :caption: In ``settings.py``

    DTT_SCRIPT_EXTENSION = ".js"

.. _fine-grained-script-generation:

Fine-grained script generation
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

The default behavior for script generation is to keep the template's directory layout and name and just replace the
extension by :ref:`the configured script extension <dtt-script-extension>`. So, if you specify ``forms/identities.html``
as a :ref:`transpile target <transpiled-target>`, DTT will generate ``forms/identities.js`` by default. If you want
to customize the path or name of the generated script, you can use :class:`django_template_transpiler.finders.target`:

.. code-block:: python
   :caption: In ``transpile_targets.py``

   from django_template_transpiler.finders import target

    targets = [
        target("people.html", script_name="party-people.js"),
        target("forms/identities.html", flat=True),
    ]

In this example, ``people.html`` will be transpiled into ``party-people.js``.

The ``flat`` parameter can be used to strip the template name from any leading directory. So ``forms/identities.html``
will be transpiled to ``identities.js``. If the ``script_name`` parameter is specified, the ``flat`` parameter and the
:ref:`DTT_SCRIPT_EXTENSION setting <dtt-script-extension>` are completely ignored.
