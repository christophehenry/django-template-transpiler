# Django Template Transpiler

DTT is a library to parse Django template and produce JS code to render them in the frontend.
It is not designed to replace either Django's template engine or any other frontend templating
framework like Vue or React. Instead, it is design to supplement DTL in situations where is is
not convenient or desirable to render parts or the UI on the server using a framework like
HTMX or Hotwired's Turbo.

## Rationale

Django provides a lot of tools to handle different situations. For instance
[formsets](https://docs.djangoproject.com/en/5.2/topics/forms/formsets/) to handle multiple
forms on the same page. However, there are situations where Django's tools fail to meet the
need. For instance, when you find yourself working with a designer who's feeling fancy and
design components that formsets are particularly unsuited to render. In these situations,
you may end up writing a nice custom template. However, you may also end up with a template
that's too complex to render on the front by just replacing the
[formset's `empty_form` `__prefix__`](https://docs.djangoproject.com/en/5.2/topics/forms/formsets/#empty-form)
placeholder.

In the recent years, in a situation like this, the Django community turned to a pattern
most notoriously [reprensented by HTMX](https://htmx.org/essays/template-fragments/) where
parts of the UI can be updated or refreshed by issuing HTTP requests and doing SSR.

However, as of today, there are still parts of the world were internet access is severely
limited by slow bandwidth, access quotas or some other form of resource cost. In these
situation, you can't afford to perform an HTTP request each time you want to add a new
subform on your page.

This is Django Template Transpiler comes into light, by allowing you to use your previous
Django template as a single source of truth for both the front and the back. Very basically
Django Template Transpiler parses a Django template and converts it into a JS render function
to use on the front to update small parts of the UI.

## State

DTT is still in development and absolutely not usable as of now. Any contribution will be
warmly welcomed.

## Acknowledgments

DTT heavily relies on [`django-rusty-templates`](https://github.com/LilyFirefly/django-rusty-templates)
for parsing templates—to the point that is it basically a fork of it. It also relies on
[oxc](https://github.com/oxc-project/oxc) to generate JS code from the parsed template.

## TODO

1. Basically [every built-in template tags and filters](https://docs.djangoproject.com/en/5.2/ref/templates/builtins/).
2. Support [`string_if_invalid`](https://docs.djangoproject.com/en/5.2/ref/templates/api/#invalid-template-variables):
   when a variable is not found in context, the engine uses this option as a default value instead. This affects
   the `default` filter.
3. Support controlling autoescape.
