from django.views.generic.base import TemplateView


class TagTestTemplate(TemplateView):
    template_name = "tag_test_base.html"

    def setup(self, request, render_script_name: str, *args, **kwargs):
        self.script_name = f"{render_script_name.removesuffix('.js')}.js"
        super().setup(request, *args, **kwargs)

    def get_context_data(self, **kwargs):
        return super().get_context_data(**kwargs, script_template=self.script_name)
