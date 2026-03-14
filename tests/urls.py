from django.urls import path

from . import views

urlpatterns = [
    path(
        "<path:render_script_name>/", views.TagTestTemplate.as_view(), name="tag_test"
    ),
]
