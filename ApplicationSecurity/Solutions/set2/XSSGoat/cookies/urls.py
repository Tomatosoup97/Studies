from django.conf.urls import url

from . import views


urlpatterns = [
    url(r'^$', views.cookies_view, name='cookies'),
]
