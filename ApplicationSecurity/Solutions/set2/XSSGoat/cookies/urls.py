from django.conf.urls import url

from . import views


urlpatterns = [
    url(r'^$', views.CookiesView.as_view(), name='cookies'),
]
