from django.conf.urls import url

from . import views


urlpatterns = [
    url(r'^$', views.LocationView.as_view(), name='dom'),
]
