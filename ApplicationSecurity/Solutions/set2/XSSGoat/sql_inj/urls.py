from django.conf.urls import url, include

from . import views


urlpatterns = [
    url(r'^$', views.search_event, name='events-list'),
]
