from django.conf.urls import url

from . import views


urlpatterns = [
    url(r'^$', views.CoreRouterView.as_view(), name='router'),
]
