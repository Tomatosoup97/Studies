from django.conf.urls import url, include

from . import views


urlpatterns = [
    url(r'^$', views.UserWelcomeView.as_view(), name='user-welcome-page'),
]
