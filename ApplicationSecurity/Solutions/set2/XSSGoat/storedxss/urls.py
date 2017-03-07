from django.conf.urls import url, include

from . import views


urlpatterns = [
    url(r'^create/$', views.CommentsFormView.as_view(), name='comments-create'),
    url(r'^$', views.CommentsListView.as_view(), name='comments-list'),
]
