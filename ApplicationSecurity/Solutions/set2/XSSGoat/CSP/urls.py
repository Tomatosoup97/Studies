from django.conf.urls import url, include

from rest_framework import routers

from . import views


router = routers.DefaultRouter()
router.register(
    r'voilation-reports',
    views.VoilationReportViewSet,
    base_name='reports',
)


urlpatterns = [
    url(r'^$', views.BasePageView.as_view(), name='base'),
    url(r'^api/', include(router.urls, namespace='api')),
]
