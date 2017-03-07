from django.conf.urls import url, include
from django.contrib import admin


urlpatterns = [
    url(r'^api-auth/', include('rest_framework.urls', namespace='rest_framework')),
    url(r'^admin/', admin.site.urls),
    url(r'^csp/', include('CSP.urls', namespace='csp')),
    url(r'^xssheader/', include('xssheader.urls', namespace='xssheader')),
    url(r'^storedxss/', include('storedxss.urls', namespace='storedxss')),
    url(r'^', include('core.urls', namespace='core')),
]
