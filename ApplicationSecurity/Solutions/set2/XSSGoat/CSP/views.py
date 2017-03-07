from django.utils import timezone
from django.views.generic import TemplateView

from rest_framework import viewsets
from rest_framework import status
from rest_framework.response import Response

from .models import VoilationReport
from .serializers import VoilationReportSerializer
from .parsers import ReportParser


class BasePageView(TemplateView):
    template_name = 'csp.html'


class VoilationReportViewSet(viewsets.ModelViewSet):
    serializer_class = VoilationReportSerializer
    queryset = VoilationReport.objects.all()
    parser_classes = (ReportParser,)

    def create(self, request, *args, **kwargs):
        data = {'data': request.data.pop('csp-report')}
        serializer = self.get_serializer(data=data)
        serializer.is_valid(raise_exception=True)
        self.perform_create(serializer)
        headers = self.get_success_headers(serializer.data)
        return Response(serializer.data,
                        status=status.HTTP_201_CREATED,
                        headers=headers)

    def perform_create(self, serializer):
        serializer.save(created=timezone.now())
