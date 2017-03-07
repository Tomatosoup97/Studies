from rest_framework import serializers

from .models import VoilationReport


class VoilationReportSerializer(serializers.ModelSerializer):
    class Meta:
        model = VoilationReport
        fields = ('id', 'data', 'created',)
        readonly_fields = ('created',)
