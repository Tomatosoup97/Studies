from django.db import models
from django.contrib.postgres.fields import JSONField


class VoilationReport(models.Model):
    created = models.DateTimeField(auto_now=True)
    data = JSONField()

    def __str__(self):
        return "CSP Voilation report {}".format(self.id)
