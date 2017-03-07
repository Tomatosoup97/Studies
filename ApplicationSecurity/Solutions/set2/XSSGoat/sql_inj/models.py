from django.contrib.auth.models import User
from django.db import models


class Event(models.Model):
    title = models.CharField(max_length=64)
    description = models.TextField()
    date = models.DateField()
    user = models.ForeignKey(User)

    def __str__(self):
        return 'Event: {}'.format(self.title)

