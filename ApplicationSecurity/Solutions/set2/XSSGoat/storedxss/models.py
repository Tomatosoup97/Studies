from django.db import models


class Comment(models.Model):
    title = models.CharField(max_length=128)
    text = models.TextField()
