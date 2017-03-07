from django.db import models


class Comment(models.Model):
    title = models.CharField(max_length=128)
    text = models.TextField()

    def __str__(self):
        return 'Comment: {}...'.format(self.title[:32])
