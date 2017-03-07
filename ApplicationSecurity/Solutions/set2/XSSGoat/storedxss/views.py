from django.views.generic import ListView
from django.views.generic.edit import CreateView
from django.core.urlresolvers import reverse_lazy
from .models import Comment
from .forms import CommentForm


class CommentsFormView(CreateView):
    model = Comment
    form_class = CommentForm
    template_name = 'comments-form.html'
    success_url = reverse_lazy('storedxss:comments-list')


class CommentsListView(ListView):
    model = Comment
    context_object_name = 'comments'
    template_name = 'comments-list.html'
