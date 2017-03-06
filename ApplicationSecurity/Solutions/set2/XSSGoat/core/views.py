from django.shortcuts import render
from django.views.generic import TemplateView


class CoreRouterView(TemplateView):
    template_name = 'router.html'
