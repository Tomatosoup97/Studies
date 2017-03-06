from django.shortcuts import render
from django.views.generic import TemplateView


class BasePageView(TemplateView):
    template_name = 'csp.html'
