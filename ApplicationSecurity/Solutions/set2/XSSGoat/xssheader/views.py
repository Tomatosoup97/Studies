from django.views.generic import TemplateView


class UserWelcomeView(TemplateView):
    template_name = 'user_welcome.html'

    def get(self, request, *args, **kwargs):
        context = self.get_context_data(**kwargs)
        context['username'] = request.GET.get('username') or 'mysterious user'
        return self.render_to_response(context)
