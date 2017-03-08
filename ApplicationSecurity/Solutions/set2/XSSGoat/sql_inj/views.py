from django.shortcuts import render
from django.http.request import HttpRequest

from .models import Event


def search_event(request):
    if not request.user.id:
        return render(request, 'authenticate.html')

    if 'start_date' in request.GET and 'end_date' in request.GET:
        query = "SELECT * FROM sql_inj_event WHERE "\
                "sql_inj_event.user_id={0} AND "\
                "sql_inj_event.date BETWEEN '{1}' AND '{2}'".format(
                    request.user.id,
                    request.GET.get('start_date'),
                    request.GET.get('end_date'))
        events = Event.objects.raw(query)
    else:
        query = 'SELECT * FROM sql_inj_event WHERE '\
                'sql_inj_event.user_id={}'.format(request.user.id)
        events = Event.objects.raw(query)

    return render(request, 'events-list.html', {'events': events})
