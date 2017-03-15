from django.views.generic import TemplateView
from django.conf import settings
from django.shortcuts import render, render_to_response

from cryptography.fernet import Fernet


def encrypt(value, key):
    key = key.encode('base-64')
    f = Fernet(key)
    ciphertext = f.encrypt(bytes(value))
    return ciphertext


def cookies_view(request):
    response = render(request, 'cookies/cookies.html', {}) 

    if request.method == 'POST':
        response.set_cookie('confidential-data', request.POST['text'])
        response.set_cookie('is-pope', request.POST['check'])
        
        encrypted_data = encrypt(request.POST['text'], settings.SECRET_KEY)
        encrypted_pope = encrypt(request.POST['text'], settings.SECRET_KEY)
        response.set_cookie('data-encrypted', encrypted_data)
        response.set_cookie('pope-encrypted', encrypted_pope)

    return response
