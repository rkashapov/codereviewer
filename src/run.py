#!/usr/bin/env python
import os
import base64
import logging
from functools import wraps, partial
from urllib.parse import quote

from aiohttp import web
from aiohttp_security import is_anonymous
from aiohttp_session import setup
from aiohttp_session.cookie_storage import EncryptedCookieStorage
from aiohttp_security import setup as setup_security
from cryptography import fernet

from backend import config
from backend import auth
from backend import api


app = web.Application()


def init_session(app):
    fernet_key = fernet.Fernet.generate_key()
    secret_key = base64.urlsafe_b64decode(fernet_key)
    setup(app, EncryptedCookieStorage(secret_key))


def init_security(app):
    policy = auth.SessionIdentityPolicy()
    setup_security(app, policy, auth.UserAuthenticationPolicy())


def init_router(app):
    app.router.add_get('/oauth', handler=auth.oauth, name='oauth')
    app.router.add_get(
        '/oauth/callback',
        handler=auth.oauth_callback,
        name='oauth_callback',
    )
    app.router.add_routes([web.static('/static', config.STATIC_DIR)])
    app.router.add_route(method='*', path='/gql', handler=api.gql_view)
    app.router.add_get('/{path:.*}', handler=index, name='index')


def auth_required(response=None, view=None):
    if view is None:
        return partial(auth_required, response=response)

    @wraps(view)
    async def wrapper(request, *args, **kwargs):
        if await is_anonymous(request):
            return response or web.HTTPNotFound()
        return await view(request, *args, **kwargs)

    return wrapper


async def index(request):
    if await is_anonymous(request):
        next_url = quote(request.path_qs)
        return web.HTTPFound(f'/oauth?next={next_url}')
    return web.Response(body=INDEX, content_type='text/html')


INDEX = '''<!DOCTYPE html>
<html lang="en">
<head>
  <meta charset="UTF-8">
  <meta name="viewport" content="width=device-width, initial-scale=1.0">
  <style>
    .elmsh { background-color: transparent !important; }
  </style>
</head>
<body>
  <div id="app"></div>
</body>
  <script src="/static/bundle.js.gz"></script>
</html>
'''


if __name__ == '__main__':
    logging.basicConfig(level=logging.DEBUG)
    init_session(app)
    init_router(app)
    init_security(app)
    web.run_app(app)
