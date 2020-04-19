from urllib.parse import urlencode, urlparse, quote, urljoin

from aiohttp import web, ClientSession
from aiohttp_security import is_anonymous, remember, \
    SessionIdentityPolicy
from aiohttp_security.abc import AbstractAuthorizationPolicy

from . import config
from . import github
from . db import create_or_update, find_user


__all__ = [
    'SessionIdentityPolicy',
    'UserAuthenticationPolicy',
    'oauth',
    'oauth_callback',
]


STATE = 'reviewer'


def is_safe_url(url):
    netloc = urlparse(url).netloc
    return not netloc or netloc == config.HOSTNAME


async def oauth(request):
    if not await is_anonymous(request):
        return web.HTTPFound('/')

    next_url = request.query.get('next')
    if not is_safe_url(next_url):
        return web.HTTPForbidden()

    query = urlencode(dict(
        client_id=config.GITHUB_CLIENT_ID,
        redirect_uri=config.DOMAIN + f'/oauth/callback?next={quote(next_url)}',
        scope='repo',
        state=STATE,
    ))
    url = github.AUTHORIZE_URL + '?' + query
    return web.HTTPFound(url)


async def oauth_callback(request):
    code = request.query.get('code')
    state = request.query.get('state')
    next_url = request.query.get('next')
    if not is_safe_url(next_url):
        return web.HTTPForbidden()

    if not code or state != STATE:
        return web.HTTPUnauthorized()

    async with ClientSession() as session:
        data = {
            'client_id': config.GITHUB_CLIENT_ID,
            'client_secret': config.GITHUB_CLIENT_SECRET,
            'code': code,
        }
        headers = {'Accept': 'application/json'}
        async with session.post(
            github.ACCESS_TOKEN_URL,
            json=data,
            headers=headers,
        ) as resp:
            if resp.status != 200:
                return web.HTTPUnauthorized()
            
            data = await resp.json()

    access_token = data.get('access_token')
    if not access_token:
        return web.HTTPUnauthorized()
    user = await create_or_update(access_token)
    if user is None:
        return web.HTTPUnauthorized()

    redirect_response = web.HTTPFound(next_url)
    await remember(request, redirect_response, str(user['profile']['id']))
    return redirect_response


class UserAuthenticationPolicy(AbstractAuthorizationPolicy):
    async def authorized_userid(self, identity):
        if find_user(identity):
            return identity

    async def permits(self, identity, permission, context=None):
        return True
