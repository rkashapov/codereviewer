from aiohttp import ClientSession


AUTHORIZE_URL = 'https://github.com/login/oauth/authorize'
ACCESS_TOKEN_URL = 'https://github.com/login/oauth/access_token'
GET_USER_URL = 'https://api.github.com/user'
COMPARE_COMMITS_URL = 'https://api.github.com/repos/{owner}/{repo}/compare/{base}...{head}'


async def request(url, access_token):
    async with ClientSession() as session:
        headers = {'Authorization': f'token {access_token}'}
        async with session.get(url, headers=headers) as resp:
            return await resp.json()


async def user(*, access_token):
    return await request(GET_USER_URL, access_token)


async def compare(owner, repo, base, head, *, access_token):
    args = {
        'owner': owner,
        'repo': repo,
        'base': base,
        'head': head,
    }
    url = COMPARE_COMMITS_URL.format(**args)
    return await request(url, access_token)
