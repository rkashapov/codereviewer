from . import github


# super-fast document database
db = {
    'user': {},
    'comment': {},
    'diff': {},
}


async def create_or_update(access_token):
    profile = await github.user(access_token=access_token)
    user = db['user'][profile['id']] = {
        'profile': profile,
        'access_token': access_token,
    }
    return user


def find_user(github_id):
    if user := db['user'].get(int(github_id)):
        return user


def upsert_comment(sha, lineno, author_id, text):
    for diff in db['diff'].values():
        for file in diff['files']:
            if file['sha'] == sha:
                comments = diff.setdefault('comments', [])
                comments.append((sha, lineno, author_id, text))
                return diff, file


def find_diff(owner, repository, base, head):
    key = (owner, repository, base, head)
    return db['diff'].get(key)


def save_diff(owner, repository, base, head, diff):
    key = (owner, repository, base, head)
    db['diff'][key] = diff
