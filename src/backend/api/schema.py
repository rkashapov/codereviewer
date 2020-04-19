import graphene
from aiohttp_security import authorized_userid

from ..db import find_user, upsert_comment, find_diff, save_diff
from .. import github
from .. import notifier


class Change(graphene.ObjectType):
    sha = graphene.String(required=True)
    filename = graphene.String(required=True)
    additions = graphene.Int(required=True)
    deletions = graphene.Int(required=True)
    changes = graphene.Int(required=True)
    blob_url = graphene.String(required=True)
    raw_url = graphene.String(required=True)
    contents_url = graphene.String(required=True)
    patch = graphene.String(required=True)


class Comment(graphene.ObjectType):
    sha = graphene.String(required=True)
    lineno = graphene.Int(required=True)
    author = graphene.Field('backend.api.schema.User', required=True)
    text = graphene.String(required=True)


class GitCommitPerson(graphene.ObjectType):
    name = graphene.String(required=True)
    email = graphene.String(required=True)


class GitCommit(graphene.ObjectType):
    url = graphene.String(required=True)
    author = graphene.Field(GitCommitPerson, required=True)
    committer = graphene.Field(GitCommitPerson, required=True)
    message = graphene.String(required=True)


class Commit(graphene.ObjectType):
    url = graphene.String(required=True)
    html_url = graphene.String(required=True)
    commit = graphene.Field(GitCommit, required=True)
    author = graphene.Field('backend.api.schema.User', required=True)


class Diff(graphene.ObjectType):
    url = graphene.String(required=True)
    files = graphene.List(Change, required=True)
    comments = graphene.List(Comment, required=True)
    commits = graphene.List(Commit, required=True)


class Repository(graphene.ObjectType):
    name = graphene.String(required=True)
    owner = graphene.Field('backend.api.schema.Organization', required=True)
    compare = graphene.Field(
        Diff,
        base=graphene.String(required=True),
        head=graphene.String(required=True),
        required=True,
    )

    async def resolve_compare(self, info, base, head):
        github_id = await authorized_userid(info.context['request'])
        diff = find_diff(self.owner.name, self.name, base, head)
        if not diff:
            user = find_user(github_id)
            access_token = user['access_token']
            diff = await github.compare(
                self.owner.name,
                self.name,
                base,
                head,
                access_token=access_token,
            )
            save_diff(self.owner.name, self.name, base, head, diff)
        files = [
            Change(
                sha=change['sha'],
                filename=change['filename'],
                additions=change['additions'],
                deletions=change['deletions'],
                changes=change['changes'],
                blob_url=change['blob_url'],
                raw_url=change['raw_url'],
                contents_url=change['contents_url'],
                patch=change.get('patch', ''),
            ) for change in diff['files']
        ]
        return Diff(
            url=diff['url'],
            files=files,
            comments=[
                Comment(
                    sha=sha,
                    lineno=lineno,
                    author=User(
                        id=find_user(author_id)['profile']['id'],
                        name=find_user(author_id)['profile']['name'],
                        email=find_user(author_id)['profile']['email'],
                        login=find_user(author_id)['profile']['login'],
                        avatar_url=find_user(author_id)['profile']['avatar_url'],
                    ),
                    text=text,
                ) for (sha, lineno, author_id, text) in
                diff.get('comments', [])
            ],
            commits=[
                Commit(
                    url=commit['url'],
                    html_url=commit['html_url'],
                    commit=GitCommit(
                        url=commit['commit']['url'],
                        message=commit['commit']['message'],
                        author=GitCommitPerson(
                            name=commit['commit']['author']['name'],
                            email=commit['commit']['author']['email'],
                        ),
                        committer=GitCommitPerson(
                            name=commit['commit']['committer']['name'],
                            email=commit['commit']['committer']['email'],
                        ),
                    ),
                    author=User(
                        id=commit['author']['id'],
                        name=commit['commit']['author']['name'],
                        email=commit['commit']['author']['email'],
                        login=commit['author']['login'],
                        avatar_url=commit['author']['avatar_url'],
                    ),
                )
                for commit in diff['commits']
            ]
        )


class Organization(graphene.ObjectType):
    name = graphene.String(required=True)
    repository = graphene.Field(
        Repository,
        name=graphene.String(required=True),
        required=True,
    )

    async def resolve_repository(self, info, name):
        return Repository(name=name, owner=self)


class User(graphene.ObjectType):
    id = graphene.ID(required=True)
    name = graphene.String()
    email = graphene.String()
    login = graphene.String()
    avatar_url = graphene.String()
    organization = graphene.Field(
        Organization,
        name=graphene.String(required=True),
        required=True,
    )

    async def resolve_organization(self, info, name):
        return Organization(name=name)


class Query(graphene.ObjectType):
    viewer = graphene.Field(User, required=True)

    async def resolve_viewer(self, info):
        github_id = await authorized_userid(info.context['request'])
        user = find_user(github_id)
        viewer = user['profile']
        return User(
            id=viewer['id'],
            name=viewer['name'],
            email=viewer['email'],
            login=viewer['login'],
            avatar_url=viewer['avatar_url'],
        )


class UpsertComment(graphene.Mutation):
    class Arguments:
        sha = graphene.String(required=True)
        lineno = graphene.Int(required=True)
        text = graphene.String(required=True)

    Output = Comment

    async def mutate(self, info, sha, lineno, text):
        github_id = await authorized_userid(info.context['request'])
        diff, file_ = upsert_comment(sha, lineno, github_id, text)
        author = find_user(github_id)['profile']
        await notifier.send_message(author, diff, file_, lineno, text)
        return Comment(
            author=User(
                id=author['id'],
                name=author['name'],
                email=author['email'],
                login=author['login'],
                avatar_url=author['avatar_url'],
            ),
            sha=sha,
            lineno=lineno,
            text=text,
        )


class Mutation(graphene.ObjectType):
    upsert_comment = UpsertComment.Field()


schema = graphene.Schema(query=Query, mutation=Mutation)
