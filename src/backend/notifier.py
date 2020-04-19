import re
import itertools

from aiohttp import ClientSession

from . import config


class Patch:
    header_regex = re.compile(r'@@ -(\d+),(\d+) \+(\d+),(\d+) @@(.*)')

    def __init__(self, patch):
        self.patch = list(self._with_lineno(patch.split('\n')))

    def highlight(self, lineno, context=4):
        start, end = max(0, lineno - context), lineno + context
        fragment = itertools.chain(
            reversed(list(itertools.takewhile(
                lambda x: x is not None,
                self.patch[lineno-1:start:-1],
            ))),
            itertools.takewhile(
                lambda x: x is not None,
                self.patch[lineno:end],
            ),
        )

        lines = []
        for current_lineno, base_lineno, head_lineno, line in fragment:
            if line.startswith('-'):
                head_lineno = ''
            elif line.startswith('+'):
                base_lineno = ''
            selected = '*' if current_lineno == lineno else ' '
            formatted = f'{base_lineno:2} {head_lineno:<2} {selected} {line}'
            lines.append(formatted)
        return '\n'.join(lines)

    def _with_lineno(self, patch):
        current_line = 0
        base_start = head_start = 0
        while current_line < len(patch):
            patch_line = patch[current_line]
            if patch_line.startswith('@@'):
                match = self.header_regex.match(patch_line)
                base_start, _, head_start, _, _ = match.groups()
                base_start = int(base_start)
                head_start = int(head_start)
                current_line += 1
                yield None
                continue

            yield current_line, base_start, head_start, patch_line

            if patch_line.startswith('-'):
                base_start += 1
            elif patch_line.startswith('+'):
                head_start += 1
            else:
                base_start += 1
                head_start += 1
            current_line += 1


async def send_message(viewer, diff, file_, lineno, text):
    patch = Patch(file_['patch'])
    code = patch.highlight(lineno)
    lineno_head = patch.patch[lineno][2]

    commit = diff['commits'][0]
    author = commit['author']
    author_name = commit['commit']['author']['name']

    async with ClientSession() as session:
        message = {
            "blocks": [
                {
                    "type": "context",
                    "elements": [
                        {
                            "type": "image",
                            "image_url": viewer['avatar_url'],
                            "alt_text": viewer['name'],
                        },
                        {
                            "type": "plain_text",
                            "text": viewer['name'],
                        },
                        {
                            "type": "image",
                            "image_url": author['avatar_url'],
                            "alt_text": author_name,
                        },
                        {
                            "type": "plain_text",
                            "text": author_name,
                        },
                    ],
                },
                {
                    "type": "section",
                    "text": {
                        "type": "mrkdwn",
                        "text": text,
                    },
                },
                {
                    "type": "divider",
                },
                {
                    "type": "section",
                    "text": {
                        'type': 'mrkdwn',
                        'text':
                            f'<{file_["blob_url"]}#L{lineno_head}|'
                            f'{file_["filename"]}>',
                    },
                },
                {
                    "type": "section",
                    "text": {
                        "type": "mrkdwn",
                        "text": f'```{code}```',
                    },
                }
        ],
        }
        response = await session.post(
            config.SLACK_WORKSPACE_WEBHOOK_URL,
            json=message,
        )
        return await response.text()
