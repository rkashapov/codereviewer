import os
from os.path import dirname, join


STATIC_DIR = join(dirname(dirname(dirname(__file__))), 'static')
HOSTNAME = os.environ.get('HOSTNAME', 'localhost:8080')
PROTOCOL = os.environ.get('PROTOCOL', 'http')
DOMAIN = f'{PROTOCOL}://{HOSTNAME}'
GITHUB_CLIENT_ID = os.environ.get('GITHUB_CLIENT_ID')
GITHUB_CLIENT_SECRET = os.environ.get('GITHUB_CLIENT_SECRET')
SLACK_WORKSPACE_WEBHOOK_URL = os.environ.get('SLACK_WORKSPACE_WEBHOOK_URL')
