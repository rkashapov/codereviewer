FROM python:3.8.2-slim-buster

ADD ./requirements.txt /tmp/reqirements.txt
RUN pip install -r /tmp/reqirements.txt
ADD src /code
ADD static /static
WORKDIR /code
CMD python run.py