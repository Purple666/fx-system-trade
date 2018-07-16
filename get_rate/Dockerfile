FROM python:3
ENV PYTHONUNBUFFERED 1
WORKDIR /python
COPY . /python
COPY requirements.txt /python
RUN pip install -r requirements.txt
ENTRYPOINT ["python3", "get_rate.py"]
        