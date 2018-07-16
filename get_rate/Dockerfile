FROM python:3
ENV PYTHONUNBUFFERED 1
WORKDIR /python
COPY requirements.txt /python
RUN pip install -r requirements.txt
ENTRYPOINT ["python", "get_rate.py"]
        