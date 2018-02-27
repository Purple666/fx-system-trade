FROM python:3
ENV PYTHONUNBUFFERED 1
WORKDIR /python
COPY requirements.txt /python
COPY get_rate_data_realtime /python
RUN pip install -r requirements.txt
ENTRYPOINT ["python", "get_rate.py"]
        