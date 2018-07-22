FROM python:3
ENV PYTHONUNBUFFERED 1
WORKDIR /python
COPY requirements.txt /python
RUN pip install -r requirements.txt
WORKDIR /get_rate_data
ENTRYPOINT ["sleep", "10000"]
#ENTRYPOINT ["./get_forexite.sh"]
        