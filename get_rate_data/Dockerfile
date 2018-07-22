FROM python:3
ENV PYTHONUNBUFFERED 1
WORKDIR /get_rate_data
COPY requirements.txt /get_rate_data
RUN pip install -r requirements.txt
ENTRYPOINT ["sleep", "10000"]
#ENTRYPOINT ["./get_forexite.sh"]
        