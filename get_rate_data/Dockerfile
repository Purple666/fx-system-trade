FROM python:3
ENV PYTHONUNBUFFERED 1
WORKDIR /get_rate_data
RUN pip install -r requirements.txt
ENTRYPOINT ["./get_forexite.sh"]
        