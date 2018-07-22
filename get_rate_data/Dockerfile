FROM python:3
ENV PYTHONUNBUFFERED 1
WORKDIR /get_rate_data
RUN pip install -r requirements.txt
USER 1030:1002
ENTRYPOINT ["./get_forexite.sh"]
        