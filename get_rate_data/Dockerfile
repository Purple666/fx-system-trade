FROM python:3
ENV PYTHONUNBUFFERED 1
USER 1030:1002
WORKDIR /get_rate_data
RUN pip install -r requirements.txt
ENTRYPOINT ["./get_forexite.sh"]
        