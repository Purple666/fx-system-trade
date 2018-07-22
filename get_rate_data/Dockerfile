FROM python:3
ENV PYTHONUNBUFFERED 1
RUN apt-get update && apt-get unzip        
WORKDIR /python
COPY requirements.txt /python
RUN pip install -r requirements.txt
WORKDIR /get_rate_data
USER 1030:1002
ENTRYPOINT ["./get_forexite.sh"]
        