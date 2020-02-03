FROM python
ENV PYTHONUNBUFFERED 1
COPY openssl.cnf /etc/ssl/openssl.cnf
WORKDIR /python
COPY . /python
COPY requirements.txt /python
RUN pip install -r requirements.txt
ENTRYPOINT ["python3", "get_rate.py"]
        