FROM redis
COPY redis.conf /etc/redis.conf
USER 999
ENTRYPOINT ["/usr/local/bin/redis-server", "/etc/redis.conf"]
