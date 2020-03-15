FROM redis
COPY redis.conf /etc/redis.conf
USER redis
ENTRYPOINT ["/usr/local/bin/redis-server", "/etc/redis.conf"]
        