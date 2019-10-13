FROM redis
COPY redis.conf /etc/redis.conf
USER 0        
ENTRYPOINT ["/usr/local/bin/redis-server", "/etc/redis.conf"]

        