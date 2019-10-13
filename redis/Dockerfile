FROM redis
COPY redis.conf /etc/redis.conf
RUN mkdir -p /var/run/redis        
ENTRYPOINT ["/usr/local/bin/redis-server", "/etc/redis.conf"]
        