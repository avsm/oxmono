# Official multi-platform manifest, resolved on 2026-09-07.
FROM httpd:2.4.68-trixie@sha256:979c38c2228d28c2edfd45c6e27dcee1c7b4a101a5526721ae8ece454e89e99e

COPY httpd.conf conf/httpd.conf
COPY start.sh /usr/local/bin/start-webdav
RUN htpasswd -bcB conf/test-users alice alice-password \
    && htpasswd -bB conf/test-users bob bob-password \
    && openssl req -x509 -newkey rsa:2048 -nodes -days 3650 \
         -subj '/CN=Fetch DAV fixture CA' \
         -addext 'basicConstraints=critical,CA:TRUE' \
         -addext 'keyUsage=critical,keyCertSign,cRLSign' \
         -keyout /tmp/test-ca-key.pem -out conf/test-ca.pem \
    && openssl req -new -newkey rsa:2048 -nodes -subj /CN=localhost \
         -keyout conf/test-key.pem -out /tmp/test-csr.pem \
    && printf 'subjectAltName=DNS:localhost,IP:127.0.0.1\nbasicConstraints=critical,CA:FALSE\nextendedKeyUsage=serverAuth\n' > /tmp/test-ext.cnf \
    && openssl x509 -req -in /tmp/test-csr.pem -CA conf/test-ca.pem \
         -CAkey /tmp/test-ca-key.pem -CAcreateserial -days 3650 \
         -extfile /tmp/test-ext.cnf -out conf/test-cert.pem \
    && rm /tmp/test-ca-key.pem /tmp/test-csr.pem /tmp/test-ext.cnf conf/test-ca.srl \
    && chown www-data:www-data conf/test-key.pem \
    && chmod 600 conf/test-key.pem \
    && printf 'ready\n' > htdocs/health.txt \
    && chmod 755 /usr/local/bin/start-webdav

USER www-data
CMD ["/usr/local/bin/start-webdav"]
