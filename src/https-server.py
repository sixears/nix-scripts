#!/usr/bin/env nix-shell
#!nix-shell -i python -p python38 python38Packages.httpserver

# https://stackoverflow.com/questions/36636715/unable-to-connect-to-the-simple-https-server
# https://github.com/jupyter/notebook/issues/507
# https://stackoverflow.com/questions/30109449/what-does-sslerror-ssl-pem-lib-ssl-c2532-mean-using-the-python-ssl-libr
# https://stackoverflow.com/questions/7943751/what-is-the-python-3-equivalent-of-python-m-simplehttpserver/7943768#7943768
# https://docs.python.org/3/library/ssl.html#ssl.SSLContext.wrap_socket

from http.server import HTTPServer, SimpleHTTPRequestHandler
import ssl

# openssl req -x509 -nodes -newkey rsa:2048 -keyout key.pem -out cert.pem -days 10

httpd = HTTPServer(('localhost', 4443), SimpleHTTPRequestHandler)
httpd.socket = ssl.wrap_socket (httpd.socket, certfile='trance.sixears.co.uk.pem',
                                keyfile='csr.priv.key', server_side=True)
httpd.serve_forever()

