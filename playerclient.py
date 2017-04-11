#!/usr/bin/env python
import os, rpyc
from epc.server import EPCServer

server = EPCServer(('localhost', 0))

@server.register_function
def remote_play_mp3(host, port, uri):
    conn = rpyc.connect(host, port)
    result = conn.root.play_mp3(uri)
    return result

server.print_port()
server.serve_forever()
