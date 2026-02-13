#!/usr/bin/env python3
"""Send a length-prefixed s-expression to the EWWM IPC socket and print the response."""

import socket
import struct
import sys


def main():
    if len(sys.argv) < 3:
        print(f"Usage: {sys.argv[0]} SOCKET_PATH SEXP", file=sys.stderr)
        sys.exit(2)

    sock_path = sys.argv[1]
    payload = sys.argv[2].encode("utf-8")

    sock = socket.socket(socket.AF_UNIX, socket.SOCK_STREAM)
    sock.settimeout(5)
    sock.connect(sock_path)

    # Send: 4-byte big-endian length prefix + payload
    sock.sendall(struct.pack(">I", len(payload)) + payload)

    # Read response: 4-byte length prefix then body
    header = b""
    while len(header) < 4:
        chunk = sock.recv(4 - len(header))
        if not chunk:
            break
        header += chunk

    if len(header) < 4:
        print("ERROR: incomplete response header", file=sys.stderr)
        sock.close()
        sys.exit(1)

    resp_len = struct.unpack(">I", header)[0]
    body = b""
    while len(body) < resp_len:
        chunk = sock.recv(resp_len - len(body))
        if not chunk:
            break
        body += chunk

    sock.close()
    print(body.decode("utf-8", errors="replace"))


if __name__ == "__main__":
    main()
