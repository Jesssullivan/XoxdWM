#!/usr/bin/env python3
"""Simple IPC client for debugging ewwm-compositor.

Usage:
    python3 tools/ewwm-ipc-client.py '(:type :surface-list :id 1)'
    python3 tools/ewwm-ipc-client.py --interactive
    python3 tools/ewwm-ipc-client.py --listen

Also usable via socat:
    socat UNIX-CONNECT:$XDG_RUNTIME_DIR/ewwm-ipc.sock -
"""

import argparse
import os
import socket
import struct
import sys
import time


def get_socket_path():
    runtime_dir = os.environ.get("XDG_RUNTIME_DIR", f"/tmp/ewwm-{os.getuid()}")
    return os.path.join(runtime_dir, "ewwm-ipc.sock")


def encode_message(payload: str) -> bytes:
    """Encode a message with 4-byte big-endian length prefix."""
    payload_bytes = payload.encode("utf-8")
    return struct.pack(">I", len(payload_bytes)) + payload_bytes


def read_message(sock: socket.socket) -> str:
    """Read a length-prefixed message from the socket."""
    # Read 4-byte length prefix
    header = b""
    while len(header) < 4:
        chunk = sock.recv(4 - len(header))
        if not chunk:
            raise ConnectionError("Connection closed")
        header += chunk

    length = struct.unpack(">I", header)[0]

    # Read payload
    payload = b""
    while len(payload) < length:
        chunk = sock.recv(length - len(payload))
        if not chunk:
            raise ConnectionError("Connection closed")
        payload += chunk

    return payload.decode("utf-8")


def send_and_receive(sock, message):
    """Send a message and print the response."""
    sock.sendall(encode_message(message))
    print(f">> {message}")
    response = read_message(sock)
    print(f"<< {response}")
    return response


def main():
    parser = argparse.ArgumentParser(description="EWWM IPC debug client")
    parser.add_argument("message", nargs="?", help="S-expression message to send")
    parser.add_argument("--socket", default=None, help="Socket path")
    parser.add_argument("--interactive", "-i", action="store_true", help="Interactive mode")
    parser.add_argument("--listen", "-l", action="store_true", help="Listen for events")
    parser.add_argument("--benchmark", "-b", type=int, default=0, help="Ping benchmark N times")
    args = parser.parse_args()

    socket_path = args.socket or get_socket_path()

    sock = socket.socket(socket.AF_UNIX, socket.SOCK_STREAM)
    try:
        sock.connect(socket_path)
        print(f"Connected to {socket_path}")

        # Hello handshake
        send_and_receive(sock, '(:type :hello :id 0 :version 1 :client "ewwm-ipc-client.py")')

        if args.benchmark > 0:
            times = []
            for i in range(args.benchmark):
                ts = int(time.time() * 1000)
                start = time.monotonic()
                sock.sendall(encode_message(f"(:type :ping :id {i+1} :timestamp {ts})"))
                read_message(sock)
                elapsed = (time.monotonic() - start) * 1000
                times.append(elapsed)
            times.sort()
            n = len(times)
            print(f"\nBenchmark ({n} pings):")
            print(f"  min={times[0]:.2f}ms max={times[-1]:.2f}ms mean={sum(times)/n:.2f}ms")
            print(f"  p50={times[n//2]:.2f}ms p95={times[int(n*0.95)]:.2f}ms p99={times[int(n*0.99)]:.2f}ms")

        elif args.listen:
            print("Listening for events (Ctrl+C to quit)...")
            try:
                while True:
                    msg = read_message(sock)
                    ts = time.strftime("%H:%M:%S")
                    print(f"[{ts}] {msg}")
            except KeyboardInterrupt:
                print("\nDisconnected.")

        elif args.interactive:
            print("Interactive mode. Type s-expressions (Ctrl+D to quit):")
            msg_id = 1
            try:
                while True:
                    line = input("ewwm> ").strip()
                    if not line:
                        continue
                    send_and_receive(sock, line)
                    msg_id += 1
            except (EOFError, KeyboardInterrupt):
                print("\nDisconnected.")

        elif args.message:
            send_and_receive(sock, args.message)

        else:
            parser.print_help()

    except ConnectionRefusedError:
        print(f"Error: Cannot connect to {socket_path}", file=sys.stderr)
        print("Is ewwm-compositor running?", file=sys.stderr)
        sys.exit(1)
    finally:
        sock.close()


if __name__ == "__main__":
    main()
