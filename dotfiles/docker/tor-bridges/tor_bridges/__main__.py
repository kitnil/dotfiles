"""Send and receiving email to get Tor bridges.

This module provides an HTTP service that facilitates communication with the
Tor project to obtain a new key. Upon sending an email request, the service
responds with a payload containing the Tor bridge information.

Features:
- Send email requests to the Tor project.
- Receive responses containing bridge payloads.

Usage:
1. Initialize the service.
2. Send an email request to the Tor project.
3. Retrieve and handle the response containing the Tor bridge information.
"""

from datetime import datetime
from http.server import BaseHTTPRequestHandler, HTTPServer
from imapclient import IMAPClient
from socket import gaierror
import email
import json
import logging
import os
import quopri
import smtplib


email_username = os.getenv("TOR_BRIDGES_EMAIL_USERNAME")
email_password = os.getenv("TOR_BRIDGES_EMAIL_PASSWORD")


def mail_send():
    """Send email message to Tor project."""
    try:
        s = smtplib.SMTP_SSL("smtp.gmail.com", 465)
        s.login(email_username, email_password)
        message = "\r\n".join(
            [
                f"From:{email_username}",
                "To: bridges@torproject.org",
                "Subject: ",
                "",
                '"get transport obfs4"',
            ]
        )
        s.sendmail(email_username, "bridges@torproject.org", message)
        s.quit()
        return datetime.utcnow().time()
    except gaierror:
        exit("Ошибка соединения")
    except smtplib.SMTPAuthenticationError:
        exit("Ошибка аутентификации. Проверьте логин или пароль")


def mail_read():
    """Get email message from Tor project and parse it."""
    with IMAPClient(host="imap.gmail.com") as client:
        client.login(email_username, email_password)
        client.select_folder("INBOX")
        messages = [client.search(["FROM", "bridges@torproject.org"])[0]]
        for uid, message_data in client.fetch(messages, "RFC822").items():
            email_message = email.message_from_bytes(message_data[b"RFC822"])
            for ct in email_message.walk():
                payload = ct.get_payload(decode=False)
                if "Here is your bridge:\r\n" in payload:
                    lines = quopri.decodestring(payload).decode("utf-8").splitlines()
                    out = []
                    for idx, x in enumerate(lines):
                        if x.startswith("obfs4"):
                            try:
                                out[:0] = [x]
                            except:
                                pass
            return out


global output
output = []


class S(BaseHTTPRequestHandler):
    """HTTP server."""

    def _set_response(self):
        self.send_response(200)
        self.send_header("Content-type", "text/html")
        self.end_headers()

    def do_GET(self):
        global output
        logging.info(
            "GET request,\nPath: %s\nHeaders:\n%s\n", str(self.path), str(self.headers)
        )
        self._set_response()
        out = mail_read()
        if out:
            output = out
        self.wfile.write("{}".format(json.dumps(output)).encode("utf-8"))

    def do_POST(self):
        mail_send()


def run(server_class=HTTPServer, handler_class=S, port=8080):
    """HTTP server."""
    logging.basicConfig(level=logging.INFO)
    server_address = ("", port)
    httpd = server_class(server_address, handler_class)
    logging.info("Starting httpd...\n")
    try:
        httpd.serve_forever()
    except KeyboardInterrupt:
        pass
    httpd.server_close()
    logging.info("Stopping httpd...\n")


def main():
    """Entrypoint."""
    run(port=int(os.getenv("TOR_BRIDGES_HTTP_PORT", "8080")))


if __name__ == "__main__":
    main()
