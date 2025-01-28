"""
This script functions as the sender in a remote microphone streaming setup.
It captures audio from the system's default microphone using pyaudio, encodes this
audio data, and sends it over a WebSocket connection to a server. The server
then relays this data to one or more receivers.

Usage:
    Execute the script with Python 3.x to begin capturing and streaming audio.
    The script sends audio data to the WebSocket server specified in WS_URL.
    If a "panic" message is received from the server, the script stops streaming,
    closes the audio stream, and exits.
"""

import pyaudio
import websocket
import threading

FORMAT = pyaudio.paInt16
CHANNELS = 1
RATE = 44100
CHUNK = 1024 * 10
stream = None
p = pyaudio.PyAudio()
WS_URL = (
    "ws://192.168.0.196/ws?password=demopassword"
)


def on_message(ws, message):
    print("Received message from server", message)
    if message == "panic":
        stream.stop_stream()
        stream.close()
        p.terminate()
        print("Terminated")
        ws.close()



def on_error(ws, error):
    print(error)


def on_open(ws):
    def run(*args):
        global stream
        stream = p.open(
            format=FORMAT,
            channels=CHANNELS,
            rate=RATE,
            input=True,
            frames_per_buffer=CHUNK,
        )
        print("Recording...")

        while True:
            data = stream.read(CHUNK, exception_on_overflow=True)
            ws.send(data, opcode=websocket.ABNF.OPCODE_BINARY)

        stream.stop_stream()
        stream.close()
        p.terminate()
        print("Terminated")
        ws.close()

    thread = threading.Thread(target=run)
    thread.start()


if __name__ == "__main__":
    ws = websocket.WebSocketApp(WS_URL, on_message=on_message, on_error=on_error)
    ws.on_open = on_open
    ws.run_forever()
