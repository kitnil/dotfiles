"""
This script is the receiver component of a remote microphone streaming system.
It connects to a specified WebSocket server, receives audio data streamed from a sender,
and plays it back in real-time. It uses pyaudio for audio playback and numpy for
manipulation of the audio data.

Usage:
    Run the script with Python 3.x. It will automatically connect to the WebSocket server
    specified in the WS_URL variable and start playing received audio data.
    To stop the script, focus on the terminal and press Ctrl+C.
"""

import pyaudio
import websocket
import threading
import numpy as np
from multiprocessing import Process
from vosk import Model, KaldiRecognizer
import hashlib
import json
import logging
import numpy as np
import os
import pathlib
import pyaudio
import subprocess
import threading
import websocket

# Audio configuration
FORMAT = pyaudio.paInt16
CHANNELS = 1
RATE = 44100
CHUNK = 1024 * 10

# WebSocket
WS_URL = (
    "ws://192.168.0.196/ws?password=demopassword"
)

logging.basicConfig()
log = logging.getLogger("vosk")
log_level = os.getenv("VOSK_LOG_LEVEL", "DEBUG")
log.info(f"{log_level}: log_level")
log.setLevel(log_level)
log.info("Starting")

model = Model("/home/oleg/.local/share/chezmoi/vosk/small_model")
rec = KaldiRecognizer(model, RATE)


def listen(data):
    while True:
        if (rec.AcceptWaveform(data)) and (len(data) > 0):
            answer = json.loads(rec.Result())
            log.debug(answer)
            if answer["text"]:
                yield answer["text"]


def on_message(ws, message):
    # Convert the message data to a numpy array of the correct type
    data = np.frombuffer(message, dtype=np.int16)
    bytes = data.tobytes()
    stream.write(bytes)

    # while True:
    #     data = message
    #     rec.AcceptWaveform(data)
    #     answer = json.loads(rec.Result())
    #     log.debug(answer)
    #     # if answer["text"]:
    #     #     yield answer["text"]
    #     if (rec.AcceptWaveform(data)) and (len(data) > 0):
    #         answer = json.loads(rec.Result())
    #         if answer["text"]:
    #             yield answer["text"]

    # something = stream.read(4000, exception_on_overflow=True)

    rec.AcceptWaveform(bytes)
    output = json.loads(rec.Result())
    log.debug(output)

    # if output['text'] is not "":
    #     log.debug(output['text'])
    # if (rec.AcceptWaveform(data)) and (len(data) > 0):
    #     answer = json.loads(rec.Result())
    #     if answer["text"]:
    #         yield answer["text"]


def on_error(ws, error):
    print(error)

def on_open(ws):
    log.debug("Opened connection")

def send_panic_and_exit():
    log.info("Panic! Sending 'panic' message...")
    ws.send("panic")
    ws.close()
    stream.stop_stream()
    stream.close()
    p.terminate()
    log.info("Terminated. Exiting.")
    exit()

if __name__ == "__main__":
    ws = websocket.WebSocketApp(
        WS_URL, on_message=on_message, on_error=on_error
    )
    p = pyaudio.PyAudio()
    stream = p.open(
        format=FORMAT,
        channels=CHANNELS,
        rate=RATE,
        output=True,
        frames_per_buffer=CHUNK,
    )

    wst = threading.Thread(target=lambda: ws.run_forever())
    wst.daemon = True
    wst.start()

    log.info("Receiving... Press Ctrl+C to stop")
    try:
        while True:
            pass  # Keep the main thread alive
    except KeyboardInterrupt:
        send_panic_and_exit()
