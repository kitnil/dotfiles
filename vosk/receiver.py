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

# Audio configuration
FORMAT = pyaudio.paInt16
CHANNELS = 1
RATE = 44100
CHUNK = 1024 * 10

# WebSocket
WS_URL = (
    "ws://audioendpoint.yourserver.com/ws?password=demopassword"
)


def on_message(ws, message):
    # Convert the message data to a numpy array of the correct type
    data = np.frombuffer(message, dtype=np.int16)
    stream.write(data.tobytes())


def on_error(ws, error):
    print(error)

def on_open(ws):
    print("Opened connection")

def send_panic_and_exit():
    print("Panic! Sending 'panic' message...")
    ws.send("panic")
    ws.close()
    stream.stop_stream()
    stream.close()
    p.terminate()
    print("Terminated. Exiting.")
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

    print("Receiving... Press Ctrl+C to stop")
    try:
        while True:
            pass  # Keep the main thread alive
    except KeyboardInterrupt:
        send_panic_and_exit()
