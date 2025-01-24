"""
A WebSocket server designed to relay audio data from a sender to a receiver
in a remote microphone streaming system. It uses aiohttp to handle WebSocket
connections. The server listens for incoming audio data on a specified port,
authenticates clients based on a query parameter (password), and forwards audio
data to all connected and authenticated receiver clients.

Usage:
    Start the server by running this script with Python 3.x. It will listen for
    connections on 0.0.0.0 at the port defined at the bottom of the script.
    Senders and receivers connect to this server using the WebSocket protocol,
    allowing for real-time audio streaming from the sender to the receiver.
"""

import asyncio
from aiohttp import web

# Set of connected WebSocket clients
connected = set()

# Your predefined password
PASSWORD = "demopassword"


async def websocket_handler(request):
    # Check password from query parameters
    if request.query.get("password") != PASSWORD:
        return web.Response(text="Unauthorized", status=401)

    ws = web.WebSocketResponse()
    await ws.prepare(request)

    print("New client with IP address {0} connected.".format(request.remote))
    connected.add(ws)

    async for msg in ws:
        if msg.type == web.WSMsgType.TEXT:
            # Broadcast incoming text message to all connected websockets
            for socket in connected:
                if socket != ws:
                    await socket.send_str(msg.data)
        elif msg.type == web.WSMsgType.BINARY:
            # Broadcast incoming binary message to all connected websockets
            for socket in connected:
                if socket != ws:
                    try:
                        await socket.send_bytes(msg.data)
                    except Exception as e:
                        pass
        elif msg.type == web.WSMsgType.ERROR:
            print("ws connection closed with exception %s" % ws.exception())

    print("Client disconnected.")
    connected.remove(ws)
    return ws


async def init_app():
    app = web.Application()
    app.add_routes([web.get("/ws", websocket_handler)])
    return app


loop = asyncio.get_event_loop()
app = loop.run_until_complete(init_app())

web.run_app(app, host="0.0.0.0", port=80) # Your host and port settings here
