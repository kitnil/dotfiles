import json
import pymongo
from pprint import pprint as pp
import ipaddress

mongo_user = os.environ['MONGO_USER']
mongo_password = os.environ['MONGO_PASSWORD']

# Setup Mongo
myclient = pymongo.MongoClient("mongodb://" + mongo_user ":" mongo_password "@hms01-mr.intr:27017,hms02-mr.intr:27017,hms03-mr.intr:27017/admin?replicaSet=hms-rs0")
service = myclient["staffResourceController"]["service"]
socket = myclient["staffResourceController"]["socket"]
service_socket = myclient["staffResourceController"]["serviceSocket"]

ip_address_to_int = lambda ip_address: int(ipaddress.ip_address(ip_address))

web_find_socket = lambda socket_name, server: [socket for socket in socket.find({"name": socket_name + "@" + server})]
web_socket_id = lambda socket_name, server: str(web_find_socket(socket_name, server)[0]["_id"])
web_create_socket = lambda socket_name, ip_address, protocol, server, port: web_find_socket(socket_name, server) and web_socket_id(socket_name, server) or socket.insert_one({"protocol": protocol, "address": ip_address_to_int(ip_address), "port": port, "name": socket_name + "@" + server, "switchedOn": True, "_class" : "NetworkSocket"}).inserted_id and web_socket_id(socket_name, server)

web_find_service_socket = lambda service_socket_name, server: [service_socket for service_socket in service_socket.find({"name": service_socket_name + "@" + server})]
web_service_socket_id = lambda service_socket_name, server: str(web_find_service_socket(service_socket_name, server)[0]["_id"])
web_create_service_socket = lambda service_socket_name, ip_address, server, port: web_find_service_socket(service_socket_name, server) and web_service_socket_id(service_socket_name, server) or service_socket.insert_one({"address": ip_address_to_int(ip_address), "port": port, "name": service_socket_name + "@" + server, "switchedOn": True, "_class" : "ru.majordomo.hms.rc.staff.resources.ServiceSocket"}).inserted_id and web_service_socket_id(service_socket_name, server)

# service_name e.g. "nginx"
server_id = lambda service_name, server: service.find_one({"name": service_name + "@" + server})['serverId']
web_find_service = lambda service_name, server: [service for service in service.find({"name": service_name + "@" + server})]
web_service_id = lambda service_name, server: str(web_find_service(service_name, server)[0]["_id"])
web_create_service = lambda service_name, service_template_id, template_id, server, server_id, socket, service_socket_id: web_find_service(service_name, server) and web_service_id(service_name, server) or service.insert_one({"serverId": server_id, "serviceTemplateId": service_template_id, "templateId": template_id, "socketIds": [socket], "serviceSocketIds": [service_socket_id], "instanceProps": {}, "name": service_name + "@" + server, "switchedOn": True, "_class": "ru.majordomo.hms.rc.staff.resources.Service"}) and web_service_id(service_name, server)

web_create_ssh_service = lambda server, ip_address: web_create_service("ssh-guest-room", "5d6693a6f7619300012fbbd5", "5d8a2a2e708301660013776d", server, server_id('nginx', server), web_create_socket("ssh-guest-room-ssh", ip_address, "ssh", server, "1022"), web_create_service_socket("ssh-guest-room-ssh", ip_address, server, "1022"))

web_create_php_service = lambda server, socket, security: web_create_service(server, web_create_socket(server, socket, security), web_create_service_socket(server, socket, security), security)

web_create_php_services = lambda server: list(map (lambda manifest: web_create_php_service(manifest[0], manifest[1], manifest[2]), [[server, 8074, "default"], [server, 8274, "hardened"], [server, 8374, "hardened_nochmod"], [server, 8174, "unsafe"]]))
