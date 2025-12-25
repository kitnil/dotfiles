# Run this to create everything
mkdir -p openvpn-certs
cd openvpn-certs

# Create directory structure
mkdir -p demoCA/newcerts demoCA/private
touch demoCA/index.txt
echo "01" > demoCA/serial

# Generate CA
openssl genrsa -out demoCA/private/cakey.pem 2048
openssl req -new -x509 -days 3650 -key demoCA/private/cakey.pem \
  -out demoCA/cacert.pem -subj "/C=US/ST=State/L=City/O=Company/CN=OpenVPN CA"

# Generate server certificate
openssl genrsa -out server.key 2048
openssl req -new -key server.key -out server.csr -subj "/CN=server"
openssl x509 -req -in server.csr -CA demoCA/cacert.pem \
  -CAkey demoCA/private/cakey.pem -CAcreateserial -out server.crt -days 365

# Generate client certificate
openssl genrsa -out client1.key 2048
openssl req -new -key client1.key -out client1.csr -subj "/CN=client1"
openssl x509 -req -in client1.csr -CA demoCA/cacert.pem \
  -CAkey demoCA/private/cakey.pem -CAserial demoCA/serial -out client1.crt -days 365

# Generate DH parameters
openssl dhparam -out dh2048.pem 2048

echo "All certificates generated successfully!"
