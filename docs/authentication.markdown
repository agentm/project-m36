# Authentication in Project:M36

Project:M36 supports mutual TLS ([mTLS](https://en.wikipedia.org/wiki/Mutual_authentication#mTLS)) authentication via information contained within the required X.509 certificates. The [curryer-rpc](https://hackage.haskell.org/package/curryer-rpc) module implements this feature.

## Introduction

To access Project:M36 with authentication, mutual TLS with X.509 certificate is required. Basic certificate and public-private key infrastructure knowledge is necessary to implement such a deployment.

Project:M36 contains an SQLite database for login role names. Certificates created by the server administrator can then be dispensed to users using those role names in the organization unit (OU) field.

In this example deployment, we will setup a self-signed certificate authority and certificate chain. For production deployments, sign your certficates using your existing certificate authority.

## Add the Login Role for the User

As the database administrator:

```
TutorialD (master/main): :addloginrole johnsmith maylogin
```

## Setup a Self-Signed Server Certificate Authority

```bash
# generate the private key
openssl genrsa -out $PEMS/ca/private/cakey.pem 4096

# generate the certificate authority certificate by filling in the organization and server name (canonical name)
ORGANIZATION="mycompany"
SERVER_CN="dbhostname"
SERVER_SUBJ="/C=US/ST=DC/O=$ORGANIZATION/CN=$SERVER_CN"
openssl req -new -x509 -nodes -days 3650 -config openssl.cnf -key $PEMS/ca/private/cakey.pem -out $PEMS/ca/certs/cacert.pem -subj "$SERVER_SUBJ"
```

## Setup a Server Certificate

```bash
# Generate the private key for the server
openssl genrsa -out $PEMS/server/server.key.pem 4096

# Generate a Certificate Signing Request (CSR) for the Server.
openssl req -new -key $PEMS/server/server.key.pem -out $PEMS/server/server.csr -subj "$SERVER_SUBJ"
# Create the Server Certificate
openssl ca -config openssl.cnf -days 1650 -notext -batch -in $PEMS/server/server.csr -out $PEMS/server/server.cert.pem
```

## Run the Server

```bash
project-m36-server --private-x509-key $PEMS/server/server.key.pem --public-x509-key $PEMS/server.cert.pem
```

To allow anonymous clients to connect (specifically, clients who do not present a client certificate), use the `--allow-anonymous clients` option.


## Setup a Client Certificate

The client certificates must be created on the trusted server.

```bash
# Generate the private key for the client.
openssl genrsa -out $PEMS/client/client.key.pem 4096

# Generate a Certificate Signing Request (CSR) for the Client
# IMPORTANT: Make sure the organization unit (OU) is the name of the role that will login to the database.
ORGANIZATION_UNIT="johnsmith"
CLIENT_CN="johnsmith_laptop"
CLIENT_SUBJ="/C=US/ST=DC/O=$ORGANIZATION/OU=$ORGANIZATION_UNIT/CN=$CLIENT_CN"
openssl req -new -key $PEMS/client/client.key.pem -out $PEMS/ca/client.csr -nodes -subj "$CLIENT_SUBJ"

# Create the Client Certificate
openssl ca -config openssl.cnf -days 1650 -notext -batch -in $PEMS/ca/client.csr -out $PEMS/client/client.cert.pem

```

## Connect Client to the Server

Next, provide the client with the server certificate, client certificate, and client private key. On the client, run:

```bash
tutd --private-x509-key $PEMS/client/client.key.pem --public-x509-key $PEMS/client/client.cert.pem --certificate-x509-path $PEMS/server/server.cert.pem -h $HOSTNAME $DBNAME
```

## Future Direction

Mutual TLS provides a high level of security guarantees above common password-based authentication, but it is clearly more difficult to setup. Project:M36 will aim to make authentication smoother in the future. Please create tickets or discussions if you have suggestions or need help setting up authentication.
