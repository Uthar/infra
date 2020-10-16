#! /usr/bin/env nix-shell
#! nix-shell -i bash -p easyrsa openvpn libressl

if [ -d "pki" ]; then
    echo "pki/ already exists, exiting"
    exit 1
fi

easyrsa init-pki
easyrsa --batch build-ca nopass
openssl dhparam -out pki/dh.pem 2048
EASYRSA_CRL_DAYS=3650 easyrsa gen-crl
easyrsa build-server-full server nopass
openvpn --genkey --secret pki/ta.key
