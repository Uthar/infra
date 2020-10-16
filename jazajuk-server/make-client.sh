#! /usr/bin/env nix-shell
#! nix-shell -i bash -p easyrsa

newclient () {
        cp client-common.txt $1.ovpn
        echo "<ca>" >> $1.ovpn
        cat pki/ca.crt >> $1.ovpn
        echo "</ca>" >> $1.ovpn
        echo "<cert>" >> $1.ovpn
        sed -ne '/BEGIN CERTIFICATE/,$ p' pki/issued/$1.crt >> $1.ovpn
        echo "</cert>" >> $1.ovpn
        echo "<key>" >> $1.ovpn
        cat pki/private/$1.key >> $1.ovpn
        echo "</key>" >> $1.ovpn
        echo "<tls-auth>" >> $1.ovpn
        sed -ne '/BEGIN OpenVPN Static key/,$ p' pki/ta.key >> $1.ovpn
        echo "</tls-auth>" >> $1.ovpn
}

easyrsa build-client-full $1 nopass
newclient $1
