#!/bin/bash
BW_CLIENTID=$(pass show bitwarden.com |grep client-id| awk -F': ' '{print $2}')
BW_CLIENTSECRET=$(pass show bitwarden.com |grep client_secret| awk -F': ' '{print $2}')
BW_MASTER=$(pass show bitwarden.com |head -n 1)
if [ "$1" == "login" ]; then
    BW_CLIENTID=$BW_CLIENTID BW_CLIENTSECRET=$BW_CLIENTSECRET bw login --nointeraction  --apikey
    exit 0
elif [ "$1" == "unlock" ]; then
    BW_SESSION=$(BW_MASTER=$BW_MASTER bw unlock --nointeraction  --passwordenv BW_MASTER --raw)
    echo $BW_SESSION
    exit 0
elif [ "$1" == "sync" ]; then
    BW_SESSION=$(BW_MASTER=$BW_MASTER bw unlock --nointeraction  --passwordenv BW_MASTER --raw)
    BW_SESSION=$BW_SESSION bw sync
    exit 0
fi
BW_SESSION=$(BW_MASTER=$BW_MASTER bw unlock --nointeraction  --passwordenv BW_MASTER --raw)
export BW_SESSION
# https://bitwarden.com/help/cli/
# bw get password github.com
# bw get (item|username|password|uri|totp|exposed|attachment|folder|collection|organization|org-collection|template|fingerprint) <id> [options]
# bw sync  >/dev/null 2>/dev/null
bw $@
bw lock >/dev/null 2>/dev/null