nmcli con add type wifi ifname wlp0s20f3 con-name Parmiters ssid Parmiters
nmcli con mod foo +802-1x.identity USERNAME(16pearce)  802-1x.eap peap 802-1x.phase2-auth mschapv2 802-1x.password PASSWORD
nmcli con mod foo wifi-sec.key-mgmt wpa-eap

