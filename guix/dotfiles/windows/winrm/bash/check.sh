#!/usr/bin/env bash

curl --header "Content-Type: application/soap+xml;charset=UTF-8" \
     --header "WSMANIDENTIFY: unauthenticated" \
     "http://${WINRM_HOSTNAME}:5985/wsman" \
     --data '&lt;s:Envelope xmlns:s="http://www.w3.org/2003/05/soap-envelope" xmlns:wsmid="http://schemas.dmtf.org/wbem/wsman/identity/1/wsmanidentity.xsd"&gt;&lt;s:Header/&gt;&lt;s:Body&gt;&lt;wsmid:Identify/&gt;&lt;/s:Body&gt;&lt;/s:Envelope&gt;'
