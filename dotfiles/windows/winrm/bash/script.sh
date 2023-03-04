#!/usr/bin/env bash

winrm_host="$WINRM_HOSTNAME"
username="$WINRM_USERNAME"
password="$WINRM_PASSWORD"
script_path="script.ps1"

auth=$(echo -n "$username:$password" | base64)
uri="http://${winrm_host}:5985/wsman"

# Encode the PowerShell script as a Base64 string
script_content=$(cat $script_path | base64 -w 0)

# Construct the PowerShell command to execute the script
powershell_command="powershell.exe -encodedCommand $script_content"

# Construct the SOAP request to execute the PowerShell command
soap_request="<s:Envelope xmlns:s='http://www.w3.org/2003/05/soap-envelope' xmlns:wsmid='http://schemas.dmtf.org/wbem/wsman/identity/1/wsmanidentity.xsd' xmlns:wsm='http://schemas.dmtf.org/wbem/wsman/1/wsman.xsd'><s:Header><wsmid:Identify/></s:Header><s:Body><wsm:Invoke><wsm:Action>http://schemas.microsoft.com/wbem/wsman/1/windows/shell/invoke</wsm:Action><wsm:Shell><wsm:InputStreams>stdin</wsm:InputStreams><wsm:OutputStreams>stdout stderr</wsm:OutputStreams></wsm:Shell><wsm:Command>$powershell_command</wsm:Command></wsm:Invoke></s:Body></s:Envelope>"

# Send the SOAP request via WinRM using curl
curl -X POST \
  -H "Authorization: Basic $auth" \
  -H "Content-Type: application/soap+xml;charset=UTF-8" \
  -d "$soap_request" \
  $uri
