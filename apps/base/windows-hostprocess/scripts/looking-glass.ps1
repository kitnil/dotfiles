Write-Host "looking-glass-host: Installing looking-glass-host."
$filePath = "C:\Program Files\Looking Glass (host)\looking-glass-host.exe"
if (Test-Path $filePath) {
    Write-Host "looking-glass-host: '$filePath' file already exists."
} else {
    $url="https://looking-glass.io/artifact/B5/host"
    (New-Object System.Net.WebClient).DownloadFile($url, "$env:TEMP\looking-glass.zip")
    Expand-Archive -Path $env:TEMP\looking-glass.zip -DestinationPath $env:TEMP\looking-glass -Force
    rm "$env:TEMP\looking-glass.zip"
    rm -recurse "$env:TEMP\looking-glass"
}
