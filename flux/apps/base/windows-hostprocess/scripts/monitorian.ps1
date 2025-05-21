Write-Host "monitorian: Installing looking-glass-host."
$filePath = "C:\Program Files (x86)\Monitorian"
if (Test-Path $filePath) {
    Write-Host "monitorian: '$filePath' file already exists."
} else {
    $url = 'https://github.com/emoacht/Monitorian/releases/download/4.3.0-Installer/MonitorianInstaller430.zip'
}
