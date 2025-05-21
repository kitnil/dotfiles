Write-Host "filelight: Installing filelight."
$filePath = "C:\Program Files\Filelight"
$out = "$env:TEMP\filelight-22.08.0-windows-msvc2019_64-cl.exe"
if (Test-Path $filePath) {
    Write-Host "filelight: '$filePath' file already exists."
    if (Test-Path $out) {
        rm $out
    }
} else {
    $url="\\samba.home\public\filelight-22.08.0-windows-msvc2019_64-cl.exe"
    Copy-Item $url $out
    Write-Host "filelight: '$out' requires human interaction for installation."
}
