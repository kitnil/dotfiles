Get-ChildItem -Path "C:\scripts" -Filter "*.ps1" | ForEach-Object {
    if ($_.Name -notmatch "main.ps1") {
        Invoke-Expression $_.FullName
    }
}
