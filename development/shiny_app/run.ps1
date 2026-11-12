param([int]$Port=3838,[string]$Rscript='D:\R\R-4.5.2\bin\Rscript.exe')
$ErrorActionPreference='Stop'
$env:REPEATSCOPE_PORT=[string]$Port
$env:LANG='en_US.UTF-8'
$env:LC_ALL='en_US.UTF-8'
& $Rscript --vanilla (Join-Path $PSScriptRoot 'run.R')
exit $LASTEXITCODE
