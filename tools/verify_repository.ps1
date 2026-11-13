$ErrorActionPreference='Stop'
$repoRoot=Split-Path -Parent $PSScriptRoot
$allowed=@(Get-Content -LiteralPath (Join-Path $repoRoot 'repository-files.txt') | Where-Object {$_ -and -not $_.StartsWith('#')} | Sort-Object -Unique)
$tracked=@(& git -C $repoRoot ls-files | Sort-Object -Unique)
if($LASTEXITCODE -ne 0){throw 'Cannot read the Git index.'}
$difference=Compare-Object $allowed $tracked
if($difference){$difference | Format-Table; throw 'Tracked files must exactly match the reviewed repository manifest.'}
$badModes=@(& git -C $repoRoot ls-files --stage | Where-Object {$_ -notmatch '^100644 '})
if($badModes){throw 'Unexpected symlink, executable or submodule in repository manifest.'}
$secretPattern='gh[pousr]_[A-Za-z0-9]{30,}|github_pat_[A-Za-z0-9_]{30,}|AKIA[0-9A-Z]{16}|-----BEGIN (RSA |EC |OPENSSH )?PRIVATE KEY-----'
foreach($file in $tracked){
  # The approved Figma logo is the sole reviewed raster runtime asset.
  if($file -eq 'development/shiny_app/www/brand-logo.png'){
    $stagedLogo=(& git -C $repoRoot rev-parse (':'+$file))
    $workingLogo=(& git -C $repoRoot hash-object -- $file)
    if($stagedLogo -ne $workingLogo){throw 'Staged logo differs from the approved working asset.'}
    $logoHash=(Get-FileHash -LiteralPath (Join-Path $repoRoot $file) -Algorithm SHA256).Hash
    if($logoHash -ne 'BBDF46E809313704201042694E68D75FAC6429427D60958B0B8D857DF9253638'){throw 'Approved logo checksum changed.'}
    continue
  }
  if($file -match '(^|/)(qa|private_inputs|\.library|\.aws|\.codex|\.agents)(/|$)|\.(rds|fsa|RData|zip|png|jpg|pdf)$'){throw "Private/generated artifact is tracked: $file"}
  if($file -notmatch '\.xlsx$'){
    $text=(& git -C $repoRoot show (':'+$file)) -join "`n"
    if($LASTEXITCODE -ne 0){throw "Cannot inspect staged file: $file"}
    if($text -match $secretPattern){throw "Potential credential in staged file: $file"}
  }
}
Write-Output ("REPOSITORY MANIFEST PASS: {0} reviewed files; no excluded artifacts or credential patterns." -f $tracked.Count)
