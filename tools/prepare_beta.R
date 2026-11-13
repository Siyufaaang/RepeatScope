# Build into a new directory only. The checked-in runtime manifest is authoritative.
prepare_beta <- function(root, destination) {
  root<-normalizePath(root,winslash='/',mustWork=TRUE)
  manifest<-file.path(root,'deployment/runtime-files.txt')
  paths<-readLines(manifest,warn=FALSE)
  paths<-paths[nzchar(paths) & !startsWith(paths,'#')]
  reviewed<-readLines(file.path(root,'repository-files.txt'),warn=FALSE)
  if(!length(paths) || anyDuplicated(paths) || any(!paths%in%reviewed) ||
     any(grepl('(^|/)\\.\\.(/|$)|^[/.]|[\\\\:]|(^|/)(qa|tests|private_inputs|rsconnect|\\.library|\\.deploy-library)(/|$)|\\.(rds|fsa|RData)$',paths,ignore.case=TRUE)))
    stop('Runtime manifest must contain unique, reviewed relative source paths only.')
  source_paths<-normalizePath(file.path(root,paths),winslash='/',mustWork=TRUE)
  if(any(!startsWith(source_paths,paste0(root,'/'))) || any(dir.exists(source_paths)))stop('Runtime source escapes the checkout or is a directory.')
  if(file.exists(destination) || dir.exists(destination))stop('Use a new, empty destination; an existing deployment is never overwritten.')
  if(!dir.create(destination,recursive=TRUE))stop('Cannot create bundle directory.')
  destination<-normalizePath(destination,winslash='/',mustWork=TRUE)
  outputs<-paths
  outputs[paths=='deployment/app.R']<-'app.R'
  outputs[paths=='deployment/DESCRIPTION']<-'DESCRIPTION'
  if(anyDuplicated(outputs))stop('Runtime manifest creates duplicate output paths.')
  for(i in seq_along(paths)){
    out<-file.path(destination,outputs[i])
    dir.create(dirname(out),recursive=TRUE,showWarnings=FALSE)
    if(!file.copy(source_paths[i],out,overwrite=FALSE))stop('Cannot copy runtime file: ',paths[i])
  }
  hashes<-vapply(file.path(destination,outputs),digest::digest,character(1),algo='sha256',file=TRUE)
  utils::write.csv(data.frame(path=outputs,sha256=unname(hashes)),file.path(destination,'bundle-checksums.csv'),row.names=FALSE)
  list(directory=destination,files=outputs,checksums=hashes)
}

# Compare the selected upload directory with the current reviewed source, not only
# with its own checksum file. This rejects a valid but stale bundle.
verify_beta_bundle <- function(root, destination) {
  root<-normalizePath(root,winslash='/',mustWork=TRUE)
  destination<-normalizePath(destination,winslash='/',mustWork=TRUE)
  paths<-readLines(file.path(root,'deployment/runtime-files.txt'),warn=FALSE)
  paths<-paths[nzchar(paths) & !startsWith(paths,'#')]
  reviewed<-readLines(file.path(root,'repository-files.txt'),warn=FALSE)
  if(!length(paths) || anyDuplicated(paths) || any(!paths%in%reviewed) ||
     any(grepl('(^|/)\\.\\.(/|$)|^[/.]|[\\\\:]',paths)))stop('Invalid reviewed runtime manifest.')
  outputs<-paths;outputs[paths=='deployment/app.R']<-'app.R';outputs[paths=='deployment/DESCRIPTION']<-'DESCRIPTION'
  checks<-utils::read.csv(file.path(destination,'bundle-checksums.csv'),stringsAsFactors=FALSE)
  if(!identical(names(checks),c('path','sha256')) || anyDuplicated(checks$path) || !setequal(checks$path,outputs))
    stop('Bundle file list differs from the reviewed runtime manifest.')
  source_paths<-normalizePath(file.path(root,paths),winslash='/',mustWork=TRUE)
  target_paths<-normalizePath(file.path(destination,outputs),winslash='/',mustWork=TRUE)
  if(any(!startsWith(source_paths,paste0(root,'/'))) || any(!startsWith(target_paths,paste0(destination,'/'))))
    stop('Runtime source or destination escapes its directory.')
  expected<-vapply(source_paths,digest::digest,character(1),algo='sha256',file=TRUE)
  actual<-vapply(target_paths,digest::digest,character(1),algo='sha256',file=TRUE)
  if(!identical(unname(expected),unname(actual)) || !identical(unname(actual),checks$sha256[match(outputs,checks$path)]))
    stop('Bundle differs from the tested source or recorded hashes; rebuild and verify it.')
  list(directory=destination,files=outputs,checksums=actual)
}
