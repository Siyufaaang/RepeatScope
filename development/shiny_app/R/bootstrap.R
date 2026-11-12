# App-local dependencies first; existing compatible libraries are read-only.
# Windows may inherit an unsupported C.UTF-8 locale from a launcher. Parse the
# app's UTF-8 source consistently (including labels and scientific symbols).
if(.Platform$OS.type=='windows' && !isTRUE(l10n_info()[['UTF-8']]))Sys.setlocale('LC_CTYPE','.UTF-8')
app_root <- function() {
  candidates<-c(getOption('repeatscope.app',''),getwd(),file.path(getwd(),'development/shiny_app'))
  ok<-vapply(candidates,function(x)nzchar(x) && file.exists(file.path(x,'R/state.R')),logical(1))
  if(!any(ok))stop('Run from the project root or development/shiny_app.')
  normalizePath(candidates[which(ok)[1]],winslash='/',mustWork=TRUE)
}
rs_app_dir<-app_root()
rs_root<-normalizePath(file.path(rs_app_dir,'../..'),winslash='/',mustWork=TRUE)
.libPaths(c(file.path(rs_app_dir,'.library'),Sys.getenv('REPEATSCOPE_R_LIBRARY'), 'D:/R/R-4.5.0/library',.libPaths()))
rs_dependencies<-c('shiny','bslib','jsonlite','readxl','writexl','zip','curl','ggplot2','digest')
require_dependencies <- function() {
  absent<-rs_dependencies[!vapply(rs_dependencies,requireNamespace,logical(1),quietly=TRUE)]
  if(length(absent))stop('Missing packages: ',paste(absent,collapse=', '),'. Run development/shiny_app/setup.R.')
}
for(rs_module in c('state','import_export','reference','localization','sample_layout','engine','report','synthetic','workflow','field_help')) {
  rs_path<-file.path(rs_app_dir,'R',paste0(rs_module,'.R'))
  if(file.exists(rs_path))sys.source(rs_path,envir=environment())
}
