# Run from a reviewed checkout; only synthetic data leaves the source tree.
root<-normalizePath(getwd(),winslash='/',mustWork=TRUE)
.libPaths(c(Sys.getenv('REPEATSCOPE_R_LIBRARY'),.libPaths()))
check<-function(ok,msg){if(!isTRUE(ok))stop('FAIL: ',msg,call.=FALSE);cat('PASS:',msg,'\n')}
check(file.exists('tools/prepare_beta.R'),'Beta packager exists')
source('tools/prepare_beta.R')
destination<-tempfile('repeatscope-clean-beta-')
bundle<-prepare_beta(root,destination)
check(exists('verify_beta_bundle',mode='function'),'deployment verifies the selected bundle against reviewed source')
verify_beta_bundle(root,destination)
entry<-file.path(destination,'app.R');entry_before<-readBin(entry,'raw',file.info(entry)$size)
writeLines('# stale or replaced bundle entry',entry)
checks<-read.csv(file.path(destination,'bundle-checksums.csv'),stringsAsFactors=FALSE)
checks$sha256[checks$path=='app.R']<-digest::digest(entry,algo='sha256',file=TRUE)
write.csv(checks,file.path(destination,'bundle-checksums.csv'),row.names=FALSE)
check(inherits(try(verify_beta_bundle(root,destination),silent=TRUE),'try-error'),
      'self-consistent bundle checksums cannot substitute for the tested source version')
writeBin(entry_before,entry)
checks$sha256[checks$path=='app.R']<-digest::digest(entry,algo='sha256',file=TRUE)
write.csv(checks,file.path(destination,'bundle-checksums.csv'),row.names=FALSE)
verify_beta_bundle(root,destination)
args<-commandArgs(trailingOnly=TRUE)
check(exists('verify_beta_manifest',mode='function'),'deployment supports a frozen application dependency manifest')
manifest_file<-file.path(destination,'manifest.json')
manifest<-list(metadata=list(appmode='shiny'),files=setNames(lapply(bundle$files,function(path)
  list(checksum=unname(tools::md5sum(file.path(destination,path))))),bundle$files))
jsonlite::write_json(manifest,manifest_file,auto_unbox=TRUE)
verify_beta_manifest(bundle)
manifest$files[['unreviewed.txt']]<-list(checksum='unused')
jsonlite::write_json(manifest,manifest_file,auto_unbox=TRUE)
check(inherits(try(verify_beta_manifest(bundle),silent=TRUE),'try-error'),'frozen manifest cannot add unreviewed upload files')
manifest$files[['unreviewed.txt']]<-NULL
manifest$files[['app.R']]$checksum<-'stale'
jsonlite::write_json(manifest,manifest_file,auto_unbox=TRUE)
check(inherits(try(verify_beta_manifest(bundle),silent=TRUE),'try-error'),'frozen manifest checksums must match the selected source')
manifest$files[['app.R']]$checksum<-unname(tools::md5sum(file.path(destination,'app.R')))
jsonlite::write_json(manifest,manifest_file,auto_unbox=TRUE)
temporary_destination<-destination
if(length(args)){
  destination<-normalizePath(args[1],winslash='/',mustWork=TRUE)
  bundle<-verify_beta_bundle(root,destination)
}
verify_beta_manifest(bundle)
actual<-list.files(destination,recursive=TRUE,all.files=TRUE,no..=TRUE)
check(all(c(bundle$files,'bundle-checksums.csv')%in%actual),'selected bundle contains all reviewed runtime files and checksums')
check(!any(grepl('(^|/)(qa|tests|private_inputs|\\.library|\\.deploy-library|rsconnect)(/|$)|\\.(rds|fsa|RData)$',actual,ignore.case=TRUE)),
      'bundle excludes private inputs, saved projects, tests, credentials and libraries')
check(all(file.exists(file.path(destination,c('app.R','DESCRIPTION','development/shiny_app/www/dashboard.js',
 'development/shiny_app/profiles/diseases.json','development/benchmark/bayesian/model.R')))),
      'bundle includes entry point, assets, profiles and scientific source')
check(inherits(try(prepare_beta(root,destination),silent=TRUE),'try-error'),'packager refuses an existing destination rather than mixing files')

# Load the packaged runtime from its own folder, with no checkout app/root options.
oldwd<-getwd();oldopts<-options(repeatscope.app=NULL,repeatscope.root=NULL,repeatscope.public_beta=NULL)
setwd(destination)
app<-source('app.R',local=new.env(parent=globalenv()))$value
check(inherits(app,'shiny.appobj'),'hosted root entry point produces a Shiny app')
source('development/shiny_app/R/bootstrap.R');require_dependencies()
check(identical(rs_root,normalizePath(destination,winslash='/')),'packaged app resolves its own method root')
options(repeatscope.root=rs_root)
p<-fit_project(synthetic_project());frozen<-p$fit
for(scope in c('tail10','extended')){
  p$window<-scope;r<-threshold_results(p);rows<-r[r$Well=='A04',]
  expected<-if(scope=='tail10')c(adaptive=57821/28685,bayesian=57821/28685) else
    c(adaptive=88651/30843,bayesian=76076/30163)
  check(nrow(rows)==2L && all(is.finite(rows$Preview_EI)) &&
    max(abs(rows$Preview_EI-expected[rows$Method]))<1e-12 && identical(p$fit,frozen),
    paste('packaged',scope,'preserves independently summed method values'))
}
project_path<-tempfile(fileext='.rds');save_project(p,project_path);restored<-read_project(project_path)
check(identical(threshold_results(p),threshold_results(restored)),'packaged synthetic save/restore preserves all result rows')
source('development/shiny_app/R/server.R');library(shiny)
testServer(rs_server,{
  session$setInputs(load_demo=1)
  rv$p$plate$Code[1]<-'SESSION_ONE_ONLY'
  rv$p$threshold<-.2
})
testServer(rs_server,{
  check(nrow(rv$p$peaks)==0L && !any(rv$p$plate$Code=='SESSION_ONE_ONLY') && rv$p$threshold==.03,
    'new hosted session starts empty without previous metadata or threshold')
  session$setInputs(load_demo=1)
  check(!any(rv$p$plate$Code=='SESSION_ONE_ONLY') && rv$p$threshold==.03,'new demo is independent of another session edits')
})
setwd(oldwd);options(oldopts)
unlink(project_path)
stopifnot(startsWith(normalizePath(temporary_destination,winslash='/'),paste0(normalizePath(tempdir(),winslash='/'),'/')))
unlink(temporary_destination,recursive=TRUE)
cat('BETA PACKAGE ACCEPTANCE PASS\n')
