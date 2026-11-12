root <- normalizePath(getwd(), winslash='/', mustWork=TRUE)
options(repeatscope.root=root)
app_dir <- file.path(root, 'development/shiny_app')
.libPaths(c(file.path(app_dir,'.library'), 'D:/R/R-4.5.0/library', .libPaths()))
check <- function(ok, message) { if (!isTRUE(ok)) stop('FAIL: ', message, call.=FALSE); cat('PASS:', message, '\n') }
fails <- function(expr) inherits(try(force(expr),silent=TRUE),'try-error')
near <- function(a,b,tol=1e-10) isTRUE(all.equal(a,b,tolerance=tol,check.attributes=FALSE))
load_module <- function(name) {
  p <- file.path(app_dir,'R',paste0(name,'.R'))
  if(file.exists(p)) sys.source(p,envir=.GlobalEnv)
}
load_module('state')
load_module('import_export')
