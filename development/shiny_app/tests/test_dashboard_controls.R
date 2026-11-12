source('development/shiny_app/R/bootstrap.R');require_dependencies()
p<-fit_project(synthetic_project())
rec<-p$records[1,,drop=FALSE]
stopifnot(any(is.finite(threshold_results(p)$Preview_EI[threshold_results(p)$Record_ID==rec$Record_ID])))
p$plate$Role[p$plate$Well==rec$Well]<-'ce_fill'
p$records$Role[p$records$Record_ID==rec$Record_ID]<-'ce_fill'
d<-dashboard_data(p,threshold_results(p))
w<-d$wells[[which(vapply(d$wells,function(x)x$w==rec$Well,logical(1)))]]
stopifnot(w$acquired,length(w$records[[1]]$all)>0,
  is.null(w$records[[1]]$a),is.null(w$records[[1]]$b))
cat('DASHBOARD CONTROL WELL PASS\n')
