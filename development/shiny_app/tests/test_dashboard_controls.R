source('development/shiny_app/R/bootstrap.R');require_dependencies()
p<-fit_project(synthetic_project())

# The synthetic A04 tail has method-only peaks at delta 18 and 19.
# Tail10 must agree; Extended must preserve distinct EI and peak flags.
# Expected fractions are independently summed from the fixture's RFU heights.
frozen_fit<-p$fit
for(scope in c('tail10','extended')) {
 p$window<-scope;r<-threshold_results(p);d<-dashboard_data(p,r)
 w<-d$wells[[which(vapply(d$wells,function(x)x$w=='A04',logical(1)))]]
 entry<-w$records[[1]];rows<-r[r$Well=='A04',]
 expected<-if(scope=='tail10')c(adaptive=57821/28685,bayesian=57821/28685) else
   c(adaptive=88651/30843,bayesian=76076/30163)
 stopifnot(nrow(rows)==2L,setequal(rows$Method,c('adaptive','bayesian')),
   length(entry$a)==1L,length(entry$b)==1L,is.finite(entry$a),is.finite(entry$b),
   max(abs(rows$Preview_EI-expected[rows$Method]))<1e-12,
   abs(entry$a-expected['adaptive'])<1e-12,abs(entry$b-expected['bayesian'])<1e-12,
   identical(p$fit,frozen_fit))
 flags<-do.call(rbind,entry$all)
 if(scope=='tail10')stopifnot(all(flags[,3]==flags[,4]),sum(flags[,3])==11) else
   stopifnot(sum(flags[,3])==17,sum(flags[,4])==15,sum(flags[,3]!=flags[,4])==2)
}
cat('DASHBOARD METHOD AND WINDOW ROUTING PASS\n')

rec<-p$records[1,,drop=FALSE]
stopifnot(any(is.finite(threshold_results(p)$Preview_EI[threshold_results(p)$Record_ID==rec$Record_ID])))
p$plate$Role[p$plate$Well==rec$Well]<-'ce_fill'
p$records$Role[p$records$Record_ID==rec$Record_ID]<-'ce_fill'
d<-dashboard_data(p,threshold_results(p))
w<-d$wells[[which(vapply(d$wells,function(x)x$w==rec$Well,logical(1)))]]
stopifnot(w$acquired,length(w$records[[1]]$all)>0,
  is.null(w$records[[1]]$a),is.null(w$records[[1]]$b))
cat('DASHBOARD CONTROL WELL PASS\n')
