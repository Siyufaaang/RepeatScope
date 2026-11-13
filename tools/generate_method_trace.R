# Export a synthetic teaching trace from the unmodified analysis functions.
# This file is never run by a browser or on uploaded user data.
source('development/shiny_app/R/bootstrap.R');require_dependencies()
p<-synthetic_project();rec<-p$records[p$records$Well=='A04',,drop=FALSE]
f<-fit_record(p$peaks,rec,p$assay);stopifnot(f$status=='fitted')
a<-f$adaptive$selected;b<-f$bayesian
summarise<-function(selected) {
  z<-selected[selected$Height>=.03*f$modal_height,,drop=FALSE]
  list(ids=unname(z$Peak_ID),numerator=sum(z$Height*z$Delta),denominator=sum(z$Height),ei=sum(z$Height*z$Delta)/sum(z$Height))
}
trace<-list(source='SYNTHETIC A04 · Extended · 3% · unchanged RepeatScope methods',
  modal=f$modal_size,modalHeight=f$modal_height,threshold=.03,upper=f$upper,
  peaks=f$peaks[,c('Peak_ID','Size','Height')],
  adaptive=a[,c('Peak_ID','Delta','Expected_size_bp','Local_repeat_spacing_bp','Size')],
  bayesian=list(seeds=unname(b$seeds$Peak_ID),seedAudit=b$seed_audit,predictions=b$predictions,fit=b$fit,selected=b$selected[,c('Peak_ID','Delta')]),
  adaptiveEI=summarise(a),bayesianEI=summarise(b$selected),tolerance=p$assay$tolerance)
jsonlite::write_json(trace,'development/shiny_app/www/method-trace.json',auto_unbox=TRUE,pretty=TRUE,digits=16,na='null')
cat('Synthetic teaching trace exported from the existing methods.\n')
