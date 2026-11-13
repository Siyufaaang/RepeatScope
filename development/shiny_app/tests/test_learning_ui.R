source('development/shiny_app/R/bootstrap.R');require_dependencies();library(shiny)
source('development/shiny_app/R/ui.R')
stopifnot(all(vapply(field_guidance,function(h)all(c('unit','example','requirement','condition')%in%names(h)) && nzchar(h$unit) && nzchar(h$example),logical(1))))
stopifnot(field_guidance$loc_uncertainty$requirement=='Optional',field_guidance$loc_intercept$requirement=='Conditional',field_guidance$map_Size$unit=='bp')
html<-as.character(numericInput('threshold_number','Exact %',3,min=0,max=100))
stopifnot(grepl('threshold_number-help',html,fixed=TRUE),grepl('data-field-help',html,fixed=TRUE),grepl('value="3"',html,fixed=TRUE),grepl('type="button"',html,fixed=TRUE))
check_html<-as.character(checkboxInput('edit_group','Change Group',FALSE))
stopifnot(grepl('</label>.*field-help-trigger',gsub('[\r\n]','',check_html)),!grepl('checked=',check_html,fixed=TRUE))
cat('FIELD HELP METADATA AND NON-MUTATING INPUT WRAPPERS PASS\n')
trace<-jsonlite::fromJSON(file.path(rs_app_dir,'www/method-trace.json'))
p<-synthetic_project();r<-p$records[p$records$Well=='A04',,drop=FALSE];f<-fit_record(p$peaks,r,p$assay)
stopifnot(identical(trace$peaks$Peak_ID,f$peaks$Peak_ID),isTRUE(all.equal(trace$peaks$Size,f$peaks$Size)),
 isTRUE(all.equal(trace$adaptive$Expected_size_bp,f$adaptive$selected$Expected_size_bp)),
 isTRUE(all.equal(trace$bayesian$seedAudit$Low,f$bayesian$seed_audit$Low)),
 isTRUE(all.equal(trace$bayesian$predictions$High,f$bayesian$predictions$High)),
 abs(trace$adaptiveEI$ei-threshold_ei(f$adaptive$selected,f$modal_height,.03,Inf)$Value)<1e-12,
 abs(trace$bayesianEI$ei-threshold_ei(f$bayesian$selected,f$modal_height,.03,Inf)$Value)<1e-12)
cat('FROZEN TEACHING TRACE MATCHES ACTUAL UNMODIFIED R METHODS\n')
