# Synthetic fixtures: a low peak may change assignments without contributing EI.
source('development/shiny_app/R/bootstrap.R');require_dependencies()
failures<-character()
check<-function(ok,msg){if(!isTRUE(ok)){failures<<-c(failures,msg);cat('FAIL:',msg,'\n')}else cat('PASS:',msg,'\n')}
fixture<-function(size,height) {
  p<-new_project();p$id<-'synthetic-threshold-order';p$plate$Code[1]<-'SYNTHETIC';p$plate$Role[1]<-'sample'
  p$peaks<-normalize_peaks(data.frame(Well='A01',Size=size,Height=height),list(Well='Well',Size='Size',Height='Height'),p$id)$peaks
  p<-sync_records(p);p$assay$minimum_bp<-100;p$assay$channel<-'ALL'
  p$records$Modal_rule<-'manual';p$records$Modal_peak_ID<-p$peaks$Peak_ID[p$peaks$Size==200]
  p$records$Category<-'expanded';p
}
fit<-function(p)fit_record(p$peaks,p$records[1,,drop=FALSE],p$assay)
base_size<-200+3*(0:5);base_height<-c(10000,5000,3000,2000,1200,800)

p<-fixture(c(base_size,218,221),c(base_height,299,300));f<-fit(p)
check(f$status=='fitted' && all(vapply(c('adaptive','bayesian'),function(m)
  !218%in%f[[m]]$selected$Size && 221%in%f[[m]]$selected$Size,logical(1))),
  'both methods exclude 299 RFU and retain exactly 300 RFU before assignment')
check(identical(f$peaks$Peak_ID,p$peaks$Peak_ID) && f$upper==221,
  'raw physical inventory and original search endpoint remain available')
check(all(vapply(c('adaptive','bayesian'),function(m)
  f[[m]]$selected$Delta[f[[m]]$selected$Size==221]==7,logical(1))),
  'removing Delta 6 does not compress the later repeat index')

p<-fixture(c(base_size,218.5,220.25),c(base_height,100,400));f<-fit(p)
s<-f$adaptive$selected
check(220.25%in%s$Size && s$Delta[s$Size==220.25]==7 && s$Expected_size_bp[s$Size==220.25]==221,
  'weak tail peak cannot move the Adaptive window away from a valid later peak')
check(abs(threshold_ei(s,10000,.03,Inf)$Value-28600/22400)<1e-12,
  'recovered Delta 7 contributes its independently calculated height-weighted EI')
check(identical(threshold_ei(s,10000,0,Inf)$Peaks$Peak_ID,threshold_ei(s,10000,.03,Inf)$Peaks$Peak_ID),
  'a lower reporting threshold cannot restore an excluded physical peak')

p<-fixture(c(base_size,218,218.05),c(base_height,800,100));f<-fit(p)
pr<-f$bayesian$predictions
check(f$bayesian$status=='calibrated' && pr$Status[pr$Delta==6]=='unique_candidate' &&
  f$bayesian$selected$Size[f$bayesian$selected$Delta==6]==218 && !method_unresolved(f,'bayesian',Inf),
  'below-floor competitor cannot make a valid Bayesian tail assignment ambiguous')
p<-fixture(c(base_size,218,218.05),c(base_height,800,300));f<-fit(p)
check(f$bayesian$predictions$Status[f$bayesian$predictions$Delta==6]=='ambiguous' && method_unresolved(f,'bayesian',Inf),
  'two eligible candidates remain ambiguous, including exact threshold equality')

p<-fixture(c(base_size,202.99),c(10000,rep(400,5),200));f<-fit(p)
check(f$bayesian$status=='calibrated' && nrow(f$bayesian$seeds)==5L && f$bayesian$seeds$Size[1]==203,
  'low competitor is removed before Bayesian seed dominance is evaluated')
p<-fixture(c(200,203,206,209),c(10000,400,400,299));f<-fit(p)
check(f$bayesian$status=='insufficient_seeds' && nrow(f$bayesian$seeds)==2,
  'filtering never manufactures a third calibration seed')

p<-fit_project(fixture(c(base_size,218,221),c(base_height,800,100)))
id<-p$records$Record_ID[1];high_id<-p$peaks$Peak_ID[p$peaks$Size==218]
reviewed<-create_reviewed(p,id,p$fit$records[[id]]$modal_id,exclude_ids=high_id,
  reason='Synthetic manual exclusion',evidence='Synthetic provenance regression')
decisions<-peak_decisions(reviewed,variant='reviewed-1',record_id=id)
check(all(decisions$Decision[decisions$Peak_ID==high_id]=='manually excluded from reviewed assignment') &&
  all(!decisions$Assigned[decisions$Peak_ID==high_id]),
  'reviewed high-peak exclusions retain their manual provenance instead of a false threshold label')
check(all(decisions$Decision[decisions$Size==221]=='below 3% assignment floor'),
  'a genuinely low peak remains distinguished from a manually excluded high peak')

p<-fit_project(fixture(c(base_size,218),c(base_height,299)))
check(!fit_is_stale(p),'new policy fits are current')
old_fingerprint<-function(p) {
  rec<-p$records[,c('Record_ID','Source_ID','Allele','Modal_rule','Modal_peak_ID','ROI_low','ROI_high'),drop=FALSE]
  assay<-p$assay[setdiff(names(p$assay),c('name','gene','organism','source','validation_scope','status','include_intermediate','ranges'))]
  object_hash(list(policy='non-normal-ei-v1.10',peaks=p$peaks,records=rec,assay=assay,reference=p$reference,structure=p$structure,
    disease_profile=p$disease_profile,profile_custom=p$profile_custom,localization=p$localization,prescreen=p$prescreen,
    target_overrides=p$target_overrides,localization_pending=p$localization_pending))
}
p$fit$fingerprint<-old_fingerprint(p)
check(fit_is_stale(p) && all(threshold_results(p)$Preview_status=='stale_results'),
  'saved pre-policy fits cannot display current EI without rerunning')
if(length(failures))stop(paste(failures,collapse='; '))
cat('THRESHOLD-FIRST SYNTHETIC SUITE PASS\n')
