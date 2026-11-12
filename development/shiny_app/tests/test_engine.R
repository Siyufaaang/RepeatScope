source('development/shiny_app/tests/helper.R');load_module('reference');load_module('engine');load_module('report')
check(exists('fit_record'),'analysis adapters are implemented')
z<-data.frame(Delta=c(0,1,12,13),Height=c(1000,200,30,29),Peak_ID=letters[1:4],Size=c(200,203,236,239))
check(near(threshold_ei(z,1000,.03,Inf)$Value,560/1230),'inclusive threshold, modal denominator and extended EI')
check(near(threshold_ei(z,1000,.03,10)$Value,1/6),'tail10 excludes delta12')
check(threshold_ei(z[1,,drop=FALSE],1000,.03,10)$Value==0,'valid modal-only EI is zero')
check(is.na(threshold_ei(z[-1,],1000,.03,10)$Value),'missing modal EI is unavailable')
p<-new_project();p$id<-'test';p$plate$Code[1]<-'001';p$plate$Role[1]<-'sample'
raw<-data.frame(Well='A01',Size=200+2.9*(0:20),Height=c(10000,6000,3500,2000,1400,1000,rep(400,15)))
p$peaks<-normalize_peaks(raw,list(Well='Well',Size='Size',Height='Height'),p$id)$peaks;p<-sync_records(p)
p$records$Category<-'expanded';p$records$Evidence<-'Synthetic stated category';p$records$Target_confirmed<-TRUE;p$records$Target_evidence<-'Synthetic target'
p$assay$status<-'configured';p$assay$source<-'Synthetic specification';p$structure<-list(status='confirmed')
p$assay$ranges<-data.frame(Category=c('normal','intermediate','expanded'),Lower=c(0,20,35),Upper=c(20,35,Inf),Lower_inclusive=TRUE,Upper_inclusive=FALSE,EI_allowed=c(FALSE,TRUE,TRUE),Source='Synthetic only')
p<-fit_project(p);f<-p$fit
r<-threshold_results(p,.03,'extended')
check(all(r$Status=='qc_unconfirmed') && all(is.na(r$EI)) && all(is.finite(r$Preview_EI)),'unknown QC preserves preview but withholds formal EI')
r2<-threshold_results(p,.2,'extended')
check(identical(f,p$fit) && r$Preview_EI[1]!=r2$Preview_EI[1],'slider changes only height filtering, not frozen models')
p$records$QC_status<-'passed';p$records$QC_evidence<-'Synthetic QC demonstration';p$control_review<-list(status='passed',scope='assay',batches=character(),evidence='Synthetic review, not laboratory QC')
r<-threshold_results(p,.03,'extended')
check(all(r$Status=='success') && all(is.finite(r$EI)),'documented synthetic eligibility and QC allow formal demonstration')
p$records$Category<-'normal'
check(all(is.na(threshold_results(p,.001,'extended')$EI)) && all(threshold_results(p,.001,'extended')$Status=='not_applicable_normal'),'slider never bypasses normal-range NA')
p$records$Category<-'intermediate'
check(all(threshold_results(p,.03,'tail10')$Status=='intermediate_excluded'),'intermediate is excluded by default')
p$assay$include_intermediate<-TRUE
check(all(threshold_results(p,.03,'tail10')$Status=='success'),'intermediate checkbox changes eligibility without refitting')
p$records$Category<-'expanded';p$peaks$Saturation[1]<-'TRUE';p<-fit_project(p)
check(all(threshold_results(p,.03,'tail10')$Status=='qc_failed'),'manual QC pass cannot override imported saturation')
p$peaks$Saturation[1]<-'';p<-fit_project(p);p$control_review$status<-'failed';p$control_review$scope<-'unresolved'
check(p$records$QC_status=='unknown' && !p$records$Target_confirmed,'changed input clears prior QC and target approval')
p$records$QC_status<-'passed';p$records$QC_evidence<-'Synthetic repeat review';p$records$Target_confirmed<-TRUE;p$records$Target_evidence<-'Synthetic repeat target review'
check(all(threshold_results(p,.03,'tail10')$Status=='control_review_required'),'unmapped control anomaly cannot silently pass')
p$control_review$status<-'passed';p$peaks$Height[3]<-999
check(all(threshold_results(p,.03,'tail10')$Status=='stale_results'),'changed input keeps old results visibly stale')
check(classify_repeat(19.9,20.1,p$assay$ranges)=='unresolved','cutoff-crossing estimates remain unresolved')
check(classify_repeat(20,20,p$assay$ranges)=='intermediate','explicit inclusive range boundary')
bad<-p$assay$ranges;bad$Upper_inclusive[1]<-TRUE
check(fails(validate_ranges(bad)),'overlapping disease cutoffs rejected')
rr<-data.frame(Record_ID=c('a','a','b','b','c','c'),Source_ID=c('a','a','b','b','c','c'),Method=rep(c('adaptive','bayesian'),3),Group='G',Code=rep(c('01','02','03'),each=2),Well=rep(c('A01','A02','A03'),each=2),
  EI=c(1,2,3,NA,NA,NA),Status=c('success','success','success','analysis_failed','qc_unconfirmed','qc_unconfirmed'),Role='sample',Representative=TRUE,Individual_ID='',Material_ID='',Duplicate_status='unreviewed',Window='tail10',Threshold=.03,Variant='automatic')
sm<-summarise_results(rr)
check(sm$N[sm$Method=='adaptive']==2 && sm$Common_valid_n[1]==1,'method n and shared-valid n use their own denominators')
check(is.na(sm$SD[sm$Method=='bayesian']) && is.na(sm$IQR[sm$Method=='bayesian']) && all(is.na(sm$Individual_n)),'singleton dispersion and unknown individual n are NA')
cat('ENGINE SUITE PASS\n')
