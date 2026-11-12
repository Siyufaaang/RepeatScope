source('development/shiny_app/tests/helper.R');load_module('reference');load_module('engine');load_module('report');load_module('synthetic')
failures<-character()
check<-function(ok,msg){if(!isTRUE(ok)){failures<<-c(failures,msg);cat('FAIL:',msg,'\n')}else cat('PASS:',msg,'\n')}
p<-fit_project(synthetic_project());r<-p$records[1,]
p$control_review$scope<-'unresolved'
check(eligibility(r,p)$status=='control_review_required','passed control with unresolved scope cannot clear formal EI')
p$control_review$scope<-'batch';p$control_review$batches<-'another-batch'
check(eligibility(r,p)$status=='control_review_required','batch pass applies only to named matching batch')
p$control_review$batches<-r$Batch
check(eligibility(r,p)$status=='success','evidenced pass for matching batch clears control gate')
new<-r;new$Record_ID<-paste0(r$Source_ID,'::target-2');new$Allele<-'target-2';p$records<-rbind(p$records,new)
check(eligibility(r,p)$status=='eligibility_unresolved','overlapping same-acquisition allele regions are not independent tails')
p$records$ROI_low[nrow(p$records)]<-300;p$records$ROI_high[1]<-250
check(eligibility(p$records[1,],p)$status=='success','separate same-locus allele regions can be reviewed without multiplex rejection')
check(length(names(record_defaults(0)))>10,'zero-row metadata schema remains available')
raw<-data.frame(Well='A01',Acquisition=c('injection-1',''),Size=c(200,203),Height=c(1000,300))
im<-normalize_peaks(raw,list(Well='Well',Acquisition='Acquisition',Size='Size',Height='Height'))
check(nrow(im$issues)==1 && nrow(im$peaks)==1,'blank mapped acquisition IDs are quarantined instead of merged')
p<-fit_project(synthetic_project());p$records$Material_ID[1:2]<-'same-material'
check(nrow(duplicate_records(p))>=2 && eligibility(p$records[1,],p)$status=='duplicate_unresolved','different codes with same material require one representative review')
p<-synthetic_project();p$peaks$Height[1]<-12345;p<-sync_records(p)
check(p$records$QC_status[1]=='unknown' && !p$records$Target_confirmed[1],'new physical input invalidates previous upstream and allocation approval')
p<-fit_project(synthetic_project());p<-fit_hierarchical(p,p$records$Record_ID[1:4]);id<-p$records$Record_ID[1]
f<-p$fit$records[[id]];p<-create_reviewed(p,id,f$bayesian$seeds$Peak_ID[1],reason='Synthetic changed target',evidence='Synthetic review only')
reviewed<-threshold_results(p,variant='reviewed-1')
check(!'hierarchical'%in%reviewed$Method,'reviewed variant does not reuse automatic hierarchy with different modal')
man<-analysis_manifest(p,variant='reviewed-1')
check(man$run_id=='reviewed-1' && identical(man$review$evidence,'Synthetic review only'),'reviewed manifest names selected run and its review evidence')
s<-synthetic_project()$structure;ref<-list(accession='EXAMPLE.1',start=1001,end=1067,strand=2,length=67)
co<-reference_coordinates(s$amplicon,ref)
check(identical(co$range,c(1001,1067)) && co$strand==-1,'accession coordinates compose fetched interval and strand')
p<-fit_project(synthetic_project());f<-p$fit$records[[p$records$Record_ID[p$records$Well=='A04']]]
a<-threshold_ei(f$adaptive$selected,f$modal_height,.03,Inf)$Peaks;b<-threshold_ei(f$bayesian$selected,f$modal_height,.03,Inf)$Peaks
check(all(c(18,19)%in%a$Delta[!a$Peak_ID%in%b$Peak_ID]),'synthetic A04 has visible above-threshold extended method conflicts')
if(length(failures))stop(paste(failures,collapse='; '))
cat('EDGE CASE SUITE PASS\n')
