source('development/shiny_app/R/bootstrap.R');require_dependencies()
options(repeatscope.root=rs_root)
check<-function(x,msg){if(!isTRUE(x))stop('FAIL: ',msg);cat('PASS:',msg,'\n')}
fixture<-function(pattern='expanded_present',normal=FALSE,second=FALSE,weak=FALSE) {
 p<-ensure_localization(new_project());p$id<-'SYNTHETIC-v110'
 p$plate$Code[1]<-'SYN-001';p$plate$Role[1]<-'sample'
 size<-197.52+2.9*(0:15);height<-c(10000,6000,3500,2000,1400,1000,rep(400,10))
 if(normal){size<-c(165.79,size);height<-c(30000,height)}
 if(second){size<-c(size,260+2.9*(0:5));height<-c(height,10000,6000,3500,2000,1400,1000)}
 if(weak){size<-c(165.79,198.69);height<-c(32089,88)}
 p$peaks<-normalize_peaks(data.frame(Well='A01',Size=size,Height=height,Channel='B'),
   list(Well='Well',Size='Size',Height='Height',Channel='Channel'),p$id)$peaks
 p<-sync_records(p);p$assay$channel<-'B';p$assay$minimum_bp<-103
 p$localization<-list(name='SYNTHETIC',gene='SYNTHETIC',normal_max=31,normal_inclusive=TRUE,
  expanded_min=33,expanded_inclusive=TRUE,source='SYNTHETIC test ranges',counting='SYNTHETIC triplets',
  mode='linear',unit=3,intercept=103,slope=3,uncertainty=0,mapping_source='SYNTHETIC exact fixture')
 set_prescreen(p,'A01',pattern)
}
p<-prepare_localized_project(fixture())
check(nrow(p$records)==1 && p$records$Target_kind=='intermediate' && p$records$Modal_rule=='manual',
 'unique intermediate cluster is allocated despite expanded prescreen')
p<-fit_project(p);r<-threshold_results(p)
check(all(is.finite(r$Preview_EI)),'both methods calculate intermediate EI')
check(all(is.na(r$EI)),'calculation does not invent formal evidence')
p2<-fixture('intermediate');p2<-fit_project(prepare_localized_project(p2))
check(all(is.finite(threshold_results(p2)$Preview_EI)),'explicit intermediate prescreen calculates')
mixed<-prepare_localized_project(fixture('normal_expanded',normal=TRUE))
check(setequal(mixed$records$Target_kind,c('normal','intermediate')),'normal and intermediate retain distinct identities')
mixed<-fit_project(mixed);rr<-threshold_results(mixed)
check(all(is.na(rr$Preview_EI[rr$Allele=='normal-1'])) && all(is.finite(rr$Preview_EI[rr$Allele=='intermediate-1'])),
 'only normal target is excluded by category')
amb<-prepare_localized_project(fixture(second=TRUE))
check(all(amb$records$Modal_rule=='blocked'),'two competing non-normal clusters do not get guessed')
norm<-fit_project(prepare_localized_project(fixture('normal_only',weak=TRUE)))
check(all(norm$records$Target_kind=='normal') && all(threshold_results(norm)$Preview_status=='not_applicable_normal'),
 'weak regional boundary fragment does not turn a normal target into a conflict')
boundary<-fixture();boundary$localization$uncertainty<-3
boundary<-fit_project(prepare_localized_project(boundary))
check(all(boundary$records$Target_kind=='research') && all(is.finite(threshold_results(boundary)$Preview_EI)),
 'unique boundary target calculates without asserting a category')
overlap<-p;dup<-overlap$records;dup$Record_ID<-paste0(dup$Record_ID,'-second');overlap$records<-rbind(overlap$records,dup)
overlap<-fit_project(overlap)
check(all(vapply(overlap$fit$records,function(f)f$status=='target_overlap',logical(1))),
 'intermediate targets participate in shared-physical-tail detection')
old<-p;old$fit$fingerprint<-'pre-v110-policy'
check(fit_is_stale(old) && all(is.na(threshold_results(old)$Preview_EI)),'obsolete saved fits cannot display as current')
cat('INTERMEDIATE EI SUITE PASS\n')

pc<-localized_synthetic_demo();pc$localization$uncertainty<-NA_real_
pc$plate$Role[pc$plate$Well=='A01']<-'positive_control'
pc<-fit_project(prepare_localized_project(pc));pr<-threshold_results(pc);pr<-pr[pr$Well=='A01',]
check(all(is.na(pr$EI)) && all(pr$Formal_status=='eligibility_unresolved') && all(is.finite(pr$Preview_EI)),
 'positive control with unknown CE precision retains research EI without formal clearance')
legacy<-synthetic_project();legacy$plate$Role[legacy$plate$Well=='A01']<-'positive_control'
legacy<-fit_project(sync_records(legacy));lr<-threshold_results(legacy)
check(all(lr$Formal_status[lr$Well=='A01']=='success'),'legacy non-localized positive controls retain existing clearance')
