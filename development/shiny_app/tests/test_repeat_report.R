source('development/shiny_app/R/bootstrap.R')
source('development/shiny_app/tests/helper.R')
for(m in c('reference','guided','localization','engine','report','synthetic'))load_module(m)
p<-ensure_localization(synthetic_project())
p$localization<-utils::modifyList(default_localization(),list(mode='theoretical',unit=3,counting='Test bounded triplets',mapping_source='Synthetic test mapping',uncertainty=.6))
p$records$Target_kind<-p$records$Category
p$records$Target_kind[2]<-'intermediate'
p<-fit_project(p)
original<-threshold_results(p);decisions<-peak_decisions(p);before<-object_hash(p)
r<-allele_repeat_report(p)
check(nrow(r)==nrow(p$records) && !anyDuplicated(r$Record_ID),'repeat report preserves each acquisition / allele record')
modal<-vapply(p$fit$records,function(x)x$modal_size%||%NA_real_,numeric(1))
check(near(r$Estimated_repeat_number,(unname(modal[r$Record_ID])-project_structure_view(p)$fixed_bp)/3),'theoretical mapping uses the current reference fixed length')
check(all(is.finite(r$Estimated_repeat_number[r$Target_kind%in%c('normal','expanded','intermediate')])),'normal, expanded and intermediate counts do not depend on EI eligibility')
check(all(abs((r$Repeat_margin_high-r$Estimated_repeat_number)-.2)<1e-10,na.rm=TRUE),'known CE bp margin propagates to repeat units')
out<-report_results(p)
check(identical(out[,names(original)],original) && identical(peak_decisions(p),decisions) && identical(object_hash(p),before),'report additions do not mutate input, EI or peak selection')
draft<-p;draft$reference_draft<-draft$structure;draft['structure']<-list(NULL)
draft$reference_draft$suggestions<-repeat_candidates(draft$reference$sequence,'CAG')
draft<-fit_project(draft);draft_counts<-allele_repeat_report(draft)
draft$reference_draft$end<-draft$reference_draft$end-3
check(fit_is_stale(draft) && all(is.na(allele_repeat_report(draft)$Estimated_repeat_number)),'editing an unconfirmed counted tract invalidates the old fit and repeat count')
draft<-fit_project(draft)
check(near(allele_repeat_report(draft)$Estimated_repeat_number,draft_counts$Estimated_repeat_number-1),'rerun uses the newly applied draft structure mapping')
multi<-p;first<-multi$records[1,,drop=FALSE];second<-first
second$Record_ID<-paste0(first$Record_ID,'|other');second$Allele<-'other';second$Target_kind<-'intermediate'
second$ROI_low<-204;second$ROI_high<-206
multi$records$ROI_low[1]<-200;multi$records$ROI_high[1]<-203
multi$records<-rbind(multi$records,second)
third<-first;third$Record_ID<-paste0(first$Record_ID,'-repeat');third$Source_ID<-third$Record_ID;third$Acquisition<-'repeat-injection'
pk<-multi$peaks[multi$peaks$Record_ID==first$Source_ID,,drop=FALSE]
pk$Record_ID<-third$Source_ID;pk$Acquisition<-third$Acquisition;pk$Peak_ID<-paste0(pk$Peak_ID,'-repeat');pk$Size<-pk$Size+30
multi$peaks<-rbind(multi$peaks,pk);multi$records<-rbind(multi$records,third);multi<-fit_project(multi)
mr<-allele_repeat_report(multi);z<-mr[mr$Well=='A01',]
check(nrow(z)==3 && length(unique(z$Estimated_repeat_number))==3 && length(unique(z$Acquisition))==2,'separate targets and repeat acquisitions keep distinct modal counts')
payload<-dashboard_data(multi,report_results(multi));w<-payload$wells[[which(multi$plate$Well=='A01')]]
check(length(w$records)==3 && all(vapply(w$records,function(x)near(x$repeat_number$Estimated_repeat_number,mr$Estimated_repeat_number[match(x$id,mr$Record_ID)]),logical(1))),'dashboard repeat values match each exported record identity')
p$threshold<-.15;p$window<-'tail10'
check(identical(allele_repeat_report(p),r),'height threshold and EI window do not change repeat number')
p$localization$mode<-'linear';p$localization$intercept<-100;p$localization$slope<-2.9;p$localization$uncertainty<-.58
p<-fit_project(p);lin<-allele_repeat_report(p)
check(near(lin$Estimated_repeat_number,(unname(modal[lin$Record_ID])-100)/2.9) && all(abs(lin$Repeat_margin_high-lin$Estimated_repeat_number-.2)<1e-10,na.rm=TRUE),'documented linear CE mapping is used without rounding to integer repeats')
p$localization$uncertainty<-NA_real_;p<-fit_project(p)
check(all(is.na(allele_repeat_report(p)$Repeat_margin_low)) && any(is.finite(allele_repeat_report(p)$Estimated_repeat_number)),'unknown uncertainty stays unavailable while nominal estimates remain labelled estimates')
p$localization$intercept<-101
check(all(is.na(allele_repeat_report(p)$Estimated_repeat_number)) && all(allele_repeat_report(p)$Repeat_status=='stale'),'stale fits suppress all old repeat estimates')
p$fit<-NULL
check(all(allele_repeat_report(p)$Repeat_status=='not_fitted'),'unfitted records retain explicit unavailable reasons')
p$localization$mode<-'';p<-fit_project(p)
check(all(is.na(allele_repeat_report(p)$Estimated_repeat_number)),'unconfigured mapping never falls back to an empirical constant')
p$localization$mode<-'linear';p$localization$slope<-0;p<-fit_project(p)
check(all(is.na(allele_repeat_report(p)$Estimated_repeat_number)),'invalid mapping cannot divide by zero')
p$localization$slope<-3;p$localization$intercept<-0;p<-fit_project(p)
p$fit$records[[1]]$status<-'target_overlap'
check(is.na(allele_repeat_report(p)$Estimated_repeat_number[1]),'overlapping target fit cannot present an unqualified repeat estimate')
empty<-allele_repeat_report(new_project())
check(nrow(empty)==0 && 'Estimated_repeat_number'%in%names(empty),'empty project exports a typed empty repeat table')
tables_dir<-tempfile();dir.create(tables_dir)
tables<-write_tables(p,tables_dir)
check(file.exists(file.path(tables_dir,'allele_repeat_numbers.csv')) && 'allele_repeat_numbers'%in%names(tables),'report CSV and workbook include the separate per-allele repeat table')
check('Estimated_repeat_number'%in%names(tables$EI_comparison),'paired method export retains allele repeat estimates')
unlink(tables_dir,recursive=TRUE)
