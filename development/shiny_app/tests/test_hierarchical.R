source('development/shiny_app/tests/helper.R');load_module('engine');load_module('report')
check(exists('fit_hierarchical'),'hierarchical adapter is implemented')
p<-new_project();p$id<-'synthetic-pool'
for(i in 1:4){p$plate$Code[i]<-paste0('S',i);p$plate$Role[i]<-'sample'}
raw<-do.call(rbind,lapply(1:4,function(i)data.frame(Well=p$plate$Well[i],Size=200+(2.9+i*.005)*(0:16),Height=c(10000,6000,4000,3000,2000,1000,rep(400,11)))))
p$peaks<-normalize_peaks(raw,list(Well='Well',Size='Size',Height='Height'),p$id)$peaks;p<-sync_records(p)
p$records$Material_ID<-c('M1','M1','M3','M4');p$records$Duplicate_status<-'technical_repeat';p<-fit_project(p)
pool<-p$records$Record_ID
check(fit_hierarchical(p,pool)$hierarchical$result$records[[pool[1]]]$status=='duplicate_unresolved','shared material with two representatives blocks training')
p<-resolve_duplicate(p,duplicate_records(p)$Duplicate_set[1],'technical_repeat','M1',p$records$Source_ID[1],'Synthetic explicit representative')
q<-fit_hierarchical(p,pool)
h<-q$hierarchical$result$records[[pool[1]]]
check(h$status=='calibrated' && setequal(h$peer_records,pool[3:4]),'target and same-material record excluded from explicit pool')
check(all(c(pool[1],pool[2])%in%h$excluded_records),'actual material-exclusion units recorded')
check(identical(h$selected$Height,p$peaks$Height[match(h$selected$Peak_ID,p$peaks$Peak_ID)]),'hierarchical always keeps own-well heights')
p2<-p;p2$fit$records[[pool[1]]]$adaptive$selected$Size[10]<-1e6;p2$fit$records[[pool[1]]]$bayesian$selected$Size[10]<-1e6
p2$fit$records[[pool[2]]]$adaptive$selected$Size[10]<-1e6;p2$fit$records[[pool[2]]]$bayesian$selected$Size[10]<-1e6
h2<-fit_hierarchical(p2,pool)$hierarchical$result$records[[pool[1]]]
check(identical(h$fit,h2$fit),'target and material duplicate tail perturbations cannot enter training')
q2<-fit_hierarchical(p,pool[1:3]);check(q2$hierarchical$result$records[[pool[1]]]$status=='insufficient_peers','fewer than two independent material peers gives no fake model')
p$plate$Group[1:4]<-'Different report group';p<-sync_records(p)
check(identical(hierarchy_fingerprint(p,pool),hierarchy_fingerprint(q,pool)),'display groups do not change explicit training pool')
p$records$Material_ID[2]<-'different tissue';h3<-fit_hierarchical(p,pool)$hierarchical$result$records[[pool[1]]]
check(pool[2]%in%h3$peer_records,'different materials from same individual are not automatically excluded')
cat('HIERARCHICAL SUITE PASS\n')
