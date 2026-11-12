source('development/shiny_app/R/bootstrap.R');require_dependencies()
check<-function(ok,msg){if(!isTRUE(ok))stop('FAIL: ',msg);cat('PASS:',msg,'\n')}
p<-new_project()
p$plate$Code[1:3]<-c('tissue-A','tissue-B','water')
p$plate$Role[1:3]<-c('sample','sample','ce_fill')
p$plate$Group[1:2]<-'Case_1'
p<-sync_records(p)
p$records$Individual_ID[p$records$Well=='A02']<-'Case_1'
raw<-data.frame(Well=c('A01','A01'),Size=c(200,203),Height=c(1000,200),Acquisition='A01A01.fsa')
p$peaks<-normalize_peaks(raw,list(Well='Well',Size='Size',Height='Height',Acquisition='Acquisition'),p$id)$peaks
p<-sync_records(p)
check(nrow(p$records)==3,'Partial plate import retains wells with no acquisitions')
check(p$records$Acquisition[p$records$Well=='A01']=='A01A01.fsa','Actual acquisition replaces its empty placeholder')
check(p$records$Individual_ID[p$records$Well=='A02']=='Case_1','Existing no-peak metadata is preserved')
check(all(p$records$Group[p$records$Well%in%c('A01','A02')]=='Case_1'),'Groups survive mixed old and new record binding')
check(p$records$Role[p$records$Well=='A03']=='ce_fill','CE filling role survives without peak data')
