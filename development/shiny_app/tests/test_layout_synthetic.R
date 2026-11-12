# Data-free coverage of the v1.9 layout workflow and review regressions.
source('development/shiny_app/R/bootstrap.R');require_dependencies();library(shiny)
options(repeatscope.root=rs_root)
source(file.path(rs_app_dir,'R/server.R'));source(file.path(rs_app_dir,'R/ui.R'))
check<-function(x,msg){if(!isTRUE(x))stop(msg);cat('PASS:',msg,'\n')}

p<-ensure_sample_layout(new_project());p$plate$Code[1]<-'0001';p$plate$Role[1]<-'sample'
p<-apply_layout_patch(p,list(list(Well='A01',Individual_ID='001',Genotype='normal_only')))
p$peaks<-normalize_peaks(data.frame(Well='A01',Acquisition='synthetic new injection',Size=100,Height=200),
  list(Well='Well',Acquisition='Acquisition',Size='Size',Height='Height'),p$id)$peaks
p<-sync_records(p)
check(all(p$records$Individual_ID=='001'),'new acquisitions inherit an explicit text Individual ID')
q<-apply_layout_patch(p,list(list(Well='A01',Group='Synthetic group')))
check(identical(q$prescreen,p$prescreen),'group-only edit preserves genotype and provenance exactly')
q<-undo_plate(p)
check(q$plate$Individual_ID[1]=='' && all(q$records$Individual_ID==''),'Undo restores IDs inherited after the edit')

p<-ensure_sample_layout(localized_synthetic_demo())
# Non-default row names simulate restored legacy tables without private fixtures.
row.names(p$prescreen)<-paste0('saved-',seq_len(nrow(p$prescreen)))
p<-fit_project(p)
for(field in c('Group','Individual_ID')) {
  patch<-list(Well='A01');patch[[field]]<-'Synthetic label'
  q<-apply_layout_patch(p,list(patch))
  check(identical(q$prescreen,p$prescreen) && !fit_is_stale(q),paste('label-only',field,'preserves a current fit'))
}
p$plate$Individual_ID[1]<-''
r<-p$records[p$records$Well=='A01',,drop=FALSE][1,,drop=FALSE]
r$Record_ID<-paste0(r$Record_ID,'::synthetic-second');r$Allele<-'synthetic-second';r$Individual_ID<-'second-label'
p$records$Individual_ID[p$records$Well=='A01']<-'first-label';p$records<-rbind(p$records,r)
q<-ensure_sample_layout(p)
check(q$plate$Individual_ID[1]=='' && identical(q$records$Individual_ID,p$records$Individual_ID),'migration retains independent legacy labels')
q<-undo_plate(apply_layout_patch(q,list(list(Well='A01',Individual_ID='combined'))))
check(identical(q$records$Individual_ID[match(p$records$Record_ID,q$records$Record_ID)],p$records$Individual_ID),'Undo restores independent legacy labels by record identity')

testServer(rs_server,{
  session$setInputs(load_demo=1)
  session$setInputs(layout_select=list(mode='group',value=rv$p$plate$Group[1]))
  selected<-rv$selected;old<-ensure_localization(rv$p)$prescreen
  check(length(selected)>0,'Group selects wells')
  session$setInputs(layout_apply=list(fields=list(Individual_ID='001'),nonce=1))
  check(all(rv$p$plate$Individual_ID[rv$p$plate$Well%in%selected]=='001') && identical(rv$p$prescreen,old),'bulk edits only explicitly requested fields')
  session$setInputs(layout_select=list(mode='individual',value='001',nonce=2))
  check(setequal(rv$selected,selected),'Individual selection roundtrip')
  before<-object_hash(rv$p);rows<-sample_layout_rows(rv$p)
  session$setInputs(layout_table_apply=list(key=object_hash(rows),patch=list(list(Well='A01',Code='0004'),list(Well='Z99',Group='invalid')),nonce=1))
  check(identical(object_hash(rv$p),before) && nzchar(rv$layout_error),'invalid table is rejected atomically')
  session$setInputs(layout_table_apply=list(key='old',patch=list(list(Well='A01',Code='0004')),nonce=2))
  check(identical(object_hash(rv$p),before),'stale table cannot overwrite current metadata')
  session$setInputs(layout_draft=list(pending=TRUE,table_pending=TRUE,affects_fit=FALSE))
  session$setInputs(layout_apply=list(fields=list(Individual_ID='002'),nonce=2))
  check(identical(object_hash(rv$p),before) && grepl('table',rv$layout_error),'bulk apply preserves a staged table')
  session$setInputs(layout_draft=list(pending=FALSE,table_pending=FALSE,affects_fit=FALSE))
  session$setInputs(layout_table_apply=list(key=object_hash(rows),patch=list(list(Well='A01',Code='0004',Individual_ID='')),nonce=3))
  check(rv$p$plate$Code[1]=='0004' && rv$p$plate$Individual_ID[1]=='','table keeps leading zeros and explicit blanks')
  session$setInputs(undo_plate=1);check(identical(sample_layout_rows(rv$p),rows),'Undo restores the full table patch')
  session$setInputs(run_analysis=1);frozen<-rv$p$fit
  check(!is.null(frozen),'synthetic demo computes')
  session$setInputs(layout_draft=list(pending=TRUE,affects_fit=TRUE))
  check(all(is.na(results()$Preview_EI)),'genotype draft hides old EI')
  session$setInputs(layout_draft=list(pending=FALSE,affects_fit=FALSE))
  check(identical(rv$p$fit,frozen) && any(is.finite(results()$Preview_EI)),'discard restores the unchanged fit')
  session$setInputs(edit_layout=1);check(rv$step==2,'report opens sample information')
  session$setInputs(edit_analysis=1);check(rv$step==4,'report opens analysis settings')
  session$setInputs(show_report=1);check(rv$step==5,'return to report')
  p<-rv$p;p$prescreen<-p$prescreen[p$prescreen$Well!='A01',,drop=FALSE];rv$p<-p
  session$setInputs(run_analysis=2)
  check(rv$step==2 && 'A01'%in%rv$selected,'missing genotype routes Run back to the affected well')
})
cat('SYNTHETIC V19 LAYOUT PASS\n')
