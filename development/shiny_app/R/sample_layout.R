# Plate-level sample annotations; never infer genotype from group or individual.
genotype_labels<-c('Not set'='', 'Normal only'='normal_only','Normal + expanded'='normal_expanded',
  'Expanded; other unresolved'='expanded_present','Two expanded expected'='two_expanded',
  'Intermediate'='intermediate','Unclear'='unclear')
ensure_sample_layout<-function(p) {
  p<-ensure_localization(p)
  if(!'Individual_ID'%in%names(p$plate)) {
    p$plate$Individual_ID<-''
    if(nrow(p$records))for(w in p$plate$Well) {
      ids<-unique(p$records$Individual_ID[p$records$Well==w & nzchar(p$records$Individual_ID)])
      if(length(ids)==1)p$plate$Individual_ID[p$plate$Well==w]<-ids
    }
  }
  p
}
sample_layout_rows<-function(p) {
  p<-ensure_sample_layout(p);z<-p$plate[,c('Well','Code','Individual_ID','Group','Role'),drop=FALSE]
  z$Genotype<-text_value(p$prescreen$Pattern[match(z$Well,p$prescreen$Well)])
  z$Genotype[z$Role!='sample']<-'';z
}
layout_select<-function(p,mode,value='') {
  z<-sample_layout_rows(p)
  switch(mode,group=z$Well[z$Group==value & nzchar(value)],individual=z$Well[z$Individual_ID==value & nzchar(value)],
    missing=z$Well[z$Role=='sample' & !nzchar(z$Genotype)],samples=z$Well[z$Role=='sample'],character())
}
layout_snapshot<-function(p)list(plate=p$plate,prescreen=p$prescreen,
  individuals=if(nrow(p$records))p$records[,c('Record_ID','Source_ID','Well','Allele','Individual_ID'),drop=FALSE] else data.frame())
apply_layout_patch<-function(p,patch,expected_key=NULL) {
  p<-ensure_sample_layout(p)
  if(!is.null(expected_key) && !identical(expected_key,object_hash(sample_layout_rows(p))))stop('Plate information changed. Reload the table before applying edits.')
  if(is.data.frame(patch))patch<-lapply(seq_len(nrow(patch)),function(i)as.list(patch[i,,drop=FALSE]))
  if(!length(patch))return(p)
  allowed<-c('Well','Code','Individual_ID','Group','Role','Genotype')
  wells<-vapply(patch,function(r){if(!is.list(r) || !'Well'%in%names(r) || any(!names(r)%in%allowed))stop('Use Well, Sample ID, Individual ID, Group, Role and Genotype columns.');as.character(r$Well)},character(1))
  if(anyNA(wells) || any(!wells%in%p$plate$Well) || anyDuplicated(wells))stop('Each edit must name one unique existing well A01–H12.')
  for(r in patch) {
    if(any(!vapply(r,function(v)is.character(v) && length(v)==1 && !is.na(v),logical(1))))stop('Use one text value per cell; blank clears that field.')
    if('Role'%in%names(r) && !r$Role%in%roles)stop('Invalid Role in ',r$Well)
    if('Genotype'%in%names(r) && !r$Genotype%in%unname(genotype_labels))stop('Invalid Genotype in ',r$Well)
    role<-r$Role%||%p$plate$Role[match(r$Well,p$plate$Well)]
    if(role!='sample' && nzchar(r$Genotype%||%''))stop('Genotype applies to samples only: ',r$Well)
  }
  snapshot<-layout_snapshot(p);prescreen_changed<-FALSE
  for(r in patch) {
    i<-match(r$Well,p$plate$Well)
    for(nm in intersect(names(r),c('Code','Individual_ID','Group','Role')))p$plate[[nm]][i]<-trimws(r[[nm]])
    if('Individual_ID'%in%names(r) && nrow(p$records))p$records$Individual_ID[p$records$Well==r$Well]<-trimws(r$Individual_ID)
    if('Genotype'%in%names(r)) {
      old<-p$prescreen[p$prescreen$Well==r$Well,,drop=FALSE]
      if(nrow(old)!=1 || !identical(old$Pattern,r$Genotype)) {
        p$prescreen<-p$prescreen[p$prescreen$Well!=r$Well,,drop=FALSE]
        if(nzchar(r$Genotype))p$prescreen<-rbind(p$prescreen,data.frame(Well=r$Well,Pattern=r$Genotype,Method='',Evidence=''))
        prescreen_changed<-TRUE
      }
    }
    if(p$plate$Role[i]!='sample' && r$Well%in%p$prescreen$Well) {
      p$prescreen<-p$prescreen[p$prescreen$Well!=r$Well,,drop=FALSE];prescreen_changed<-TRUE
    }
  }
  if(prescreen_changed){p$prescreen<-p$prescreen[order(p$prescreen$Well),,drop=FALSE];rownames(p$prescreen)<-NULL}
  p$groups<-unique(c(p$groups,p$plate$Group[nzchar(p$plate$Group)]))
  p$undo<-c(p$undo,list(snapshot));p$audit<-c(p$audit,list(list(time=stamp(),action='sample_layout_edit',actor='user',changes=patch,previous_prescreen=snapshot$prescreen[snapshot$prescreen$Well%in%wells,,drop=FALSE])))
  sync_records(p)
}
