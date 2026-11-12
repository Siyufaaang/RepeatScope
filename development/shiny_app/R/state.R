`%||%` <- function(x,y) if(is.null(x) || !length(x)) y else x
text_value <- function(x) { x<-as.character(x);x[is.na(x)]<-'';x }
stamp <- function() format(Sys.time(),'%Y-%m-%dT%H:%M:%SZ',tz='UTC')
object_hash <- function(x) digest::digest(x,algo='sha256',serialize=TRUE)
well_coordinates <- function() as.vector(t(outer(LETTERS[1:8],sprintf('%02d',1:12),paste0)))
normalize_well <- function(x) {
  x<-toupper(trimws(as.character(x)))
  ok<-grepl('^[A-H]0?([1-9]|1[0-2])$',x)
  out<-rep(NA_character_,length(x))
  out[ok]<-paste0(substr(x[ok],1,1),sprintf('%02d',as.integer(sub('^[A-H]','',x[ok]))))
  out
}
default_assay <- function() list(id='draft-assay',gene='',organism='',name='Unconfigured assay',
  status='unconfigured',validation_scope='No experimental validation supplied',source='',
  fixed_bp=NA_real_,minimum_bp=0,repeat_bp=3,motif='CAG',channel='',
  nominal_spacing=3,spacing_low=2.5,spacing_high=3.2,tolerance=.8,
  seed_min_rfu=100,seed_fraction=.03,predictive_level=.95,prior_kappa=.05,
  prior_alpha=2,prior_beta=.01,seed_dominance=3,seed_min=3L,seed_max=5L,
  envelope_fraction=.03,envelope_margin=1.5,minimum_support=33,extended_upper=NA_real_,
  include_intermediate=TRUE,ranges=data.frame(Category=character(),Lower=numeric(),Upper=numeric(),
  Lower_inclusive=logical(),Upper_inclusive=logical(),EI_allowed=logical(),Source=character()))
new_project <- function() list(schema='RepeatScope/1',id=paste0('plate-',format(Sys.time(),'%Y%m%d-%H%M%S')),
  created=stamp(),synthetic=FALSE,plate=data.frame(Well=well_coordinates(),Code='',Full_name='',Group='',
  Role='unlabelled',Batch='',Control_expected='',Control_evidence='',stringsAsFactors=FALSE),
  peaks=data.frame(),records=data.frame(),import_issues=data.frame(),assay=default_assay(),
  reference=NULL,reference_draft=NULL,structure=NULL,fit=NULL,fit_history=list(),reviewed=list(),audit=list(),undo=list(),
  originals=list(),threshold=.03,window='tail10',control_review=list(status='unknown',scope='unresolved',
  batches=character(),evidence=''),hierarchical=list(enabled=FALSE,pool=character(),result=NULL),
  groups_enabled=FALSE,groups=c('Group 1','Group 2'))
plate_from_matrix <- function(x) {
  x<-as.matrix(x);x[is.na(x)]<-''
  if(identical(dim(x),c(9L,13L))) {
    if(!identical(as.character(x[1,-1]),as.character(1:12)) || !identical(toupper(x[-1,1]),LETTERS[1:8]))
      stop('Expected column headers 1–12 and row headers A–H. Select the correct Excel range.')
    x<-x[-1,-1,drop=FALSE]
  }
  if(!identical(dim(x),c(8L,12L)))stop('Select an 8 × 12 matrix, or a 9 × 13 range including A–H / 1–12 headers.')
  p<-new_project()$plate;p$Code<-as.vector(t(x));p
}
normalize_peaks <- function(raw,mapping,plate_id='plate') {
  required<-c('Well','Size','Height')
  if(!all(required%in%names(mapping)) || any(!unlist(mapping[required])%in%names(raw)))stop('Map Well, Size (bp), and Height (RFU).')
  if(grepl('area',mapping$Height,ignore.case=TRUE))stop('Area is not Height. Select the RFU peak-height column.')
  get<-function(n,default='') if(!is.null(mapping[[n]]) && nzchar(mapping[[n]]) && mapping[[n]]%in%names(raw)) text_value(raw[[mapping[[n]]]]) else rep(default,nrow(raw))
  w<-normalize_well(get('Well'));a<-trimws(get('Acquisition','injection-1'))
  missing_acquisition<-!nzchar(a)
  p<-data.frame(Well=w,Acquisition=a,Size=suppressWarnings(as.numeric(get('Size'))),Height=suppressWarnings(as.numeric(get('Height'))),
    Original_peak_ID=get('Peak_ID'),Data_Point=get('Data_Point'),Channel=get('Channel'),QC=get('QC'),Saturation=get('Saturation'),
    Source_row=seq_len(nrow(raw)),stringsAsFactors=FALSE)
  p$Record_ID<-paste(plate_id,p$Well,p$Acquisition,sep='|')
  missing_id<-!nzchar(p$Original_peak_ID);p$Original_peak_ID[missing_id]<-sprintf('row%07d',p$Source_row[missing_id])
  p$Peak_ID<-paste(p$Record_ID,p$Original_peak_ID,sep='|')
  duplicate_id<-duplicated(p$Peak_ID) | duplicated(p$Peak_ID,fromLast=TRUE)
  reasons<-vapply(seq_len(nrow(p)),function(i) paste(c(if(is.na(p$Well[i]))'Invalid well',
    if(missing_acquisition[i])'Missing acquisition ID in mapped column',
    if(!is.finite(p$Size[i]) || p$Size[i]<=0)'Invalid Size',if(!is.finite(p$Height[i]) || p$Height[i]<=0)'Non-positive or invalid Height',
    if(duplicate_id[i])'Duplicate physical peak ID within acquisition'),collapse='; '),character(1))
  issues<-data.frame(Source_row=which(nzchar(reasons)),Reason=reasons[nzchar(reasons)],stringsAsFactors=FALSE)
  if(nrow(issues)) issues$Original_row<-vapply(issues$Source_row,function(i)paste(paste(names(raw),text_value(raw[i,]),sep='='),collapse='; '),character(1))
  list(peaks=p[!nzchar(reasons),,drop=FALSE],issues=issues,raw=raw,mapping=mapping)
}
record_defaults <- function(n) {
  z<-data.frame(Allele='target-1',Source_ID='',Input_hash='',Material_ID='',Individual_ID='',
  Duplicate_status='unreviewed',Representative=TRUE,Clinical='Unknown',Carrier='Unknown',Zygosity='Unknown',Observation='Unknown',
  Category='unresolved',Repeat_low=NA_real_,Repeat_high=NA_real_,Evidence='',QC_status='unknown',QC_evidence='',
  Modal_rule='strongest',Modal_peak_ID='',Target_confirmed=FALSE,Target_evidence='',ROI_low=-Inf,ROI_high=Inf,
  Target_kind='unassigned',Localization_status='',Localization_reason='',Localization_key='',Auto_target=FALSE,Manual_identity=FALSE,
  stringsAsFactors=FALSE)
  z[rep(1L,n),,drop=FALSE]
}
sync_records <- function(p) {
  existing<-p$records
  # Upgrade restored records before combining them with newly imported sources.
  if(nrow(existing))for(nm in names(record_defaults(1)))if(!nm%in%names(existing))
    existing[[nm]]<-record_defaults(nrow(existing))[[nm]]
  peak_sources<-if(nrow(p$peaks))unique(p$peaks[,c('Record_ID','Well','Acquisition')]) else data.frame(Record_ID=character(),Well=character(),Acquisition=character())
  # Preserve samples that have no peaks, as explicit insufficient-data records.
  empty<-p$plate[nzchar(p$plate$Code) & !p$plate$Well%in%peak_sources$Well & p$plate$Role!='unused_empty',]
  if(nrow(empty))peak_sources<-rbind(peak_sources,data.frame(Record_ID=paste(p$id,empty$Well,'injection-1',sep='|'),Well=empty$Well,Acquisition='injection-1'))
  rows<-lapply(seq_len(nrow(peak_sources)),function(i) {
    s<-peak_sources[i,];old<-if(nrow(existing))existing[existing$Source_ID==s$Record_ID,,drop=FALSE] else data.frame()
    if(nrow(old))return(old)
    z<-cbind(s,record_defaults(1));z$Source_ID<-s$Record_ID
    # New acquisitions and retained no-peak records must have the same schema.
    for(nm in c('Code','Full_name','Group','Role','Batch'))z[[nm]]<-p$plate[[nm]][match(s$Well,p$plate$Well)]
    if('Individual_ID'%in%names(p$plate))z$Individual_ID<-p$plate$Individual_ID[match(s$Well,p$plate$Well)]
    z
  })
  p$records<-if(length(rows))do.call(rbind,rows) else data.frame()
  if(nrow(p$records)) {
    if(!'Input_hash'%in%names(p$records))p$records$Input_hash<-''
    for(source in unique(p$records$Source_ID)) {
      ix<-which(p$records$Source_ID==source)
      physical<-p$peaks[p$peaks$Record_ID==source,intersect(c('Peak_ID','Size','Height','Data_Point','Channel','QC','Saturation'),names(p$peaks)),drop=FALSE]
      if(nrow(physical))physical<-physical[order(physical$Peak_ID),,drop=FALSE]
      rownames(physical)<-NULL;hash<-object_hash(physical)
      changed<-nzchar(p$records$Input_hash[ix]) & p$records$Input_hash[ix]!=hash
      if(any(changed)) {
        p$audit<-c(p$audit,list(list(time=stamp(),action='input_changed_review_invalidated',source=source,previous=p$records[ix,,drop=FALSE])))
        p$records$QC_status[ix]<-'unknown';p$records$QC_evidence[ix]<-''
        p$records$Target_confirmed[ix]<-FALSE;p$records$Target_evidence[ix]<-''
      }
      p$records$Input_hash[ix]<-hash
    }
    for(nm in c('Code','Full_name','Group','Role','Batch'))p$records[[nm]]<-p$plate[[nm]][match(p$records$Well,p$plate$Well)]
    p$records$Representative[p$records$Duplicate_status=='unreviewed' & !nzchar(p$records$Material_ID)]<-TRUE
    rownames(p$records)<-NULL
  }
  p
}
acquisition_counts <- function(p) {
  actual<-unique(p$peaks$Record_ID)
  c(actual=length(actual),placeholder=length(setdiff(unique(p$records$Source_ID),actual)))
}
duplicate_records <- function(p) {
  z<-p$records;if(!nrow(z))return(z)
  z<-z[!duplicated(z$Source_ID),,drop=FALSE]
  key<-ifelse(nzchar(z$Code),z$Code,paste0('well:',z$Well))
  component<-seq_len(nrow(z))
  for(i in seq_len(nrow(z))) {
    linked<-key==key[i] | (nzchar(z$Material_ID[i]) & z$Material_ID==z$Material_ID[i])
    components<-unique(component[linked]);component[component%in%components]<-min(components)
  }
  z$Duplicate_set<-paste0('set-',component)
  z[duplicated(component) | duplicated(component,fromLast=TRUE),,drop=FALSE]
}
unresolved_duplicate_sources <- function(p) {
  d<-duplicate_records(p);if(!nrow(d))return(character())
  pending<-d$Source_ID[d$Duplicate_status=='unreviewed']
  for(material in unique(d$Material_ID[nzchar(d$Material_ID)])) {
    same<-d[d$Material_ID==material,,drop=FALSE]
    if(nrow(same)>1 && sum(same$Representative)!=1)pending<-union(pending,same$Source_ID)
  }
  pending
}
resolve_duplicate <- function(p,code,relationship,material,representative,evidence='') {
  if(!relationship%in%c('repeat_injection','technical_repeat','distinct_materials','entry_corrected'))stop('Choose the duplicate relationship.')
  d<-duplicate_records(p)
  sources<-if(code%in%d$Duplicate_set)d$Source_ID[d$Duplicate_set==code] else d$Source_ID[d$Code==code]
  ids<-which(p$records$Source_ID%in%sources)
  if(!length(ids))stop('No records for that code.')
  if(!nzchar(evidence))stop('Record the basis for the duplicate relationship.')
  if(relationship%in%c('repeat_injection','technical_repeat') && (!nzchar(material) || !representative%in%p$records$Source_ID[ids]))stop('Specify shared material and one representative acquisition.')
  p$records$Duplicate_status[ids]<-relationship
  if(relationship%in%c('repeat_injection','technical_repeat')) {
    p$records$Material_ID[ids]<-material
    p$records$Representative[ids]<-p$records$Source_ID[ids]==representative
  } else {
    known<-p$records[ids,c('Source_ID','Material_ID')];known<-unique(known[nzchar(known$Material_ID),,drop=FALSE])
    if(anyDuplicated(known$Material_ID))stop('Known shared material still requires one representative. Correct the material IDs first if these are different materials.')
    p$records$Representative[ids]<-TRUE
  }
  p$audit<-c(p$audit,list(list(time=stamp(),action='duplicate_review',code=code,relationship=relationship,evidence=evidence)))
  p
}
role_conflicts <- function(p) p$plate[p$plate$Role=='unused_empty' & p$plate$Well%in%p$peaks$Well,,drop=FALSE]
roles <- c('unlabelled','sample','unused_empty','ce_fill','ntc','positive_control','normal_control','extraction_blank','other_negative','other')
apply_roles <- function(p,wells,role,confirm=FALSE) {
  if(!role%in%roles || any(!wells%in%p$plate$Well))stop('Invalid role or well.')
  ix<-match(wells,p$plate$Well)
  affected<-p$plate$Role[ix]!='unlabelled' | nzchar(p$plate$Code[ix]) | wells%in%p$peaks$Well
  if(any(affected) && !confirm)stop('Confirm role changes for: ',paste(wells[affected],collapse=', '))
  p$undo<-c(p$undo,list(p$plate));p$plate$Role[ix]<-role
  p$audit<-c(p$audit,list(list(time=stamp(),action='role',wells=wells,role=role)))
  sync_records(p)
}
set_groups <- function(p,wells,group,toggle=FALSE) {
  p$undo<-c(p$undo,list(p$plate));ix<-match(wells,p$plate$Well);ix<-ix[!is.na(ix)]
  if(nzchar(group))p$groups<-unique(c(p$groups,group))
  p$plate$Group[ix]<-if(toggle)ifelse(p$plate$Group[ix]==group,'',group) else group
  sync_records(p)
}
undo_plate <- function(p) {
  if(!length(p$undo))return(p)
  previous<-p$undo[[length(p$undo)]]
  if(is.data.frame(previous))p$plate<-previous else {
    p$plate<-previous$plate;p$prescreen<-previous$prescreen
    if(nrow(p$records) && nrow(previous$individuals)) {
      j<-match(p$records$Record_ID,previous$individuals$Record_ID);ok<-!is.na(j)
      p$records$Individual_ID[ok]<-previous$individuals$Individual_ID[j[ok]]
      for(i in which(!ok)) {
        old<-previous$individuals
        if(all(c('Source_ID','Allele')%in%names(old))) {
          same<-old[old$Source_ID==p$records$Source_ID[i] & old$Allele==p$records$Allele[i],,drop=FALSE]
          if(nrow(same)==1){p$records$Individual_ID[i]<-same$Individual_ID;next}
          same<-unique(old$Individual_ID[old$Source_ID==p$records$Source_ID[i]])
          if(length(same)==1){p$records$Individual_ID[i]<-same;next}
        }
        if('Individual_ID'%in%names(previous$plate))p$records$Individual_ID[i]<-previous$plate$Individual_ID[match(p$records$Well[i],previous$plate$Well)]
      }
    } else if(nrow(p$records) && 'Individual_ID'%in%names(previous$plate)) {
      p$records$Individual_ID<-previous$plate$Individual_ID[match(p$records$Well,previous$plate$Well)]
    }
  }
  p$undo<-head(p$undo,-1);sync_records(p)
}
analysis_fingerprint <- function(p) {
  rec<-if(nrow(p$records))p$records[,intersect(c('Record_ID','Source_ID','Allele','Modal_rule','Modal_peak_ID','ROI_low','ROI_high'),names(p$records)),drop=FALSE] else NULL
  assay<-p$assay[setdiff(names(p$assay),c('name','gene','organism','source','validation_scope','status','include_intermediate','ranges'))]
  object_hash(list(policy='non-normal-ei-v1.10',peaks=p$peaks,records=rec,assay=assay,reference=p$reference,structure=p$structure,
    localization=p$localization,prescreen=p$prescreen,target_overrides=p$target_overrides,localization_pending=p$localization_pending))
}
fit_is_stale <- function(p) !is.null(p$fit) && !identical(p$fit$fingerprint,analysis_fingerprint(p))
validate_ranges <- function(x) {
  req<-c('Category','Lower','Upper','Lower_inclusive','Upper_inclusive','EI_allowed','Source')
  if(!all(req%in%names(x)) || !nrow(x))stop('Configure disease ranges and their sources.')
  if(any(!nzchar(x$Category)) || anyDuplicated(x$Category) || any(is.na(x$Lower) | is.na(x$Upper)) || any(x$Lower>x$Upper) || any(!nzchar(x$Source)))stop('Invalid disease range, category or missing source.')
  if(anyNA(x[,c('Lower_inclusive','Upper_inclusive','EI_allowed')]))stop('Set inclusive boundaries and EI policy explicitly.')
  if(any(tolower(x$Category)=='normal' & x$EI_allowed))stop('Normal range cannot enter expanded EI.')
  for(i in seq_len(nrow(x)))for(j in seq_len(nrow(x)))if(i<j) {
    left<-max(x$Lower[c(i,j)]);right<-min(x$Upper[c(i,j)])
    has<-function(k,v)(v>x$Lower[k] || (v==x$Lower[k] && x$Lower_inclusive[k])) && (v<x$Upper[k] || (v==x$Upper[k] && x$Upper_inclusive[k]))
    if(left<right || (left==right && has(i,left) && has(j,left)))stop('Disease ranges overlap; clarify boundary inclusion.')
  }
  invisible(TRUE)
}
classify_repeat <- function(low,high,ranges) {
  if(!is.finite(low) || !is.finite(high) || low>high || !nrow(ranges))return('unresolved')
  validate_ranges(ranges)
  inside<-(low>ranges$Lower | (low==ranges$Lower & ranges$Lower_inclusive)) & (high<ranges$Upper | (high==ranges$Upper & ranges$Upper_inclusive))
  if(sum(inside)==1)ranges$Category[inside] else 'unresolved'
}
