# Target allocation only. Tail fitting and EI live in the unchanged method modules.
default_localization <- function() list(name='',gene='',normal_max=NA_real_,normal_inclusive=TRUE,
  expanded_min=NA_real_,expanded_inclusive=TRUE,source='',counting='',mode='',unit=NA_real_,
  intercept=NA_real_,slope=NA_real_,uncertainty=NA_real_,mapping_source='')
prescreen_choices <- c('Choose prescreen interpretation'='', 'Normal only (prescreen)'='normal_only',
  'Normal + expanded (prescreen)'='normal_expanded','Expanded present; other allele unresolved'='expanded_present',
  'Two expanded alleles expected'='two_expanded','Intermediate / uncertain range suspected'='intermediate',
  'Unclear / not resolved by prescreen'='unclear')
ensure_localization <- function(p) {
  p$localization<-utils::modifyList(default_localization(),p$localization%||%list())
  if(is.null(p$prescreen))p$prescreen<-data.frame(Well=character(),Pattern=character(),Method=character(),Evidence=character())
  if(is.null(p$target_overrides))p$target_overrides<-data.frame(Record_ID=character(),Source_ID=character(),Peak_ID=character(),Kind=character(),Basis=character(),Key=character())
  defaults<-list(Target_kind='unassigned',Localization_status='',Localization_reason='',Localization_key='',Auto_target=FALSE,Manual_identity=FALSE)
  for(nm in names(defaults))if(!nm%in%names(p$records))p$records[[nm]]<-rep(defaults[[nm]],nrow(p$records))
  p
}
set_prescreen <- function(p,wells,pattern,method='',evidence='') {
  p<-ensure_localization(p);wells<-unique(intersect(wells,p$plate$Well[p$plate$Role=='sample']))
  if(!length(wells))stop('Select biological sample wells explicitly. Group does not assign genotype.')
  if(!pattern%in%unname(prescreen_choices[-1]))stop('Choose a prescreen genotype, including Intermediate or Unclear when appropriate.')
  p$prescreen<-rbind(p$prescreen[!p$prescreen$Well%in%wells,,drop=FALSE],data.frame(Well=wells,Pattern=pattern,Method=trimws(method),Evidence=trimws(evidence)))
  p$prescreen<-p$prescreen[order(p$prescreen$Well),,drop=FALSE]
  p$audit<-c(p$audit,list(list(time=stamp(),action='prescreen',wells=wells,pattern=pattern,method=method,evidence=evidence)));p
}
localization_mapping <- function(p) {
  cfg<-p$localization
  if(cfg$mode=='theoretical') {
    s<-project_structure_view(p)
    if(is.null(s) || !is.finite(s$fixed_bp))stop('Match reference/primers and counted structure before using theoretical length mapping.')
    c(intercept=s$fixed_bp,slope=cfg$unit)
  } else c(intercept=cfg$intercept,slope=cfg$slope)
}
localization_issues <- function(p,include_prescreen=TRUE) {
  p<-ensure_localization(p);x<-p$localization;issues<-character()
  for(n in c('name','gene','source','counting','mapping_source'))if(!nzchar(trimws(x[[n]])))issues<-c(issues,paste('Required:',n))
  for(n in c('normal_max','expanded_min','unit'))if(length(x[[n]])!=1 || !is.finite(x[[n]]))issues<-c(issues,paste('Required numeric:',n))
  if(all(is.finite(c(x$normal_max,x$expanded_min))) && (x$normal_max<0 || x$expanded_min<=x$normal_max))issues<-c(issues,'Normal upper bound must be below expanded lower bound.')
  if(is.finite(x$unit) && x$unit<=0)issues<-c(issues,'Repeat unit must be positive.')
  if(is.finite(x$uncertainty) && x$uncertainty<0)issues<-c(issues,'Length uncertainty cannot be negative.')
  if(!x$mode%in%c('theoretical','linear'))issues<-c(issues,'Choose theoretical or documented linear CE length mapping.')
  if(x$mode=='linear' && (!all(is.finite(c(x$intercept,x$slope))) || x$slope<=0))issues<-c(issues,'Linear CE mapping requires finite intercept and positive slope.')
  if(x$mode=='theoretical')tryCatch(localization_mapping(p),error=function(e)issues<<-c(issues,conditionMessage(e)))
  if(!nzchar(p$assay$channel))issues<-c(issues,'Apply an explicit target channel (or enter ALL for all channels).')
  if(include_prescreen) {
    wells<-p$plate$Well[p$plate$Role=='sample'];z<-p$prescreen[match(wells,p$prescreen$Well),,drop=FALSE]
    missing<-is.na(z$Pattern) | !z$Pattern%in%unname(prescreen_choices[-1])
    if(any(missing))issues<-c(issues,paste('Prescreen required for',paste(wells[missing],collapse=', ')))
  }
  unique(issues)
}
localization_classify <- function(repeats,cfg,intercept=0,slope=cfg$unit) {
  # Missing CE precision permits nominal research localization, never formal clearance.
  error<-if(is.finite(cfg$uncertainty))cfg$uncertainty/slope else 0;lo<-repeats-error;hi<-repeats+error
  normal<-if(isTRUE(cfg$normal_inclusive))hi<=cfg$normal_max else hi<cfg$normal_max
  expanded<-if(isTRUE(cfg$expanded_inclusive))lo>=cfg$expanded_min else lo>cfg$expanded_min
  middle<-(if(isTRUE(cfg$normal_inclusive))lo>cfg$normal_max else lo>=cfg$normal_max) &
    (if(isTRUE(cfg$expanded_inclusive))hi<cfg$expanded_min else hi<=cfg$expanded_min)
  ifelse(normal,'normal',ifelse(expanded,'expanded',ifelse(middle,'intermediate','boundary')))
}
localization_context_key <- function(p,source) {
  w<-p$peaks$Well[match(source,p$peaks$Record_ID)]
  object_hash(list(policy='non-normal-ei-v1.10',profile=p$disease_profile,config=p$localization,prescreen=p$prescreen[p$prescreen$Well%in%w,,drop=FALSE],
    filter=p$assay[c('channel','minimum_bp')],structure=project_structure_view(p),
    peaks=p$peaks[p$peaks$Record_ID==source,,drop=FALSE]))
}
localization_profile_classify <- function(repeats,p,map) {
  ans<-localization_classify(repeats,p$localization,map[1],map[2])
  if(!is.null(p$disease_profile) && !isTRUE(p$profile_custom)) {
    error<-if(is.finite(p$localization$uncertainty))p$localization$uncertainty/map[2] else 0
    lo<-repeats-error;hi<-repeats+error
    for(b in p$disease_profile$conditional_intervals)
      ans[is.finite(repeats) & hi>=b$min & lo<=b$max]<-'boundary'
  }
  ans
}
localization_candidates <- function(p,record) {
  z<-candidate_inventory(p,record);if(!nrow(z))return(data.frame())
  map<-localization_mapping(p);cfg<-p$localization;env<-load_methods();gap<-2*p$assay$repeat_bp
  rr<-(z$Size-map['intercept'])/map['slope'];classes<-localization_profile_classify(rr,p,map)
  global<-env$find_modal_candidates(z,gap_bp=gap)
  # Keep complete existing high-signal clusters intact across category boundaries.
  # A tail fragment above a boundary must not become a new modal by cropping.
  hi<-z[z$Height>mean(z$Height),,drop=FALSE];hi<-hi[order(hi$Size),,drop=FALSE]
  spans<-if(nrow(hi))split(hi$Size,cumsum(c(TRUE,diff(hi$Size)>gap))) else list()
  local<-lapply(unique(classes),function(k)env$find_modal_candidates(z[classes==k,,drop=FALSE],gap_bp=gap))
  sizes<-unique(c(global$Modal_size_bp,unlist(lapply(local,function(x)x$Modal_size_bp))))
  for(span in spans) {
    within<-sizes>=min(span) & sizes<=max(span)
    sizes<-sizes[!within | sizes%in%global$Modal_size_bp]
  }
  out<-z[z$Size%in%sizes,,drop=FALSE]
  out$Repeat_estimate<-(out$Size-map[1])/map[2]
  out$Range<-localization_profile_classify(out$Repeat_estimate,p,map)
  out$Candidate_origin<-ifelse(out$Size%in%global$Modal_size_bp,'existing_cluster','regional_review')
  out[order(out$Size,out$Peak_ID),,drop=FALSE]
}
set_localization_override <- function(p,record_id,peak_id,kind,basis) {
  p<-ensure_localization(p);i<-match(record_id,p$records$Record_ID)
  if(is.na(i) || !kind%in%c('normal','expanded','intermediate','research') || !nzchar(trimws(basis)))stop('Choose a record, target kind and manual review basis.')
  rec<-p$records[i,,drop=FALSE];z<-candidate_inventory(p,rec)
  if(!peak_id%in%z$Peak_ID)stop('Manual target must be a physical peak in this acquisition and applied filter.')
  source<-rec$Source_ID
  p$target_overrides<-rbind(p$target_overrides[p$target_overrides$Record_ID!=record_id,,drop=FALSE],
    data.frame(Record_ID=record_id,Source_ID=source,Peak_ID=peak_id,Kind=kind,Basis=trimws(basis),Key=localization_context_key(p,source)))
  p$audit<-c(p$audit,list(list(time=stamp(),action='manual_target_review',record=record_id,peak=peak_id,kind=kind,basis=basis)));p
}
prepare_localized_project <- function(p) {
  p<-ensure_localization(p);p<-sync_records(p);issues<-localization_issues(p)
  if(length(issues))stop(paste(issues,collapse='\n'))
  previous<-p$records;rows<-list()
  for(source in unique(previous$Source_ID)) {
    old<-previous[previous$Source_ID==source,,drop=FALSE];base<-old[1,,drop=FALSE]
    if(base$Role!='sample'){rows[[length(rows)+1L]]<-old;next}
    base$ROI_low<--Inf;base$ROI_high<-Inf
    candidates<-localization_candidates(p,base);key<-localization_context_key(p,source)
    pre<-p$prescreen[p$prescreen$Well==base$Well,,drop=FALSE];pattern<-pre$Pattern
    make<-function(kind,index=1L,peak='',status='assigned',reason='') {
      rec<-base;rec$Record_ID<-paste0(source,'::',kind,'-',index);rec$Allele<-paste0(kind,'-',index)
      prior<-old[old$Record_ID==rec$Record_ID & old$Modal_peak_ID==peak & old$Localization_key==key,,drop=FALSE]
      preserved<-nrow(prior)==1L && nzchar(peak)
      if(preserved)rec<-prior
      rec$Auto_target<-TRUE;rec$Target_kind<-kind;rec$Localization_status<-status;rec$Localization_reason<-reason;rec$Localization_key<-key
      rec$Modal_rule<-if(nzchar(peak))'manual' else 'blocked';rec$Modal_peak_ID<-peak
      if(!preserved){rec$Category<-'unresolved';rec$Evidence<-'';rec$Target_confirmed<-FALSE;rec$Target_evidence<-''
        rec$Repeat_low<-NA_real_;rec$Repeat_high<-NA_real_};rec
    }
    # Regional supplements remain inspectable; their new signal context is not
    # silently treated as equivalent to the previously validated cluster screen.
    trusted<-candidates[candidates$Candidate_origin=='existing_cluster',,drop=FALSE]
    normal<-trusted[trusted$Range=='normal',,drop=FALSE];expanded<-trusted[trusted$Range=='expanded',,drop=FALSE]
    uncertain<-trusted[trusted$Range%in%c('intermediate','boundary'),,drop=FALSE]
    result<-list()
    add_kind<-function(kind,n=1L) {
      z<-if(kind=='normal')normal else expanded
      valid<-nrow(z)==n && !anyDuplicated(z$Size)
      if(valid)for(j in seq_len(n))result[[length(result)+1L]]<<-make(kind,j,z$Peak_ID[j],reason=paste('Prescreen',pattern,'+',kind,'range; unique allocation. Identity remains provisional.'))
      else result[[length(result)+1L]]<<-make(kind,status=if(!nrow(z))'target_missing' else 'target_ambiguous',reason=paste('Prescreen',pattern,'expects',n,kind,'target(s); found',nrow(z),'candidate(s). Review physical peaks.'))
    }
    add_non_normal<-function(n=1L) {
      z<-trusted[trusted$Range%in%c('expanded','intermediate','boundary'),,drop=FALSE]
      if(nrow(z)==n && !anyDuplicated(z$Size))for(j in seq_len(n)) {
        kind<-if(z$Range[j]=='boundary')'research' else z$Range[j]
        index<-sum(vapply(result,function(r)r$Target_kind==kind,logical(1)))+1L
        result[[length(result)+1L]]<<-make(kind,index,z$Peak_ID[j],
          reason=paste('Unique non-normal physical cluster; estimated range:',z$Range[j],'. Prescreen:',pattern,'. EI calculation is independent of disease classification.'))
      } else result[[length(result)+1L]]<<-make('unresolved',
        status=if(!nrow(z))'target_missing' else 'target_ambiguous',
        reason=paste('Expected',n,'non-normal physical target(s); found',nrow(z),'. Choose the intended modal in target review.'))
    }
    if(pattern=='normal_only') {
      if(any(trusted$Range%in%c('expanded','intermediate','boundary')))result<-list(make('unresolved',status='target_conflict',reason='Normal-only prescreen conflicts with trusted expanded/intermediate/boundary clusters.'))
      else add_kind('normal')
    } else if(pattern=='normal_expanded'){add_kind('normal');add_non_normal()}
    else if(pattern%in%c('expanded_present','intermediate'))add_non_normal()
    else if(pattern=='two_expanded')add_non_normal(2L)
    else result<-list(make('unresolved',status='target_review',reason=paste('Prescreen:',pattern,'— choose a physical target and record its intended interpretation.')))
    # Explicit extra target identities remain visible, even before a decision or
    # after its context expires. Never drop them during automatic reallocation.
    extras<-old[!old$Auto_target & old$Record_ID!=source &
      (old$Manual_identity | old$Localization_status%in%c('manual_required','manual_preview') | old$Record_ID%in%p$target_overrides$Record_ID) &
      !old$Record_ID%in%vapply(result,function(r)r$Record_ID,character(1)),,drop=FALSE]
    for(j in seq_len(nrow(extras))) {
      extra<-extras[j,,drop=FALSE];extra$Manual_identity<-TRUE;extra$Modal_rule<-'blocked';extra$Modal_peak_ID<-''
      extra$Localization_status<-'manual_required';extra$Localization_reason<-'Separate target retained; choose a physical peak, interpretation and current review basis.'
      extra$Target_confirmed<-FALSE;result[[length(result)+1L]]<-extra
    }
    overrides<-p$target_overrides[p$target_overrides$Source_ID==source & p$target_overrides$Key==key,,drop=FALSE]
    for(j in seq_len(nrow(overrides))) {
      o<-overrides[j,];if(!o$Peak_ID%in%candidate_inventory(p,base)$Peak_ID)next
      match_slot<-which(vapply(result,function(r)r$Record_ID==o$Record_ID,logical(1)))
      if(!length(match_slot) && o$Record_ID==source)match_slot<-which(vapply(result,function(r)r$Target_kind==o$Kind,logical(1)))
      if(length(match_slot)>1)match_slot<-integer()
      rec<-make(o$Kind,peak=o$Peak_ID,status='manual_preview',reason=paste('Manual research review:',o$Basis))
      rec$Record_ID<-o$Record_ID;rec$Allele<-paste0(o$Kind,' · manual');rec$Auto_target<-FALSE
      identity<-old[old$Record_ID==o$Record_ID,,drop=FALSE]
      if(nrow(identity)==1)for(nm in c('Material_ID','Individual_ID','Duplicate_status','Representative','Clinical','Carrier','Zygosity','Observation','QC_status','QC_evidence','Manual_identity'))rec[[nm]]<-identity[[nm]]
      if(o$Record_ID%in%extras$Record_ID)rec$Manual_identity<-TRUE
      saved<-old[old$Record_ID==o$Record_ID & old$Modal_peak_ID==o$Peak_ID & old$Localization_key==key,,drop=FALSE]
      if(nrow(saved)==1)for(nm in c('ROI_low','ROI_high','Category','Evidence','Target_confirmed','Target_evidence','Repeat_low','Repeat_high'))rec[[nm]]<-saved[[nm]]
      if(length(match_slot))result[[match_slot]]<-rec else {
        if(length(result)==1 && !nzchar(result[[1]]$Modal_peak_ID))result<-list()
        result[[length(result)+1L]]<-rec
      }
    }
    # Never use one physical modal for two target identities in the acquisition.
    ids<-vapply(result,function(r)r$Modal_peak_ID,character(1));dup<-nzchar(ids) & (duplicated(ids)|duplicated(ids,fromLast=TRUE))
    if(any(dup))for(j in which(dup)){result[[j]]$Modal_rule<-'blocked';result[[j]]$Modal_peak_ID<-'';result[[j]]$Localization_status<-'target_conflict';result[[j]]$Localization_reason<-'The same physical peak was assigned to multiple target alleles.'}
    rows<-c(rows,result)
  }
  p$records<-do.call(rbind,rows);rownames(p$records)<-NULL
  if(!identical(previous,p$records))p$audit<-c(p$audit,list(list(time=stamp(),action='localize_targets',previous_records=previous,
    targets=p$records[,c('Record_ID','Target_kind','Modal_peak_ID','Localization_status','Localization_reason')],config=p$localization)))
  p
}
check_target_tail_ownership <- function(p,fits) {
  if(is.null(p$localization))return(fits)
  for(source in unique(p$records$Source_ID)) {
    ids<-p$records$Record_ID[p$records$Source_ID==source & p$records$Target_kind%in%c('expanded','intermediate','research')]
    ids<-ids[vapply(ids,function(id)identical(fits[[id]]$status,'fitted'),logical(1))]
    if(length(ids)<2)next
    assigned<-lapply(ids,function(id)unique(c(fits[[id]]$adaptive$selected$Peak_ID,fits[[id]]$bayesian$selected$Peak_ID)))
    for(i in seq_along(ids))for(j in seq_along(ids))if(i<j) {
      shared<-intersect(assigned[[i]],assigned[[j]])
      if(length(shared))for(id in ids[c(i,j)]) {
        fits[[id]]$status<-'target_overlap';fits[[id]]$reason<-paste(length(shared),'shared physical peaks between target alleles. Review per-target allocation bounds before EI.')
      }
    }
  }
  fits
}
localized_synthetic_demo <- function() {
  p<-ensure_localization(synthetic_project())
  p$localization<-list(name='SYNTHETIC demonstration',gene='SYNTHETIC',normal_max=20,normal_inclusive=FALSE,
    expanded_min=35,expanded_inclusive=TRUE,source='SYNTHETIC ranges, not clinical',counting='SYNTHETIC bounded CAG triplets',
    mode='theoretical',unit=3,intercept=NA_real_,slope=NA_real_,uncertainty=0,mapping_source='SYNTHETIC exact generated sizes; not an empirical assay')
  p<-set_prescreen(p,p$plate$Well[p$plate$Role=='sample'],'unclear','SYNTHETIC generator','SYNTHETIC fixture; manual target identities supplied by generator')
  for(i in which(p$records$Role=='sample')) {
    rec<-p$records[i,,drop=FALSE];z<-candidate_inventory(p,rec)
    p<-set_localization_override(p,rec$Record_ID,z$Peak_ID[which.max(z$Height)],if(rec$Category=='normal')'normal' else 'expanded','SYNTHETIC generated modal and category; not laboratory evidence')
  }
  p
}
