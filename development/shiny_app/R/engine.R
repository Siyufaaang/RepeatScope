load_methods <- local({
  cached<-NULL
  function(root=getOption('repeatscope.root','D:/R/Summer_Pipeline_to_UI')) {
    if(!is.null(cached))return(cached)
    env<-new.env(parent=baseenv())
    # Models use base/stats only. Never source a benchmark runner.
    env$qt<-stats::qt;env$setNames<-stats::setNames;env$head<-utils::head;env$tail<-utils::tail
    opts<-options(v2.4_define_only=TRUE,v2.4_method_define_only=TRUE)
    on.exit(options(opts))
    for(f in c('development/v2_4/repeat_peak.v2.4_core.R','development/v2_4/repeat_peak.v2.4_method02_adaptive_walk.R',
      'development/benchmark/bayesian/model.R','development/benchmark/bayesian/extended_helpers.R','development/benchmark/hierarchical/model.R'))sys.source(file.path(root,f),envir=env)
    cached<<-env;env
  }
})
benchmark_assay <- function() {
  a<-default_assay();a$id<-'SCA2-existing-3pct';a$name<-'SCA2 research benchmark (incomplete assay evidence)';a$gene<-'ATXN2';a$organism<-'Homo sapiens'
  a$status<-'benchmark';a$fixed_bp<-103;a$minimum_bp<-103;a$channel<-'B'
  a$source<-'development/v2_4 core and benchmark/bayesian/run_3pct_review.R (read-only parameter provenance)'
  a$validation_scope<-'Computational reproduction of 29 expanded and 12 normal records only; actual primer/counting/cutoff/QC evidence remains incomplete.';a
}
# Explicit research preview for the historical SCA2 acquisitions.
historical_sca2_preview <- function(p,path) {
  s<-project_structure_view(p)
  if(is.null(s) || !identical(p$reference$accession,'NG_011572.3') ||
     s$amplicon$forward!='CGTGCGAGCCGGTGTATGGG' || s$amplicon$reverse!='GGCGACGCTAGAAGGCCGCT' ||
     s$amplicon$expected_bp!=172 || s$fixed_bp!=103 ||
     s$start!=5658 || s$end!=5726 || s$repeat_pattern!='(CAG)13 CAA (CAG)9')
    stop('Use the documented U13/U10 reference structure before applying historical target rules.')
  ref<-read.csv(path,stringsAsFactors=FALSE)
  ref<-ref[!ref$Is_water,]
  records<-p$records
  key<-paste(records$Well,records$Acquisition)
  index<-match(key,paste(ref$Well,ref$Sample.File.Name))
  use<-!is.na(index) & records$Role=='sample' & records$Allele=='target-1'
  if(sum(use)!=nrow(ref) || anyDuplicated(index[use]))
    stop('The historical target list requires the 41 documented SCA2 sample acquisitions with the sample role.')
  if(any(records$Target_confirmed[use]))
    stop('Historical preview rules would replace an already reviewed target.')
  rule<-ref$Expanded_reference_rule[index[use]]
  if(any(!rule%in%c('single_resolved_cluster_modal','larger_size_of_two_resolved_clusters','none_normal_genotype')))
    stop('The historical target list contains an unknown rule.')
  normal<-which(use)[rule=='none_normal_genotype']
  source<-paste0('SCA2_sample_reference_table_v2_4.csv · ',records$Well[use],': ',rule,' · historical research preview (2026-11-08)')
  records$Modal_rule[use]<-ifelse(rule=='larger_size_of_two_resolved_clusters','larger_of_two','strongest')
  records$Target_evidence[use]<-source
  records$Category[normal]<-'normal'
  records$Evidence[normal]<-records$Target_evidence[normal]
  p$records<-records
  p$assay<-benchmark_assay()
  p$assay$source<-paste(p$assay$source,'· U13/U10 in SCA2_amplicon_structure.docx · sequence-derived fixed length')
  p$audit<-c(p$audit,list(list(time=stamp(),action='historical_sca2_preview',source=basename(path),targets=source)))
  p
}
validate_analysis_config <- function(a) {
  nums<-c('minimum_bp','repeat_bp','nominal_spacing','spacing_low','spacing_high','tolerance','seed_min_rfu','seed_fraction','predictive_level','prior_kappa','prior_alpha','prior_beta','seed_dominance','seed_min','seed_max','envelope_fraction','envelope_margin','minimum_support')
  if(any(!vapply(a[nums],function(x)is.numeric(x) && length(x)==1 && is.finite(x),logical(1))))stop('Analysis configuration requires finite numeric parameters.')
  if(a$repeat_bp<=0 || a$spacing_low<=0 || a$spacing_high<a$spacing_low || a$tolerance<=0 || a$envelope_fraction<0 || a$envelope_fraction>1 || a$minimum_support<=0 || a$envelope_margin<0)stop('Invalid repeat spacing or search support.')
  invisible(TRUE)
}
fit_record <- function(peaks,record,assay) {
  env<-load_methods();validate_analysis_config(assay)
  p<-peaks[peaks$Record_ID==record$Source_ID & peaks$Size>=assay$minimum_bp & peaks$Size>=record$ROI_low & peaks$Size<=record$ROI_high,,drop=FALSE]
  if(nzchar(assay$channel) && assay$channel!='ALL' && 'Channel'%in%names(p))p<-p[p$Channel==assay$channel,,drop=FALSE]
  p<-p[order(p$Size,p$Peak_ID),,drop=FALSE]
  base<-list(status='insufficient_data',reason='No positive-height peaks in target region.',peaks=p,record_id=record$Record_ID,
    modal_id=NA_character_,modal_size=NA_real_,modal_height=NA_real_,upper=NA_real_,max_delta=NA_integer_,
    adaptive=list(status='failed',selected=data.frame()),bayesian=list(status='failed',selected=data.frame(),predictions=data.frame(),seeds=data.frame()))
  if(!nrow(p))return(base)
  if(record$Modal_rule=='blocked'){base$status<-record$Localization_status;base$reason<-record$Localization_reason;return(base)}
  tryCatch({
    if(record$Modal_rule=='manual') {
      ix<-match(record$Modal_peak_ID,p$Peak_ID);if(is.na(ix))stop('The selected target peak is absent from the current candidate region.')
    } else {
      modes<-env$find_modal_candidates(p,gap_bp=2*assay$repeat_bp)
      if(record$Modal_rule=='larger_of_two') {
        if(nrow(modes)<2)stop('Two resolved modal clusters required by the configured target rule.')
        two<-modes[order(-modes$Modal_signal),][1:2,];size<-max(two$Modal_size_bp)
      } else if(nrow(modes))size<-modes$Modal_size_bp[which.max(modes$Modal_signal)] else if(nrow(p)==1)size<-p$Size[1] else stop('No resolved modal candidate.')
      ix<-which(abs(p$Size-size)<1e-8);if(length(ix)!=1)stop('Modal Size does not uniquely identify a physical peak.')
    }
    m<-p$Size[ix];mh<-p$Height[ix]
    cfg<-env$bayes_tail_config(prior_mean=assay$nominal_spacing,prior_kappa=assay$prior_kappa,prior_alpha=assay$prior_alpha,prior_beta=assay$prior_beta,
      seed_gap_low=assay$spacing_low,seed_gap_high=assay$spacing_high,seed_min_rfu=assay$seed_min_rfu,seed_modal_fraction=assay$seed_fraction,
      seed_dominance=assay$seed_dominance,seed_max=as.integer(assay$seed_max),seed_min=as.integer(assay$seed_min),seed_predictive=1,predictive_level=assay$predictive_level)
    first<-env$assign_bayesian_tail(p,m,mh,cfg)
    eligible_sizes<-p$Size[p$Size>m & p$Height>=assay$envelope_fraction*mh]
    upper<-min(max(p$Size),max(c(m+assay$minimum_support,eligible_sizes+assay$envelope_margin)))
    if(is.finite(assay$extended_upper)) {
      if(assay$extended_upper<m)stop('Explicit extended upper boundary is below the target modal.')
      upper<-min(max(p$Size),assay$extended_upper)
    }
    slope<-if(is.null(first$fit))assay$nominal_spacing else first$fit$mean
    limit<-max(10L,as.integer(ceiling((upper-m)/min(assay$spacing_low,slope))+2L))
    if(!is.finite(limit) || limit>5000)stop('Search support exceeds 5,000 repeat indices; review target or assay spacing.')
    cfg$max_delta<-limit;inside<-p[p$Size<=upper+1e-8,,drop=FALSE]
    b<-env$assign_bayesian_tail(inside,m,mh,cfg)
    a<-env$walk_adaptive_tail(inside,m,mh,max_delta=limit,nominal_repeat_size=assay$nominal_spacing,tolerance_bp=assay$tolerance,spacing_bounds_bp=c(assay$spacing_low,assay$spacing_high))
    a$selected$Delta<-a$selected$Delta_CAG;a$status<-'calibrated'
    base$status<-'fitted';base$reason<-'';base$modal_id<-p$Peak_ID[ix];base$modal_size<-m;base$modal_height<-mh;base$upper<-upper;base$max_delta<-limit
    base$adaptive<-a;base$bayesian<-b;base$config<-cfg;base
  },error=function(e){base$status<-'analysis_failed';base$reason<-conditionMessage(e);base})
}
fit_project <- function(p) {
  p<-sync_records(p)
  if(!nrow(p$records))stop('Import a plate and peak table first.')
  if(!is.null(p$fit))p$fit_history<-c(p$fit_history,list(p$fit))
  fits<-lapply(seq_len(nrow(p$records)),function(i)fit_record(p$peaks,p$records[i,,drop=FALSE],p$assay))
  names(fits)<-p$records$Record_ID
  if(!is.null(p$localization))fits<-check_target_tail_ownership(p,fits)
  p$fit<-list(id=paste0('run-',format(Sys.time(),'%Y%m%dT%H%M%S'),'-',substr(object_hash(fits),1,8)),time=stamp(),fingerprint=analysis_fingerprint(p),
    assay=p$assay,reference=p$reference,structure=p$structure,record_inputs=p$records,records=fits,
    versions=list(adaptive='v2.4 Method 02 walk_adaptive_tail (unchanged)',bayesian='benchmark/bayesian/model.R assign_bayesian_tail; seed_predictive=1 (unchanged)',adapter='RepeatScope 1.10; non-normal EI with independent range classification'))
  p$audit<-c(p$audit,list(list(time=stamp(),action='fit',run=p$fit$id)))
  p$hierarchical$result<-NULL;p
}
threshold_ei <- function(selected,modal_height,p,max_delta) {
  if(length(p)!=1 || !is.finite(p) || p<0 || p>1)stop('Height threshold must be between 0 and 100 percent.')
  if(!nrow(selected) || !all(c('Delta','Height')%in%names(selected)) || sum(selected$Delta==0)!=1 || !is.finite(modal_height) || modal_height<=0)
    return(list(Value=NA_real_,N=0L,Peaks=selected[FALSE,,drop=FALSE]))
  load_methods()$scoped_ei(selected,modal_height,p,max_delta)
}
candidate_inventory <- function(project,record) {
  z<-project$peaks
  if(!nrow(z))return(z)
  z<-z[z$Record_ID==record$Source_ID & z$Size>=project$assay$minimum_bp & z$Size>=record$ROI_low & z$Size<=record$ROI_high,,drop=FALSE]
  if(nzchar(project$assay$channel) && project$assay$channel!='ALL')z<-z[z$Channel==project$assay$channel,,drop=FALSE]
  z[order(z$Size,z$Peak_ID),,drop=FALSE]
}
target_description <- function(project,record,f=NULL) {
  z<-candidate_inventory(project,record)
  modes<-if(nrow(z))load_methods()$find_modal_candidates(z,gap_bp=2*project$assay$repeat_bp) else data.frame()
  status<-if(!nrow(z))'missing_target' else if(record$Modal_rule=='manual')'selected_preview' else if(nrow(modes)>1)'multiple_candidates' else 'automatic_candidate'
  if(record$Modal_rule=='manual' && !record$Modal_peak_ID%in%z$Peak_ID)status<-'missing_target'
  if(isTRUE(record$Target_confirmed) && status!='missing_target')status<-'reviewed_target'
  text<-switch(status,missing_target='No usable target in the current candidate filter.',
    selected_preview='Manually selected for research preview; allele identity unconfirmed.',
    multiple_candidates=paste(nrow(modes),'candidate clusters; provisional automatic target. Choose a peak; genotype is not inferred.'),
    automatic_candidate='Automatic cluster candidate; allele identity unconfirmed.',reviewed_target='Target review recorded; formal eligibility is checked separately.')
  if(!is.null(f) && is.finite(f$modal_size))text<-paste(sprintf('%.2f bp / %g RFU.',f$modal_size,f$modal_height),text)
  if(nzchar(record$Localization_status%||%'')) {
    status<-record$Localization_status;text<-paste(record$Target_kind,'·',record$Localization_reason)
    if(!is.null(f) && is.finite(f$modal_size))text<-paste(sprintf('%.2f bp / %g RFU.',f$modal_size,f$modal_height),text)
  }
  if(!is.null(f) && identical(f$status,'target_overlap')){status<-f$status;text<-f$reason}
  list(status=status,text=text,candidates=modes)
}
seed_failure_reason <- function(f) {
  b<-f$bayesian;why<-b$reason%||%b$seed_stop%||%'Insufficient calibration seeds.'
  if(!is.data.frame(b$seed_audit) || !nrow(b$seed_audit))return(why)
  z<-tail(b$seed_audit,1)
  if(!is.finite(z$Low) || !is.finite(z$High))return(why)
  candidates<-f$peaks[f$peaks$Size>=z$Low-1e-8 & f$peaks$Size<=z$High+1e-8,,drop=FALSE]
  peaks<-if(nrow(candidates))paste(sprintf('%.2f bp / %g RFU',candidates$Size,candidates$Height),collapse=', ') else 'no candidate'
  sprintf('%s: seed %s window %.2f–%.2f bp; %s; requires >= %.2f RFU and %s accepted seeds (have %s).',
    why,z$Delta,z$Low,z$High,peaks,b$seed_floor_rfu%||%NA_real_,b$config$seed_min%||%3L,nrow(b$seeds))
}
method_unresolved <- function(f,method,max_delta) {
  r<-f[[method]]
  if(method=='adaptive' || is.null(r$predictions) || !nrow(r$predictions))return(FALSE)
  if(method=='bayesian')return(load_methods()$bayesian_scope_unresolved(r,f$upper,max_delta))
  pr<-r$predictions;any(pr$Delta<=max_delta & pr$Center<=f$upper & pr$Status%in%c('ambiguous','uncertain_window','overlapping_windows','nonmonotonic'))
}
threshold_results <- function(project,p=project$threshold,window=project$window,variant='automatic') {
  if(is.null(project$fit))return(data.frame())
  if(!window%in%c('tail10','extended'))stop('Unknown analysis window.')
  kd<-if(window=='tail10')10 else Inf
  run<-if(variant=='automatic')project$fit else project$reviewed[[variant]]
  if(is.null(run))stop('Reviewed run unavailable.')
  stale<-!identical(run$fingerprint,analysis_fingerprint(project));rows<-list()
  for(i in seq_len(nrow(project$records))) {
    rec<-project$records[i,,drop=FALSE];f<-run$records[[rec$Record_ID]]
    if(is.null(f))next
    methods<-c('adaptive','bayesian')
    if(variant=='automatic' && !is.null(project$hierarchical$result) && !is.null(project$hierarchical$result$records[[rec$Record_ID]])) {
      f$hierarchical<-project$hierarchical$result$records[[rec$Record_ID]];methods<-c(methods,'hierarchical')
    }
    for(method in methods) {
      result<-f[[method]];v<-threshold_ei(result$selected,f$modal_height,p,kd)
      status<-'success';reason<-''
      if(f$status!='fitted'){status<-f$status;reason<-f$reason} else if(result$status!='calibrated') {status<-'insufficient_data';reason<-result$reason%||%result$seed_stop%||%'Insufficient calibration seeds.'}
      else if(method_unresolved(f,method,kd)){status<-'unresolved_assignment';reason<-'Competing, overlapping or unsupported prediction windows; partial diagnostic only.'}
      if(method=='bayesian' && status=='insufficient_data')reason<-seed_failure_reason(f)
      preview_status<-status;preview_reason<-reason
      if(status=='success') {
        preview_status<-if(is.finite(v$Value) && v$Value==0)'zero_tail' else 'calculated'
        preview_reason<-if(preview_status=='zero_tail')sprintf('EI = 0: only modal remains; no assigned tail peak passes this window and >= %.2f RFU threshold.',p*f$modal_height) else
          sprintf('%s tail peaks + modal; denominator %.0f RFU; height >= %.2f RFU.',sum(v$Peaks$Delta>0),sum(v$Peaks$Height),p*f$modal_height)
      } else if(method=='bayesian' && status=='insufficient_data')preview_status<-result$seed_stop%||%'insufficient_seeds'
      if(f$status!='fitted' && !is.finite(f$modal_size))preview_status<-if(startsWith(f$status,'target_') || f$status=='intermediate_review')f$status else 'missing_target'
      computational<-if(status=='success' && rec$Category!='normal')v$Value else NA_real_
      gate<-eligibility(rec,project)
      if(gate$status!='success' && (status=='success' || gate$status=='not_applicable_normal')){status<-gate$status;reason<-gate$reason}
      if(stale){status<-'stale_results';reason<-'Inputs or calibration changed. Explicitly run analysis again.'}
      if(method=='hierarchical' && !identical(project$hierarchical$result$fingerprint,hierarchy_fingerprint(project,project$hierarchical$pool))) {status<-'stale_results';reason<-'Training pool or duplicate/material mapping changed. Re-run experimental model.'}
      if(method=='hierarchical' && status=='success'){status<-'experimental_preview';reason<-'Experimental conditional LOWO; excluded from formal biological summaries.'}
      if(rec$Category=='normal' || (identical(rec$Target_kind,'normal') && f$status=='fitted')){preview_status<-'not_applicable_normal';preview_reason<-'Normal target: expanded EI is not applicable; modal and profile retained.';computational<-NA_real_}
      if(!rec$Role%in%c('sample','positive_control')){preview_status<-'not_applicable_control';preview_reason<-paste(rec$Role,'profile only; excluded from biological EI summaries.')}
      if(status=='stale_results'){preview_status<-'stale_results';preview_reason<-'Results outdated. Apply current inputs and run analysis again.';computational<-NA_real_}
      preview<-if(preview_status%in%c('calculated','zero_tail'))computational else NA_real_
      target<-target_description(project,rec,if(stale)NULL else f)
      rows[[length(rows)+1L]]<-data.frame(Record_ID=rec$Record_ID,Source_ID=rec$Source_ID,Code=rec$Code,Well=rec$Well,Acquisition=rec$Acquisition,Allele=rec$Allele,
        Group=if(nzchar(rec$Group))rec$Group else 'Ungrouped',Role=rec$Role,Method=method,Window=window,Threshold=p,EI=if(status=='success')computational else NA_real_,
        Preview_EI=preview,Computational_EI=computational,Diagnostic_selected_EI=if(stale)NA_real_ else v$Value,Status=status,Reason=reason,
        Preview_status=preview_status,Preview_reason=preview_reason,Formal_status=gate$status,Formal_reason=gate$reason,
        Target_status=target$status,Target_description=target$text,
        N_included=if(is.finite(preview))v$N else 0L,N_tail=if(is.finite(preview))sum(v$Peaks$Delta>0) else 0L,
        Denominator_RFU=if(is.finite(preview))sum(v$Peaks$Height) else NA_real_,Threshold_RFU=p*f$modal_height,
        Modal_peak_ID=f$modal_id,
        Modal_bp=f$modal_size,Modal_RFU=f$modal_height,Search_upper_bp=f$upper,Computed_max_delta=f$max_delta,Category=rec$Category,
        Representative=isTRUE(rec$Representative) || (rec$Duplicate_status=='unreviewed' && !nzchar(rec$Material_ID)),Duplicate_status=rec$Duplicate_status,Individual_ID=rec$Individual_ID,Material_ID=rec$Material_ID,
        Variant=variant,Run_ID=run$id,Reference=run$reference$accession%||%'Unconfirmed',
        Method_version=run$versions[[method]]%||%'experimental hierarchical conditional LOWO; adapter 1.0',stringsAsFactors=FALSE)
    }
  }
  if(!length(rows))return(data.frame())
  out<-do.call(rbind,rows);base<-out[out$Method=='bayesian',]
  out$EI_difference_vs_Bayesian<-out$Computational_EI-base$Computational_EI[match(out$Record_ID,base$Record_ID)]
  outdated<-out$Preview_status=='stale_results'
  out[outdated,c('Modal_bp','Modal_RFU','Threshold_RFU','Search_upper_bp','Computed_max_delta')]<-NA_real_
  out$Modal_peak_ID[outdated]<-NA_character_
  out
}
peak_decisions <- function(project,p=project$threshold,window=project$window,variant='automatic',record_id=NULL) {
  if(is.null(project$fit))return(data.frame())
  run<-if(variant=='automatic')project$fit else project$reviewed[[variant]]
  kd<-if(window=='tail10')10 else Inf;rows<-list()
  rr<-threshold_results(project,p,window,variant)
  for(id in if(is.null(record_id))names(run$records) else intersect(record_id,names(run$records))) {
    f<-run$records[[id]];if(!nrow(f$peaks))next
    for(method in c('adaptive','bayesian')) {
      rec<-project$records[match(id,project$records$Record_ID),]
      z<-project$peaks[project$peaks$Record_ID==rec$Source_ID,,drop=FALSE];s<-f[[method]]$selected;j<-match(z$Peak_ID,s$Peak_ID)
      row<-rr[rr$Record_ID==id & rr$Method==method,,drop=FALSE]
      z$Analysis_record<-id;z$Method<-method;z$Delta<-if(nrow(s))s$Delta[j] else NA_real_
      z$Assigned<-!is.na(j);z$Included<-z$Assigned & z$Delta>=0 & z$Delta<=kd & z$Height>=p*f$modal_height & is.finite(row$Preview_EI[1])
      z$Modal<-z$Peak_ID%in%f$modal_id;z$Candidate<-z$Peak_ID%in%f$peaks$Peak_ID
      z$In_search_support<-z$Size>=f$modal_size & z$Size<=f$upper;z$Window<-window;z$Threshold<-p;z$Variant<-variant
      z$Decision<-ifelse(z$Included,'included',ifelse(!z$Assigned,'not assigned',ifelse(z$Delta>kd,'outside window','below height threshold')))
      z$Decision[!z$Candidate]<-'outside candidate filter / target region'
      if(!is.finite(row$Preview_EI[1]))z$Decision<-paste('not used for EI:',row$Preview_status[1])
      if(row$Preview_status[1]=='stale_results'){z$Modal<-FALSE;z$Assigned<-FALSE;z$Delta<-NA_real_}
      rows[[length(rows)+1L]]<-z
    }
  }
  if(length(rows))do.call(rbind,rows) else data.frame()
}

hierarchy_fingerprint <- function(project,pool) object_hash(list(fit=project$fit$fingerprint,pool=sort(pool),
  materials=project$records[,intersect(c('Record_ID','Source_ID','Well','Material_ID','Duplicate_status','Code'),names(project$records)),drop=FALSE]))
fit_hierarchical <- function(project,pool) {
  if(is.null(project$fit) || fit_is_stale(project))stop('Fit the current dual-method analysis before experimental pooling.')
  if(!length(pool) || any(!pool%in%project$records$Record_ID))stop('Explicitly choose valid training-pool records.')
  env<-load_methods();pool<-unique(pool);recs<-project$records;anchors<-list()
  for(id in pool) {
    f<-project$fit$records[[id]]
    if(f$status!='fitted' || f$bayesian$status!='calibrated')next
    a<-f$adaptive$selected;b<-f$bayesian$selected
    key<-function(z)paste(z$Peak_ID,z$Delta,sep=':')
    z<-a[key(a)%in%key(b) & a$Delta>0 & a$Height>=project$assay$seed_fraction*f$modal_height,,drop=FALSE]
    if(nrow(z))anchors[[id]]<-data.frame(Well=id,Group='explicit_pool',Peak_ID=z$Peak_ID,Delta=z$Delta,Size=z$Size,Height=z$Height,Y=z$Size-f$modal_size)
  }
  anchors<-if(length(anchors))do.call(rbind,anchors) else data.frame(Well=character(),Group=character(),Peak_ID=character(),Delta=integer(),Size=numeric(),Height=numeric(),Y=numeric())
  unreviewed<-unresolved_duplicate_sources(project);results<-list()
  for(i in seq_len(nrow(recs))) {
    rec<-recs[i,];id<-rec$Record_ID;f<-project$fit$records[[id]]
    same<-recs$Well==rec$Well | recs$Source_ID==rec$Source_ID | (nzchar(rec$Material_ID) & recs$Material_ID==rec$Material_ID)
    excluded<-recs$Record_ID[same];allowed<-setdiff(pool,excluded)
    z<-anchors[anchors$Well%in%allowed,,drop=FALSE]
    # Keep at most one acquisition per known material among peers, explicitly recorded.
    peer_meta<-recs[match(unique(z$Well),recs$Record_ID),,drop=FALSE]
    peer_material<-ifelse(nzchar(peer_meta$Material_ID),peer_meta$Material_ID,peer_meta$Source_ID)
    redundant<-peer_meta$Record_ID[duplicated(peer_material)]
    z<-z[!z$Well%in%redundant,,drop=FALSE]
    result<-list(status='insufficient_peers',reason='At least two peer materials with consensus anchors are required.',selected=f$bayesian$selected[FALSE,,drop=FALSE],predictions=data.frame(),
      peer_records=unique(z$Well),excluded_records=excluded,redundant_peer_records=redundant,training=z,
      support=if(nrow(z))range(z$Delta) else c(NA,NA),label='Experimental conditional LOWO + identified same-material repeat exclusion')
    if(any(recs$Source_ID[recs$Record_ID%in%c(pool,id)]%in%unreviewed)) {
      result$status<-'duplicate_unresolved';result$reason<-'Resolve duplicate relationships in target and pool before training.'
    } else if(f$status!='fitted' || f$bayesian$status!='calibrated' || nrow(f$bayesian$seeds)<3) {
      result$status<-'insufficient_seeds';result$reason<-'Target early-seed calibration is insufficient; peers cannot manufacture target evidence.'
    } else if(length(unique(z$Well))>=2) {
      result<-tryCatch({
        prior<-env$fit_case_lowo(z,id,'explicit_pool',curve=TRUE,effect_mult=1)
        seeds<-f$bayesian$seeds;fit<-env$condition_well(prior,seeds$Delta,seeds$Size-f$modal_size)
        pred<-env$predict_hier(fit,seq_len(f$max_delta),f$modal_size,project$assay$predictive_level)
        a<-env$assign_hier(f$peaks[f$peaks$Size<=f$upper+1e-8,,drop=FALSE],f$modal_id,seeds,pred)
        result$status<-'calibrated';result$reason<-'';result$selected<-a$selected;result$predictions<-a$predictions;result$candidates<-a$candidates
        result$fit<-fit;result$prior<-prior;result$target_seeds<-seeds;result
      },error=function(e){result$status<-'analysis_failed';result$reason<-conditionMessage(e);result})
    }
    results[[id]]<-result
  }
  project$hierarchical<-list(enabled=TRUE,pool=pool,result=list(time=stamp(),fingerprint=hierarchy_fingerprint(project,pool),records=results,anchor_fraction=project$assay$seed_fraction))
  project
}
hierarchical_comparison <- function(project,record_id,p=project$threshold,window=project$window) {
  f<-project$fit$records[[record_id]];h<-project$hierarchical$result$records[[record_id]]
  if(is.null(h) || h$status!='calibrated')return(data.frame())
  kd<-if(window=='tail10')10 else Inf
  b<-threshold_ei(f$bayesian$selected,f$modal_height,p,kd)$Peaks;z<-threshold_ei(h$selected,f$modal_height,p,kd)$Peaks
  keys<-union(paste(b$Peak_ID,b$Delta),paste(z$Peak_ID,z$Delta));bk<-paste(b$Peak_ID,b$Delta);zk<-paste(z$Peak_ID,z$Delta)
  all<-rbind(b[,c('Peak_ID','Delta','Size','Height')],z[,c('Peak_ID','Delta','Size','Height')]);all<-all[!duplicated(paste(all$Peak_ID,all$Delta)),]
  all$Change<-ifelse(keys%in%bk & keys%in%zk,'same',ifelse(keys%in%zk,'added','excluded'));all
}
create_reviewed <- function(project,record_id,modal_peak_id,exclude_ids=character(),reason,evidence) {
  if(is.null(project$fit) || fit_is_stale(project))stop('Run the automatic analysis for current inputs first.')
  if(!nzchar(trimws(reason)) || !nzchar(trimws(evidence)))stop('Reviewed versions require both a reason and an evidence/source record.')
  rec<-project$records[project$records$Record_ID==record_id,,drop=FALSE]
  if(nrow(rec) && nzchar(rec$Localization_status%||%''))stop('Localized targets require the visible manual target kind and review basis. Apply that decision and rerun; the legacy reviewed-version path cannot bypass allocation conflicts.')
  if(nrow(rec)!=1 || !modal_peak_id%in%project$peaks$Peak_ID[project$peaks$Record_ID==rec$Source_ID])stop('Choose a physical modal belonging to this acquisition.')
  if(modal_peak_id%in%exclude_ids)stop('Cannot exclude the target modal.')
  if(any(!exclude_ids%in%project$peaks$Peak_ID[project$peaks$Record_ID==rec$Source_ID]))stop('Excluded peaks must belong to this acquisition.')
  rec$Modal_rule<-'manual';rec$Modal_peak_ID<-modal_peak_id
  reviewed<-project$fit;id<-paste0('reviewed-',length(project$reviewed)+1L)
  revised<-fit_record(project$peaks[!project$peaks$Peak_ID%in%exclude_ids,,drop=FALSE],rec,project$assay)
  revised$peaks<-project$fit$records[[record_id]]$peaks;revised$manual_exclusions<-exclude_ids
  reviewed$records[[record_id]]<-revised;reviewed$id<-id;reviewed$review<-list(time=stamp(),record=record_id,modal=modal_peak_id,excluded=exclude_ids,reason=reason,evidence=evidence)
  project$reviewed[[id]]<-reviewed;project$audit<-c(project$audit,list(reviewed$review));project
}
