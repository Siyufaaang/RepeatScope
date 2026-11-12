eligibility <- function(record,project) {
  answer<-function(status,reason='')list(status=status,reason=reason)
  if(nzchar(record$Localization_status%||%'')) {
    if(!record$Localization_status%in%c('assigned','manual_preview'))return(answer('eligibility_unresolved',record$Localization_reason))
    if(!identical(record$Localization_key,localization_context_key(ensure_localization(project),record$Source_ID)))return(answer('eligibility_unresolved','Target allocation inputs changed; reassign and review the current target.'))
    f<-project$fit$records[[record$Record_ID]]
    if(identical(f$status,'target_overlap'))return(answer('eligibility_unresolved',f$reason))
    if(record$Target_kind=='normal')return(answer('not_applicable_normal','Normal target; expanded EI is not applicable.'))
    if(!is.finite(project$localization$uncertainty))return(answer('eligibility_unresolved','CE length uncertainty is unknown; nominal range and EI remain a research calculation.'))
    if(record$Target_kind%in%c('research','unassigned'))return(answer('eligibility_unresolved','Research target identity is unresolved. An explicit target interpretation is required for formal clearance.'))
    if(record$Category!='unresolved' && record$Category!=record$Target_kind)return(answer('eligibility_unresolved','Formal category conflicts with the localized target interpretation.'))
  }
  if(record$Category=='normal' && nzchar(record$Evidence))return(answer('not_applicable_normal','Not applicable — normal range; profile retained.'))
  if(record$Role=='unused_empty')return(answer('role_conflict','Unused/empty position has an analysis record. Resolve role and peak mapping.'))
  if(record$Role!='sample' && record$Role!='positive_control')return(answer('not_applicable_control',if(record$Role=='unlabelled')'Well contents/role unconfirmed.' else 'Control or filling well; excluded from biological EI summaries.'))
  pk<-project$peaks[project$peaks$Record_ID==record$Source_ID,,drop=FALSE]
  sat<-tolower(trimws(text_value(pk$Saturation)))%in%c('true','1','yes','saturated','offscale','off-scale','fail','failed')
  qc<-tolower(trimws(text_value(pk$QC)))
  if(any(sat) || any(grepl('fail|off.?scale|saturat|poor',qc)) || record$QC_status=='failed')return(answer('qc_failed','Imported or recorded upstream QC affects quantification; a manual pass cannot override this signal failure.'))
  if(!nzchar(record$Evidence) || record$Category=='unresolved')return(answer('eligibility_unresolved','Record evidence for the target allele category; bp conversion alone is not confirmation.'))
  if(!isTRUE(record$Target_confirmed) || !nzchar(record$Target_evidence))return(answer('eligibility_unresolved','Target allele/modal and assignment region have not been reviewed.'))
  other<-project$records[project$records$Source_ID==record$Source_ID & project$records$Record_ID!=record$Record_ID,,drop=FALSE]
  if(nrow(other) && any(other$ROI_low<=record$ROI_high & other$ROI_high>=record$ROI_low))return(answer('eligibility_unresolved','Target allele candidate regions overlap. Resolve per-allele peak allocation; two alleles at one locus are supported.'))
  if(project$assay$status!='configured' || is.null(project$structure) || project$structure$status!='confirmed')return(answer('assay_unconfirmed','Assay/reference/primer counting structure is draft or benchmark-only.'))
  range_ok<-try(validate_ranges(project$assay$ranges),silent=TRUE)
  if(inherits(range_ok,'try-error'))return(answer('eligibility_unresolved','Disease ranges and sources are incomplete or invalid.'))
  range<-project$assay$ranges[project$assay$ranges$Category==record$Category,,drop=FALSE]
  if(!nrow(range) || (!range$EI_allowed && record$Category!='intermediate'))return(answer('not_applicable_category','This assay category is not eligible for expanded EI.'))
  if(is.finite(record$Repeat_low) || is.finite(record$Repeat_high)) {
    category<-classify_repeat(record$Repeat_low,record$Repeat_high,project$assay$ranges)
    if(category!=record$Category)return(answer('eligibility_unresolved','Repeat evidence interval crosses a boundary or conflicts with the selected category.'))
  }
  if(record$QC_status!='passed' || !nzchar(record$QC_evidence))return(answer('qc_unconfirmed','Upstream sizing/saturation QC review and evidence are required for formal results.'))
  cr<-project$control_review
  covered<-identical(cr$scope,'assay') || (identical(cr$scope,'batch') && nzchar(record$Batch) && record$Batch%in%cr$batches)
  if(cr$status!='passed' || !nzchar(cr$evidence) || !covered)return(answer('control_review_required','An evidenced control pass covering this assay/batch is required. Unresolved scope or a different batch cannot clear this record.'))
  if(nzchar(record$Material_ID) && record$Source_ID%in%unresolved_duplicate_sources(project))
    return(answer('duplicate_unresolved','Resolve the explicitly recorded shared material before formal results.'))
  answer('success')
}
summarise_results <- function(results) {
  if(!nrow(results))return(data.frame())
  r<-results[results$Method%in%c('adaptive','bayesian'),,drop=FALSE]
  groups<-unique(r$Group);rows<-list()
  for(g in groups)for(m in c('adaptive','bayesian')) {
    z<-r[r$Group==g & r$Method==m,,drop=FALSE]
    take<-is.finite(z$EI) & z$Status=='success' & z$Role=='sample' & z$Representative
    x<-z$EI[take];n<-length(x)
    other<-r[r$Group==g & r$Method!=m,,drop=FALSE]
    ok_other<-other$Record_ID[is.finite(other$EI) & other$Status=='success' & other$Role=='sample' & other$Representative]
    common<-sum(z$Record_ID[take]%in%ok_other)
    excluded<-ifelse(!z$Representative,'not_selected_representative',ifelse(z$Role!='sample','control_or_unlabelled',z$Status))
    counts<-table(excluded[!take]);reason<-if(length(counts))paste(paste(names(counts),as.integer(counts),sep=': '),collapse='; ') else ''
    ind<-z$Individual_ID[take]
    rows[[length(rows)+1L]]<-data.frame(Group=g,Method=m,Unit='selected sample/allele records',N=n,Total=nrow(z),Excluded=nrow(z)-n,
      Individual_n=if(n && all(nzchar(ind)))length(unique(ind)) else NA_integer_,Common_valid_n=common,
      Mean=if(n)mean(x) else NA_real_,SD=if(n>=2)stats::sd(x) else NA_real_,Median=if(n)stats::median(x) else NA_real_,
      IQR=if(n>=2)stats::IQR(x,type=7) else NA_real_,Q1=if(n>=2)unname(stats::quantile(x,.25,type=7)) else NA_real_,
      Q3=if(n>=2)unname(stats::quantile(x,.75,type=7)) else NA_real_,Exclusion_reasons=reason,stringsAsFactors=FALSE)
  }
  do.call(rbind,rows)
}

empty_plot <- function(message) ggplot2::ggplot()+ggplot2::annotate('text',x=0,y=0,label=message,color='#57727c',size=5)+ggplot2::theme_void()
rs_theme <- function() ggplot2::theme_minimal(base_size=12)+ggplot2::theme(panel.grid.minor=ggplot2::element_blank(),
  plot.title=ggplot2::element_text(face='bold',color='#163d49'),plot.subtitle=ggplot2::element_text(color='#526b74'),
  strip.text=ggplot2::element_text(face='bold'),legend.position='bottom',axis.title=ggplot2::element_text(color='#526b74'))

dashboard_data <- function(p,results,variant='automatic') {
  run<-if(variant=='automatic')p$fit else p$reviewed[[variant]]
  stale<-!is.null(run) && !identical(run$fingerprint,analysis_fingerprint(p))
  decisions<-if(!is.null(run) && !stale)peak_decisions(p,variant=variant) else data.frame()
  wells<-lapply(seq_len(nrow(p$plate)),function(i) {
    well<-p$plate[i,,drop=FALSE]
    records<-if(nrow(p$records))p$records[p$records$Well==well$Well,,drop=FALSE] else data.frame()
    entries<-lapply(seq_len(nrow(records)),function(j) {
      rec<-records[j,,drop=FALSE]
      rr<-if(nrow(results))results[results$Record_ID==rec$Record_ID,,drop=FALSE] else data.frame()
      val<-function(method) {
        z<-rr[rr$Method==method,,drop=FALSE]
        if(!rec$Role%in%c('sample','positive_control') || !nrow(z) || stale || !is.finite(z$Preview_EI[1]))NULL else unname(z$Preview_EI[1])
      }
      peaks<-p$peaks[p$peaks$Record_ID==rec$Source_ID,,drop=FALSE]
      d<-if(nrow(decisions))decisions[decisions$Analysis_record==rec$Record_ID,,drop=FALSE] else data.frame()
      flag<-function(method) if(nrow(d))d$Peak_ID[d$Method==method & d$Included] else character()
      a<-flag('adaptive');b<-flag('bayesian')
      all<-lapply(seq_len(nrow(peaks)),function(k)c(peaks$Size[k],peaks$Height[k],
        as.integer(peaks$Peak_ID[k]%in%a),as.integer(peaks$Peak_ID[k]%in%b)))
      f<-if(!is.null(run))run$records[[rec$Record_ID]] else NULL
      field<-function(method,name,default='')if(any(rr$Method==method))rr[[name]][match(method,rr$Method)] else default
      target<-target_description(p,rec,if(stale)NULL else f)
      list(id=rec$Record_ID,acquisition=rec$Acquisition,allele=rec$Allele,kind=rec$Target_kind%||%'unassigned',
        a=val('adaptive'),b=val('bayesian'),all=all,modal=if(!stale && !is.null(f) && is.finite(f$modal_size))f$modal_size else NULL,
        modal_rfu=if(!stale && !is.null(f))f$modal_height else NULL,
        upper=if(!stale && !is.null(f))f$upper else NULL,
        target_status=target$status,target=target$text,
        candidates=if(nrow(target$candidates))lapply(seq_len(nrow(target$candidates)),function(k) {
          z<-target$candidates[k,];pk<-candidate_inventory(p,rec);j<-which(abs(pk$Size-z$Modal_size_bp)<1e-8)
          list(id=if(length(j)==1)pk$Peak_ID[j] else '',size=z$Modal_size_bp,rfu=z$Modal_signal)
        }) else list(),
        selected_peak=if(rec$Modal_rule=='manual')rec$Modal_peak_ID else '',
        formal=if(nrow(rr))paste(rr$Formal_status[1],rr$Formal_reason[1],sep=' · ') else 'Not run',
        a_status=field('adaptive','Preview_status','not_fitted'),b_status=field('bayesian','Preview_status','not_fitted'),
        status=if(nrow(rr))rr$Status[1] else 'not_fitted',reason=if(nrow(rr))rr$Reason[1] else '',
        a_reason=field('adaptive','Preview_reason'),b_reason=field('bayesian','Preview_reason'))
    })
    list(w=well$Well,name=well$Code,group=well$Group,role=well$Role,
      acquired=if(nrow(p$peaks))any(p$peaks$Well==well$Well) else FALSE,records=entries)
  })
  list(wells=wells,threshold=100*p$threshold,window=p$window,stale=stale,
    gene=p$assay$gene%||%'',synthetic=p$synthetic,
    filter=paste0('Channel ',if(nzchar(p$assay$channel))p$assay$channel else 'all',' · Size >= ',p$assay$minimum_bp,' bp'))
}
method_colors <- c(adaptive='#167d9a',bayesian='#cf8051',hierarchical='#8264a5')
overview_plot <- function(results,synthetic=FALSE) {
  if(!nrow(results))return(empty_plot('Import a plate and sizing table, then run analysis.'))
  r<-results[results$Method%in%c('adaptive','bayesian'),,drop=FALSE]
  r$Label<-paste0(r$Well,' · ',ifelse(nzchar(r$Code),r$Code,'Unlabelled'),
    ifelse(duplicated(paste(r$Code,r$Well,r$Method)) | duplicated(paste(r$Code,r$Well,r$Method),fromLast=TRUE),paste0(' / ',r$Acquisition),'') ,
    ifelse(r$Allele!='target-1',paste0(' / ',r$Allele),''))
  r$Value<-ifelse(is.finite(r$EI),r$EI,r$Preview_EI);r$Display<-ifelse(is.finite(r$EI),'Formal','Preview / not cleared')
  r$Label<-factor(r$Label,levels=unique(r$Label))
  ggplot2::ggplot(r,ggplot2::aes(Label,Value,color=Method,shape=Display))+ggplot2::geom_point(position=ggplot2::position_dodge(width=.45),size=2.8,na.rm=TRUE)+
    ggplot2::geom_point(data=r[!is.finite(r$Value),,drop=FALSE],ggplot2::aes(y=0),shape=4,alpha=.35,position=ggplot2::position_dodge(width=.45),na.rm=TRUE)+
    ggplot2::facet_wrap(~Group,scales='free_x',ncol=1)+ggplot2::scale_color_manual(values=method_colors)+ggplot2::scale_shape_manual(values=c(Formal=16,'Preview / not cleared'=1))+
    ggplot2::labs(title=if(synthetic)'SYNTHETIC · paired EI overview' else 'Paired EI overview',subtitle=paste0('Height ≥ ',100*r$Threshold[1],'% own modal · ',r$Window[1],
      '\nHollow = preview; × at baseline = unavailable, not EI = 0'),x='Sample code · well / acquisition',y='Expansion index (repeat units)')+
    rs_theme()+ggplot2::theme(axis.text.x=ggplot2::element_text(angle=45,hjust=1,size=9))
}
tail_plot <- function(p,record_id,variant='automatic',decisions=NULL) {
  run<-if(variant=='automatic')p$fit else p$reviewed[[variant]]
  rec<-p$records[match(record_id,p$records$Record_ID),]
  title<-paste(rec$Well,if(nzchar(rec$Code))rec$Code else 'Unlabelled',sep=' · ')
  if(is.null(run))return(empty_plot('Run analysis to compare tail selection.')+ggplot2::labs(title=title))
  if(!identical(run$fingerprint,analysis_fingerprint(p)))return(empty_plot('Results outdated — run analysis again. Imported profiles remain in the plate detail.')+ggplot2::labs(title=title))
  f<-run$records[[record_id]]
  if(is.null(f) || !nrow(f$peaks) || f$status!='fitted')return(empty_plot(f$reason%||%'No fitted target for this record.')+ggplot2::labs(title=title))
  d<-decisions %||% peak_decisions(p,p$threshold,p$window,variant,record_id)
  d$Method<-factor(d$Method,levels=c('adaptive','bayesian'));d$State<-ifelse(d$Included,'Included','Not included')
  a<-d[d$Method=='adaptive' & d$Included,];b<-d[d$Method=='bayesian' & d$Included,]
  ak<-paste(a$Peak_ID,a$Delta);bk<-paste(b$Peak_ID,b$Delta)
  conflicts<-union(a$Peak_ID[!ak%in%bk],b$Peak_ID[!bk%in%ak]);d$State[d$Included & d$Peak_ID%in%conflicts]<-'Method-only included'
  d$State[d$Modal]<-'Modal'
  rr<-threshold_results(p,variant=variant);rr<-rr[rr$Record_ID==record_id,]
  labels<-setNames(paste0(rr$Method,' · Preview EI ',ifelse(is.finite(rr$Preview_EI),sprintf('%.6f',rr$Preview_EI),'unavailable')),rr$Method)
  pr<-f$bayesian$predictions;pr<-pr[is.finite(pr$Center) & pr$Low<=f$upper & pr$High>=f$modal_size,,drop=FALSE];pr$Method<-factor(rep('bayesian',nrow(pr)),levels=c('adaptive','bayesian'))
  maxh<-max(d$Height[d$Size>=f$modal_size & d$Size<=f$upper],f$modal_height,na.rm=TRUE)
  plot<-ggplot2::ggplot(d,ggplot2::aes(Size,Height))+
    ggplot2::geom_rect(data=pr,ggplot2::aes(xmin=Low,xmax=High,ymin=0,ymax=maxh*1.04),inherit.aes=FALSE,fill='#debb89',alpha=.15)+
    ggplot2::geom_segment(ggplot2::aes(xend=Size,y=0,yend=Height,color=State),linewidth=.55)+
    ggplot2::geom_point(data=d[d$Included,,drop=FALSE],ggplot2::aes(color=State),size=1.8)+
    ggplot2::geom_hline(yintercept=p$threshold*f$modal_height,linetype='dashed',color='#66828d')+
    ggplot2::geom_vline(xintercept=f$modal_size,linetype='dotted',color='#50636a')+
    ggplot2::facet_grid(~Method,labeller=ggplot2::labeller(Method=labels))+ggplot2::scale_color_manual(values=c(Included='#167d9a','Not included'='#aebdc2','Method-only included'='#c85a42',Modal='#713b91'))+
    ggplot2::coord_cartesian(xlim=c(f$modal_size-1,f$upper+1),ylim=c(0,maxh*1.08))+
    ggplot2::labs(title=title,subtitle=paste0('Stick plot from sized peaks · ',100*p$threshold,'% own modal · ',p$window,
      '\nBayesian shading: ',100*p$assay$predictive_level,'% pointwise position prediction intervals (bp)',
      '\nThese are not peak probabilities or EI uncertainty intervals.'),
      caption=paste(paste(rr$Method,rr$Preview_reason,sep=': '),collapse='\n'),
      x='GeneMapper Size (bp)',y='Own-well Height (RFU)',color='Peak decision')+rs_theme()
  plot
}
save_figure <- function(plot,path,width=12,height=6) {
  ext<-tolower(tools::file_ext(path))
  if(ext=='svg')grDevices::svg(path,width=width,height=height,bg='white') else if(ext=='pdf')grDevices::cairo_pdf(path,width=width,height=height,bg='white') else if(ext=='png')grDevices::png(path,width=width,height=height,units='in',res=140,type='cairo',bg='white') else stop('Unsupported figure format.')
  on.exit(grDevices::dev.off());print(plot);invisible(path)
}
