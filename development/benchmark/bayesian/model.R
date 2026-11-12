# Experimental two-stage Bayesian repeat-spacing model. No package dependencies.
# Conditional on the observed modal and the accepted seed labels; no peak-identity probabilities.
bayes_tail_config <- function(...) {
  defaults <- list(prior_mean=3,prior_kappa=.05,prior_alpha=2,prior_beta=.01,
    seed_gap_low=2.5,seed_gap_high=3.2,seed_min_rfu=100,seed_modal_fraction=.03,
    seed_dominance=3,seed_max=5L,seed_min=3L,max_delta=10L,predictive_level=.95,seed_predictive=0)
  supplied<-list(...)
  if(length(setdiff(names(supplied),names(defaults))))stop('Unknown configuration field')
  defaults[names(supplied)]<-supplied
  defaults
}

validate_bayes_config <- function(cfg) {
  required<-names(bayes_tail_config())
  if(!all(required %in% names(cfg)))stop('Incomplete configuration')
  if(any(!vapply(cfg[required],function(x)is.numeric(x) && length(x)==1L && is.finite(x),logical(1))))
    stop('Configuration entries must be finite numeric scalars')
  positive<-c('prior_mean','prior_kappa','prior_alpha','prior_beta','seed_gap_low','seed_gap_high','seed_dominance')
  if(any(unlist(cfg[positive])<=0) || cfg$seed_gap_high<=cfg$seed_gap_low || cfg$seed_dominance<=1 ||
     cfg$seed_min_rfu<0 || cfg$seed_modal_fraction<0 || cfg$seed_modal_fraction>1 ||
     cfg$predictive_level<=0 || cfg$predictive_level>=1)stop('Invalid prior, seed rule or probability')
  if(any(unlist(cfg[c('seed_min','seed_max','max_delta')])%%1!=0) || cfg$seed_min<3 ||
     cfg$seed_max<cfg$seed_min || cfg$max_delta<cfg$seed_max)stop('Invalid seed/delta counts')
  if(!cfg$seed_predictive %in% c(0,1))stop('Invalid seed mode')
  invisible(TRUE)
}

fit_tail_spacing <- function(k,y,cfg=bayes_tail_config()) {
  validate_bayes_config(cfg)
  if(length(k)!=length(y) || !length(k) || any(!is.finite(c(k,y))) || any(k<=0) ||
     any(k%%1!=0) || anyDuplicated(k))stop('Invalid labelled seed positions')
  # sigma^2 ~ IG(alpha0,beta0); s|sigma^2 ~ N(mean0,sigma^2/kappa0).
  kappa<-cfg$prior_kappa+sum(k^2)
  mu<-(cfg$prior_kappa*cfg$prior_mean+sum(k*y))/kappa
  alpha<-cfg$prior_alpha+length(k)/2
  beta<-cfg$prior_beta+.5*sum((y-k*mu)^2)+.5*cfg$prior_kappa*(mu-cfg$prior_mean)^2
  scale<-sqrt(beta/(alpha*kappa))
  ci<-mu+qt(c(.025,.975),df=2*alpha)*scale
  list(mean=mu,kappa=kappa,alpha=alpha,beta=beta,df=2*alpha,scale=scale,
       low95=ci[1],high95=ci[2],n_seed=length(k),residual_rms=sqrt(mean((y-k*mu)^2)))
}

predict_tail_spacing <- function(fit,k,modal,level=.95) {
  if(length(modal)!=1L || !is.finite(modal) || any(!is.finite(k)) || any(k<0) ||
     length(level)!=1L || !is.finite(level) || level<=0 || level>=1)stop('Invalid prediction arguments')
  scale<-sqrt(fit$beta/fit$alpha*(1+k^2/fit$kappa))
  center<-modal+k*fit$mean
  half<-qt((1+level)/2,df=fit$df)*scale
  data.frame(Delta=k,Center=center,Low=center-half,High=center+half,Scale=scale,Level=level)
}

assign_bayesian_tail <- function(peaks,modal,modal_height,cfg=bayes_tail_config()) {
  validate_bayes_config(cfg)
  if(!all(c('Peak_ID','Size','Height') %in% names(peaks)) || !nrow(peaks) ||
     anyNA(peaks$Peak_ID) || anyDuplicated(peaks$Peak_ID) ||
     any(!is.finite(c(peaks$Size,peaks$Height))) || any(peaks$Height<=0))stop('Invalid physical peak inventory')
  if(length(modal)!=1L || length(modal_height)!=1L || !is.finite(modal) || !is.finite(modal_height))
    stop('Invalid modal')
  p<-peaks[order(peaks$Size,peaks$Peak_ID),,drop=FALSE]
  ix<-which(abs(p$Size-modal)<1e-8)
  if(length(ix)!=1L || p$Height[ix]!=modal_height)stop('Modal must identify exactly one physical peak with matching height')
  modal_row<-p[ix,,drop=FALSE];modal_row$Delta<-0L;modal_row$Role<-'modal'
  seeds<-modal_row[FALSE,,drop=FALSE]
  seed_audit<-data.frame(Delta=integer(),Low=numeric(),High=numeric(),N_candidates=integer(),
                         Chosen_ID=character(),Status=character())
  previous<-modal;floor_rfu<-max(cfg$seed_min_rfu,cfg$seed_modal_fraction*modal_height)
  stop_reason<-'max_seed_reached'
  for(k in seq_len(cfg$seed_max)) {
    low<-previous+cfg$seed_gap_low;high<-previous+cfg$seed_gap_high
    broad_seed<-FALSE
    if(cfg$seed_predictive==1) {
      provisional<-if(nrow(seeds))fit_tail_spacing(seeds$Delta,seeds$Size-modal,cfg) else
        list(mean=cfg$prior_mean,kappa=cfg$prior_kappa,alpha=cfg$prior_alpha,beta=cfg$prior_beta,df=2*cfg$prior_alpha)
      window<-predict_tail_spacing(provisional,k,modal,cfg$predictive_level)
      low<-window$Low;high<-window$High
      broad_seed<-provisional$mean<=0 || (high-low)/2>=provisional$mean/2
    }
    # GeneMapper decimal sizes may lie exactly on an inclusive bound.
    z<-p[p$Size>=low-1e-8 & p$Size<=high+1e-8 & p$Size>previous+1e-8,,drop=FALSE]
    z<-z[order(-z$Height,z$Size,z$Peak_ID),,drop=FALSE]
    status<-if(broad_seed)'uncertain_seed_window' else if(!nrow(z))'missing_seed' else if(z$Height[1]<floor_rfu)'weak_seed' else
      if(nrow(z)>1L && z$Height[1]/z$Height[2]<cfg$seed_dominance)'competing_seed' else 'accepted_seed'
    seed_audit<-rbind(seed_audit,data.frame(Delta=k,Low=low,High=high,N_candidates=nrow(z),
                       Chosen_ID=if(status=='accepted_seed')z$Peak_ID[1] else NA_character_,Status=status))
    if(status!='accepted_seed'){stop_reason<-status;break}
    chosen<-z[1,,drop=FALSE];chosen$Delta<-k;chosen$Role<-'calibration'
    seeds<-rbind(seeds,chosen);previous<-chosen$Size
  }
  pred<-data.frame(Delta=seq_len(cfg$max_delta),Center=NA_real_,Low=NA_real_,High=NA_real_,
                  Scale=NA_real_,Level=cfg$predictive_level,Status='uncalibrated',N_candidates=NA_integer_,Chosen_ID=NA_character_)
  candidates<-data.frame(Delta=integer(),Peak_ID=character(),Size=numeric(),Height=numeric(),Standardized_residual=numeric(),Status=character())
  result<-list(status='insufficient_seeds',seed_stop=stop_reason,seed_floor_rfu=floor_rfu,
               seeds=seeds,seed_audit=seed_audit,fit=NULL,predictions=pred,candidates=candidates,selected=modal_row,config=cfg)
  if(nrow(seeds)<cfg$seed_min)return(result)
  fit<-fit_tail_spacing(seeds$Delta,seeds$Size-modal,cfg)
  pred<-predict_tail_spacing(fit,seq_len(cfg$max_delta),modal,cfg$predictive_level)
  pred$Status<-'no_candidate';pred$N_candidates<-0L;pred$Chosen_ID<-NA_character_
  seed_ids<-c(modal_row$Peak_ID,seeds$Peak_ID)
  for(j in seq_len(nrow(pred))) {
    k<-pred$Delta[j]
    if(k %in% seeds$Delta) {
      pred$Status[j]<-'calibration';pred$Chosen_ID[j]<-seeds$Peak_ID[match(k,seeds$Delta)]
      pred$N_candidates[j]<-NA_integer_;next
    }
    z<-p[p$Size>=pred$Low[j]-1e-8 & p$Size<=pred$High[j]+1e-8 & !p$Peak_ID %in% seed_ids,,drop=FALSE]
    pred$N_candidates[j]<-nrow(z)
    broad<-(pred$High[j]-pred$Low[j])/2>=fit$mean/2 || fit$mean<=0
    pred$Status[j]<-if(broad)'uncertain_window' else if(!nrow(z))'no_candidate' else if(nrow(z)==1L)'unique_candidate' else 'ambiguous'
    if(pred$Status[j]=='unique_candidate')pred$Chosen_ID[j]<-z$Peak_ID
    if(nrow(z))candidates<-rbind(candidates,data.frame(Delta=k,Peak_ID=z$Peak_ID,Size=z$Size,Height=z$Height,
      Standardized_residual=(z$Size-pred$Center[j])/pred$Scale[j],Status=pred$Status[j]))
  }
  # Competing windows may not reuse one physical peak; mark all affected positions.
  repeated<-unique(candidates$Peak_ID[duplicated(candidates$Peak_ID)])
  if(length(repeated)) {
    conflict_k<-unique(candidates$Delta[candidates$Peak_ID %in% repeated])
    pred$Status[pred$Delta %in% conflict_k]<-'overlapping_windows'
    pred$Chosen_ID[pred$Delta %in% conflict_k]<-NA_character_
    candidates$Status[candidates$Delta %in% conflict_k]<-'overlapping_windows'
  }
  ids<-pred$Chosen_ID[pred$Status=='unique_candidate']
  tail<-p[match(ids,p$Peak_ID),,drop=FALSE]
  tail$Delta<-pred$Delta[match(tail$Peak_ID,pred$Chosen_ID)]
  tail$Role<-rep('prediction',nrow(tail))
  selected<-rbind(modal_row,seeds,tail);selected<-selected[order(selected$Delta),,drop=FALSE]
  stopifnot(!anyDuplicated(selected$Peak_ID),!anyDuplicated(selected$Delta),selected$Peak_ID[1]==modal_row$Peak_ID)
  result$status<-'calibrated';result$fit<-fit;result$predictions<-pred
  result$candidates<-candidates;result$selected<-selected
  result
}

bayesian_ei <- function(result,expanded_eligible,threshold=0) {
  if(length(threshold)!=1L || !is.finite(threshold) || threshold<0 || threshold>1)stop('Invalid EI threshold')
  if(result$status!='calibrated')return(data.frame(EI=NA_real_,Diagnostic_EI_selected=NA_real_,N=NA_integer_,Interpretation='uncalibrated'))
  x<-result$selected;mh<-x$Height[x$Delta==0]
  take<-x$Height>=mh*threshold
  partial<-sum(x$Height[take]*x$Delta[take])/sum(x$Height[take])
  unresolved<-any(result$predictions$Status %in% c('ambiguous','uncertain_window','overlapping_windows'))
  status<-if(unresolved)'unresolved_assignment_partial_only' else if(!expanded_eligible)'normal_reference_diagnostic_only' else 'conditional_selected_peak_EI'
  data.frame(EI=if(expanded_eligible && !unresolved)partial else NA_real_,Diagnostic_EI_selected=partial,N=sum(take),Interpretation=status)
}
