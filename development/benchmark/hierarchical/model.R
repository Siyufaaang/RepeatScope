# Experimental conditional LOWO position model; heights are never pooled.
nig_update <- function(X,y,mu,V,a,b) {
  X<-as.matrix(X);mu<-as.numeric(mu);y<-as.numeric(y)
  stopifnot(ncol(X)==length(mu),nrow(X)==length(y),a>0,b>0,all(is.finite(c(X,y,mu,V))))
  P0<-solve(V);Vn<-solve(P0+crossprod(X));mn<-drop(Vn%*%(P0%*%mu+crossprod(X,y)))
  # Residual form avoids cancellation of large squared observed sizes.
  bn<-b+.5*(sum((y-drop(X%*%mn))^2)+drop(crossprod(mn-mu,P0%*%(mn-mu))))
  list(mu=mn,V=Vn,a=a+length(y)/2,b=bn)
}
hier_basis <- function(k,curve=TRUE) {
  if(curve)cbind(slope=k,curve=k*(k-1)/100) else matrix(k,ncol=1,dimnames=list(NULL,'slope'))
}
hier_prior <- function(curve=TRUE) {
  d<-if(curve)2L else 1L
  list(mu=c(3,0)[seq_len(d)],V=diag(c(20,1)[seq_len(d)],nrow=d),a=2,b=.01,curve=curve,peer_wells=character())
}
fit_case_lowo <- function(anchors,target,group,curve=TRUE,effect_mult=1) {
  z<-anchors[anchors$Well!=target & anchors$Group==group,,drop=FALSE]
  wells<-sort(unique(z$Well));d<-if(curve)2L else 1L
  if(length(wells)<2L)stop('At least two peer wells required')
  B<-hier_basis(z$Delta,curve);X<-matrix(0,nrow(z),d*(1+length(wells)));X[,seq_len(d)]<-B
  D<-diag(c(.04,.25)[seq_len(d)]*effect_mult,nrow=d)
  V<-matrix(0,ncol(X),ncol(X));V[seq_len(d),seq_len(d)]<-hier_prior(curve)$V
  for(j in seq_along(wells)) {
    ix<-d*j+seq_len(d);rows<-which(z$Well==wells[j]);X[rows,ix]<-B[rows,,drop=FALSE];V[ix,ix]<-D
  }
  mu<-c(hier_prior(curve)$mu,rep(0,d*length(wells)))
  f<-nig_update(X,z$Y,mu,V,2,.01)
  # Marginal posterior for a new well, including its as-yet unknown random effect.
  list(mu=f$mu[seq_len(d)],V=f$V[seq_len(d),seq_len(d),drop=FALSE]+D,
       a=f$a,b=f$b,curve=curve,peer_wells=wells,n_peer_peaks=nrow(z),effect_mult=effect_mult)
}
condition_well <- function(fit,k,y) {
  stopifnot(length(k)>=3,all(k>0),!anyDuplicated(k))
  c(nig_update(hier_basis(k,fit$curve),y,fit$mu,fit$V,fit$a,fit$b),
    list(curve=fit$curve,peer_wells=fit$peer_wells))
}
predict_hier <- function(fit,k,modal,level=.95) {
  B<-hier_basis(k,fit$curve);center<-modal+drop(B%*%fit$mu)
  scale<-sqrt(fit$b/fit$a*(1+rowSums((B%*%fit$V)*B)));half<-qt((1+level)/2,2*fit$a)*scale
  data.frame(Delta=k,Center=center,Low=center-half,High=center+half,Scale=scale,Level=level)
}
assign_hier <- function(peaks,modal_id,seeds,predictions) {
  stopifnot(!anyDuplicated(peaks$Peak_ID),sum(peaks$Peak_ID==modal_id)==1,!anyDuplicated(seeds$Peak_ID),!anyDuplicated(seeds$Delta))
  pr<-predictions;pr$Status<-'no_candidate';pr$N_candidates<-0L;pr$Chosen_ID<-NA_character_
  fixed<-data.frame(Peak_ID=c(modal_id,seeds$Peak_ID),Delta=c(0,seeds$Delta),Role=c('modal',rep('seed',nrow(seeds))))
  stopifnot(all(fixed$Peak_ID%in%peaks$Peak_ID),!anyDuplicated(fixed$Peak_ID))
  cand<-list()
  modal<-peaks$Size[match(modal_id,peaks$Peak_ID)]
  increments<-diff(c(modal,pr$Center));neighbor_gap<-pmin(increments,c(increments[-1],tail(increments,1)))
  for(i in seq_len(nrow(pr))) {
    k<-pr$Delta[i]
    if(k%in%seeds$Delta) {
      pr$Status[i]<-'seed';pr$Chosen_ID[i]<-seeds$Peak_ID[match(k,seeds$Delta)];next
    }
    ids<-which(peaks$Size>=pr$Low[i] & peaks$Size<=pr$High[i] & !peaks$Peak_ID%in%fixed$Peak_ID & peaks$Size>modal)
    pr$N_candidates[i]<-length(ids)
    if(length(ids))cand[[length(cand)+1L]]<-data.frame(Delta=k,Peak_ID=peaks$Peak_ID[ids],Size=peaks$Size[ids])
    pr$Status[i]<-if(!is.finite(neighbor_gap[i]) || neighbor_gap[i]<=0)'nonmonotonic' else
      if((pr$High[i]-pr$Low[i])/2>=neighbor_gap[i]/2)'uncertain_window' else
      if(length(ids)>1)'ambiguous' else if(length(ids)==1)'selected' else 'no_candidate'
    if(pr$Status[i]=='selected')pr$Chosen_ID[i]<-peaks$Peak_ID[ids]
  }
  candidates<-if(length(cand))do.call(rbind,cand) else data.frame(Delta=integer(),Peak_ID=character(),Size=numeric())
  duplicate_ids<-unique(candidates$Peak_ID[duplicated(candidates$Peak_ID)])
  overlap<-pr$Delta%in%candidates$Delta[candidates$Peak_ID%in%duplicate_ids]
  pr$Status[overlap]<-'overlapping_windows';pr$Chosen_ID[overlap]<-NA_character_
  added<-pr[pr$Status=='selected',]
  chosen<-rbind(fixed,data.frame(Peak_ID=added$Chosen_ID,Delta=added$Delta,Role=rep('tail',nrow(added))))
  selected<-cbind(peaks[match(chosen$Peak_ID,peaks$Peak_ID),,drop=FALSE],chosen[,c('Delta','Role'),drop=FALSE])
  selected<-selected[order(selected$Delta),,drop=FALSE]
  # Reject tail assignments that would reverse the observed physical peak sequence.
  bad<-which(diff(selected$Size)<=0)+1L
  if(length(bad)) {
    bad_delta<-unique(c(selected$Delta[bad],selected$Delta[pmax(1,bad-1)]));bad_delta<-setdiff(bad_delta,fixed$Delta)
    pr$Status[pr$Delta%in%bad_delta]<-'nonmonotonic';pr$Chosen_ID[pr$Delta%in%bad_delta]<-NA_character_
    selected<-selected[!selected$Delta%in%bad_delta,,drop=FALSE]
  }
  candidates$Status<-pr$Status[match(candidates$Delta,pr$Delta)]
  list(selected=selected,predictions=pr,candidates=candidates)
}
