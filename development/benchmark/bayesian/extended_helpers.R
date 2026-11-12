scoped_ei <- function(selected,modal_height,threshold=.03,max_delta=Inf) {
  z<-selected[selected$Delta>=0 & selected$Delta<=max_delta & selected$Height>=threshold*modal_height,,drop=FALSE]
  list(Value=if(nrow(z))sum(z$Delta*z$Height)/sum(z$Height) else NA_real_,N=nrow(z),Peaks=z)
}

bayesian_scope_unresolved <- function(result,upper,max_delta=Inf) {
  pr<-result$predictions
  # A window is relevant if its center is in the review envelope or it contains an observed candidate there.
  candidate_delta<-result$candidates$Delta[result$candidates$Size<=upper]
  scope<-pr$Delta<=max_delta & (is.finite(pr$Center) & pr$Center<=upper | pr$Delta %in% candidate_delta)
  any(scope & pr$Status %in% c('ambiguous','uncertain_window','overlapping_windows'))
}
