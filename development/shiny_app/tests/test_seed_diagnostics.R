source('development/shiny_app/R/bootstrap.R');require_dependencies()
p<-fit_project(synthetic_project())
id<-names(p$fit$records)[1]
p$fit$records[[id]]$bayesian$status<-'insufficient_seeds'
p$fit$records[[id]]$bayesian$seed_stop<-'missing_seed'
p$fit$records[[id]]$bayesian$seed_audit<-data.frame(Delta=1L,Low=168.4003,High=170.1997,N_candidates=0L,Chosen_ID=NA_character_,Status='missing_seed')
r<-threshold_results(p);row<-r[r$Record_ID==id & r$Method=='bayesian',]
stopifnot(is.na(row$Preview_EI),grepl('missing_seed',row$Reason,fixed=TRUE),
  grepl('168.40',row$Reason,fixed=TRUE),grepl('170.20',row$Reason,fixed=TRUE))
cat('SEED DIAGNOSTICS PASS\n')
