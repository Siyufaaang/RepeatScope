source('development/shiny_app/R/bootstrap.R');require_dependencies();library(shiny)
source(file.path(rs_app_dir,'R/server.R'))
options(repeatscope.root=rs_root)
testServer(rs_server,{
  session$setInputs(minimum_bp=103,target_channel='B',apply_preview_filter=1)
  stopifnot(rv$p$assay$minimum_bp==103,rv$p$assay$channel=='B',rv$p$assay$status=='unconfigured')
  session$setInputs(minimum_bp=-1,target_channel='B',apply_preview_filter=2)
  stopifnot(rv$p$assay$minimum_bp==103)
})
demo<-localized_synthetic_demo()
demo$assay$minimum_bp<-1e6
check_message<-candidate_filter_issue(demo)
stopifnot(length(check_message)==1L,grepl('All sample peaks are excluded',check_message),grepl('1000000',check_message,fixed=TRUE))
demo$assay$minimum_bp<-0;demo$assay$channel<-'Y'
stopifnot(grepl('channel',candidate_filter_issue(demo),ignore.case=TRUE))
demo$assay$channel<-'B'
stopifnot(!length(candidate_filter_issue(demo)))

# A missing target never reached Bayesian seed fitting, so report the original
# filter/target failure for both methods rather than inventing a seed failure.
demo$assay$minimum_bp<-1e6
demo<-fit_project(prepare_localized_project(demo))
result<-threshold_results(demo)
samples<-result[result$Role=='sample',]
stopifnot(all(samples$Preview_status=='missing_target'),
          all(grepl('No positive-height peaks',samples$Preview_reason,fixed=TRUE)))
cat('PREVIEW FILTER PASS\n')
