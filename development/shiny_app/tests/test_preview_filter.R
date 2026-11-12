source('development/shiny_app/R/bootstrap.R');require_dependencies();library(shiny)
source(file.path(rs_app_dir,'R/server.R'))
testServer(rs_server,{
  session$setInputs(minimum_bp=103,target_channel='B',apply_preview_filter=1)
  stopifnot(rv$p$assay$minimum_bp==103,rv$p$assay$channel=='B',rv$p$assay$status=='unconfigured')
  session$setInputs(minimum_bp=-1,target_channel='B',apply_preview_filter=2)
  stopifnot(rv$p$assay$minimum_bp==103)
})
cat('PREVIEW FILTER PASS\n')
