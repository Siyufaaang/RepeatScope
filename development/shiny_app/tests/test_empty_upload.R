source('development/shiny_app/R/bootstrap.R');require_dependencies();library(shiny)
source(file.path(rs_app_dir,'R/server.R'))
testServer(rs_server,{
  session$setInputs(peak_range='',try_workflow=1)
  stopifnot(is.null(rv$peak_import_error))
  stopifnot(!grepl('Import did not complete',output$import_feedback$html,fixed=TRUE))
})
cat('EMPTY UPLOAD IS NOT AN IMPORT ERROR PASS\n')
