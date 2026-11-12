source('development/shiny_app/R/bootstrap.R');require_dependencies();library(shiny)
source(file.path(rs_app_dir,'R/server.R'))
check<-function(ok,msg){if(!isTRUE(ok))stop('FAIL: ',msg);cat('PASS:',msg,'\n')}
testServer(rs_server,{
  session$setInputs(try_workflow=1)
  path<-file.path(rs_app_dir,'examples/SYNTHETIC_peaks.csv')
  session$setInputs(peak_file=list(name=basename(path),datapath=path,size=file.info(path)$size),peak_range='',
                    map_Well='Well',map_Size='Size',map_Height='Area',map_Acquisition='',map_Peak_ID='',
                    map_Data_Point='',map_Channel='',map_QC='',map_Saturation='',well_from_filename=FALSE)
  rv$raw_peaks<-data.frame(Well='A01',Size='200',Height='1000',Area='9000')
  rv$raw_peaks_key<-upload_source_key(input,'peak')
  before<-object_hash(rv$p)
  session$setInputs(commit_peaks=1)
  check(length(rv$peak_import_error)==1 && grepl('Area is not Height',rv$peak_import_error),'Rejected mapping keeps a visible persistent import error')
  check(grepl('Area is not Height',output$import_feedback$html),'Import checks display the cause beside the failed import')
  check(identical(before,object_hash(rv$p)),'Failed import leaves the existing project unchanged')
})
