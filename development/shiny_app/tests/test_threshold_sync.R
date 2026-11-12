source('development/shiny_app/R/bootstrap.R');require_dependencies();library(shiny)
sys.source(file.path(rs_app_dir,'R/server.R'),envir=.GlobalEnv)
check<-function(ok,msg){if(!isTRUE(ok))stop('FAIL: ',msg);cat('PASS:',msg,'\n')}
shiny::testServer(rs_server,{
  rv$p<-fit_project(synthetic_project());p<-rv$p;p$threshold<-.2;rv$p<-p
  refresh(rv$p);session$flushReact();fit<-rv$p$fit
  session$setInputs(reset_threshold=1)
  check(rv$p$threshold==.03,'Reset sets the canonical threshold to 3 percent')
  # The two bound widgets can echo an earlier update after Reset has been handled.
  session$setInputs(threshold=20,threshold_number=20)
  check(rv$p$threshold==.03,'Late widget echoes cannot restore the old 20 percent threshold')
  session$setInputs(threshold_change=list(value=20,source='slider',nonce=1))
  check(rv$p$threshold==.2,'A user slider event changes the canonical threshold')
  session$setInputs(threshold_change=list(value=5,source='number',nonce=2))
  check(rv$p$threshold==.05,'A user number edit changes the canonical threshold')
  session$setInputs(reset_threshold=2)
  session$setInputs(threshold=5,threshold_number=20)
  check(rv$p$threshold==.03 && identical(rv$p$fit,fit),'Reset survives out-of-order echoes without refitting')
  session$setInputs(threshold_change=list(value=101,source='number',nonce=3))
  check(rv$p$threshold==.03,'Out-of-range edits preserve the last valid threshold')
  dir<-tempfile('threshold-sync-');dir.create(dir)
  save_project(rv$p,file.path(dir,'project.rds'));q<-read_project(file.path(dir,'project.rds'))
  write_tables(rv$p,dir);rows<-read.csv(file.path(dir,'results_long.csv'))
  check(q$threshold==.03 && all(rows$Threshold==.03) && all(results()$Threshold==.03) &&
    analysis_manifest(rv$p)$threshold_fraction==.03,'Project, report tables, manifest and displayed results share 3 percent')
  unlink(dir,recursive=TRUE)
})
cat('THRESHOLD SYNCHRONIZATION SUITE PASS\n')
