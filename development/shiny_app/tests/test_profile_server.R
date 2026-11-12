source('development/shiny_app/R/bootstrap.R');require_dependencies();library(shiny)
options(repeatscope.root=rs_root);source(file.path(rs_app_dir,'R/server.R'));source(file.path(rs_app_dir,'R/ui.R'))
check<-function(x,msg){if(!isTRUE(x))stop('FAIL: ',msg);cat('PASS:',msg,'\n')}
testServer(rs_server,{
 session$setInputs(edit_analysis=1,gene='ATXN2')
 check(rv$p$localization$normal_max==31 && rv$p$disease_profile$gene=='ATXN2','gene entry loads its profile in the real server')
 session$setInputs(gene='HTT')
 check(rv$p$localization$normal_max==26 && rv$p$disease_profile$gene=='HTT','server switches profiles by gene')
 session$setInputs(gene='UNKNOWN')
 check(is.null(rv$p$disease_profile) && is.na(rv$p$localization$normal_max),'unknown gene does not retain HTT defaults')
 p<-apply_disease_profile(new_project(),'ATXN2');p$localization$normal_max<-30;p$profile_custom<-TRUE
 rv$p<-p;refresh(p,replacement=TRUE);session$setInputs(gene='ATXN2')
 check(rv$p$localization$normal_max==30,'restored custom profile is not overwritten by input hydration')
 session$setInputs(reset_gene_profile=1)
 check(rv$p$localization$normal_max==31 && !rv$p$profile_custom,'explicit reset reloads catalog defaults')
})
cat('PROFILE SERVER SUITE PASS\n')
