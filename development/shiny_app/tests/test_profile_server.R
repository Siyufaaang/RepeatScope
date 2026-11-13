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

testServer(rs_server,{
 p<-apply_disease_profile(new_project(),'ATXN2');p$assay$gene<-'SCA2';p$localization$gene<-'SCA2'
 p$localization$normal_max<-30;p$profile_custom<-TRUE
 fixture<-synthetic_project();p$reference<-fixture$reference;p$structure<-fixture$structure
 session$setInputs(forward='TTATGCGA',reverse='ACGGTCAA',forward_tail='',reverse_tail='',motif='CAG',tract_start=9,tract_end=59,counting_rule='tract_length')
 rv$p<-p;refresh(p,replacement=TRUE);session$setInputs(edit_analysis=1,gene='SCA2')
 check(rv$p$localization$normal_max==30 && isTRUE(rv$p$profile_custom),'opening saved alias preserves custom ranges')
 check(identical(rv$p$reference,p$reference) && identical(rv$p$structure,p$structure),'opening saved alias preserves matched structure')
 session$setInputs(gene='ATXN2')
 check(rv$p$localization$normal_max==30,'canonical spelling of the same gene preserves custom ranges')
})
