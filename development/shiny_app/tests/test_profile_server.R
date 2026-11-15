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

# Two edits can be in flight before the browser receives the first response.
# Replay actual server input messages in delivery order, with a bounded queue
# so an echo loop fails rather than hanging the test process.
testServer(rs_server,{
 session$setInputs(edit_analysis=1,gene='')
 gene_updates<-list()
 session$sendInputMessage<-function(inputId,message) {
   if(identical(inputId,'gene'))gene_updates[[length(gene_updates)+1L]]<<-message$value
 }
 session$setInputs(gene='ATXN')
 session$setInputs(gene='ATXN2')
 for(i in seq_len(8)) {
   if(!length(gene_updates))break
   value<-gene_updates[[1]];gene_updates<-gene_updates[-1]
   session$setInputs(gene=value)
 }
 check(!length(gene_updates),'typing ahead of server responses settles without a gene input echo loop')
 check(rv$p$assay$gene=='ATXN2' && rv$p$disease_profile$gene=='ATXN2','the last typed gene keeps its disease profile')
})

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

# Keep the user's alias in the input, while reference lookup uses its gene.
# Substitute only NCBI lookup so this server interaction stays offline.
lookup<-new.env(parent=emptyenv())
reference_server<-register_reference_server
environment(reference_server)<-list2env(list(ncbi_search=function(gene,organism,type) {
  lookup$gene<-gene
  list(status='no_results',message='Offline test; no reference selected.')
}),parent=environment(register_reference_server))
alias_server<-rs_server
environment(alias_server)<-list2env(list(register_reference_server=reference_server),parent=environment(rs_server))
testServer(alias_server,{
 session$setInputs(edit_analysis=1,gene='Huntington disease',organism='Homo sapiens',reference_type='genomic')
 session$setInputs(search_ncbi=1)
 check(lookup$gene=='HTT','NCBI search resolves a disease alias without changing the active input')
 session$setInputs(fasta_text='>SYNTHETIC.1\nACGT',use_fasta=1)
 check(rv$p$reference$gene=='HTT','offline reference metadata uses the same canonical gene')
})
testServer(rs_server,{
 fixture<-synthetic_project()
 p<-apply_disease_profile(new_project(),'ATXN2');p$reference<-fixture$reference
 rv$p<-p
 session$setInputs(edit_analysis=1,gene='ATXN2',forward='TTATGCGA',reverse='ACGGTCAA',
   forward_tail='',reverse_tail='',motif='CAG',tract_start=9,tract_end=59,counting_rule='tract_length',locate_primers=1)
 check(rv$p$assay$minimum_bp==16,'first primer match supplies automatic minimum')
 session$setInputs(forward='AAAATTATGCGA',forward_tail='AAAA',locate_primers=2)
 check(rv$p$assay$minimum_bp==20,'revised primer pair updates the actual server filter')
 session$setInputs(minimum_bp=7,target_channel='B',apply_preview_filter=1)
 session$setInputs(forward='TTATGCGA',forward_tail='',locate_primers=3)
 check(rv$p$assay$minimum_bp==7,'later primer matching respects an applied manual filter')
})
testServer(rs_server,{
 fixture<-synthetic_project()
 p<-apply_disease_profile(new_project(),'ATXN2');p$reference<-fixture$reference
 rv$p<-p
 session$setInputs(edit_analysis=1,gene='ATXN2',minimum_bp=7,target_channel='B',apply_preview_filter=1)
 session$setInputs(forward='TTATGCGA',reverse='ACGGTCAA',forward_tail='',reverse_tail='',
   motif='CAG',tract_start=9,tract_end=59,counting_rule='tract_length',locate_primers=1)
 check(rv$p$assay$minimum_bp==7,'manual minimum applied before the first primer match is preserved')
})
cat('PROFILE SERVER SUITE PASS\n')
