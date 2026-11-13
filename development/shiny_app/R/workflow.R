workflow_steps<-c('Import plate','Arrange plate','Import peaks','Reference & analysis','Results')
upload_source_key<-function(input,kind) {
  f<-input[[paste0(kind,'_file')]];if(is.null(f))return(NULL)
  object_hash(list(file=f,range=input[[paste0(kind,'_range')]],sheet=input[[paste0(kind,'_sheet')]],transpose=if(kind=='plate')isTRUE(input$transpose_plate)))
}
peak_import_key<-function(input) {
  source<-upload_source_key(input,'peak');if(is.null(source))return(NULL)
  object_hash(list(source=source,mapping=lapply(names(suggest_mapping(character())),function(n)input[[paste0('map_',n)]]%||%''),well_from_filename=isTRUE(input$well_from_filename)))
}
workflow_ready<-function(p,step) {
  answer<-function(ready,reason)list(ready=isTRUE(ready),reason=if(isTRUE(ready))'' else reason)
  a<-p$structure$amplicon%||%p$reference_draft$amplicon
  switch(as.character(step),
    '1'=answer(isTRUE(p$workflow$plate_imported),'Upload, preview and use a plate map, or restore a project.'),
    '2'=answer(TRUE,''),
    '3'=answer(nrow(p$peaks)>0,'Upload a peak table, map Well / Size / Height and import it.'),
    '4'=answer(!is.null(p$reference) && !is.null(a) && a$status=='located' && !is.null(project_structure_view(p)),
      'Match both primers and select a valid repeat tract to continue.'),
    answer(TRUE,''))
}
primer_key<-function(sequence,forward,reverse,ft='',rt='',motif='CAG')object_hash(lapply(list(sequence,forward,reverse,ft,rt,motif),clean_dna))
current_primer_key<-function(p,input)primer_key(p$reference$sequence%||%'',input$forward%||%'',input$reverse%||%'',input$forward_tail%||%'',input$reverse_tail%||%'',input$motif%||%'')
saved_primer_key<-function(p) {
  if(isTRUE(p$reference_draft$matching_pending) && is.null(p$structure))return('')
  a<-p$structure$amplicon%||%p$reference_draft$amplicon;if(is.null(a))return('')
  primer_key(a$sequence,a$forward,a$reverse,a$forward_tail,a$reverse_tail,p$assay$motif)
}
register_workflow_server<-function(input,output,session,rv) {
  go<-function(step){rv$step<-step;updateTabsetPanel(session,'main_tabs',selected=c('welcome','plate_import','layout','peaks','reference','results')[step+1L])}
  observeEvent(input$try_workflow,go(1L))
  observeEvent(input$home_section,{
    target<-input$home_section$section
    if(length(target)!=1L || !target%in%c('landing-top','how-it-works','methodology','demo-data'))return()
    go(0L)
    session$onFlushed(function()session$sendCustomMessage('landing-scroll',target),once=TRUE)
  })
  readiness<-reactive({
    p<-rv$p
    if(rv$step==1 && !identical(upload_source_key(input,'plate'),rv$plate_committed_key))return(list(ready=FALSE,reason='Use the current plate map to continue.'))
    if(rv$step==3 && !identical(peak_import_key(input),rv$peak_committed_key))return(list(ready=FALSE,reason='Import the current sizing table to continue.'))
    if(rv$step==4L && !identical(current_primer_key(p,input),saved_primer_key(p)))return(list(ready=FALSE,reason=
      if(is.null(p$reference) || !all(nzchar(c(input$forward%||%'',input$reverse%||%'',input$motif%||%''))))'Select a reference and enter both primers and the reported motif.' else 'Primer inputs have changed. Matching the current inputs before continuing.'))
    if(rv$step>=2)for(s in seq_len(rv$step-1L)){g<-workflow_ready(p,s);if(!g$ready)return(list(ready=FALSE,reason=paste('Revisit',workflow_steps[s],':',g$reason)))}
    workflow_ready(p,rv$step)
  })
  observeEvent(input$workflow_next,{if(readiness()$ready && rv$step<length(workflow_steps))go(rv$step+1L)})
  observeEvent(input$workflow_back,{if(rv$step>0)go(rv$step-1L)})
  observeEvent(input$edit_layout,go(2L))
  observeEvent(input$edit_analysis,go(4L))
  observeEvent(input$open_report,{go(5L);updateTabsetPanel(session,'result_view',selected='report')})
  observeEvent(input$show_report,{go(5L);updateTabsetPanel(session,'result_view',selected='report')})
  observeEvent(input$show_targets,updateTabsetPanel(session,'result_view',selected='targets'))
  observeEvent(input$show_tables,updateTabsetPanel(session,'result_view',selected='tables'))
  observeEvent(input$show_formal,updateTabsetPanel(session,'result_view',selected='formal'))
  observeEvent(input$load_demo,{p<-rv$p;p$workflow<-list(plate_imported=TRUE);rv$p<-p;go(2L)},priority=-10)
  output$workflow_progress<-renderUI({req(rv$step>0);div(class='workflow-progress',
    div(class='progress-caption',paste('Step',rv$step,'of',length(workflow_steps)),strong(workflow_steps[rv$step])),
    tags$progress(value=rv$step,max=length(workflow_steps),'aria-label'=paste('Workflow step',rv$step,'of',length(workflow_steps))),
    tags$ol(lapply(seq_along(workflow_steps),function(i)tags$li(class=if(i==rv$step)'current' else if(i<rv$step)'complete' else '',paste(i,workflow_steps[i])))))})
  output$workflow_reason<-renderText(readiness()$reason)
  observe(session$sendCustomMessage('workflow-next-enabled',readiness()$ready))
}
