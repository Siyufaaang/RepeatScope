layout_payload<-function(p) {
  rows<-sample_layout_rows(p)
  list(key=object_hash(rows),rows=lapply(seq_len(nrow(rows)),function(i)as.list(rows[i,,drop=FALSE])))
}
register_layout_server<-function(input,output,session,rv,attempt) {
  selected_wells<-function(wells){rv$selected<-intersect(wells,rv$p$plate$Well);session$sendCustomMessage('plate-selection',rv$selected)}
  reject<-function(message){rv$layout_error<-message;session$sendCustomMessage('layout-rejected',message)}
  applied<-function(p,scope){rv$p<-p;rv$layout_error<-NULL;session$sendCustomMessage('layout-applied',c(layout_payload(p),list(scope=scope)))}
  observe({
    p<-rv$p;z<-sample_layout_rows(p)
    session$sendCustomMessage('layout-data',layout_payload(p))
    groups<-unique(c(p$groups,z$Group[nzchar(z$Group)]));individuals<-sort(unique(z$Individual_ID[nzchar(z$Individual_ID)]))
    updateSelectInput(session,'select_group_value',choices=c('Choose group'='',groups),selected=isolate(input$select_group_value)%||%'')
    updateSelectInput(session,'select_individual_value',choices=c('Choose individual'='',individuals),selected=isolate(input$select_individual_value)%||%'')
  })
  observeEvent(input$layout_select,{
    req(!is.null(input$layout_select$mode));mode<-input$layout_select$mode
    wells<-switch(mode,all=rv$p$plate$Well,clear=character(),layout_select(rv$p,mode,input$layout_select$value%||%''))
    selected_wells(wells)
  })
  observeEvent(input$layout_apply,attempt({
    rv$layout_error<-NULL
    if(isTRUE(input$layout_draft$table_pending))stop('Apply or discard the staged table edits before applying the selected-well editor. Its fields will be kept.')
    if(!length(rv$selected))stop('Select wells first.')
    fields<-input$layout_apply$fields
    if(!is.list(fields) || !length(fields))stop('Tick the fields you want to change. Unticked fields stay as they are.')
    if(any(!names(fields)%in%c('Group','Individual_ID','Role','Genotype')))stop('Unsupported sample field.')
    patch<-lapply(rv$selected,function(w)c(list(Well=w),fields))
    applied(apply_layout_patch(rv$p,patch),'bulk')
  },on_error=reject))
  observeEvent(input$layout_table_apply,attempt({
    rv$layout_error<-NULL;x<-input$layout_table_apply
    if(!is.character(x$key)||length(x$key)!=1||!is.list(x$patch))stop('Reload the table and stage valid edits.')
    applied(apply_layout_patch(rv$p,x$patch,expected_key=x$key),'table')
  },on_error=reject))
  observeEvent(input$layout_discard,{
    rv$layout_error<-NULL;session$sendCustomMessage('layout-reset',layout_payload(rv$p))
  })
  output$layout_feedback<-renderUI({if(!is.null(rv$layout_error))div(class='notice danger',role='alert',rv$layout_error)})
  output$layout_selection_summary<-renderText({
    z<-sample_layout_rows(rv$p);z<-z[z$Well%in%rv$selected,,drop=FALSE]
    if(!nrow(z))return('Select wells on the plate, or use Group / Individual above.')
    describe<-function(values){u<-unique(values);if(length(u)>1)'mixed' else if(!nzchar(u))'not set' else u}
    paste0(nrow(z),' wells selected · Group: ',describe(z$Group),' · Individual: ',describe(z$Individual_ID),' · Genotype: ',describe(z$Genotype))
  })
  output$plate_assignment_status<-renderUI({
    z<-sample_layout_rows(rv$p);samples<-z$Role=='sample';done<-sum(samples & nzchar(z$Genotype));total<-sum(samples)
    div(class='layout-completeness',strong(paste(total,'Samples ·',sum(z$Role=='ce_fill'),'CE Fill')),
      span(paste0('Genotype ',done,'/',total,' complete · ',sum(z$Role=='unlabelled'),' roles pending')),
      p(class='status-small','Group and Individual select wells. Genotype is always your explicit entry; Individual ID is optional.'))
  })
  output$report_sample_status<-renderText({z<-sample_layout_rows(rv$p);s<-z$Role=='sample';paste0(sum(s & nzchar(z$Genotype)),'/',sum(s),' sample genotypes · ',sum(z$Genotype=='unclear' & s),' unclear')})
}
