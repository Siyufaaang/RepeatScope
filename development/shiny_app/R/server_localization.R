localization_input_config <- function(input) {
  cfg<-default_localization()
  for(n in names(cfg)) {
    value<-input[[paste0('loc_',n)]]
    cfg[[n]]<-if(is.numeric(cfg[[n]]))as.numeric(value%||%NA_real_) else if(is.logical(cfg[[n]]))isTRUE(value) else trimws(value%||%'')
  }
  cfg
}
refresh_localization <- function(session,p) {
  p<-ensure_localization(p)
  for(n in names(p$localization)) {
    x<-p$localization[[n]];id<-paste0('loc_',n)
    if(is.numeric(x))updateNumericInput(session,id,value=x)
    else if(is.logical(x))updateCheckboxInput(session,id,value=x)
    else if(n=='mode')updateSelectInput(session,id,selected=x)
    else updateTextInput(session,id,value=x)
  }
  wells<-p$plate[p$plate$Role=='sample',,drop=FALSE]
  updateSelectInput(session,'prescreen_wells',choices=setNames(wells$Well,paste(wells$Well,wells$Code)),selected=character())
}
register_localization_server <- function(input,output,session,rv,attempt,refresh,current_record,localization_pending,display_project) {
  sync_profile_inputs<-function(p) {
    refresh_localization(session,p)
    # The browser owns the gene being typed. Echoing an earlier edit here can
    # overwrite a newer one and start an endless input/profile feedback loop.
    # Canonical identity is stored in the project; refresh() restores saved input.
    updateTextInput(session,'organism',value=p$assay$organism)
    updateTextInput(session,'motif',value=p$assay$motif)
    updateNumericInput(session,'minimum_bp',value=p$assay$minimum_bp)
    updateTextInput(session,'target_channel',value=p$assay$channel)
  }
  observeEvent(list(input$gene,rv$step),{
    if(rv$step!=4L || is.null(input$gene))return()
    gene<-trimws(input$gene);d<-find_disease_profile(gene)
    canonical<-if(is.null(d))gene else d$gene
    p<-rv$p
    if(profile_identity(canonical)!=profile_identity(p$assay$gene))p<-apply_disease_profile(p,canonical)
    p<-profile_setup(p)
    if(!identical(p,rv$p)){rv$p<-p;sync_profile_inputs(p)}
  },priority=5)
  observeEvent(input$reset_gene_profile,attempt({
    if(is.null(find_disease_profile(input$gene)))stop('No built-in profile for this gene; use custom ranges.')
    p<-apply_disease_profile(rv$p,input$gene);rv$p<-p;sync_profile_inputs(p)
  },'Gene defaults loaded.'))
  output$disease_profile_summary<-renderUI({
    p<-rv$p;d<-p$disease_profile
    if(is.null(d))return(p(class='status-small',if(!nzchar(p$assay$gene))'Enter a gene symbol or disease alias, e.g. ATXN2 / SCA2 or HTT.' else 'No built-in profile: enter custom ranges in Advanced.'))
    bands<-vapply(d$bands,function(b)paste0(b$label,': ',b$min,if(is.null(b$max))'+' else if(b$min==b$max)'' else paste0('–',b$max)),character(1))
    tagList(p(strong(paste(d$id,'·',d$gene)),if(isTRUE(p$profile_custom))' · custom overrides' else ' · built-in profile'),
      p(class='status-small',paste(bands,collapse=' · ')),
      tags$details(tags$summary('Profile source and counting notes'),
        p(d$counting),p(d$notes),p(paste('Version',d$version,'· reviewed',d$source$reviewed)),
        tags$a(href=d$source$url,target='_blank',rel='noopener',d$source$title)))
  })
  observeEvent(input$loc_sca2_source,{
    updateNumericInput(session,'loc_normal_max',value=31);updateNumericInput(session,'loc_expanded_min',value=33)
    updateCheckboxInput(session,'loc_normal_inclusive',value=TRUE);updateCheckboxInput(session,'loc_expanded_inclusive',value=TRUE)
    updateTextInput(session,'loc_source',value='GeneReviews SCA2 NBK1275, accessed 2026-11-10; <=31 normal, 32 uncertain, >=33 SCA2-causing; 33-34 reduced penetrance. https://www.ncbi.nlm.nih.gov/books/NBK1275/')
  })
  observeEvent(input$save_localization,attempt({
    p<-ensure_localization(rv$p);p$localization<-localization_input_config(input)
    if(!is.null(input$minimum_bp)){
      if(!is.finite(input$minimum_bp) || input$minimum_bp<0)stop('Enter a non-negative candidate size.')
      if(p$assay$minimum_bp!=input$minimum_bp)p$analysis_defaults$minimum_manual<-TRUE
      p$assay$minimum_bp<-input$minimum_bp
    }
    if(!is.null(input$target_channel))p$assay$channel<-trimws(input$target_channel)
    if(!is.null(p$disease_profile)) {
      d<-p$disease_profile;x<-p$localization
      p$profile_custom<-!identical(as.numeric(x$normal_max),as.numeric(d$normal_max)) || !identical(as.numeric(x$expanded_min),as.numeric(d$expanded_min)) || x$counting!=d$counting || x$unit!=d$unit || x$normal_inclusive!=d$normal_inclusive || x$expanded_inclusive!=d$expanded_inclusive
    }
    issues<-localization_issues(p,include_prescreen=FALSE);if(length(issues))stop(paste(issues,collapse='\n'))
    x<-p$localization;p$assay$gene<-x$gene
    p$assay$ranges<-localization_ranges(x)
    updateTextAreaInput(session,'disease_ranges',value=paste(capture.output(write.csv(p$assay$ranges,row.names=FALSE)),collapse='\n'))
    p$audit<-c(p$audit,list(list(time=stamp(),action='localization_config',config=p$localization)))
    rv$p<-p
  },'Analysis setup applied.'))
  observeEvent(input$prescreen_select_samples,updateSelectInput(session,'prescreen_wells',selected=rv$p$plate$Well[rv$p$plate$Role=='sample']))
  observeEvent(input$apply_prescreen,attempt({
    rv$p<-set_prescreen(rv$p,input$prescreen_wells,input$prescreen_pattern,input$prescreen_method,input$prescreen_source)
    updateSelectInput(session,'prescreen_wells',selected=character())
  },'Prescreen applied only to the selected wells. Re-run target assignment.'))
  output$prescreen_table<-renderTable({
    p<-ensure_localization(rv$p);z<-p$plate[p$plate$Role=='sample',c('Well','Code','Group'),drop=FALSE]
    j<-match(z$Well,p$prescreen$Well);z$Prescreen<-p$prescreen$Pattern[j];z$Method<-p$prescreen$Method[j];z$Source<-p$prescreen$Evidence[j];z
  },na='Required',striped=TRUE)
  output$localization_status<-renderUI({
    p<-ensure_localization(rv$p);issues<-localization_issues(p)
    issues<-c(issues,candidate_filter_issue(p))
    s<-project_structure_view(p)
    tagList(if(localization_pending())p(class='status-small','Setup has unapplied edits.'),
      if(length(issues))div(class='notice',tags$ul(lapply(issues,tags$li))) else
        p(class='status-small',paste('Ready · channel',p$assay$channel,'·',if(!is.null(s))paste('reference fixed length',s$fixed_bp,'bp') else 'linear CE mapping','· intermediate EI included')),
      if(!is.finite(p$localization$uncertainty))p(class='status-small','CE precision unspecified; range labels are provisional.'))
  })
  output$localization_table<-renderTable({
    p<-ensure_localization(display_project());z<-p$records[p$records$Role=='sample',,drop=FALSE]
    if(!nrow(z))return(NULL)
    sizes<-p$peaks$Size[match(z$Modal_peak_ID,p$peaks$Peak_ID)]
    if(!is.null(p$fit))for(i in seq_len(nrow(z))){f<-p$fit$records[[z$Record_ID[i]]];if(identical(f$status,'target_overlap')){z$Localization_status[i]<-f$status;z$Localization_reason[i]<-f$reason}}
    if(fit_is_stale(p) || isTRUE(p$localization_pending)){sizes[]<-NA_real_;z$Localization_status<-'rerun_required';z$Localization_reason<-'Inputs changed; previous target allocation is not current.'}
    data.frame(Well=z$Well,Acquisition=z$Acquisition,Target=z$Allele,Kind=z$Target_kind,Modal_bp=sizes,Status=z$Localization_status,Reason=z$Localization_reason)
  },digits=2,na='Pending',striped=TRUE)
  output$localization_candidates<-renderTable({
    rec<-current_record();req(!is.null(rec));p<-ensure_localization(rv$p)
    if(length(localization_issues(p,FALSE)))return(NULL)
    z<-localization_candidates(p,rec);if(nrow(z))z[,c('Size','Height','Repeat_estimate','Range','Candidate_origin')]
  },digits=3,striped=TRUE)
  observeEvent(input$apply_localization_override,attempt({
    rec<-current_record();req(!is.null(rec))
    if(localization_pending())stop('Apply localization inputs before reviewing a target.')
    rv$p<-set_localization_override(rv$p,rec$Record_ID,input$target_candidate_peak%||%'',input$manual_target_kind,input$manual_target_basis)
  },'Manual research decision recorded. Re-run analysis. This does not clear formal identity or QC.'))
  observeEvent(input$clear_localization_override,attempt({
    rec<-current_record();req(!is.null(rec));p<-ensure_localization(rv$p)
    p$target_overrides<-p$target_overrides[p$target_overrides$Record_ID!=rec$Record_ID,,drop=FALSE];rv$p<-p
  },'Manual decision removed. Re-run to use prescreen and size-based assignment.'))
}
