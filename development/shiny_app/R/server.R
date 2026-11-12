for(server_file in c('server_import','server_reference','server_records','server_downloads','server_localization','server_layout')) {
  source_path<-file.path(rs_app_dir,'R',paste0(server_file,'.R'))
  if(file.exists(source_path))sys.source(source_path,envir=environment())
}
rs_server <- function(input,output,session) {
  rv<-reactiveValues(p=new_project(),step=0L,selected=character(),plate_preview=NULL,raw_peaks=NULL,query=NULL,amplicon=NULL,candidates=NULL,ncbi_message='No query yet.')
  role_options<-c('Biological Sample'='sample','Positive Control'='positive_control',
    'No-Template Control (PCR)'='ntc','CE Water/Buffer Filling'='ce_fill','Other'='other')
  role_labels<-c(setNames(names(role_options),unname(role_options)),unlabelled='Pending',
    unused_empty='Unused',normal_control='Normal-range control',extraction_blank='Extraction blank',other_negative='Other negative control')
  role_tags<-c(sample='Sample',positive_control='Pos Ctrl',ntc='NTC',ce_fill='CE Fill',other='Other',
    unused_empty='Unused',normal_control='Normal',extraction_blank='Blank',other_negative='Other')
  attempt<-function(expr,success=NULL,on_error=NULL)tryCatch({v<-force(expr);if(!is.null(success))showNotification(success,type='message',duration=5);invisible(v)},error=function(e){if(inherits(e,'shiny.silent.error'))return(invisible(NULL));if(!is.null(on_error))on_error(conditionMessage(e));showNotification(conditionMessage(e),type='error',duration=12);invisible(NULL)})
  refresh<-function(p,replacement=FALSE) {
    refresh_localization(session,p)
    if(replacement){rv$plate_preview<-NULL;rv$raw_peaks<-NULL;rv$plate_preview_key<-NULL;rv$raw_peaks_key<-NULL
      rv$plate_committed_key<-isolate(upload_source_key(input,'plate'));rv$peak_committed_key<-isolate(peak_import_key(input));rv$query<-NULL;rv$ncbi_message<-'No query yet.'}
    rv$amplicon<-NULL;rv$candidates<-NULL
    choices<-if(nrow(p$records))setNames(p$records$Record_ID,paste(p$records$Well,p$records$Code,p$records$Acquisition,p$records$Allele,sep=' · ')) else character()
    current<-isolate(input$record_id)%||%''
    selected<-if(current%in%p$records$Record_ID)current else unname(choices[1])
    updateSelectInput(session,'record_id',choices=choices,selected=selected)
    updateSelectizeInput(session,'training_pool',choices=choices,selected=p$hierarchical$pool,server=TRUE)
    updateSliderInput(session,'threshold',value=100*p$threshold);updateNumericInput(session,'threshold_number',value=100*p$threshold)
    updateSelectInput(session,'window',selected=p$window)
    updateSelectInput(session,'variant',choices=c('Automatic'='automatic',setNames(names(p$reviewed),names(p$reviewed))),selected='automatic')
    updateTextInput(session,'gene',value=p$assay$gene);updateTextInput(session,'organism',value=p$assay$organism)
    updateTextInput(session,'assay_name',value=p$assay$name);updateTextInput(session,'assay_source',value=p$assay$source);updateTextInput(session,'assay_scope',value=p$assay$validation_scope)
    updateTextInput(session,'target_channel',value=p$assay$channel);updateTextInput(session,'motif',value=p$assay$motif)
    for(pair in list(c('fixed_bp','fixed_bp'),c('minimum_bp','minimum_bp'),c('nominal_spacing','nominal_spacing'),c('spacing_low','spacing_low'),c('spacing_high','spacing_high'),c('tolerance','tolerance'),c('seed_rfu','seed_min_rfu'),c('seed_fraction','seed_fraction'),c('predictive_level','predictive_level'),c('search_upper','extended_upper'),c('repeat_bp','repeat_bp')))updateNumericInput(session,pair[1],value=p$assay[[pair[2]]])
    updateTextAreaInput(session,'disease_ranges',value=paste(capture.output(write.csv(p$assay$ranges,row.names=FALSE)),collapse='\n'))
    updateCheckboxInput(session,'include_intermediate',value=p$assay$include_intermediate);updateCheckboxInput(session,'assay_confirmed',value=p$assay$status=='configured')
    saved_structure<-p$structure%||%p$reference_draft
    if(!is.null(saved_structure$amplicon)) {
      a<-saved_structure$amplicon;rv$amplicon<-a;rv$candidates<-saved_structure$suggestions
      for(nm in c('forward','reverse','forward_tail','reverse_tail','dye'))updateTextInput(session,nm,value=a[[nm]]%||%'')
      updateNumericInput(session,'tract_start',value=saved_structure$start%||%NA_real_);updateNumericInput(session,'tract_end',value=saved_structure$end%||%NA_real_)
      updateSelectInput(session,'counting_rule',selected=saved_structure$counting_rule%||%'tract_length')
      updateTextInput(session,'counting_basis',value=saved_structure$basis%||%'');updateTextInput(session,'boundary_reason',value=saved_structure$review_reason%||%'')
    } else {
      for(nm in c('forward','reverse','forward_tail','reverse_tail','dye','counting_basis','boundary_reason'))updateTextInput(session,nm,value='')
      updateNumericInput(session,'tract_start',value=NA_real_);updateNumericInput(session,'tract_end',value=NA_real_)
    }
    updateSelectInput(session,'control_status',selected=p$control_review$status);updateSelectInput(session,'control_scope',selected=p$control_review$scope)
    updateTextInput(session,'control_batches',value=paste(p$control_review$batches,collapse=', '));updateTextAreaInput(session,'control_evidence',value=p$control_review$evidence)
  }
  current_variant<-reactive({v<-input$variant%||%'automatic';if(v!='automatic' && !v%in%names(rv$p$reviewed))'automatic' else v})
  filter_pending<-reactive(!is.null(input$minimum_bp) &&
    (!identical(as.numeric(input$minimum_bp),as.numeric(rv$p$assay$minimum_bp)) ||
     !identical(trimws(input$target_channel%||%''),rv$p$assay$channel)))
  localization_pending<-reactive(!is.null(input$loc_name) && !identical(localization_input_config(input),ensure_localization(rv$p)$localization))
  prescreen_pending<-reactive({
    wells<-input$prescreen_wells%||%character();pattern<-input$prescreen_pattern%||%''
    if(!length(wells) || !nzchar(pattern))return(FALSE)
    z<-ensure_localization(rv$p)$prescreen;j<-match(wells,z$Well)
    any(is.na(j)) || any(z$Pattern[j]!=pattern | z$Method[j]!=trimws(input$prescreen_method%||%'') | z$Evidence[j]!=trimws(input$prescreen_source%||%''))
  })
  display_project<-reactive({p<-rv$p;if(filter_pending())p$assay$ui_filter_pending<-TRUE
    if(localization_pending() || prescreen_pending() || isTRUE(input$layout_draft$affects_fit))p$localization_pending<-TRUE;p})
  results<-reactive(threshold_results(display_project(),variant=current_variant()))
  observe({
    req(rv$step==5L)
    session$sendCustomMessage('dashboard-data',dashboard_data(display_project(),results(),current_variant()))
  })
  current_record<-reactive({z<-rv$p$records;if(!nrow(z))return(NULL);i<-match(input$record_id,z$Record_ID);if(!length(i) || is.na(i))i<-1L;z[i,,drop=FALSE]})
  candidate_peaks<-reactive({
    rec<-current_record();p<-rv$p
    if(is.null(rec) || !nrow(p$peaks))return(data.frame())
    peaks<-p$peaks[p$peaks$Record_ID==rec$Source_ID & p$peaks$Size>=p$assay$minimum_bp &
      p$peaks$Size>=rec$ROI_low & p$peaks$Size<=rec$ROI_high,,drop=FALSE]
    if(nzchar(p$assay$channel) && p$assay$channel!='ALL')peaks<-peaks[peaks$Channel==p$assay$channel,,drop=FALSE]
    if(!nrow(peaks))return(data.frame())
    peaks[order(peaks$Size,peaks$Peak_ID),,drop=FALSE]
  })
  output$target_candidate_picker<-renderUI({
    rec<-current_record();req(!is.null(rec))
    peaks<-candidate_peaks()
    if(!nrow(peaks))return(p('No uniquely identifiable candidate peaks for this well and filter.'))
    choices<-setNames(peaks$Peak_ID,paste0(format(peaks$Size,trim=TRUE),' bp · ',peaks$Height,' RFU'))
    selected<-if(identical(rv$manual_candidate$record,rec$Record_ID))rv$manual_candidate$peak else ''
    selectInput('target_candidate_peak','Physical peak for manual target',choices=c('Choose a physical peak'='',choices),selected=selected)
  })
  select_preview_target<-function(record_id,id) {
    rec<-rv$p$records[rv$p$records$Record_ID==record_id,,drop=FALSE]
    if(nrow(rec)!=1)stop('Choose one acquisition / allele record.')
    peaks<-candidate_inventory(rv$p,rec)
    if(!nzchar(id) || !id%in%peaks$Peak_ID)stop('Choose a current candidate peak for this well.')
    p<-rv$p;i<-match(rec$Record_ID,p$records$Record_ID)
    p$records$Modal_rule[i]<-'manual';p$records$Modal_peak_ID[i]<-id
    p$records$Target_confirmed[i]<-FALSE;p$records$Target_evidence[i]<-''
    p$records$Category[i]<-'unresolved';p$records$Evidence[i]<-''
    p$records$Repeat_low[i]<-NA_real_;p$records$Repeat_high[i]<-NA_real_
    p$audit<-c(p$audit,list(list(time=stamp(),action='candidate_target',record=rec$Record_ID,peak=id)))
    rv$p<-p;updateSelectInput(session,'record_id',selected=record_id)
  }
  observeEvent(input$apply_candidate_target,attempt({
    rec<-current_record();req(!is.null(rec));stop('Use the visible target kind and review basis, then Apply manual research decision.')
  },'Candidate selected for preview. Run analysis again; target identity remains unconfirmed.'))
  observeEvent(input$dashboard_target,{
    rv$manual_candidate<-input$dashboard_target
    updateSelectInput(session,'record_id',selected=input$dashboard_target$record)
    updateSelectInput(session,'target_candidate_peak',selected=input$dashboard_target$peak)
    updateTabsetPanel(session,'result_view',selected='targets')
    showNotification('Choose the target kind and record the review basis, then apply and rerun.',duration=8)
  })
  observeEvent(input$dashboard_record,{if(input$dashboard_record%in%rv$p$records$Record_ID)updateSelectInput(session,'record_id',selected=input$dashboard_record)})
  observeEvent(input$load_demo,attempt({rv$p<-localized_synthetic_demo();rv$selected<-character();session$sendCustomMessage('plate-selection',character());refresh(rv$p,replacement=TRUE)},'Synthetic demonstration loaded with explicit synthetic localization inputs. Run the dual-method analysis.'))
  observeEvent(input$apply_preview_filter,attempt({
    minimum<-input$minimum_bp;channel<-trimws(input$target_channel%||%'')
    if(length(minimum)!=1 || !is.finite(minimum) || minimum<0)stop('Enter a non-negative minimum Size.')
    p<-rv$p
    if(!identical(p$assay$minimum_bp,minimum) || !identical(p$assay$channel,channel)) {
      p$assay$minimum_bp<-minimum;p$assay$channel<-channel;p$assay$status<-'unconfigured'
      p$audit<-c(p$audit,list(list(time=stamp(),action='candidate_filter',minimum_bp=minimum,channel=channel)))
      rv$p<-p
    }
  },'Candidate filter applied. Run analysis to update the preview.'))
  observeEvent(input$run_analysis,attempt({
    if(isTRUE(input$layout_draft$pending))stop('Apply or discard the sample information edits in Arrange plate before running.')
    missing<-layout_select(rv$p,'missing')
    if(length(missing)) {
      rv$selected<-missing;rv$step<-2L;session$sendCustomMessage('plate-selection',missing)
      updateTabsetPanel(session,'main_tabs',selected='layout');updateTabsetPanel(session,'layout_view',selected='plate')
      stop(length(missing),' sample wells need Genotype. They are selected in Arrange plate; Unclear is a valid entry.')
    }
    if(localization_pending() || prescreen_pending())stop('Apply the edited localization / prescreen inputs before running analysis.')
    if(!is.null(input$minimum_bp) && (!identical(as.numeric(input$minimum_bp),as.numeric(rv$p$assay$minimum_bp)) ||
      !identical(trimws(input$target_channel%||%''),rv$p$assay$channel)))
      stop('Select Apply candidate filter before running analysis.')
    withProgress(message='Assigning targets and fitting independent records',value=.4,{rv$p<-fit_project(prepare_localized_project(rv$p))});refresh(rv$p)
  },'Analysis fitted. Height changes now use frozen models.'))
  set_threshold_percent<-function(value) {
    if(!is.numeric(value) || length(value)!=1 || !is.finite(value) || value<0 || value>100)return(invisible(FALSE))
    if(!identical(rv$p$threshold,value/100)){p<-rv$p;p$threshold<-value/100;rv$p<-p}
    updateSliderInput(session,'threshold',value=value);updateNumericInput(session,'threshold_number',value=value)
    invisible(TRUE)
  }
  # Bound widgets echo programmatic updates asynchronously. Only explicit user
  # edits from threshold.js may change the shared value; echoes are display-only.
  observeEvent(input$threshold_change,set_threshold_percent(input$threshold_change$value),ignoreInit=TRUE)
  observeEvent(input$reset_threshold,set_threshold_percent(3))
  observeEvent(input$window,{if(input$window%in%c('tail10','extended')){p<-rv$p;p$window<-input$window;rv$p<-p}},ignoreInit=TRUE)
  observeEvent(input$include_intermediate,{p<-rv$p;p$assay$include_intermediate<-isTRUE(input$include_intermediate);rv$p<-p},ignoreInit=TRUE)
  observeEvent(input$plate_selection,{
    rv$selected<-intersect(as.character(input$plate_selection),rv$p$plate$Well)
  },ignoreNULL=FALSE,priority=10)
  observeEvent(input$undo_plate,{rv$p<-undo_plate(rv$p)})
  observeEvent(input$open_group,{
    req(length(rv$selected))
    p<-rv$p;rv$group_wells<-rv$selected
    groups<-unique(c(p$groups,p$plate$Group[nzchar(p$plate$Group)]))
    showModal(modalDialog(title=paste('Group',length(rv$group_wells),'wells'),
      selectInput('active_group','Existing group',c('Ungrouped'='',groups),selected=groups[1]),
      textInput('group_name','Or enter a new group',''),
      easyClose=TRUE,footer=tagList(modalButton('Cancel'),tags$button(id='confirm_group',type='button',class='btn btn-primary','Apply'))))
  })
  observeEvent(input$confirm_group,attempt({
    req(length(rv$group_wells))
    name<-trimws(input$confirm_group$name %||% '')
    if(!nzchar(name))name<-input$confirm_group$group %||% ''
    rv$p<-set_groups(rv$p,rv$group_wells,name)
    rv$group_wells<-NULL;removeModal()
  }))
  observeEvent(input$apply_role,{
    req(length(rv$selected));rv$role_wells<-rv$selected
    showModal(modalDialog(title=paste('Set role for',length(rv$role_wells),'wells'),
      tags$p(class='status-small',paste(rv$role_wells,collapse=', ')),
      selectInput('batch_role','Role',c('Choose role'='',role_options),selected='',selectize=FALSE),
      easyClose=TRUE,footer=tagList(modalButton('Cancel'),actionButton('confirm_role','Apply',class='btn-primary'))))
  })
  observeEvent(input$confirm_role,attempt({
    req(length(rv$role_wells),input$batch_role)
    rv$p<-apply_roles(rv$p,rv$role_wells,input$batch_role,confirm=TRUE)
    rv$role_wells<-NULL;removeModal()
  }))
  output$project_badge<-renderUI(tagList(span(class='status-pill',if(rv$p$synthetic)'SYNTHETIC WORKSPACE' else 'LOCAL WORKSPACE'),p(class='status-small',rv$p$id),p(class='status-small',rv$p$assay$name)))
  output$global_status<-renderUI({p<-display_project();r<-results();
    if(isTRUE(input$layout_draft$affects_fit))return(div(class='notice danger','Sample information has unapplied edits. Apply or discard them in Arrange plate; previous results are hidden.'))
    if(localization_pending() || prescreen_pending())return(div(class='notice danger','Localization / prescreen edits are pending. Apply current inputs, then Run. Previous results are hidden.'))
    if(filter_pending())return(div(class='notice danger','Candidate filter edited — Apply candidate filter, then run analysis. Previous results are hidden.'))
    if(fit_is_stale(p))return(div(class='notice danger','Results need updating: an input, reference or calibration setting changed. The previous fit is retained; run analysis to update.'))
    if(length(layout_select(p,'missing')))return(div(class='notice warning',paste(length(layout_select(p,'missing')),'samples need Genotype. Use Edit sample information; Run will select the missing wells.')))
    if(length(localization_issues(p,FALSE)))return(div(class='notice warning','Analysis settings are incomplete. Open Analysis settings, apply the candidate filter and required assay inputs.'))
    if(is.null(p$fit))return(div(class='notice',if(p$synthetic)'Synthetic demo loaded. Run dual-method analysis to calculate this plate.' else if(nrow(p$peaks))'Input ready. Run dual-method analysis to calculate this plate.' else 'Import a plate and peak table to begin.'))
    div(class='notice',paste0('Research preview · ',sum(r$Method=='adaptive' & r$Role=='sample' & is.finite(r$Preview_EI)),' Adaptive / ',
      sum(r$Method=='bayesian' & r$Role=='sample' & is.finite(r$Preview_EI)),' Bayesian sample results. Formal clearance: ',length(unique(r$Record_ID[is.finite(r$EI)])),
      ' records. Candidate identity, category and QC are separate from calculation.'))
  })
  output$filter_status<-renderText(paste0('Applied: ',if(nzchar(rv$p$assay$channel))paste('channel',rv$p$assay$channel) else 'all channels',
    ' · Size >= ',rv$p$assay$minimum_bp,' bp',if(filter_pending())' · unapplied edits' else ''))
  output$plate_assignment_status<-renderUI({p<-rv$p;tagList(
    p(class='formula-note',paste(sum(nzchar(p$plate$Code)),'labelled wells ·',sum(nzchar(p$plate$Group)),'grouped ·',sum(p$plate$Role=='sample'),'Sample ·',sum(p$plate$Role=='ce_fill'),'CE Fill ·',sum(p$plate$Role=='unlabelled'),'Role pending')),
    p(class='status-small','Group labels organize wells. Set Role separately to identify samples and controls. Group assignment does not set Role.'))})
  output$metrics<-renderUI({r<-results();p<-rv$p;vals<-c(acquisition_counts(p),length(unique(r$Record_ID[is.finite(r$EI)])),length(unique(r$Record_ID[r$Status%in%c('qc_unconfirmed','control_review_required','qc_failed')])),nrow(role_conflicts(p)))
    labels<-c('acquisitions','no-acquisition placeholders','formal EI records','QC / control reviews','NA / peak conflicts')
    div(class='metrics-row',lapply(seq_along(vals),function(i)div(class='metric',strong(vals[i]),span(labels[i]))))})
  output$plate<-renderUI({p<-ensure_sample_layout(rv$p);layout<-sample_layout_rows(p);conflicts<-role_conflicts(p)$Well;groups<-unique(p$plate$Group[nzchar(p$plate$Group)]);palette<-setNames(rep(c('#d4e8ec','#f4dfcb','#dddfef','#d9e7d5','#eadce8','#f2e9c9'),length.out=length(groups)),groups)
    cells<-list(div(class='well-label',''))
    for(c in 1:12)cells<-c(cells,list(div(class='well-label',c)))
    for(row in 1:8) {
      cells<-c(cells,list(div(class='well-label',LETTERS[row])))
      for(col in 1:12) {
        i<-(row-1)*12+col;w<-p$plate[i,];bg<-if(nzchar(w$Group))palette[[w$Group]] else '#f4f8f9'
        tag<-unname(role_tags[w$Role])
        warn<-w$Well%in%conflicts
        genotype<-layout$Genotype[i];short<-c(normal_only='N',normal_expanded='N + E',expanded_present='E + ?',two_expanded='E + E',intermediate='Int',unclear='?')[genotype]
        title<-paste(w$Well,w$Code,w$Full_name,paste0('Group: ',w$Group),paste0('Individual: ',w$Individual_ID),paste0('Role: ',role_labels[[w$Role]]),paste0('Genotype: ',genotype),sep=' | ')
        cells<-c(cells,list(tags$button(type='button',class=paste('well',if(w$Role=='unused_empty')'unused'),
          style=paste0('background:',bg),title=title,'aria-label'=title,'data-well'=w$Well,'data-row'=row,'data-col'=col,
          span(class='well-coordinate',w$Well),if(nzchar(w$Code))span(class='well-sample',w$Code),if(!is.na(tag))span(class='well-role',tag),
          if(w$Role=='sample')span(class=paste('well-genotype',if(!nzchar(genotype))'missing'),if(nzchar(genotype))unname(short) else 'Set genotype'),
          if(warn)span(class='review-mark','!'))))
      }
    }
    div(class='plate-grid',cells)
  })
  output$threshold_note<-renderText(paste0('Height >= ',format(rv$p$threshold,digits=5),' × own modal height (',100*rv$p$threshold,'%). Seeds, fit and search support stay frozen.'))
  output$overview_plot<-renderPlot({r<-results();if(nrow(r))r<-r[r$Role%in%c('sample','positive_control'),,drop=FALSE];overview_plot(r,rv$p$synthetic)},res=110)
  output$all_results<-renderTable({r<-results();if(nrow(r)){
    r$Preview_EI[!r$Role%in%c('sample','positive_control')]<-NA_real_
    r[,c('Code','Well','Acquisition','Allele','Group','Role','Method','Preview_EI','Preview_status','Preview_reason','Formal_status')]
  }},digits=6,na='NA',striped=TRUE)
  output$summary<-renderTable(summarise_results(results()),digits=4,na='Not evaluable',striped=TRUE)
  well_key<-reactive({
    p<-rv$p;rec<-current_record();v<-current_variant()
    run<-if(v=='automatic')p$fit else p$reviewed[[v]]
    list(run$id,v,rec$Record_ID,p$threshold,p$window,analysis_fingerprint(display_project()),rec$Role,rec$Category)
  })
  well_decisions<-reactive({
    rec<-current_record();req(!is.null(rec));peak_decisions(display_project(),variant=current_variant(),record_id=rec$Record_ID)
  }) |> bindCache(well_key(),cache='session')
  output$tail_plot<-renderPlot({rec<-current_record();if(is.null(rec))empty_plot('Choose an acquisition after importing peaks.') else tail_plot(display_project(),rec$Record_ID,current_variant(),well_decisions())},res=110) |>
    bindCache(well_key(),current_record()$Code,rv$p$assay$predictive_level,cache='session')
  output$decisions<-renderTable({rec<-current_record();req(!is.null(rec));d<-well_decisions();if(nrow(d))d[,c('Peak_ID','Size','Height','Method','Delta','Included','Decision')]},digits=4,na='NA')
  output$well_metrics<-renderUI({rec<-current_record();req(!is.null(rec));r<-results();r<-r[r$Record_ID==rec$Record_ID,,drop=FALSE];tagList(tableOutput('well_result_table'),if(nrow(r) && is.finite(rv$p$assay$fixed_bp))p(class='muted',paste0('BP conversion estimate: ',round((r$Modal_bp[1]-rv$p$assay$fixed_bp)/rv$p$assay$repeat_bp,3),' repeat units. This is not sequence-confirmed sample structure.')))})
  output$well_result_table<-renderTable({rec<-current_record();req(!is.null(rec));r<-results();if(nrow(r)){
    z<-r[r$Record_ID==rec$Record_ID,c('Method','Preview_EI','Preview_status','Preview_reason','Formal_status','Modal_bp','Modal_RFU','N_tail','Threshold_RFU','Search_upper_bp')]
    if(!rec$Role%in%c('sample','positive_control'))z$Preview_EI<-NA_real_
    z$Threshold_pct<-100*rv$p$threshold;z
  }},digits=6,na='NA')
  observeEvent(input$apply_historical_preview,attempt({
    p<-historical_sca2_preview(rv$p,file.path(rs_root,'development/v2_4/SCA2_brain_pipeline_outputs_v2_4/SCA2_sample_reference_table_v2_4.csv'))
    rv$p<-p;refresh(p)
  },'Historical target rules applied for preview. Run the analysis.'))
  output$report_overview<-renderUI({
    p<-rv$p;r<-results();req(nrow(r))
    counts<-acquisition_counts(p)
    tagList(
      p(class='formula-note',paste0(100*p$threshold,'% own modal height · ',p$window)),
      p(paste(counts[['actual']],'acquisitions ·',counts[['placeholder']],'no-acquisition placeholders')),
      p(paste(sum(r$Method=='adaptive' & r$Role=='sample' & is.finite(r$Preview_EI)),'adaptive and',
        sum(r$Method=='bayesian' & r$Role=='sample' & is.finite(r$Preview_EI)),'Bayesian sample previews')),
      p(class='status-small',paste('Reference',p$reference$accession %||% 'pending','·',if(fit_is_stale(p))'Fit needs updating' else 'See per-well review status')))
  })
  output$target_preview<-renderUI({
    rec<-current_record();req(nrow(rec))
    target<-target_description(rv$p,rec,if(fit_is_stale(display_project()))NULL else rv$p$fit$records[[rec$Record_ID]])
    tagList(p(class='status-small',target$text),
      if(nzchar(rec$Target_evidence))tags$details(tags$summary('Target source'),p(rec$Target_evidence)))
  })
  output$formal_current_record<-renderText({rec<-current_record();if(is.null(rec))'Choose a record in Review a target.' else paste('Current record:',rec$Well,rec$Code,rec$Acquisition,rec$Allele,sep=' · ')})
  register_import_server(input,output,session,rv,attempt,refresh)
  register_workflow_server(input,output,session,rv)
  register_reference_server(input,output,session,rv,attempt,refresh)
  register_record_server(input,output,session,rv,attempt,refresh,current_record,current_variant,results)
  register_localization_server(input,output,session,rv,attempt,refresh,current_record,localization_pending,display_project)
  register_layout_server(input,output,session,rv,attempt)
  register_download_server(input,output,session,rv,attempt,results,current_variant,display_project)
}
