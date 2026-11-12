register_record_server <- function(input,output,session,rv,attempt,refresh,current_record,current_variant,results) {
  output$record_editor<-renderUI({rec<-current_record();if(is.null(rec))return(p('Import a mapped sample/acquisition first.'))
    category_choices<-unique(c('unresolved','normal','intermediate','expanded',rv$p$assay$ranges$Category))
    tagList(textInput('rec_code','Original / short code',rec$Code),textInput('rec_name','Full display name',rec$Full_name),
      div(class='two-column',textInput('rec_material','Biological material ID',rec$Material_ID),textInput('rec_individual','Individual ID (blank = unknown)',rec$Individual_ID)),
      selectInput('rec_role','Well role',choices=roles,selected=rec$Role),textInput('rec_batch','Batch ID',rec$Batch),
      selectInput('rec_clinical','Clinical status',c('Unknown','Not applicable','Asymptomatic / clinically unaffected','Symptomatic','Other documented'),selected=rec$Clinical),
      selectInput('rec_carrier','Carrier / genetic status',c('Unknown','Not applicable','Carrier','Non-carrier','Genetically affected','Other documented'),selected=rec$Carrier),
      selectInput('rec_zygosity','Zygosity (independent of observed peaks)',c('Unknown','Not applicable','Heterozygous','Homozygous','Hemizygous','Other documented'),selected=rec$Zygosity),
      textInput('rec_observation','This run’s observed profile',rec$Observation),
      selectInput('rec_category','Evidenced target category',category_choices,selected=rec$Category),
      div(class='two-column',numericInput('rec_low','Evidenced repeat lower bound (optional)',rec$Repeat_low),numericInput('rec_high','Evidenced repeat upper bound (optional)',rec$Repeat_high)),
      textAreaInput('rec_evidence','Category / genotype evidence and source',rec$Evidence,rows=2),
      selectInput('rec_modal_rule','Automatic target selection rule',c('Strongest resolved modal'='strongest','Larger of two strongest resolved clusters'='larger_of_two','Explicit physical peak'='manual'),selected=rec$Modal_rule),
      textInput('rec_modal_id','Explicit modal Peak_ID (manual rule)',rec$Modal_peak_ID),
      div(class='two-column',numericInput('rec_roi_low','Target candidate region lower bp',if(is.finite(rec$ROI_low))rec$ROI_low else NA),numericInput('rec_roi_high','Target candidate region upper bp',if(is.finite(rec$ROI_high))rec$ROI_high else NA)),
      checkboxInput('rec_target','Target allele and allocation reviewed',rec$Target_confirmed),textInput('rec_target_evidence','Target / peak-allocation review basis',rec$Target_evidence),
      textInput('rec_control_expected','Control expected result (where applicable)',rv$p$plate$Control_expected[match(rec$Well,rv$p$plate$Well)]),
      textInput('rec_control_evidence','Control definition / source',rv$p$plate$Control_evidence[match(rec$Well,rv$p$plate$Well)]))
  })
  observeEvent(input$save_record,attempt({rec<-current_record();req(rec);p<-rv$p;i<-match(rec$Record_ID,p$records$Record_ID);wi<-match(rec$Well,p$plate$Well)
    if(!input$rec_role%in%roles)stop('Invalid role.')
    # A single-well editor is an explicit role edit; batch edits use the separate preview modal.
    p$undo<-c(p$undo,list(p$plate));p$plate$Code[wi]<-input$rec_code;p$plate$Full_name[wi]<-input$rec_name;p$plate$Role[wi]<-input$rec_role;p$plate$Batch[wi]<-input$rec_batch
    p$plate$Control_expected[wi]<-input$rec_control_expected;p$plate$Control_evidence[wi]<-input$rec_control_evidence
    fields<-c(Material_ID='rec_material',Individual_ID='rec_individual',Clinical='rec_clinical',Carrier='rec_carrier',Zygosity='rec_zygosity',Observation='rec_observation',Category='rec_category',Evidence='rec_evidence',Modal_rule='rec_modal_rule',Modal_peak_ID='rec_modal_id',Target_evidence='rec_target_evidence')
    for(nm in names(fields))p$records[[nm]][i]<-input[[fields[[nm]]]]
    p$records$Repeat_low[i]<-input$rec_low;p$records$Repeat_high[i]<-input$rec_high;p$records$Target_confirmed[i]<-isTRUE(input$rec_target)
    p$records$ROI_low[i]<-if(is.finite(input$rec_roi_low))input$rec_roi_low else -Inf;p$records$ROI_high[i]<-if(is.finite(input$rec_roi_high))input$rec_roi_high else Inf
    if(p$records$ROI_low[i]>p$records$ROI_high[i])stop('Candidate region lower bound exceeds upper bound.')
    p<-sync_records(p);p$audit<-c(p$audit,list(list(time=stamp(),action='record_review',record=rec$Record_ID,metadata=p$records[i,])));rv$p<-p
  },'Record review saved. Formal eligibility is evaluated separately from fitted values.'))
  output$qc_editor<-renderUI({rec<-current_record();if(is.null(rec))return(p('Select a record to enter upstream QC.'))
    pk<-rv$p$peaks[rv$p$peaks$Record_ID==rec$Source_ID,,drop=FALSE]
    tagList(p(class='muted',paste('Imported QC:',paste(unique(pk$QC[nzchar(pk$QC)]),collapse=', '),'· Saturation:',paste(unique(pk$Saturation[nzchar(pk$Saturation)]),collapse=', '))),
      selectInput('rec_qc','Upstream sizing / saturation review',c('Unknown / not checked'='unknown','Checked, passed'='passed','Quantification-affecting failure'='failed'),selected=rec$QC_status),
      textAreaInput('rec_qc_evidence','Upstream review record / source',rec$QC_evidence,rows=3))})
  observeEvent(input$save_qc,attempt({rec<-current_record();req(rec);p<-rv$p;i<-match(rec$Record_ID,p$records$Record_ID)
    if(input$rec_qc=='passed' && !nzchar(trimws(input$rec_qc_evidence)))stop('Record the upstream QC checks/source before marking passed.')
    # Acquisition QC applies to every target allele in that acquisition.
    ix<-p$records$Source_ID==rec$Source_ID;p$records$QC_status[ix]<-input$rec_qc;p$records$QC_evidence[ix]<-input$rec_qc_evidence
    p$audit<-c(p$audit,list(list(time=stamp(),action='upstream_qc_review',source=rec$Source_ID,status=input$rec_qc,evidence=input$rec_qc_evidence)));rv$p<-p
  },'Upstream QC record saved; imported failures remain blocking.'))
  observeEvent(input$save_control,attempt({if(input$control_status=='passed' && !nzchar(trimws(input$control_evidence)))stop('Control criteria and review evidence are required for a pass.')
    p<-rv$p;p$control_review<-list(status=input$control_status,scope=input$control_scope,batches=trimws(strsplit(input$control_batches,',',fixed=TRUE)[[1]]),evidence=input$control_evidence)
    p$control_review$batches<-p$control_review$batches[nzchar(p$control_review$batches)]
    p$audit<-c(p$audit,list(list(time=stamp(),action='control_review',review=p$control_review)));rv$p<-p
  },'Control review recorded. No peak subtraction or deletion was performed.'))
  observeEvent(input$add_allele,attempt({rec<-current_record();req(rec);label<-trimws(input$new_allele)
    if(!nzchar(label))stop('Enter an allele label.')
    p<-rv$p;new_id<-paste0(rec$Source_ID,'::',label);if(new_id%in%p$records$Record_ID)stop('That target label already exists in the acquisition.')
    rec$Record_ID<-new_id;rec$Allele<-label;rec$Category<-'unresolved';rec$Target_confirmed<-FALSE;rec$Target_evidence<-'';rec$Evidence<-''
    if(!is.null(p$localization)) {
      rec$Auto_target<-FALSE;rec$Manual_identity<-TRUE;rec$Target_kind<-'unassigned';rec$Modal_rule<-'blocked';rec$Modal_peak_ID<-''
      rec$Localization_status<-'manual_required';rec$Localization_reason<-'Separate target requires a physical peak, interpretation and review basis.';rec$Localization_key<-''
    }
    p$records<-rbind(p$records,rec);rv$p<-p;refresh(p);updateSelectInput(session,'record_id',selected=new_id)
  },'Separate target added. Bound each allele’s candidate region and review allocation before formal EI.'))
  output$review_peak_ui<-renderUI({rec<-current_record();req(rec);p<-rv$p$peaks[rv$p$peaks$Record_ID==rec$Source_ID,,drop=FALSE]
    selectInput('review_modal','Reviewed modal physical peak',choices=c('Select explicitly'='',setNames(p$Peak_ID,paste(p$Size,'bp ·',p$Height,'RFU ·',p$Peak_ID))))})
  observeEvent(input$create_review,attempt({rec<-current_record();req(rec)
    excluded<-trimws(strsplit(input$review_exclusions,',',fixed=TRUE)[[1]]);excluded<-excluded[nzchar(excluded)]
    rv$p<-create_reviewed(rv$p,rec$Record_ID,input$review_modal,excluded,input$review_reason,input$review_evidence)
    id<-tail(names(rv$p$reviewed),1);updateSelectInput(session,'variant',choices=c('Automatic'='automatic',setNames(names(rv$p$reviewed),names(rv$p$reviewed))),selected=id)
  },'Reviewed version created; automatic results and original input remain available.'))
  observeEvent(input$hier_enabled,{p<-rv$p;p$hierarchical$enabled<-isTRUE(input$hier_enabled);if(!p$hierarchical$enabled)p$hierarchical$result<-NULL;rv$p<-p},ignoreInit=TRUE)
  observeEvent(input$training_pool,{p<-rv$p;p$hierarchical$pool<-input$training_pool%||%character();rv$p<-p},ignoreInit=TRUE)
  observeEvent(input$run_hier,attempt({if(!isTRUE(input$hier_enabled))stop('Explicitly enable Experimental first.')
    withProgress(message='Conditional LOWO with same-material exclusion',value=.4,{rv$p<-fit_hierarchical(rv$p,input$training_pool)})
  },'Experimental results calculated. They remain separate from formal biological summaries.'))
  output$hier_info<-renderText({rec<-current_record();if(is.null(rec) || is.null(rv$p$hierarchical$result))return('Experimental model is off / has not been run. Choose the target in Well detail; explicitly select a training pool.')
    h<-rv$p$hierarchical$result$records[[rec$Record_ID]]
    paste('Target:',rec$Code,rec$Well,'\nStatus:',h$status,h$reason,'\n',h$label,'\nPeers:',paste(h$peer_records,collapse=', '),
      '\nExcluded target / material records:',paste(h$excluded_records,collapse=', '),'\nRedundant peer material acquisitions:',paste(h$redundant_peer_records,collapse=', '),
      '\nTraining support Δ:',paste(h$support,collapse='–'),'\nTraining anchors: algorithm consensus, not biological truth.')})
  output$hier_comparison<-renderTable({rec<-current_record();req(rec);hierarchical_comparison(rv$p,rec$Record_ID)},digits=4)
  output$hier_metrics<-renderTable({r<-results();if(nrow(r))r[r$Method=='hierarchical',c('Code','Well','Method','Preview_EI','EI_difference_vs_Bayesian','Status','Reason')]},digits=6,na='NA')
}
