register_import_server <- function(input,output,session,rv,attempt,refresh) {
  attempt_peak_import<-function(expr,success=NULL) {
    rv$peak_import_error<-NULL
    attempt(expr,success,on_error=function(message)rv$peak_import_error<-message)
  }
  remember_upload<-function(p,file) {p$originals<-c(p$originals,list(list(name=file$name,bytes=readBin(file$datapath,'raw',n=file$size))));p}
  output$plate_sheet_ui<-renderUI({req(input$plate_file);selectInput('plate_sheet','Sheet',choices=readxl::excel_sheets(input$plate_file$datapath))})
  output$peak_sheet_ui<-renderUI({req(input$peak_file);if(tolower(tools::file_ext(input$peak_file$name))=='xlsx')selectInput('peak_sheet','Sheet',choices=readxl::excel_sheets(input$peak_file$datapath))})
  output$plate_preview_status<-renderUI({
    current<-!is.null(rv$plate_preview) && identical(rv$plate_preview_key,upload_source_key(input,'plate'))
    if(current)div(class='import-preview-status',strong('96 wells'),span(paste(sum(nzchar(rv$plate_preview$Code)),'labelled positions')))
    else div(class='import-empty',strong('Your plate will appear here'),
      p(if(is.null(input$plate_file))'Choose an Excel file, check the sheet and range, then select Preview plate.' else 'Select Preview plate to review the current file and settings.'))
  })
  output$peak_preview_status<-renderUI({
    current<-!is.null(rv$raw_peaks) && identical(rv$raw_peaks_key,upload_source_key(input,'peak'))
    if(current)div(class='import-preview-status',strong(paste(nrow(rv$raw_peaks),'source rows')),span('Preview · first 8 rows'))
    else div(class='import-empty',strong('Your sizing table will appear here'),
      p('Choose a file to preview its columns. Existing imported data is retained until a new import succeeds.'))
  })
  observeEvent(input$preview_plate,attempt({req(input$plate_file)
    rv$plate_preview<-NULL;rv$plate_preview_key<-NULL
    x<-read_source_table(input$plate_file$datapath,tools::file_ext(input$plate_file$name),sheet=input$plate_sheet,range=input$plate_range,header=FALSE)
    if(isTRUE(input$transpose_plate))x<-t(as.matrix(x))
    rv$plate_preview<-plate_from_matrix(x);rv$plate_preview_key<-upload_source_key(input,'plate')
  }))
  output$plate_preview<-renderTable({req(rv$plate_preview,identical(rv$plate_preview_key,upload_source_key(input,'plate')));x<-data.frame(Row=LETTERS[1:8],matrix(rv$plate_preview$Code,8,12,byrow=TRUE),check.names=FALSE);names(x)<-c('Row',1:12);x},rownames=FALSE)
  observeEvent(input$commit_plate,attempt({req(rv$plate_preview)
    if(!identical(rv$plate_preview_key,upload_source_key(input,'plate')))stop('The plate file or settings changed. Preview the current selection first.')
    p<-rv$p;p$undo<-c(p$undo,list(p$plate));p$plate<-rv$plate_preview
    p<-sync_records(p);p$workflow<-list(plate_imported=TRUE);p<-remember_upload(p,input$plate_file);p$audit<-c(p$audit,list(list(time=stamp(),action='plate_import',name=input$plate_file$name,range=input$plate_range,sheet=input$plate_sheet,transpose=input$transpose_plate)))
    rv$p<-p;rv$selected<-character();session$sendCustomMessage('plate-selection',character());rv$plate_committed_key<-upload_source_key(input,'plate');refresh(p)
  },'Plate map imported. Select Next to assign well roles on Arrange plate.'))
  observeEvent(list(upload_source_key(input,'peak'),input$preview_peaks),attempt_peak_import({req(input$peak_file);rv$raw_peaks<-NULL;rv$raw_peaks_key<-NULL
    rv$raw_peaks<-read_source_table(input$peak_file$datapath,tools::file_ext(input$peak_file$name),sheet=input$peak_sheet,range=input$peak_range);rv$raw_peaks_key<-upload_source_key(input,'peak')}))
  output$mapping_ui<-renderUI({
    req(rv$raw_peaks,identical(rv$raw_peaks_key,upload_source_key(input,'peak')))
    cols<-names(rv$raw_peaks);suggested<-suggest_mapping(cols)
    filenames<-!nzchar(suggested$Well) && nzchar(suggested$Acquisition) &&
      any(grepl('^[A-H](0[1-9]|1[0-2])',toupper(rv$raw_peaks[[suggested$Acquisition]])))
    if(filenames)suggested$Well<-suggested$Acquisition
    core<-c('Well','Size','Height')
    mapped<-function(n)selectInput(paste0('map_',n),n,choices=c('Not mapped'='',setNames(cols,cols)),selected=suggested[[n]])
    tagList(textOutput('mapping_summary'),
      tags$details(class='column-mapping',tags$summary('Edit column mapping'),div(class='mapping-grid',lapply(core,mapped)),
        checkboxInput('well_from_filename','Read Well from filename prefix (e.g. A01A01.fsa → A01)',filenames),
        tags$details(tags$summary('Advanced columns'),div(class='mapping-grid',lapply(setdiff(names(suggested),core),mapped)))))
  })
  output$mapping_summary<-renderText({
    req(rv$raw_peaks)
    paste(paste0('Well ← ',input$map_Well %||% '',if(isTRUE(input$well_from_filename))' (filename prefix)' else ''),
          paste0('Size ← ',input$map_Size %||% ''),paste0('Height ← ',input$map_Height %||% ''),sep=' · ')
  })
  output$peak_preview<-renderTable({req(rv$raw_peaks,identical(rv$raw_peaks_key,upload_source_key(input,'peak')));head(rv$raw_peaks,8)},na='')
  observeEvent(input$commit_peaks,attempt_peak_import({req(rv$raw_peaks)
    if(!identical(rv$raw_peaks_key,upload_source_key(input,'peak')))stop('The peak file or settings changed. Read the current columns and preview first.')
    mapping<-setNames(lapply(names(suggest_mapping(names(rv$raw_peaks))),function(n)input[[paste0('map_',n)]]%||%''),names(suggest_mapping(names(rv$raw_peaks))))
    raw<-rv$raw_peaks
    if(isTRUE(input$well_from_filename)) {
      value<-toupper(as.character(raw[[mapping$Well]]));ok<-grepl('^[A-H](0[1-9]|1[0-2])',value)
      raw$.derived_well<-ifelse(ok,substr(value,1,3),'');mapping$Well<-'.derived_well'
    }
    if(identical(mapping$Peak_ID,'Dye/Sample Peak') && !nzchar(mapping$Channel)) {
      physical_id<-text_value(raw[[mapping$Peak_ID]])
      raw$.derived_dye<-ifelse(grepl('^[A-Za-z]+,[0-9]+$',physical_id),sub(',.*$','',physical_id),'')
      mapping$Channel<-'.derived_dye'
    }
    p<-rv$p;import<-normalize_peaks(raw,mapping,p$id);p$peaks<-import$peaks;p$import_issues<-import$issues
    p<-sync_records(p);p<-remember_upload(p,input$peak_file);p$audit<-c(p$audit,list(list(time=stamp(),action='peak_import',name=input$peak_file$name,mapping=mapping,well_from_filename=input$well_from_filename,issues=nrow(import$issues))))
    rv$p<-p;rv$peak_committed_key<-peak_import_key(input);refresh(p)
  },'Mapped peaks imported; inspect issues, roles and QC before formal analysis.'))
  output$import_feedback<-renderUI({p<-rv$p;conflicts<-role_conflicts(p);counts<-acquisition_counts(p)
    tagList(if(!is.null(rv$peak_import_error))div(class='notice danger',role='alert',strong('Import did not complete: '),rv$peak_import_error),
      paste(nrow(p$peaks),'valid physical peaks;',nrow(p$import_issues),'flagged source rows;',counts[['actual']],'acquisitions;',counts[['placeholder']],'no-acquisition placeholders.'),
      if(nrow(conflicts))div(class='notice danger',paste('NA positions with peak data:',paste(conflicts$Well,collapse=', '),'. Data retained.')))
  })
  output$import_issues<-renderTable({req(isTRUE(input$show_import_issues));head(rv$p$import_issues,25)},rownames=FALSE)
  observeEvent(input$restore_project,attempt({req(input$project_file);p<-read_project(input$project_file$datapath);p$workflow$plate_imported<-any(nzchar(p$plate$Code)) || nrow(p$peaks)>0;rv$p<-p;rv$selected<-character();session$sendCustomMessage('plate-selection',character());refresh(rv$p,replacement=TRUE);rv$step<-2L;updateTabsetPanel(session,'main_tabs',selected='layout')},'Project restored with original fit state and current threshold.'))
  observeEvent(input$import_metadata,attempt({req(input$metadata_file);raw<-read_source_table(input$metadata_file$datapath,'csv');p<-rv$p
    if(!'Record_ID'%in%names(raw) || anyDuplicated(raw$Record_ID) || any(!raw$Record_ID%in%p$records$Record_ID))stop('Metadata must use unique, existing Record_ID values from this project.')
    allowed<-c('Code','Full_name','Group','Role','Batch',names(record_defaults(0)))
    allowed<-setdiff(intersect(allowed,names(raw)),c('Source_ID','Allele','Input_hash'))
    for(nm in allowed) {
      values<-raw[[nm]];old<-p$records[[nm]]
      if(is.logical(old)){values<-as.logical(values);if(anyNA(values))stop('Use TRUE/FALSE in ',nm)}
      if(is.numeric(old))values<-suppressWarnings(as.numeric(values))
      p$records[[nm]][match(raw$Record_ID,p$records$Record_ID)]<-values
    }
    if(any(!p$records$Role%in%roles))stop('Invalid role in metadata.')
    for(w in unique(p$records$Well))for(nm in c('Code','Full_name','Group','Role','Batch')) {
      values<-unique(p$records[[nm]][p$records$Well==w]);if(length(values)!=1)stop('Conflicting ',nm,' values in well ',w)
      p$plate[[nm]][p$plate$Well==w]<-values
    }
    p$audit<-c(p$audit,list(list(time=stamp(),action='metadata_import',name=input$metadata_file$name)));rv$p<-sync_records(p);refresh(rv$p)
  },'Metadata applied. Calculation/eligibility state reflects these records.'))
}
