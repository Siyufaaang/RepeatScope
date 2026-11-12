register_reference_server <- function(input,output,session,rv,attempt,refresh) {
  set_reference<-function(ref) {p<-rv$p;p$reference<-ref;p['structure']<-list(NULL);p['reference_draft']<-list(NULL);p$assay$status<-'unconfigured';rv$p<-p;rv$amplicon<-NULL;rv$candidates<-NULL}
  output$ncbi_status<-renderText({
    if(is.null(rv$p$reference) && !identical(rv$ncbi_message,'No query yet.'))rv$ncbi_message else ''
  })
  observeEvent(input$search_ncbi,{
    withProgress(message='Querying NCBI (no sample data sent)',value=.4,{rv$query<-ncbi_search(input$gene,input$organism,input$reference_type)})
    rv$ncbi_message<-paste(rv$query$status,rv$query$message)
    if(rv$query$status=='ok') {
      z<-rv$query$candidates
      showModal(modalDialog(title='Choose a versioned NCBI reference',size='l',p(paste(input$gene,'·',input$organism,'·',nrow(z),'candidates shown. No default selection.')),
        selectInput('ncbi_choice','Reference accession.version',choices=c('Select explicitly'='',setNames(z$Accession,paste(z$Accession,z$Type,z$Length,'bp'))),selected=''),
        div(style='max-height:420px;overflow:auto',lapply(seq_len(nrow(z)),function(i)div(class='ncbi-row',strong(z$Accession[i]),p(z$Title[i]),
          p(class='muted',paste(z$Organism[i],z$Type[i],z$Length[i],'bp',z$Assembly_or_transcript[i],sep=' · ')),tags$a(href=z$URL[i],target='_blank',rel='noopener','Open NCBI record')))),
        p('For large genomic records, set interval coordinates in the accession panel before fetching.'),
        footer=tagList(modalButton('Cancel'),actionButton('select_ncbi','Use selected reference',class='btn-primary'))))
    } else showNotification(rv$query$message,type=if(rv$query$status=='no_results')'warning' else 'error',duration=12)
  })
  observeEvent(input$select_ncbi,attempt({req(nzchar(input$ncbi_choice));z<-rv$query$candidates[rv$query$candidates$Accession==input$ncbi_choice,]
    if(nrow(z)!=1)stop('Select one current candidate.')
    if(z$Length>2000000){updateTextInput(session,'accession',value=z$Accession);removeModal();stop('Large genomic reference: select a locus interval in the accession panel, then fetch.')}
    ref<-ncbi_fetch(z$Accession,z$Type);ref$query<-rv$query$query;ref$organism<-input$organism;ref$gene<-input$gene
    set_reference(ref);updateTextInput(session,'accession',value=z$Accession);removeModal();rv$ncbi_message<-paste('Fetched exact version',ref$accession,ref$length,'bp')
  },'Selected reference version fetched and pinned.'))
  observeEvent(input$fetch_accession,attempt({start<-if(is.finite(input$ref_start))input$ref_start else NULL;end<-if(is.finite(input$ref_end))input$ref_end else NULL
    ref<-ncbi_fetch(input$accession,input$reference_type,start,end,as.integer(input$ref_strand));ref$gene<-input$gene;ref$organism<-input$organism;set_reference(ref)
    rv$ncbi_message<-paste('Fetched exact version',ref$accession,ref$length,'bp')
  },'Reference fetched. Verify reference type and primer structure.'))
  observeEvent(input$use_fasta,attempt({text<-input$fasta_text
    if(!is.null(input$fasta_file))text<-paste(readLines(input$fasta_file$datapath,warn=FALSE),collapse='\n')
    ref<-parse_fasta(text);ref$type<-input$reference_type;ref$gene<-input$gene;ref$organism<-input$organism;set_reference(ref)
  },'Offline reference loaded; source is explicitly recorded as offline FASTA.'))
  output$reference_info<-renderUI({
    r<-rv$p$reference;req(!is.null(r))
    div(class='status-small',strong(r$accession),paste('·',r$type,'·',r$length,'bp'),
      if(!is.null(r$url))tags$a(href=r$url,target='_blank',rel='noopener',' · NCBI record'))
  })
  match_current<-function() {req(rv$p$reference)
    if(identical(rv$amplicon$status,'located') && identical(current_primer_key(rv$p,input),saved_primer_key(rv$p)))return()
    rv$amplicon<-locate_amplicon(rv$p$reference$sequence,input$forward,input$reverse,input$forward_tail,input$reverse_tail,input$dye)
    if(rv$amplicon$status=='located') {
      rv$candidates<-amplicon_candidates(rv$amplicon,input$motif)
      candidate<-rv$candidates
      if(nrow(candidate)){updateNumericInput(session,'tract_start',value=candidate$Start[1]);updateNumericInput(session,'tract_end',value=candidate$End[1])}
    } else rv$candidates<-NULL
    p<-rv$p;p['structure']<-list(NULL);p$reference_draft<-list(amplicon=rv$amplicon,suggestions=rv$candidates,
      start=if(nrow(rv$candidates%||%data.frame()))rv$candidates$Start[1] else NULL,
      end=if(nrow(rv$candidates%||%data.frame()))rv$candidates$End[1] else NULL,counting_rule='tract_length')
    updateSelectInput(session,'counting_rule',selected='tract_length')
    p$assay$motif<-clean_dna(input$motif);p$assay$status<-'unconfigured';rv$p<-p
  }
  observeEvent(input$locate_primers,attempt(match_current()))
  # Save edits immediately, including incomplete primers; debouncing only delays matching.
  observeEvent(list(rv$p$reference$sequence_hash,input$forward,input$reverse,input$forward_tail,input$reverse_tail,input$motif,rv$step),{
    if(rv$step!=4L || is.null(rv$p$reference))return()
    key<-current_primer_key(rv$p,input)
    if(identical(key,saved_primer_key(rv$p)) || identical(key,rv$p$reference_draft$input_key))return()
    complete<-all(nzchar(c(input$forward%||%'',input$reverse%||%'',input$motif%||%'')))
    a<-list(sequence=rv$p$reference$sequence,forward=input$forward%||%'',reverse=input$reverse%||%'',
      forward_tail=input$forward_tail%||%'',reverse_tail=input$reverse_tail%||%'',dye=input$dye%||%'',
      status=if(complete)'matching' else 'incomplete',reason=if(complete)'Matching updated inputs.' else 'Enter both primers and the reported motif.')
    p<-rv$p;p['structure']<-list(NULL);p$reference_draft<-list(amplicon=a,suggestions=NULL,input_key=key,matching_pending=TRUE)
    p$assay$status<-'unconfigured';p$assay$motif<-input$motif%||%'';rv$amplicon<-a;rv$candidates<-NULL;rv$p<-p
  },priority=10)
  matching_inputs<-debounce(reactive(list(reference=rv$p$reference$sequence_hash,forward=input$forward,reverse=input$reverse,ft=input$forward_tail,rt=input$reverse_tail,motif=input$motif,step=rv$step)),600)
  observeEvent(matching_inputs(),{
    x<-matching_inputs();if(rv$step!=4L || is.null(rv$p$reference) || !nzchar(x$forward%||%'') || !nzchar(x$reverse%||%'') || !nzchar(x$motif%||%''))return()
    if(!identical(current_primer_key(rv$p,input),saved_primer_key(rv$p)))attempt(match_current())
  },ignoreInit=TRUE)
  output$primer_status<-renderText({
    a<-rv$amplicon
    if(is.null(a) || identical(a$status,'located'))'' else a$reason
  })
  output$tract_candidates<-renderTable(rv$candidates,rownames=FALSE)
  observeEvent(input$confirm_structure,attempt({req(rv$amplicon)
    if(!identical(current_primer_key(rv$p,input),saved_primer_key(rv$p)))stop('Match the current primer inputs before confirming structure.')
    if(is.null(rv$p$reference) || !identical(object_hash(rv$amplicon$sequence),rv$p$reference$sequence_hash))stop('The primer draft belongs to a different reference. Locate primers again on the current reference.')
    s<-confirm_structure(rv$amplicon,input$tract_start,input$tract_end,input$counting_rule,input$counting_basis,input$boundary_reason,input$motif,rv$candidates)
    p<-rv$p;p$structure<-s;p$assay$motif<-input$motif;p$audit<-c(p$audit,list(list(time=stamp(),action='structure_confirmation',suggested=rv$candidates,start=s$start,end=s$end,basis=s$basis,reason=s$review_reason)));rv$p<-p
  },'Reference structure confirmed. Theoretical fixed length is separate from empirical CE calibration.'))
  observeEvent(list(input$tract_start,input$tract_end,input$counting_rule),{
    if(rv$step!=4L || !identical(current_primer_key(rv$p,input),saved_primer_key(rv$p)))return()
    p<-rv$p;d<-p$structure%||%p$reference_draft
    if(!identical(d$amplicon$status,'located'))return()
    if(identical(as.numeric(d$start),as.numeric(input$tract_start)) && identical(as.numeric(d$end),as.numeric(input$tract_end)) && identical(d$counting_rule,input$counting_rule))return()
    d$start<-input$tract_start;d$end<-input$tract_end;d$counting_rule<-input$counting_rule
    d$status<-'preview';d$confirmed_at<-NULL;p$reference_draft<-d;p['structure']<-list(NULL);p$assay$status<-'unconfigured';rv$p<-p
  },ignoreInit=TRUE)
  structure_view<-reactive(project_structure_view(rv$p))
  output$structure_diagram<-renderUI({s<-structure_view();div(class='reference-svg',
    if(is.null(s) && !is.null(rv$amplicon) && rv$amplicon$status=='located')div(class='notice warning','Primers located. Review the candidate tract boundaries; no valid counted region is selected yet.'),
    HTML(structure_svg(s,rv$p$reference)))})
  output$structure_sequence<-renderUI({a<-rv$amplicon;req(!is.null(a),a$status=='located');s<-structure_view()
    site<-substr(a$oriented_sequence,a$reverse_start,a$end)
    bound_reverse<-substring(a$reverse,nchar(a$reverse_tail)+1)
    aligned_reverse<-paste(rev(strsplit(bound_reverse,'',fixed=TRUE)[[1]]),collapse='')
    tagList(
    p('Reference amplicon · ',a$expected_bp,' bp including declared tails.'),
    tags$pre(class='dna-sequence',a$amplicon),
    p(strong('Reverse primer binding')),
    tags$pre(paste0('Reference       5′ ',site,' 3′\nReverse primer  3′ ',aligned_reverse,' 5′')),
    if(!is.null(s))p(paste('Interruptions:',s$interruptions,'· Theoretical fixed length:',s$fixed_bp,'bp')))
  })
  observeEvent(input$save_assay,attempt({p<-rv$p;a<-p$assay
    a$name<-input$assay_name;a$gene<-input$gene;a$organism<-input$organism;a$source<-input$assay_source;a$validation_scope<-input$assay_scope
    a$fixed_bp<-input$fixed_bp;a$minimum_bp<-input$minimum_bp;a$channel<-input$target_channel;a$motif<-input$motif
    a$nominal_spacing<-input$nominal_spacing;a$spacing_low<-input$spacing_low;a$spacing_high<-input$spacing_high;a$tolerance<-input$tolerance
    a$seed_min_rfu<-input$seed_rfu;a$seed_fraction<-input$seed_fraction;a$predictive_level<-input$predictive_level;a$extended_upper<-input$search_upper;a$repeat_bp<-input$repeat_bp
    if(is.null(p$localization))a$ranges<-read.csv(text=input$disease_ranges,stringsAsFactors=FALSE,check.names=FALSE)
    a$include_intermediate<-isTRUE(input$include_intermediate)
    validate_analysis_config(a)
    if(isTRUE(input$assay_confirmed)) {
      validate_ranges(a$ranges)
      if(!nzchar(a$source) || is.null(p$structure) || p$structure$status!='confirmed')stop('Confirmed structure and assay evidence/source are required. Save a draft otherwise.')
      if(nchar(a$motif)!=a$repeat_bp)stop('Motif length and repeat-unit bp disagree.')
      a$status<-'configured'
    } else a$status<-'unconfigured'
    p$assay<-a;p$audit<-c(p$audit,list(list(time=stamp(),action='assay_save',assay=a)));rv$p<-p
  },'Assay saved. Changed fitting parameters require an explicit analysis run.'))
  observeEvent(input$load_benchmark_config,attempt({p<-rv$p;p$assay<-benchmark_assay();rv$p<-p;refresh(p)},'SCA2 research parameters loaded; incomplete laboratory evidence remains unconfirmed.'))
}
