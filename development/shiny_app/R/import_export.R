read_source_table <- function(path,extension=tools::file_ext(path),sheet=NULL,range=NULL,header=TRUE) {
  extension<-tolower(extension)
  if(extension%in%c('xlsx','xls')) {
    if(!is.null(range) && !nzchar(trimws(range)))range<-NULL
    return(as.data.frame(readxl::read_excel(path,sheet=sheet,range=range,col_names=header,col_types='text',na='',trim_ws=FALSE,.name_repair='unique'),stringsAsFactors=FALSE))
  }
  if(!extension%in%c('csv','tsv','txt'))stop('Use a CSV, TSV, TXT or XLSX sizing table.')
  utils::read.table(path,header=header,sep=if(extension=='csv')',' else '\t',quote='"',comment.char='',colClasses='character',
    check.names=FALSE,stringsAsFactors=FALSE,fill=FALSE,na.strings=character(),fileEncoding='UTF-8-BOM')
}
suggest_mapping <- function(columns) {
  normalized<-tolower(gsub('[^A-Za-z0-9]','',columns))
  aliases<-list(Well=c('well','position','samplewell'),Acquisition=c('acquisition','acquisitionid','samplefilename','injection'),Size=c('size','sizebp','fragmentsize'),
    Height=c('height','heightrfu','peakheight'),Peak_ID=c('peakid','physicalpeakid','dyesamplepeak'),Data_Point=c('datapoint','scan'),Channel=c('channel','dyechannel','dye'),QC=c('qc','sizingquality','quality'),Saturation=c('saturation','offscale','saturated'))
  lapply(aliases,function(a){i<-match(a,normalized);i<-i[!is.na(i)];if(length(i))columns[i[1]] else ''})
}
plate_template <- function(path) {
  x<-data.frame(Row=LETTERS[1:8],matrix('',8,12),check.names=FALSE);names(x)<-c('Row',as.character(1:12))
  writexl::write_xlsx(list(Plate=x,Instructions=data.frame(Instructions=c('SYNTHETIC/BLANK TEMPLATE. Store sample codes as text to preserve leading zeros.',
    'Rows A–H, columns 1–12. Import Plate range A1:M9, including headers.',
    'Empty cells remain unlabelled until you explicitly confirm unused/empty (NA).',
    'NA means nothing was added. CE water/buffer filling and NTC are different roles.'))),path)
}
safe_filename <- function(x) {
  x<-basename(gsub('\\','/',x,fixed=TRUE));x<-gsub('[^A-Za-z0-9._-]','_',x)
  x<-sub('^\\.+','',x);if(!nzchar(x))'upload.bin' else x
}
validate_project <- function(p) {
  # Inspect plain storage and attributes before any schema access or S3 dispatch.
  safe<-function(x,depth=0L) {
    kind<-typeof(x)
    if(depth>35L || isS4(x) || !kind%in%c('NULL','list','logical','integer','double','complex','character','raw'))return(FALSE)
    cls<-attr(x,'class',exact=TRUE)
    if(!is.null(cls) && !identical(cls,'data.frame') && !identical(cls,c('matrix','array')) &&
       !identical(cls,'matrix') && !identical(cls,'array') && !identical(cls,'AsIs'))return(FALSE)
    attrs<-attributes(x)
    if(!is.null(attrs))for(i in seq_along(attrs))if(!safe(.subset2(attrs,i),depth+1L))return(FALSE)
    if(kind=='list')for(i in seq_along(x))if(!safe(.subset2(x,i),depth+1L))return(FALSE)
    TRUE
  }
  if(!safe(p))stop('Project contains unsupported executable, classed or deeply nested objects.')
  if(!is.list(p) || !identical(p$schema,'RepeatScope/1'))stop('Unsupported RepeatScope project version.')
  req<-c('plate','records','peaks','assay','threshold','window','fit','reference','structure','audit','originals','hierarchical','reviewed')
  if(!all(req%in%names(p)))stop('Project is missing restoration fields.')
  if(!is.data.frame(p$plate) || typeof(p$plate)!='list' || nrow(p$plate)!=96 || !identical(p$plate$Well,well_coordinates()) || !all(p$plate$Role%in%roles))stop('Invalid project plate schema.')
  if(!is.numeric(p$threshold) || length(p$threshold)!=1 || !is.finite(p$threshold) || p$threshold<0 || p$threshold>1 || !p$window%in%c('tail10','extended'))stop('Invalid restored threshold/window.')
  if(nrow(p$peaks) && (!all(c('Peak_ID','Record_ID','Well','Size','Height')%in%names(p$peaks)) || anyDuplicated(p$peaks$Peak_ID) || any(!p$peaks$Well%in%well_coordinates()) || any(!is.finite(p$peaks$Size) | !is.finite(p$peaks$Height) | p$peaks$Height<=0)))stop('Invalid project physical peak inventory.')
  if(nrow(p$records) && (anyDuplicated(p$records$Record_ID) || !all(c('Source_ID','Well','Allele','Category','QC_status')%in%names(p$records))))stop('Invalid project record identities.')
  invisible(TRUE)
}
save_project <- function(p,path) { validate_project(p);saveRDS(p,path,version=3,compress='gzip');invisible(path) }
read_project <- function(path) {
  if(file.info(path)$size>100*1024^2)stop('Project exceeds the 100 MB prototype upload limit.')
  p<-readRDS(path);validate_project(p);p
}
result_wide <- function(results) {
  if(!nrow(results))return(data.frame())
  id<-c('Record_ID','Code','Well','Acquisition','Allele','Group','Window','Threshold','Variant','Reference','Run_ID')
  r<-results[results$Method%in%c('adaptive','bayesian'),c(id,'Method','Method_version','EI','Preview_EI','Status','Reason')]
  stats::reshape(r,idvar=id,timevar='Method',direction='wide')
}
analysis_manifest <- function(p,variant='automatic') {
  run<-if(variant=='automatic')p$fit else p$reviewed[[variant]]
  if(variant!='automatic' && is.null(run))stop('Reviewed run unavailable.')
  list(product='RepeatScope local prototype',schema=p$schema,project_id=p$id,created=p$created,exported=stamp(),
  synthetic=p$synthetic,threshold_fraction=p$threshold,threshold_percent=100*p$threshold,threshold_rule='Height >= p * own modal height',window=p$window,
  EI_definition='sum(own Height * Delta) / sum(own Height), modal included; conditional on selected peaks',
  current_input_fingerprint=analysis_fingerprint(p),variant=variant,run_id=run$id,fit_input_fingerprint=run$fingerprint,stale=!is.null(run) && !identical(run$fingerprint,analysis_fingerprint(p)),
  review=run$review,assay=p$assay,localization=p$localization,prescreen=p$prescreen,target_overrides=p$target_overrides,reference=p$reference,structure=p$structure,reference_draft=p$reference_draft,fit_configuration=list(assay=run$assay,reference=run$reference,structure=run$structure),method_versions=run$versions,control_review=p$control_review,
  hierarchical_pool=p$hierarchical$pool,hierarchical_training_fraction=p$hierarchical$result$anchor_fraction,
  prediction_interval='Pointwise posterior predictive position interval, bp; conditional on seeds/modal; not peak probability or EI confidence interval',
  quantiles='R quantile type 7; SD and IQR unavailable for n < 2',
  provenance=list(peak_table_hash=object_hash(p$peaks),record_count=nrow(p$records),original_upload_names=vapply(p$originals,function(x)x$name,character(1))),
  audit=p$audit,limitations=c('Software/computational validation is not biological accuracy.',
    'No genome-wide primer specificity test, FSA curves, automated p-values or background subtraction.',
    'Historical hierarchical experiment recovered 0/5 B07/H06 disputes and additionally excluded H06 Delta10/15.',
    'Unknown assay/primer/cutoff/QC/control facts remain unconfirmed.'))
}
write_tables <- function(p,directory,variant='automatic') {
  r<-threshold_results(p,variant=variant);d<-peak_decisions(p,variant=variant)
  tables<-list(plate_metadata=p$plate,record_metadata=p$records,results_long=r,EI_comparison=result_wide(r),group_summary=summarise_results(r),peak_decisions=d,import_issues=p$import_issues)
  for(nm in names(tables))utils::write.csv(tables[[nm]],file.path(directory,paste0(nm,'.csv')),row.names=FALSE,na='',fileEncoding='UTF-8')
  utils::write.table(p$plate,file.path(directory,'plate_metadata.tsv'),sep='\t',row.names=FALSE,na='',fileEncoding='UTF-8')
  writexl::write_xlsx(tables[vapply(tables,ncol,integer(1))>0],file.path(directory,'tables.xlsx'))
  tables
}
write_report_bundle <- function(p,path,include_originals=FALSE,variant='automatic') {
  directory<-tempfile('repeatscope-report-');dir.create(directory)
  on.exit(unlink(directory,recursive=TRUE),add=TRUE)
  tables<-write_tables(p,directory,variant)
  jsonlite::write_json(analysis_manifest(p,variant),file.path(directory,'manifest.json'),auto_unbox=TRUE,pretty=TRUE,na='null',null='null',digits=16)
  writeLines(c('RepeatScope — local research prototype',if(p$synthetic)'SYNTHETIC SOFTWARE DEMONSTRATION. No biological validation implied.' else 'Research data. Review all eligibility and QC status fields.',
    'Formal EI is blank unless status is success. Preview_EI is a calculation preview; it is not QC clearance.',
    'All failures, normal-range NA and unresolved records are retained. Raw originals excluded unless explicitly requested.',
    'Report groups describe samples/alleles; unknown individual IDs are not independent-patient counts.',
    'Use the separate .repeatscope.rds project download to restore the complete working session.',
    paste('Variant:',variant),paste('Height threshold:',100*p$threshold,'% of own modal; window:',p$window)),file.path(directory,'README.txt'))
  plot<-overview_plot(tables$results_long,p$synthetic)
  for(ext in c('svg','pdf','png'))save_figure(plot,file.path(directory,paste0('EI_overview.',ext)),width=13,height=7)
  structure_view<-project_structure_view(p)
  if(!is.null(structure_view))writeLines(structure_svg(structure_view,p$reference),file.path(directory,'reference_structure.svg'))
  if(!is.null(p$fit))for(id in names(p$fit$records)) {
    rec<-p$records[match(id,p$records$Record_ID),];name<-paste0(safe_filename(paste(rec$Code,rec$Well,rec$Acquisition,rec$Allele,sep='_')),'_tail')
    plot<-tail_plot(p,id,variant=variant)
    for(ext in c('svg','pdf','png'))save_figure(plot,file.path(directory,paste0(name,'.',ext)),width=12,height=5)
  }
  if(length(p$reviewed)) {
    dir.create(file.path(directory,'reviewed_versions'))
    for(id in names(p$reviewed))write.csv(threshold_results(p,variant=id),file.path(directory,'reviewed_versions',paste0(safe_filename(id),'.csv')),row.names=FALSE,na='')
  }
  if(include_originals && length(p$originals)) {
    dir.create(file.path(directory,'originals'))
    for(i in seq_along(p$originals))writeBin(p$originals[[i]]$bytes,file.path(directory,'originals',paste0(sprintf('%03d_',i),safe_filename(p$originals[[i]]$name))))
  }
  zip::zipr(zipfile=normalizePath(path,winslash='/',mustWork=FALSE),files=list.files(directory,all.files=FALSE),root=directory)
  invisible(path)
}
