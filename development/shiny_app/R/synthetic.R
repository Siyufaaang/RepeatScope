synthetic_project <- function() {
  p<-new_project();p$id<-'SYNTHETIC-plate-01';p$synthetic<-TRUE
  wells<-c('A01','A02','A03','A04','B01','B02','B03','C01')
  codes<-c('001','002','003','004','005','006','007','NTC')
  ix<-match(wells,p$plate$Well);p$plate$Code[ix]<-codes;p$plate$Full_name[ix]<-paste('Synthetic demonstration',codes)
  p$plate$Role[ix]<-c(rep('sample',7),'ntc');p$plate$Group[ix]<-c(rep('Synthetic cohort A',4),rep('Synthetic cohort B',3),'Controls')
  p$plate$Batch[ix]<-'SYNTHETIC-batch';p$groups_enabled<-TRUE;p$groups<-unique(p$plate$Group[ix])
  raw<-do.call(rbind,lapply(seq_along(wells),function(i) {
    k<-0:20;heights<-round(c(10000,6500,4000,2400,1600,1000,900*exp(-.12*(1:15))))
    if(i==2)heights[1]<-12000
    if(i==3)heights[7:21]<-20
    if(i==7)heights[4:21]<-10
    if(i==8)heights<-rep(10,21)
    size<-200+i*2+(2.9+i*.005)*k
    if(i==4){size[c(19,20)]<-size[c(19,20)]+.45;heights[c(19,20)]<-c(345,335)}
    data.frame(Well=wells[i],Acquisition='SYNTHETIC-injection-1',Size=size,Height=heights,Peak_ID=paste0('SYN-',wells[i],'-',k),Data_Point=1000+i*300+k*30,Channel='B',stringsAsFactors=FALSE)
  }))
  p$peaks<-normalize_peaks(raw,list(Well='Well',Acquisition='Acquisition',Size='Size',Height='Height',Peak_ID='Peak_ID',Data_Point='Data_Point',Channel='Channel'),p$id)$peaks
  p<-sync_records(p);p$records$Material_ID<-paste0('SYN-M',seq_len(nrow(p$records)));p$records$Individual_ID<-c('SYN-I1','SYN-I2','SYN-I3','SYN-I4','SYN-I5','SYN-I6','SYN-I7','')
  p$records$Duplicate_status<-'distinct_materials';p$records$Category<-c(rep('expanded',4),'normal','expanded','expanded','unresolved')
  p$records$Evidence<-'SYNTHETIC categorical labels for software demonstration only.'
  p$records$Target_confirmed<-TRUE;p$records$Target_evidence<-'SYNTHETIC known generated target.'
  p$records$QC_status<-c(rep('passed',5),'unknown','passed','unknown');p$records$QC_evidence<-ifelse(p$records$QC_status=='passed','SYNTHETIC QC scenario; not a laboratory record.','')
  p$records$Observation<-'Synthetic one resolved target; not a zygosity inference'
  p$assay$name<-'SYNTHETIC CAG software demonstration';p$assay$id<-'SYNTHETIC-CAG';p$assay$gene<-'SYNTHETIC';p$assay$organism<-'Synthetic sequence'
  p$assay$status<-'configured';p$assay$source<-'Synthetic fixture specification; thresholds have no clinical meaning.'
  p$assay$validation_scope<-'Software workflow only. Not a real disease/assay validation.';p$assay$channel<-'B';p$assay$fixed_bp<-16
  p$assay$ranges<-data.frame(Category=c('normal','intermediate','expanded'),Lower=c(0,20,35),Upper=c(20,35,Inf),Lower_inclusive=TRUE,Upper_inclusive=FALSE,EI_allowed=c(FALSE,TRUE,TRUE),Source='SYNTHETIC example, no disease interpretation')
  seq<-paste0('TTATGCGA',paste(rep('CAG',17),collapse=''),'TTGACCGT')
  p$reference<-parse_fasta(paste0('>SYNTHETIC.1 artificial reference; not NCBI\n',seq));p$reference$type<-'synthetic genomic'
  a<-locate_amplicon(seq,'TTATGCGA','ACGGTCAA')
  p$structure<-confirm_structure(a,9,59,'tract_length','Synthetic tract length','Synthetic boundary fixture',suggestions=repeat_candidates(seq,'CAG'))
  p$control_review<-list(status='passed',scope='assay',batches='SYNTHETIC-batch',evidence='SYNTHETIC control-review scenario only; no laboratory validation.')
  p$originals<-list(list(name='SYNTHETIC_peaks.csv',bytes=charToRaw(paste(capture.output(write.csv(raw,row.names=FALSE)),collapse='\n'))))
  p
}
generate_examples <- function(directory) {
  dir.create(directory,recursive=TRUE,showWarnings=FALSE);p<-synthetic_project()
  plate_template(file.path(directory,'blank_plate_template.xlsx'))
  m<-data.frame(Row=LETTERS[1:8],matrix(p$plate$Code,8,12,byrow=TRUE),check.names=FALSE);names(m)<-c('Row',as.character(1:12))
  writexl::write_xlsx(list(Plate=m),file.path(directory,'SYNTHETIC_plate.xlsx'))
  peaks<-p$peaks[,c('Well','Acquisition','Size','Height','Original_peak_ID','Data_Point','Channel')];names(peaks)[5]<-'Peak_ID'
  write.csv(peaks,file.path(directory,'SYNTHETIC_peaks.csv'),row.names=FALSE)
  write.table(peaks,file.path(directory,'SYNTHETIC_peaks.tsv'),sep='\t',row.names=FALSE)
  writexl::write_xlsx(peaks,file.path(directory,'SYNTHETIC_peaks.xlsx'))
  writeLines(c(paste0('>',p$reference$accession,' artificial sequence'),p$reference$sequence),file.path(directory,'SYNTHETIC_reference.fasta'))
  write.csv(p$records,file.path(directory,'SYNTHETIC_metadata.csv'),row.names=FALSE,na='')
  p<-fit_project(p);save_project(p,file.path(directory,'SYNTHETIC.repeatscope.rds'));write_report_bundle(p,file.path(directory,'SYNTHETIC_report.zip'))
  writeLines(c('SYNTHETIC examples only. No patient or research data copied into this directory.',
    'Plate import: Plate sheet, A1:M9. Peak CSV/TSV/XLSX columns map by their names.',
    'Reference FASTA: forward TTATGCGA; reverse ACGGTCAA (both actual 5-prime to 3-prime). No tails.',
    'Suggested tract 9–59, tract_length counting, theoretical fixed 16 bp. This is not an experimental calibration.',
    'The built-in demo/project contains synthetic evidence, category, control and QC scenarios; none is laboratory validation.',
    'A04 has a deliberately displaced distant tail; B01 normal; B02 QC unknown; B03 insufficient seeds.',
    'To exercise missing/duplicate/NA states, edit the plate or repeat an acquisition before confirming roles.'),file.path(directory,'README.txt'))
  invisible(p)
}
