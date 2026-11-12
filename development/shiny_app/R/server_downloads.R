register_download_server <- function(input,output,session,rv,attempt,results,current_variant,display_project=function()rv$p) {
  output$download_project<-downloadHandler(function()paste0(safe_filename(rv$p$id),'.repeatscope.rds'),function(file)save_project(rv$p,file),contentType='application/octet-stream')
  output$download_template<-downloadHandler('RepeatScope_blank_plate.xlsx',function(file)plate_template(file),contentType='application/vnd.openxmlformats-officedocument.spreadsheetml.sheet')
  output$download_metadata<-downloadHandler('record_metadata.csv',function(file)write.csv(rv$p$records,file,row.names=FALSE,na=''))
  output$download_report<-downloadHandler(function()paste0(safe_filename(rv$p$id),'_',100*rv$p$threshold,'pct_',rv$p$window,'_report.zip'),function(file)write_report_bundle(display_project(),file,isTRUE(input$include_originals),current_variant()),contentType='application/zip')
  output$download_results<-downloadHandler('results_long.csv',function(file)write.csv(results(),file,row.names=FALSE,na=''))
  output$download_wide<-downloadHandler('EI_comparison.csv',function(file)write.csv(result_wide(results()),file,row.names=FALSE,na=''))
  output$download_summary<-downloadHandler('group_summary.csv',function(file)write.csv(summarise_results(results()),file,row.names=FALSE,na=''))
  output$download_decisions<-downloadHandler('peak_decisions.csv',function(file)write.csv(peak_decisions(display_project(),variant=current_variant()),file,row.names=FALSE,na=''))
  output$download_manifest<-downloadHandler('analysis_manifest.json',function(file)jsonlite::write_json(analysis_manifest(display_project(),current_variant()),file,pretty=TRUE,auto_unbox=TRUE,na='null',null='null',digits=16))
  output$download_structure<-downloadHandler('reference_structure.svg',function(file)writeLines(structure_svg(project_structure_view(rv$p),rv$p$reference),file),contentType='image/svg+xml')
  for(ext in c('svg','pdf','png'))local({fmt<-ext;output[[paste0('download_figure_',fmt)]]<-downloadHandler(paste0('EI_overview.',fmt),function(file){tmp<-tempfile(fileext=paste0('.',fmt));on.exit(unlink(tmp));save_figure(overview_plot(results(),rv$p$synthetic),tmp,13,7);file.copy(tmp,file)},contentType=switch(fmt,svg='image/svg+xml',pdf='application/pdf',png='image/png'))})
  output$demo_plate<-downloadHandler('SYNTHETIC_plate.xlsx',function(file)file.copy(file.path(rs_app_dir,'examples/SYNTHETIC_plate.xlsx'),file))
  output$demo_peaks<-downloadHandler('SYNTHETIC_peaks.csv',function(file)file.copy(file.path(rs_app_dir,'examples/SYNTHETIC_peaks.csv'),file))
  output$demo_fasta<-downloadHandler('SYNTHETIC_reference.fasta',function(file)file.copy(file.path(rs_app_dir,'examples/SYNTHETIC_reference.fasta'),file))
  output$export_status<-renderUI({r<-results();div(class='notice',paste('Current:',100*rv$p$threshold,'% own modal ·',rv$p$window,'·',current_variant(),
    if(fit_is_stale(display_project()))'· STALE RESULTS' else '',if(rv$p$synthetic)'· SYNTHETIC' else ''),br(),paste(nrow(r),'result rows; NA and reasons are preserved.'))})
}
