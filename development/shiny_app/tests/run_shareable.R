# Run from the checkout root. This suite uses generated synthetic data only.
source('development/shiny_app/R/bootstrap.R')
require_dependencies()
options(repeatscope.root=rs_root)
tests<-c('state','reference','engine','hierarchical','export','edge_cases',
  'server','workflow','guided','guided_server','grouping_mode',
  'partial_plate_import','import_error_feedback','threshold_sync',
  'dashboard_controls','empty_upload','preview_filter','seed_diagnostics','layout_synthetic','intermediate_ei','disease_profiles','profile_server','project_safety','visual_navigation','repeat_report','learning_ui')
for(test in tests) {
  cat('\nRUN',test,'\n')
  source(file.path(rs_app_dir,'tests',paste0('test_',test,'.R')),local=new.env(parent=globalenv()))
}
cat('\nALL SHAREABLE SYNTHETIC SUITES PASS\n')
