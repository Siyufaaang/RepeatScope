source('development/shiny_app/R/bootstrap.R')
dir.create(file.path(rs_app_dir,'.library'),recursive=TRUE,showWarnings=FALSE)
missing<-rs_dependencies[!vapply(rs_dependencies,requireNamespace,logical(1),quietly=TRUE)]
if(length(missing))install.packages(missing,lib=file.path(rs_app_dir,'.library'),repos='https://cloud.r-project.org',type='binary')
require_dependencies()
print(vapply(rs_dependencies,function(x)as.character(utils::packageVersion(x)),character(1)))
