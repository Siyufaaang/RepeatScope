# Usage: Rscript --vanilla tools/deploy_beta.R BUNDLE_DIR ACCOUNT
# Authenticate separately with rsconnect::connectCloudUser(). No credentials in args.
args<-commandArgs(trailingOnly=TRUE)
if(length(args)!=2L)stop('Provide the verified bundle directory and Connect Cloud account name.')
bundle<-normalizePath(args[1],winslash='/',mustWork=TRUE)
script<-sub('^--file=','',commandArgs(FALSE)[grepl('^--file=',commandArgs(FALSE))])
root<-normalizePath(file.path(dirname(script),'..'),winslash='/',mustWork=TRUE)
source(file.path(root,'tools/prepare_beta.R'))
verified<-verify_beta_bundle(root,bundle)
accounts<-rsconnect::accounts()
if(!any(accounts$name==args[2] & accounts$server=='connect.posit.cloud'))stop('Connect Cloud account is not configured. Run rsconnect::connectCloudUser() first.')
rsconnect::deployApp(appDir=bundle,appFiles=verified$files,appMode='shiny',
  appName='repeatscope-beta',appTitle='RepeatScope Beta',account=args[2],server='connect.posit.cloud',
  appVisibility='public',launch.browser=FALSE,lint=FALSE)
