source('development/shiny_app/R/bootstrap.R');require_dependencies()
options(repeatscope.root=rs_root)
check<-function(x,msg){if(!isTRUE(x))stop('FAIL: ',msg);cat('PASS:',msg,'\n')}
check(exists('disease_profiles'),'versioned disease profile loader exists')
catalog<-disease_profiles()
check(length(catalog)==9L,'nine curated polyglutamine locus profiles are available')
for(g in c('ATXN1','ATXN2','ATXN3','CACNA1A','ATXN7','TBP','HTT','ATN1','AR')) {
 d<-find_disease_profile(g)
 check(!is.null(d) && d$unit==3 && nzchar(d$source$url) && nzchar(d$version),paste(g,'has source/version and unit'))
 check(!any(c('channel','minimum_bp','fixed_bp','uncertainty','intercept','slope')%in%names(d)),paste(g,'does not invent batch parameters'))
}
check(find_disease_profile('sca 2')$gene=='ATXN2' && find_disease_profile('huntington disease')$gene=='HTT','gene and disease aliases resolve')
check(is.null(find_disease_profile('not-a-gene')),'unknown genes do not get a guessed profile')
check(identical(profile_repeat_class(find_disease_profile('ATXN2'),c(31,32,33)),c('normal','intermediate','expanded')),'SCA2 boundaries are exact')
check(identical(profile_repeat_class(find_disease_profile('HTT'),c(26,27,35,36,40)),c('normal','intermediate','intermediate','expanded','expanded')),'HTT ranges include intermediate and reduced penetrance')
check(all(profile_repeat_class(find_disease_profile('ATXN1'),c(36,39,44))=='boundary'),'ATXN1 interruptions remain conditional')
check(profile_repeat_class(find_disease_profile('ATXN3'),60)=='boundary','approximate ATXN3 boundary remains uncertain')
check(profile_repeat_class(find_disease_profile('ATN1'),35)=='boundary','overlapping ATN1 source boundary stays explicit')
p<-apply_disease_profile(new_project(),'ATXN2')
check(p$localization$normal_max==31 && p$localization$expanded_min==33 && is.na(p$localization$uncertainty),
 'gene selection supplies disease defaults and leaves CE uncertainty unknown')
check(identical(p$disease_profile,find_disease_profile('ATXN2')),'project retains the complete profile snapshot')
p$assay$channel<-'B';p$localization$mode<-'linear';p$localization$intercept<-103;p$localization$slope<-3
check(!length(localization_issues(p,FALSE)),'unknown CE precision does not block calculation setup')
check(identical(localization_classify(c(31,32,33),p$localization),c('normal','intermediate','expanded')),
 'unknown precision supports explicitly provisional nominal categories')
p$localization$normal_max<-30
f<-tempfile(fileext='.rds');save_project(p,f);restored<-read_project(f)
check(restored$localization$normal_max==30 && identical(restored$disease_profile,p$disease_profile),'restore retains a customized saved snapshot')
q<-apply_disease_profile(p,'HTT')
check(q$localization$normal_max==26 && q$disease_profile$gene=='HTT','changing gene replaces disease defaults')
q<-apply_disease_profile(q,'UNLISTED')
check(is.null(q$disease_profile) && is.na(q$localization$normal_max) && q$localization$gene=='UNLISTED','unrecognized new gene clears prior disease defaults')
s<-synthetic_project();s<-apply_disease_profile(s,'ATXN2');s$assay$channel<-''
s<-profile_setup(s)
check(s$assay$channel=='B','one imported channel is selected automatically')
s$assay$channel<-'';s$peaks$Channel[1]<-'Y';s<-profile_setup(s)
check(s$assay$channel=='','multiple channels require an explicit choice')
cat('DISEASE PROFILE SUITE PASS\n')
