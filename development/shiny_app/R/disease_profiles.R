# Disease defaults only; no batch calibration or specimen evidence belongs here.
disease_profiles <- local({
 cached<-NULL
 function() {
  if(!is.null(cached))return(cached)
  x<-jsonlite::fromJSON(file.path(rs_app_dir,'profiles/diseases.json'),simplifyVector=FALSE)
  if(!identical(x$schema,'RepeatScope/disease-profiles/1'))stop('Unsupported disease profile catalog.')
  profiles<-x$profiles
  if(anyDuplicated(vapply(profiles,function(p)p$gene,character(1))))stop('Duplicate profile gene.')
  for(p in profiles)if(!is.finite(p$normal_max) || !is.finite(p$expanded_min) ||
    p$expanded_min<=p$normal_max || p$unit!=3 || !nzchar(p$source$url) || !nzchar(p$version))stop('Invalid disease profile.')
  cached<<-profiles;cached
 }
})
profile_key <- function(x) gsub('[^A-Z0-9]','',toupper(trimws(x%||%'')))
find_disease_profile <- function(gene) {
 key<-profile_key(gene);if(!nzchar(key))return(NULL)
 hits<-Filter(function(p)key%in%vapply(c(p$gene,p$aliases),profile_key,character(1)),disease_profiles())
 if(length(hits)==1)hits[[1]] else NULL
}
profile_repeat_class <- function(profile,repeats) {
 cfg<-list(normal_max=profile$normal_max,normal_inclusive=profile$normal_inclusive,
  expanded_min=profile$expanded_min,expanded_inclusive=profile$expanded_inclusive,unit=profile$unit,uncertainty=0)
 ans<-localization_classify(repeats,cfg)
 for(b in profile$conditional_intervals)ans[is.finite(repeats) & repeats>=b$min & repeats<=b$max]<-'boundary'
 ans
}
localization_ranges <- function(x) data.frame(Category=c('normal','intermediate','expanded'),
 Lower=c(0,x$normal_max,x$expanded_min),Upper=c(x$normal_max,x$expanded_min,Inf),
 Lower_inclusive=c(TRUE,!x$normal_inclusive,x$expanded_inclusive),
 Upper_inclusive=c(x$normal_inclusive,!x$expanded_inclusive,FALSE),
 EI_allowed=c(FALSE,TRUE,TRUE),Source=x$source)
apply_disease_profile <- function(p,gene) {
 p<-ensure_localization(p);d<-find_disease_profile(gene)
 canonical<-if(is.null(d))trimws(gene) else d$gene
 changed<-nzchar(p$assay$gene) && profile_key(p$assay$gene)!=profile_key(canonical)
 if(changed){p['reference']<-list(NULL);p['structure']<-list(NULL);p['reference_draft']<-list(NULL);p$assay$channel<-''}
 x<-default_localization();x$gene<-canonical
 p['disease_profile']<-list(d);p$profile_custom<-FALSE;p$assay$gene<-canonical;p$assay$status<-'unconfigured'
 p$analysis_defaults<-list(filter_initialized=FALSE)
 if(!is.null(d)) {
  x$name<-paste(d$gene,'reference-derived assay');x$normal_max<-d$normal_max;x$normal_inclusive<-d$normal_inclusive
  x$expanded_min<-d$expanded_min;x$expanded_inclusive<-d$expanded_inclusive
  x$counting<-d$counting;x$unit<-d$unit;x$mode<-'theoretical'
  x$source<-paste(d$source$title,d$source$url,'profile',d$version,'reviewed',d$source$reviewed)
  x$mapping_source<-'Matched reference and primer structure; CE uncertainty not supplied. Nominal categories are provisional.'
  p$assay$motif<-d$motif;p$assay$repeat_bp<-d$unit;p$assay$ranges<-localization_ranges(x)
  if(!nzchar(p$assay$organism))p$assay$organism<-'Homo sapiens'
 } else p$assay$ranges<-default_assay()$ranges
 p$localization<-x;p$assay$include_intermediate<-TRUE
 p$audit<-c(p$audit,list(list(time=stamp(),action='disease_profile',gene=canonical,profile=d)))
 profile_setup(p)
}
profile_setup <- function(p) {
 p<-ensure_localization(p)
 if(!nzchar(p$assay$channel) && nrow(p$peaks) && 'Channel'%in%names(p$peaks)) {
  channels<-unique(trimws(p$peaks$Channel));channels<-channels[!is.na(channels) & nzchar(channels)]
  if(length(channels)==1 && all(nzchar(trimws(p$peaks$Channel))))p$assay$channel<-channels
 }
 if(!is.null(p$disease_profile) && !isTRUE(p$analysis_defaults$filter_initialized)) {
  s<-project_structure_view(p)
  if(!is.null(s) && is.finite(s$fixed_bp)) {
   p$assay$minimum_bp<-max(0,s$fixed_bp)
   p$analysis_defaults$filter_initialized<-TRUE
  }
 }
 p
}
