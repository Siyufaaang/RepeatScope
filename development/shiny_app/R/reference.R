clean_dna <- function(x) toupper(gsub('[[:space:]]','',x))
reverse_complement <- function(x) paste(rev(strsplit(chartr('ACGTRYMKBDHVN','TGCAYRKMVHDBN',clean_dna(x)),'')[[1]]),collapse='')
xml_escape <- function(x) { x<-gsub('&','&amp;',text_value(x),fixed=TRUE);x<-gsub('<','&lt;',x,fixed=TRUE);x<-gsub('>','&gt;',x,fixed=TRUE);gsub('"','&quot;',x,fixed=TRUE) }
parse_fasta <- function(text) {
  lines<-strsplit(text,'\n',fixed=TRUE)[[1]];lines<-trimws(lines);lines<-lines[nzchar(lines)]
  if(sum(startsWith(lines,'>'))!=1 || !startsWith(lines[1],'>'))stop('Provide exactly one FASTA record with a >header.')
  seq<-clean_dna(paste(lines[-1],collapse=''))
  if(!nzchar(seq) || grepl('[^ACGTRYMKBDHVN]',seq))stop('FASTA contains invalid nucleotide characters.')
  if(nchar(seq)>2000000)stop('Select a locus interval of at most 2,000,000 bp.')
  list(accession=sub(' .*','',substring(lines[1],2)),description=substring(lines[1],2),sequence=seq,
    sequence_hash=object_hash(seq),length=nchar(seq),source='offline FASTA',type='unconfirmed',start=1L,strand=1L,retrieved=stamp())
}
ncbi_transport <- function(url) {
  handle<-curl::new_handle(timeout=25,connecttimeout=10,useragent='RepeatScope-local-prototype/1.0')
  r<-curl::curl_fetch_memory(url,handle=handle)
  if(r$status_code!=200)stop('NCBI HTTP ',r$status_code)
  rawToChar(r$content)
}
ncbi_url <- function(endpoint,args) paste0('https://eutils.ncbi.nlm.nih.gov/entrez/eutils/',endpoint,'.fcgi?',
  paste(paste0(names(args),'=',vapply(args,function(x)utils::URLencode(as.character(x),reserved=TRUE),character(1))),collapse='&'))
ncbi_search <- function(gene,organism,type='transcript',transport=ncbi_transport) {
  empty<-data.frame(Accession=character(),Title=character(),Organism=character(),Type=character(),Length=numeric(),Assembly_or_transcript=character(),URL=character())
  if(!nzchar(trimws(gene)) || !nzchar(trimws(organism)))return(list(status='input_error',message='Enter Gene Name and organism.',candidates=empty))
  tryCatch({
    term<-paste0('"',gsub('"','',gene),'"[Gene Name] AND "',gsub('"','',organism),'"[Organism] AND refseq[filter]',
      if(type=='genomic')' AND biomol_genomic[PROP]' else ' AND biomol_mrna[PROP]')
    search<-jsonlite::fromJSON(transport(ncbi_url('esearch',list(db='nuccore',term=term,retmode='json',retmax=20,tool='RepeatScope'))))
    if(!is.null(search$error) || is.null(search$esearchresult))stop('NCBI returned an error or malformed search response.')
    ids<-search$esearchresult$idlist
    if(!length(ids))return(list(status='no_results',message='No matching RefSeq nucleotide references. Refine gene, organism or type, or use accession/FASTA.',candidates=empty,query=term,time=stamp()))
    Sys.sleep(.35)
    sm<-jsonlite::fromJSON(transport(ncbi_url('esummary',list(db='nuccore',id=paste(ids,collapse=','),retmode='json',tool='RepeatScope'))))$result
    if(is.null(sm$uids))stop('NCBI returned no usable sequence summaries.')
    rows<-lapply(sm$uids,function(id) {
      z<-sm[[id]];acc<-z$accessionversion
      if(is.null(acc) || !grepl('\\.[0-9]+$',acc))return(NULL)
      data.frame(Accession=acc,Title=z$title%||%'',Organism=z$organism%||%organism,Type=type,Length=as.numeric(z$slen%||%NA),
        Assembly_or_transcript=z$extra%||%z$caption%||%'See NCBI record',URL=paste0('https://www.ncbi.nlm.nih.gov/nuccore/',acc),stringsAsFactors=FALSE)
    })
    candidates<-do.call(rbind,rows)
    if(is.null(candidates))stop('No versioned accessions in NCBI response.')
    list(status='ok',message='Select a reference explicitly; no candidate has been selected automatically.',candidates=candidates,
      query=term,count=as.integer(search$esearchresult$count),time=stamp())
  },error=function(e)list(status='network_error',message=conditionMessage(e),candidates=empty,time=stamp()))
}
ncbi_fetch <- function(accession,type='transcript',start=NULL,end=NULL,strand=1L,transport=ncbi_transport) {
  accession<-trimws(accession)
  if(!grepl('^[A-Za-z0-9_]+\\.[0-9]+$',accession))stop('Enter and explicitly select an accession.version, for example NM_002973.4.')
  args<-list(db='nuccore',id=accession,rettype='fasta',retmode='text',strand=strand,tool='RepeatScope')
  if(!is.null(start) || !is.null(end)) {
    if(length(start)!=1 || length(end)!=1 || !is.finite(start) || !is.finite(end) || start<1 || end<start || end-start>1999999)stop('Set a valid reference interval, at most 2,000,000 bp.')
    args$seq_start<-as.integer(start);args$seq_stop<-as.integer(end)
  }
  text<-transport(ncbi_url('efetch',args));ref<-parse_fasta(text)
  if(!grepl(accession,ref$description,fixed=TRUE))stop('Returned sequence does not identify the selected accession.version.')
  ref$accession<-accession;ref$type<-type;ref$start<-start%||%1L;ref$end<-end%||%ref$length
  ref$strand<-strand;ref$source<-'NCBI EFetch';ref$url<-paste0('https://www.ncbi.nlm.nih.gov/nuccore/',accession);ref
}
exact_positions <- function(pattern,sequence) {
  hits<-gregexpr(paste0('(?=',pattern,')'),sequence,perl=TRUE)[[1]]
  as.integer(hits[hits>0])
}
locate_amplicon <- function(sequence,forward,reverse,forward_tail='',reverse_tail='',dye='') {
  sequence<-clean_dna(sequence);forward<-clean_dna(forward);reverse<-clean_dna(reverse)
  ft<-clean_dna(forward_tail);rt<-clean_dna(reverse_tail)
  base<-list(status='unsupported',reason='',sequence=sequence,forward=forward,reverse=reverse,forward_tail=ft,reverse_tail=rt,dye=dye)
  unsupported<-function(msg) {base$reason<-msg;base}
  if(any(grepl('[^ACGT]',c(forward,reverse,ft,rt))) || nchar(forward)-nchar(ft)<4 || nchar(reverse)-nchar(rt)<4)
    return(unsupported('Partial, degenerate or invalid primer binding is unsupported; retain a draft and review externally.'))
  if(!startsWith(forward,ft) || !startsWith(reverse,rt))return(unsupported('Non-binding tail must be the 5-prime prefix of the actual entered primer.'))
  f<-substring(forward,nchar(ft)+1);r<-substring(reverse,nchar(rt)+1)
  matches<-list()
  for(strand in c(1L,-1L)) {
    seq<-if(strand==1)sequence else reverse_complement(sequence)
    fi<-exact_positions(f,seq);ri<-exact_positions(reverse_complement(r),seq)
    if(length(fi) && length(ri))for(a in fi)for(b in ri)if(b+nchar(r)-1>=a+nchar(f)-1 && b>=a) {
      matches[[length(matches)+1L]]<-list(start=a,end=b+nchar(r)-1,forward_end=a+nchar(f)-1,
        reverse_start=b,strand=strand,oriented_sequence=seq)
    }
  }
  if(!length(matches))return(unsupported('No complete exact primer pair. Mismatch/partial binding is not confirmed; check 5-prime tails, reference type and orientation.'))
  if(length(matches)>1) {base$status<-'ambiguous';base$reason<-paste(length(matches),'possible primer pairs; select a smaller, evidenced reference interval.');base$matches<-matches;return(base)}
  base<-c(base,matches[[1]]);base$status<-'located';base$reason<-'Exact pair on the selected reference only; not a whole-genome specificity check.'
  base$binding_forward<-f;base$binding_reverse<-r;base$amplicon<-substr(base$oriented_sequence,base$start,base$end)
  base$expected_bp<-nchar(base$amplicon)+nchar(ft)+nchar(rt)
  base$reference_coordinates<-if(base$strand==1)c(base$start,base$end) else c(nchar(sequence)-base$end+1,nchar(sequence)-base$start+1)
  base
}
motif_variants <- function(motif) {
  motif<-clean_dna(motif);n<-nchar(motif)
  if(!n || n>12 || grepl('[^ACGT]',motif))stop('Use a nucleotide motif of 1–12 bases.')
  unique(unlist(lapply(c(motif,reverse_complement(motif)),function(m)vapply(0:(n-1),function(i)paste0(substring(m,i+1),if(i)substr(m,1,i) else ''),character(1)))))
}
repeat_candidates <- function(sequence,motif) {
  sequence<-clean_dna(sequence);variants<-motif_variants(motif);n<-nchar(motif);rows<-list()
  for(v in variants) {
    hits<-gregexpr(paste0('(?:',v,'){2,}'),sequence,perl=TRUE)[[1]];lens<-attr(hits,'match.length');ok<-hits>0
    starts<-as.integer(hits[ok]);ends<-starts+lens[ok]-1L
    if(!length(starts))next
    # Merge only short in-phase interruptions; keep both primitive and joined candidates.
    for(i in seq_along(starts)) {
      rows[[length(rows)+1L]]<-data.frame(Start=starts[i],End=ends[i],Motif=v,Kind='perfect tract')
      e<-ends[i]
      if(i<length(starts))for(j in (i+1):length(starts)) {
        gap<-starts[j]-e-1L
        if(gap>2*n || gap<0 || gap%%n!=0)break
        e<-ends[j];rows[[length(rows)+1L]]<-data.frame(Start=starts[i],End=e,Motif=v,Kind='interrupted candidate; review counting rule')
      }
    }
  }
  if(!length(rows))return(data.frame(Start=integer(),End=integer(),Motif=character(),Kind=character()))
  out<-unique(do.call(rbind,rows));out$Length_bp<-out$End-out$Start+1L
  out[order(-out$Length_bp,out$Start),,drop=FALSE]
}
amplicon_candidates <- function(amplicon,motif) {
  if(is.null(amplicon) || amplicon$status!='located')return(data.frame())
  z<-repeat_candidates(amplicon$amplicon,motif)
  if(nrow(z)){z$Start<-z$Start+amplicon$start-1L;z$End<-z$End+amplicon$start-1L
    z<-z[order(-z$Length_bp,z$Motif!=clean_dna(motif),z$Start),,drop=FALSE]}
  rownames(z)<-NULL;z
}
preview_structure <- function(amplicon,candidates,motif,start=NULL,end=NULL,rule='tract_length') {
  if(is.null(amplicon) || amplicon$status!='located' || !nrow(candidates))return(NULL)
  if(is.null(start) || !is.finite(start))start<-candidates$Start[1]
  if(is.null(end) || !is.finite(end))end<-candidates$End[1]
  s<-confirm_structure(amplicon,start,end,rule,'Unconfirmed preview only','Suggested boundaries; not reviewed',motif,candidates)
  s$status<-'preview';s$confirmed_at<-NULL;s
}
project_structure_view <- function(p) {
  if(!is.null(p$structure))return(p$structure)
  d<-p$reference_draft;if(is.null(d))return(NULL)
  tryCatch(preview_structure(d$amplicon,d$suggestions,p$assay$motif,d$start,d$end,d$counting_rule%||%'tract_length'),error=function(e)NULL)
}
confirm_structure <- function(amplicon,start,end,counting_rule,basis,review_reason,motif='CAG',suggestions=NULL) {
  if(amplicon$status!='located')stop('Only one complete exact primer pair can be confirmed.')
  if(!nzchar(trimws(basis)) || !nzchar(trimws(review_reason)))stop('Record the locus counting rule and boundary review basis.')
  if(!is.finite(start) || !is.finite(end) || start%%1 || end%%1 || start<amplicon$start || end>amplicon$end || end<start)stop('Target tract must be fully covered by this amplicon.')
  motif<-clean_dna(motif);variants<-motif_variants(motif);n<-nchar(motif)
  tract<-substr(amplicon$oriented_sequence,start,end)
  if(nchar(tract)%%n!=0)stop('Tract length is not a whole number of motif units. Resolve the counting boundaries before confirmation.')
  unit_starts<-seq.int(1,nchar(tract),by=n);units<-substring(tract,unit_starts,unit_starts+n-1)
  matched<-units%in%variants
  runs<-rle(units)
  pattern<-paste(ifelse(runs$lengths==1L,runs$values,paste0('(',runs$values,')',runs$lengths)),collapse=' ')
  if(!counting_rule%in%c('tract_length','motif_only'))stop('Choose an explicit supported counting rule.')
  counted<-if(counting_rule=='tract_length')nchar(tract) else sum(matched)*n
  if(counted==0)stop('No target repeat units in the chosen tract.')
  list(status='confirmed',amplicon=amplicon,start=as.integer(start),end=as.integer(end),motif=motif,counting_rule=counting_rule,
    basis=basis,review_reason=review_reason,suggestions=suggestions,counted_bp=counted,repeat_units=counted/n,
    expected_bp=amplicon$expected_bp,fixed_bp=amplicon$expected_bp-counted,repeat_pattern=pattern,
    fixed_components=list(left_flank=start-amplicon$start,right_flank=amplicon$end-end,
      noncounted_tract=nchar(tract)-counted,tails=nchar(amplicon$forward_tail)+nchar(amplicon$reverse_tail)),
    forward_overlap=max(0,min(end,amplicon$forward_end)-max(start,amplicon$start)+1),
    reverse_overlap=max(0,min(end,amplicon$end)-max(start,amplicon$reverse_start)+1),
    interruptions=if(any(!matched))paste(paste0(units[!matched],' @ ',start+unit_starts[!matched]-1),collapse='; ') else 'none',
    sequence_hash=object_hash(amplicon$sequence),confirmed_at=stamp())
}
reference_coordinates <- function(amplicon,reference) {
  local<-amplicon$reference_coordinates
  reverse<-reference$strand%in%c(2,-1)
  end<-reference$end%||%((reference$start%||%1)+reference$length-1)
  coordinates<-if(reverse)c(end-local[2]+1,end-local[1]+1) else local+(reference$start%||%1)-1
  list(range=coordinates,strand=amplicon$strand*if(reverse)-1 else 1)
}
structure_svg <- function(s,reference=NULL) {
  if(is.null(s) || !s$status%in%c('confirmed','preview'))return('<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 720 80"><text x="24" y="42" fill="#526b74" font-family="sans-serif">Match a primer pair and repeat tract to show the structure.</text></svg>')
  a<-s$amplicon
  coordinate<-reference_coordinates(a,reference%||%list(start=1,strand=1,length=nchar(a$sequence)))
  x<-function(v)24+672*(v-a$start)/(a$end-a$start+1)
  left<-s$start-a$start;right<-a$end-s$end
  pattern<-s$repeat_pattern%||%paste0('(',s$motif,')n')
  tokens<-strsplit(pattern,' ',fixed=TRUE)[[1]]
  pattern_svg<-paste(ifelse(grepl('^[ACGT]+$',tokens),
    paste0('<tspan fill="#8b4f12" font-weight="bold">',xml_escape(tokens),'</tspan>'),xml_escape(tokens)),collapse=' ')
  reverse_display<-paste(rev(strsplit(a$reverse,'',fixed=TRUE)[[1]]),collapse='')
  track<-function(from,to,y) sprintf(
    '<path d="M %.1f %d H %.1f m %.1f -4 l %.1f 4 l %.1f 4" fill="none" stroke="#087a8a" stroke-width="2.5"/>',
    x(from),y,x(to),-sign(to-from)*7,sign(to-from)*7,-sign(to-from)*7)
  sequence_label<-function(sequence,x_pos,y,anchor) sprintf(
    '<text x="%d" y="%d" text-anchor="%s" font-family="monospace" font-size="17"%s>%s</text>',
    x_pos,y,anchor,if(nchar(sequence)>64)' textLength="672" lengthAdjust="spacingAndGlyphs"' else '',xml_escape(sequence))
  paste0('<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 720 282" role="img" aria-label="Reference primer and repeat structure">',
    '<title>',xml_escape(pattern),'</title>',
    '<rect width="720" height="282" rx="10" fill="#f7fafb"/><g font-family="sans-serif" fill="#17313a" font-size="17">',
    '<text x="24" y="27" font-size="14" fill="#526b74">',xml_escape(reference$accession%||%'Offline reference'),
    ' · ',paste(coordinate$range,collapse='–'),' · strand ',if(coordinate$strand==1)'+' else '−','</text>',
    '<text x="24" y="55">Forward primer</text>',sequence_label(paste('5′',a$forward,'3′'),24,76,'start'),
    track(a$start,a$forward_end+1,89),
    '<text x="24" y="109" font-size="14">5′</text><text x="696" y="109" font-size="14" text-anchor="end">3′</text>',
    '<rect x="24" y="116" width="672" height="18" rx="4" fill="#dce9ed"/>',
    sprintf('<rect x="%.1f" y="116" width="%.1f" height="18" fill="#86cbbd"/>',x(s$start),x(s$end+1)-x(s$start)),
    '<text x="24" y="155">Left flank · ',left,' bp</text>',
    '<text x="360" y="155" text-anchor="middle">Repeat region · ',s$end-s$start+1,' bp</text>',
    '<text x="696" y="155" text-anchor="end">Right flank · ',right,' bp</text>',
    '<text x="360" y="176" text-anchor="middle" font-size="15" fill="#226254"',
    if(nchar(pattern)>70)' textLength="672" lengthAdjust="spacingAndGlyphs"' else '', '>',pattern_svg,'</text>',
    track(a$end+1,a$reverse_start,187),
    sequence_label(paste('3′',reverse_display,'5′'),696,208,'end'),'<text x="696" y="231" text-anchor="end">Reverse primer</text>',
    '<text x="24" y="261">Expected amplicon · ',s$expected_bp,' bp</text>',
    '<text x="696" y="261" text-anchor="end" font-size="13" fill="#526b74">',
    if(s$status=='preview')'UNCONFIRMED counting' else 'Confirmed counting','</text></g></svg>')
}
