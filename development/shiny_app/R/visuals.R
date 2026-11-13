# Presentation only. This module never reads or changes analysis results.
brand_mark <- function() tags$span(class='brand-mark',tags$img(src='brand-logo.png',alt='RepeatScope snake logo',width=154,height=77))

# Visible demo links request a transition; the existing data action runs only
# after acknowledgement when the session already contains a working project.
register_demo_navigation <- function(input,session,rv) {
  pending<-reactiveVal(FALSE)
  load<-function(){pending(FALSE);session$sendCustomMessage('demo-load',TRUE)}
  observeEvent(input$request_demo,{
    p<-rv$p
    occupied<-nrow(p$peaks)>0L || any(nzchar(p$plate$Code)) || !is.null(p$reference)
    if(!occupied){load();return()}
    pending(TRUE)
    showModal(modalDialog(title='Open the demo?',
      p('Demo data will replace your current project in this session. Save your project first if you want to keep it.'),
      footer=tagList(tags$button(type='button',class='btn btn-default','data-trigger'='download_project','Save current project'),
        modalButton('Cancel'),actionButton('confirm_demo','Replace with demo',class='btn-primary')),easyClose=TRUE),session=session)
  })
  observeEvent(input$confirm_demo,{
    if(!isTRUE(pending()))return()
    removeModal(session=session);load()
  })
}

landing_signal <- function(mosaic=FALSE) {
  x<-seq(0,1200,2);centers<-c(210,650+seq(0,8)*38)
  heights<-c(125,175,120,83,57,39,26,17,10,5)
  y<-225-vapply(x,function(v)max(heights*exp(-.5*((v-centers)/5.5)^2)),numeric(1))
  path<-paste0('M',paste(sprintf('%.1f,%.1f',x,y),collapse=' L'))
  dots<-''
  if(mosaic) {
    grid<-expand.grid(x=seq(5,1195,13),y=seq(23,238,11))
    grid$alpha<-.42*exp(-.5*((grid$y-(150+35*sin(grid$x/115)))/48)^2)
    grid$size<-ifelse((seq_len(nrow(grid))%%7)==0,2.8,1.4)
    dots<-paste(sprintf('<rect x="%g" y="%g" width="%g" height="%g" fill="white" opacity="%.3f"/>',grid$x,grid$y,grid$size,grid$size,grid$alpha),collapse='')
  }
  HTML(paste0('<svg class="signal-art" viewBox="0 0 1200 280" role="img" aria-label="Illustrative signal: a single normal allele peak and an expanded modal peak followed by a decreasing tail"><title>Illustrative normal and expanded allele signal</title>',dots,
    '<path d="',path,'" fill="none" stroke="currentColor" stroke-width="2.5" vector-effect="non-scaling-stroke"/>',
    '<text x="210" y="268" text-anchor="middle">Normal allele</text><text x="790" y="268" text-anchor="middle">Expanded allele</text></svg>'))
}

landing_step_art <- function(kind) {
  inside<-switch(kind,
    plate=paste(vapply(0:3,function(r)paste(vapply(0:5,function(c)sprintf('<rect x="%d" y="%d" width="9" height="9"/>',18+c*15,18+r*15),character(1)),collapse=''),character(1)),collapse=''),
    arrange=paste0('<path d="M12 14H115V39H12ZM12 48H115V73H12Z" stroke-dasharray="3 3"/>',paste(vapply(0:1,function(r)paste(vapply(0:5,function(c)sprintf('<rect x="%d" y="%d" width="9" height="9" %s/>',20+c*15,22+r*34,if(c<3)'fill="currentColor"' else ''),character(1)),collapse=''),character(1)),collapse='')),
    peaks='<path d="M5 74H19L21 73L24 22L27 73H61L64 12L67 74H70L73 34L76 74H80L83 47L86 74H90L93 58L96 74H100L103 65L106 74H111L114 70L117 74H125"/>',
    reference='<path d="M5 42H125M5 58H125M8 28H39L34 24M39 28L34 32M121 73H89L94 69M89 73L94 77M12 42V58M18 42V58M24 42V58M30 42V58M100 42V58M106 42V58M112 42V58M118 42V58"/><rect x="44" y="36" width="43" height="28" fill="var(--canvas)"/><text x="65" y="54" text-anchor="middle">(CAG)n</text><text x="6" y="17">Fwd</text><text x="97" y="88">Rev</text>',
    results='<path d="M4 36H17L20 9L23 36H62L65 6L68 36H72L75 19L78 36H82L85 26L88 36H93L96 31L99 36H125M6 47H124V88H6ZM6 60H124M6 74H124M57 47V88M91 47V88"/><text x="12" y="57">Group</text><text x="64" y="57">bp</text><text x="98" y="57">RFU</text><text x="12" y="71">Normal</text><text x="12" y="85">Expanded</text>')
  HTML(paste0('<svg viewBox="0 0 130 96" aria-hidden="true" fill="none" stroke="currentColor" stroke-width="1.4">',inside,'</svg>'))
}

landing_ui <- function() {
  steps<-c('Import plate','Arrange plate','Import peaks','Reference & analysis','Results')
  kinds<-c('plate','arrange','peaks','reference','results')
  div(class='rs-landing',id='landing-top',
    tags$section(class='landing-hero',
      div(class='landing-copy',h1('Read the signal.',tags$br(),'Explore the tail.'),
        p('Repeat-tail analysis, from your plate to a clear comparison.')),
      landing_signal(TRUE),
      div(class='landing-actions',tags$a(href='#how-it-works',class='landing-how','How it works ↓'),
        div(actionButton('request_demo','Try demo data ↗',class='landing-demo'),p('Explore a synthetic example. No files needed.')))),
    actionButton('load_demo','Load confirmed demo',style='display:none',tabindex='-1','aria-hidden'='true'),
    tags$section(class='landing-howto',id='how-it-works',
      p(class='section-kicker','How it works'),h2('From plate to comparison.'),
      div(class='landing-steps',lapply(seq_along(steps),function(i)div(class='landing-step',
        span(class='step-number',sprintf('%02d',i)),h3(steps[i]),landing_step_art(kinds[i])))),
      p(class='workflow-description','Import your plate and peaks, set the reference, compare both methods.')),
    tags$section(class='landing-explore',id='demo-data',
      div(class='demo-section',h2('See demo data'),div(class='landing-media demo-media',landing_signal()),
        tags$button(type='button','data-trigger'='request_demo',class='btn landing-demo-button','Try demo data ↗')),
      div(class='video-section',h2('Watch walkthrough'),
        if(file.exists(file.path(rs_app_dir,'www/walkthrough.mp4')))tags$video(class='landing-media',controls=NA,preload='none','aria-label'='RepeatScope walkthrough',tags$source(src='walkthrough.mp4',type='video/mp4')) else
          div(class='landing-media video-reservation',tags$span(class='video-play','aria-hidden'='true','▷'),p('Your walkthrough video'),span(class='video-status','Coming soon')))),
    tags$details(class='entry-options',tags$summary('Continue a saved project'),
      section_card('Reopen a project',subtitle='Upload a saved RepeatScope project to restore its inputs, settings and results.',
        fileInput('project_file','RepeatScope project',accept='.rds'),actionButton('restore_project','Restore project'),
        p(class='status-small','A report ZIP is a separate export and cannot be restored as a project.'))))
}
