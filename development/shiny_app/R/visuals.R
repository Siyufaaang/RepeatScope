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

methodology_ui <- function() tags$section(class='landing-methodology',id='methodology',
  p(class='section-kicker','Methodology'),h2('One signal. Two tail assignments.'),
  p(class='method-intro','Adaptive and Bayesian assign repeat-step indices to physical peaks to the right of the same target modal. Both then calculate the same height-weighted expansion index. Repeat number describes modal allele length; EI describes its selected right-hand tail.'),
  div(class='method-equation',HTML('EI = Σ(H<sub>k</sub> × k) / ΣH<sub>k</sub>'),
    p('k = repeat steps from the modal (modal k = 0); H = that physical peak’s own RFU height. Include the modal and assigned peaks meeting H ≥ p × modal height within the selected EI window. Default p = 0.03. No area weighting, background subtraction or posterior-probability weighting.')),
  div(class='method-columns',
    tags$article(h3('01 / Adaptive EI'),
      tags$ol(tags$li('Start at the shared modal m with nominal spacing s (default 3 bp).'),
        tags$li('Predict the next repeat position m + k × s. Search unused physical peaks within ± tolerance (default 0.8 bp). Choose the highest peak; ties use closest position, then smaller size. A missing step is skipped.'),
        tags$li(HTML('After each accepted tail peak, update spacing by modal-anchored least squares: s = Σ[k<sub>i</sub>(Size<sub>i</sub> − m)] / Σk<sub>i</sub>², constrained to the configured bounds (default 2.5–3.2 bp).')),
        tags$li('After assignment, apply the chosen height threshold and window, then calculate EI. The threshold does not refit the walk.'))),
    tags$article(h3('02 / Bayesian EI'),
      tags$ol(tags$li('Model distance from the modal as Sizeₖ − m = k × s + ε, with normal residuals and a Normal–Inverse-Gamma prior for spacing and residual variance.'),
        tags$li('Calibrate with consecutive early tail seeds (default at least 3, at most 5). Each seed must fit its sequential predictive window, meet max(100 RFU, 3% of modal), and dominate a competing peak by at least 3×. Calibration stops at the first failed step.'),
        tags$li('Freeze the posterior fitted to those seeds. For later repeat positions, use pointwise Student-t posterior predictive intervals (default 95%). Accept a single candidate; multiple, overlapping or excessively wide windows remain unresolved.'),
        tags$li('Apply the same final height threshold and EI window. An unresolved in-scope assignment can make EI unavailable; unavailable is not zero.')))),
  tags$details(class='method-math',tags$summary('Bayesian posterior and reproducibility details'),
    p(HTML('Let y<sub>i</sub> = Size<sub>i</sub> − m. Prior: σ² ~ Inv-Gamma(α₀, β₀), s | σ² ~ Normal(μ₀, σ²/κ₀). Defaults: μ₀ = configured nominal spacing, κ₀ = 0.05, α₀ = 2, β₀ = 0.01.')),
    p(class='math-line',HTML('κ = κ₀ + Σk<sub>i</sub>²; μ = (κ₀μ₀ + Σk<sub>i</sub>y<sub>i</sub>)/κ; α = α₀ + n/2')),
    p(class='math-line',HTML('β = β₀ + ½Σ(y<sub>i</sub> − k<sub>i</sub>μ)² + ½κ₀(μ − μ₀)²')),
    p(class='math-line','Predictive centre: m + kμ; scale: √[(β/α)(1 + k²/κ)]; degrees of freedom: 2α.'),
    p('Intervals use the configured two-sided coverage. These are position-prediction intervals conditional on seeds and the chosen modal, not the probability that a peak is real, a repeat-count confidence interval, or an EI confidence interval.'),
    p('Shared support is frozen at fitting: by default, the smaller of the largest observed candidate size and max(modal + 33 bp, rightmost candidate ≥3% of modal + 1.5 bp). An explicit upper bp overrides the envelope but remains capped by observed input. Tail10 retains k = 0…10; Extended uses the frozen support. Changing the reporting height threshold does not change seeds, spacing or support.'),
    p('Every export records the actual assay parameters, method versions, run/input fingerprints and physical peak decisions. Defaults above are software settings, not a claim that they suit every assay.')),
  div(class='method-repeat',h3('Repeat number ≠ expansion index'),
    p(class='math-line','Estimated repeats = (modal size in bp − mapping intercept in bp) / mapping slope in bp per repeat'),
    p('Reference mapping uses the matched primer/reference fixed length and counted repeat unit. Measured CE mapping uses the supplied calibration intercept and slope. Normal, expanded and intermediate targets each retain their own estimate; both EI methods share it. Missing targets are not invented.'),
    p('Supplied CE ±bp margins are propagated to repeat units without calling them confidence intervals. Unknown uncertainty stays unknown. Entered repeat evidence is exported separately. These nominal estimates do not confirm sequence, genotype, target category or formal QC clearance.')),
  p(class='method-guide',tags$a(href='input-guide-zh.html',target='_blank',rel='noopener','完整输入说明 · 中文 ↗')))

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
    methodology_ui(),
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
