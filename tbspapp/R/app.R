library(shiny)
library(shinyWidgets)
library(shinythemes)
library(markdown)
library(DT)
library(BiocManager)
library(SummarizedExperiment)
library(TBSignatureProfiler)
library(shinycssloaders)

siglist_hivtb=names(TBsignatures)

ui <- fluidPage(theme=shinytheme('cosmo'),
                navbarPage("TB Signature Profiler",
                           source(file.path('ui', 'ui_upload.R'), local=T)$value,
                           source(file.path('ui', 'ui_profiler.R'), local=T)$value,
                           source(file.path('ui', 'ui_visualization.R'), local=T)$value,
                           source(file.path('ui', 'ui_auc.R'), local=T)$value
                )


)

server <- function(input, output, session) {

  vals=reactiveValues(
    tbdat=NULL,
    coldat=NULL,
    covars=NULL,
    datassays=NULL,
    profilerdat=NULL
  )

  source(file.path('server', 'server_upload.R'), local=T)$value
  source(file.path('server', 'server_profiler.R'), local=T)$value
  source(file.path('server', 'server_visualization.R'), local=T)$value
  source(file.path('server', 'server_auc.R'), local=T)$value

}

shinyApp(ui, server)
