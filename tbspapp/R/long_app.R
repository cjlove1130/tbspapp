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
                           tabPanel("Upload Data",
                                    tags$div(
                                      class = "jumbotron", style = "background-color:#ededed",
                                      tags$div(
                                        class = "container",
                                        h1("TB Signature Profiler"),
                                        p("Score, compare and visualize gene signatures"),
                                      )
                                    ),
                                    sidebarPanel(
                                      radioButtons("dat", "Select Data", choices = c("Sample Data"="samp", "Upload Data"="updat")),
                                       conditionalPanel(condition = "input.dat=='samp'",
                                          radioButtons("sampdat", "Sample Datasets", choices = c('TB HIV'='tbhiv', 'TB Indian'='tbind'), selected = NULL)
                                         ),
                                      conditionalPanel(condition = "input.dat=='updat'",
                                          radioButtons("updattype","Choose File Type", choices=c('Tab Separated Text Files'='rawdat', 'Summarized Experiment'='sumexp'), selected = NULL)
                                      ),
                                      hr(),
                                      actionButton('upload', 'Upload')
                                    ),
                                    mainPanel(
                                    conditionalPanel(condition="input.dat=='updat'",
                                      conditionalPanel(condition= "input.updattype=='rawdat'",
                                                       column(width = 4,
                                                              wellPanel(
                                                                h4("Upload Count File"),
                                                                fileInput(
                                                                  "countsfile",
                                                                  HTML(
                                                                    paste("Input assay (eg. counts, required):",
                                                                          tags$span(style = "color:red", "*", sep = ""))
                                                                  ),
                                                                  accept = c(
                                                                    "text/csv", "text/comma-separated-values",
                                                                    "text/tab-separated-values", "text/plain", ".csv", ".tsv"
                                                                  )
                                                              )
                                                        )
                                                    ),
                                                    column(width = 4,
                                                           wellPanel(
                                                             h4("Upload Meta Data File"),
                                                             fileInput(
                                                               "metdatfile",
                                                               HTML(
                                                                 paste("Meta Data (eg. disease, required):",
                                                                       tags$span(style = "color:red", "*", sep = ""))
                                                               ),
                                                               accept = c(
                                                                 "text/csv", "text/comma-separated-values",
                                                                 "text/tab-separated-values", "text/plain", ".csv", ".tsv"
                                                               )
                                                             )
                                                           )
                                                    ),
                                                    selectInput("datassay", label = "Input Assay Type:",
                                                                choices = c("Counts"="counts","Log Counts"='log', "CPM"='cpm', "Log CPM"='logcpm'))
                                      ),
                                      conditionalPanel(condition= "input.updattype=='sumexp'",
                                                       h3("Choose an RDS Summarized Experiment file that contains a TBSignature Object:"),
                                                       fileInput(
                                                         "rdsFile", "TBSignature RDS file:", accept = c(".rds", ".RDS")
                                                       )
                                      )
                                    )
                                   )
                           ),
                           tabPanel("Run TB Signature Profiler",
                                    sidebarPanel(
                                      pickerInput("newassay", label = "Make Assay",
                                                  choices = c("Log Counts"='log', "CPM"='cpm', "Log CPM"='logcpm')),
                                      actionButton("mkassay", "Make Assay"),
                                      hr(),
                                      selectInput("profassay", "Select Assay for Profiler", choices = NULL),
                                      selectInput('profalg', "Select Algorithm for Profiler", choices = c("GSVA", "ssGSEA", "ASSIGN", "PLAGE", "Zscore", "singscore")),
                                      actionButton('runprofiler', "Run Profiler")
                                    ),
                                    mainPanel(
                                      h4("Covariates"),
                                      tableOutput("covars"),
                                      h4("Assays"),
                                      tableOutput("assays")
                                    )
                           ),

                           navbarMenu("Visualization",
                                      tabPanel("Heatmap with Selected Signatures",
                                               sidebarPanel(
                                                 pickerInput('selectsigs', label='Select Signatures for Profiler',choices=siglist_hivtb, options=list('actions-box'=T),multiple=T, selected = siglist_hivtb),
                                                 selectInput('allheatcovar', "Select Covariate", choices = NULL),
                                                 actionButton('allheatplot', "Plot Heatmap")
                                               ),
                                               mainPanel(
                                                 plotOutput("allheat", height = 800)
                                               )
                                      ),
                                      tabPanel("Heatmaps of Individual Signatures",
                                               sidebarPanel(
                                                 pickerInput('singheat', 'Signature(s)', choices =siglist_hivtb,multiple=F, selected =NULL),
                                                 pickerInput('genes', label='Gene(s)',choices=NULL, options=list('actions-box'=T),multiple=T, selected = NULL),
                                                 hr(),
                                                 selectInput('singheatcovar', 'Covariate', choices =NULL),
                                                 actionButton('singheatplot', "Plot Heatmap(s)")
                                               ),
                                               mainPanel(
                                                 plotOutput("indheat", height = 800)
                                               )
                                      ),
                                      tabPanel("Boxplots of Individual Signatures",
                                               sidebarPanel(
                                                 pickerInput('singbox', 'Signature(s)', choices =siglist_hivtb, options=list('actions-box'=T),multiple=T, selected =NULL),
                                                 hr(),
                                                 selectInput('singboxcovar', 'Covariate', choices =NULL),
                                                 actionButton('singboxplot', "Plot Heatmap(s)")
                                               ),

                                               mainPanel(
                                                 plotOutput("boxplotind", height = 500)
                                               )
                                      ),
                                      tabPanel("Compare Scoring Methods for a Single Signature",
                                               sidebarPanel(
                                                 selectInput('singcomp', 'Select Signature', choices =siglist_hivtb ),
                                                 selectInput('compassay', 'Select Assay', choices = NULL),
                                                 selectInput('compcovar', "Select Covariate", choices = NULL),
                                                 pickerInput('compalg', 'Select Algorithms', choices = c("GSVA", "ssGSEA", "singscore", "PLAGE", "Zscore"),options=list('actions-box'=T),multiple=T, selected =NULL),
                                                 actionButton('compplot', "Plot Heatmap")
                                               ),
                                               mainPanel(
                                                 plotOutput("heatcomp", height = 400)
                                               )
                                      )
                           ),
                           navbarMenu("Comparing Scores Via Bootstrapped AUCs",
                                      tabPanel("T-tests & AUC",
                                               DT::dataTableOutput("ttest") %>% withSpinner()
                                      ),
                                      tabPanel("Boxplots for the bootstrapped AUCs",
                                               sidebarPanel(
                                                 pickerInput('bootsigs', "Select Signatures", choices =siglist_hivtb, options=list('actions-box'=T),multiple=T, selected =NULL),
                                                 actionButton('bootplot', "Plot Bootstrapped AUCs")
                                               ),
                                               mainPanel(plotOutput("bootbox", height = 600)
                                                )
                                      ),
                                      tabPanel("Separate ROC plots, 95% CI Bands",
                                               sidebarPanel(
                                                 pickerInput('singroc', 'Select Signature(s)', choices =siglist_hivtb, options=list('actions-box'=T),multiple=T, selected =NULL),
                                                 actionButton("rocplot", "Plot ROC Curve")
                                               ),
                                               mainPanel(
                                                 plotOutput("rocsep", height = 450)
                                               )
                                      )
                           )
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

  observeEvent(input$upload,{
    if(input$dat=='samp'){
      if(input$sampdat=='tbhiv'){
        vals$tbdat=TB_hiv
        vals$coldat=colData(vals$tbdat)
        vals$covars=colnames(colData(vals$tbdat))
        vals$datassays=names(assays(vals$tbdat))
    }
      else if(input$sampdat=='tbind'){
        vals$tbdat=TB_indian
        vals$coldat=colData(TB_indian)
        vals$covars=colnames(colData(vals$tbdat))
        vals$datassays=names(assays(vals$tbdat))
      }
    }
    else if (input$dat=='updat'){
      if(input$updattype=='rawdat'){
        countdat=read.table(input$countsfile$datapath, header=T, row.names = 1)
        metadat=read.table(input$metdatfile$datapath, header=T)
        vals$tbdat=SummarizedExperiment(assays = list(counts= as.matrix(countdat)),colData=metadat)
        vals$coldat=colData(vals$tbdat)
        vals$covars=colnames(colData(vals$tbdat))
        vals$datassays=names(assays(vals$tbdat))
      }
      else if (input$updattype=='sumexp'){
        tb1=get(load(input$rdsFile$datapath))
        vals$tbdat=print(tb1)
        vals$coldat=colData(vals$tbdat)
        vals$covars=colnames(colData(vals$tbdat))
        vals$datassays=names(assays(vals$tbdat))
      }
    }
  })

  output$covars=renderTable({
    vals$covars
  }, colnames = F)

  output$assays=renderTable({
    vals$datassays
  }, colnames = F)


  observeEvent(input$mkassay,{
    if(input$newassay=='log'){
      vals$tbdat=mkAssay(vals$tbdat, input_name = 'counts', log = T, counts_to_CPM = F)
      vals$datassays=names(assays(vals$tbdat))
    }
    else if(input$newassay=='cpm'){
      vals$tbdat=mkAssay(vals$tbdat, input_name = 'counts')
      vals$datassays=names(assays(vals$tbdat))
    }
    else if(input$newassay=='logcpm'){
      vals$tbdat=mkAssay(vals$tbdat, input_name = 'counts', log = T)
      vals$datassays=names(assays(vals$tbdat))
    }
  })

  observe({
    updateSelectInput(session, 'profassay', choices = vals$datassays)
    })

  observeEvent(input$runprofiler,{
    vals$profilerdat=runTBsigProfiler(vals$tbdat, useAssay = input$profassay, signatures = TBsignatures[input$selectsigs], algorithm = input$profalg, combineSigAndAlgorithm = T, parallel.sz = 4)
    withProgress(message = 'Running Profiler', value = 0, {
      # Number of times we'll go through the loop
      n <- 34

      for (i in 1:n) {


        incProgress(1/n, detail = paste("Doing Signature", i))

        # Pause for 0.1 seconds to simulate a long computation.
        Sys.sleep(0.05)
      }
    })
  })

  observe({
    updateSelectInput(session, 'allheatcovar', choices = vals$covars)
  })

  observeEvent(input$allheatplot,
               {output$allheat=renderPlot({
    colors <- RColorBrewer::brewer.pal(6, "Spectral")
    col.me <- circlize::colorRamp2(seq(from = -2, to = 2,
                                       length.out = 6), colors)

    signatureHeatmap(vals$profilerdat, name = "Heatmap of Signatures",
                     signatureColNames = names(TBsignatures[input$selectsigs]),
                     annotationColNames = input$allheatcovar,
                     scale = TRUE,
                     showColumnNames = TRUE,
                     choose_color = col.me)
  })})

  observe({
    updateSelectInput(session, 'singheatcovar', choices = vals$covars)
  })

  observe({
    updatePickerInput(session, 'genes', choices = TBsignatures[input$singheat])
  })

  observeEvent(input$singheatplot,
               {output$indheat=renderPlot({
                 print(signatureGeneHeatmap(inputData = vals$profilerdat, useAssay = input$profassay,
                                      input$genes,
                                      signatureColNames = input$singheat,
                                      annotationColNames = input$singheatcovar,
                                      showColumnNames = TRUE))
  })})

  observe({
    updateSelectInput(session, "singboxcovar", choices = vals$covars)
  })

  observeEvent(input$singboxplot,
               {output$boxplotind=renderPlot({
                  print(signatureBoxplot(vals$profilerdat, signatureColNames = input$singbox,
                           annotationColName = input$singboxcovar))
  })})

  observe({
    updateSelectInput(session, "compassay", choices = vals$datassays)
  })

  observe({
    updateSelectInput(session, "compcovar", choices = vals$covars)
  })

  observeEvent(input$compplot,
               {output$heatcomp=renderPlot({
                  suppressWarnings(compareAlgs(vals$profilerdat, annotationColName = input$compcovar,
                                 scale = TRUE,
                                 algorithm = input$compalg,
                                 useAssay = input$compassay,
                                 signatures = TBsignatures[input$singcomp],
                                 show.pb = TRUE))
  })})

  output$ttest=renderDataTable({
    tableAUC(vals$profilerdat,
             annotationColName = "Disease",
             signatureColNames = names(TBsignatures),
             num.boot = 100,
             pb.show = FALSE)
  })

  observeEvent(input$bootplot,{
    output$bootbox=renderPlot({
                 compareBoxplots(vals$profilerdat, annotationColName = "Disease",
                    signatureColNames = names(TBsignatures[input$bootsigs]),
                    pb.show = FALSE, rotateLabels = T)
     })
  })



  observeEvent(input$rocplot,{
    output$rocsep=renderPlot({
                  print(signatureROCplot_CI(inputData = vals$profilerdat,
                              signatureColNames = input$singroc,
                              annotationColName = "Disease",
                              name = paste("ROC plot")))
    })
  })

}

shinyApp(ui, server)
