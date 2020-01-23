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
)
