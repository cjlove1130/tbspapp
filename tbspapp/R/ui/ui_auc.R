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
