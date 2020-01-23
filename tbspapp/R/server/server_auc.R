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
