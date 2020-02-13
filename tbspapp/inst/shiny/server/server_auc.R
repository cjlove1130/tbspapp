output$ttest=renderDataTable({
  tableAUC(vals$profilerdat,
           annotationColName = "Disease",
           signatureColNames = names(TBsignatures),
           num.boot = 100,
           pb.show = FALSE)
})

observe({
  if(is.null(vals$profilerdat)){
    updatePickerInput(session, 'bootsigs', choices=NULL)
  }
  else{updatePickerInput(session, 'bootsigs', choices = subset(siglist_hivtb, siglist_hivtb %in% colnames(colData(vals$profilerdat))))
  }
})

observeEvent(input$bootplot,{
  output$bootbox=renderPlot({
    isolate({compareBoxplots(vals$profilerdat, annotationColName = "Disease",
                    signatureColNames = names(TBsignatures[input$bootsigs]),
                    pb.show = FALSE, rotateLabels = T)})
  })
})

observe({
  if(is.null(vals$profilerdat)){
    updatePickerInput(session, 'singroc', choices=NULL)
  }
  else{updatePickerInput(session, 'singroc', choices = subset(siglist_hivtb, siglist_hivtb %in% colnames(colData(vals$profilerdat))))
  }
})

observeEvent(input$rocplot,{
  output$rocsep=renderPlot({
    isolate({print(signatureROCplot_CI(inputData = vals$profilerdat,
                              signatureColNames = input$singroc,
                              annotationColName = "Disease",
                              name = paste("ROC plot")))})
  })
})
