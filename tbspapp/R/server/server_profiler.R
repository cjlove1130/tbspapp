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
