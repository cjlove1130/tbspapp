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
