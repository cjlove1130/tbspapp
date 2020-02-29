TBsigProfilerinterface= function() {
  appDir <- system.file("shiny", package = "tbspapp")
  shiny::runApp(appDir, display.mode = "normal")
}
