library(shiny)
library(anytime)
library(data.table)
library(plotly)

# load csv and take value by 1 second
load_csv <- function(file) {
   if(is.null(file)) return(NULL)
   data <- read.csv(file, header = FALSE)
   specificColumns <- data.table(time = floor(data$V1/1000), bpm = data$V3)
   means = specificColumns[, mean(bpm), by=time]
}

# create plot from two input time lines
create_plot <- function(time_line1, time_line2){
   if (is.null(time_line1) || is.null(time_line2)) return(NULL)
   
   max <- max(c(max(time_line1$V1), max(time_line2$V1)))
   min <- min(c(min(time_line1$V1), min(time_line2$V1)))
   
   plot_ly(y = ~time_line1$V1, name = 'trace 0', type = 'scatter', mode = 'lines')%>%
      add_trace(y = ~time_line2$V1, name = 'trace 1', mode = 'lines')
}

# create plot of residuas from two input time lines
create_resi_plot <- function(time_line1, time_line2){
   if (is.null(time_line1) || is.null(time_line2)) return(NULL)
   
   residua <- data.table(time = time_line1$time, bpm = time_line1$V1 - time_line2$V1)
   
   plot_ly(residua, y = ~bpm, name = 'trace 0', type = 'scatter', mode = 'lines')
}




function(input, output, session) {
   
   # observe submit button to switch on next panel in navbar
   observeEvent(input$submitBtn, {
      if(is.null(inputLow1()) || is.null(inputLow2()) || is.null(inputMed1()) || is.null(inputMed2()) || is.null(inputHig1()) || is.null(inputHig2())){
         return(NULL)
      }
      updateNavbarPage(session, "navbar", selected = "tabPlot")
   })
   
   
   # load files when change associated fileInput
   inputLow1 <- reactive({
      load_csv(input$inFileLow1$datapath)
   })
   inputMed1 <- reactive({
      load_csv(input$inFileMed1$datapath)
   })
   inputHig1 <- reactive({
      load_csv(input$inFileHig1$datapath)
   })
   inputLow2 <- reactive({
      load_csv(input$inFileLow2$datapath)
   })
   inputMed2 <- reactive({
      load_csv(input$inFileMed2$datapath)
   })
   inputHig2 <- reactive({
      load_csv(input$inFileHig2$datapath)
   })
   
   
   # render plots with two lines
   output$plotLow <- renderPlotly({
      create_plot(inputLow1(), inputLow2())
   })
   output$plotMed <- renderPlotly({
      create_plot(inputMed1(), inputMed2())
   })
   output$plotHig <- renderPlotly({
      create_plot(inputHig1(), inputHig2())
   })
   
   
   # render plots with residuas
   output$plotResiLow <- renderPlotly({
      create_resi_plot(inputLow1(), inputLow2())
   })
   output$plotResiMed <- renderPlotly({
      create_resi_plot(inputMed1(), inputMed2())
   })
   output$plotResiHig <- renderPlotly({
      create_resi_plot(inputHig1(), inputHig2())
   })
   
   
   
   
   
   output$boxPlot <- renderPlot({
      means1 <- inputLow1()
      means2 <- inputLow2()
      if (is.null(means1) || is.null(means2)) return
      
      residua <- data.table(time = means1$time, bpm = means1$V1 - means2$V1)
      
      boxplot(means1$V1)
   })
   
   output$table <- renderTable({
      means1 <- inputLow1()
      if (is.null(means1)) return
      
      data.table(time = anytime(means1$time), bpm = means1$V1)
   })
}