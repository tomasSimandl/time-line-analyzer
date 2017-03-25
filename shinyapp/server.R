library(shiny)
library(anytime)
library(data.table)
library(plotly)
# =================================================================================================================================
# ========================================================= DATA READING ==========================================================
# =================================================================================================================================

load_data <- function(file, deviceSelect, startTime, endTime, timeInterval, timeShift){
   data <- switch(deviceSelect,
      chest_strap = load_chest_strap_csv(file, timeShift),
      garmin = load_garmin_tcx(file, timeShift),
      basis = load_basis_csv(file, timeShift),
      fitbit = load_fitbit_csv(file, timeShift),
      return(NULL))
   
   data_sampling(data, get_time(startTime), get_time(endTime), timeInterval)
   
}

load_garmin_tcx <- function(file, timeShift){
   if(is.null(file)) return(NULL)
   
   soubor <- xmlParse(file)
   data <- xmlToDataFrame(getNodeSet(soubor, "//ns:Trackpoint", "ns"))
   data.table(time = as.numeric(format(strptime(x = data$Time, format = "%Y-%m-%dT%H:%M:%S"), format = "%s")) + timeShift, bpm = as.numeric(as.character(data$HeartRateBpm)))
}

load_basis_csv <- function(file, timeShift){
   columnsNames <- c("", "time", "", "", "bpm", "", "")
   
   data <- load_csv(file, columnsNames, header = TRUE)
   data.table(time = as.numeric(format(as.POSIXct(data$time), format = "%s")) + timeShift, bpm = data$bpm)
}

load_fitbit_csv <- function(file, timeShift){
   columnsNames <- c("date", "time", "bpm")
   
   data <- load_csv(file, columnsNames, header = FALSE)
   data.table(time = as.numeric(format(as.POSIXct(paste(data$date, data$time, sep = " ")), format = "%s")) + timeShift, bpm = data$bpm)
}

load_chest_strap_csv <- function(file, timeShift) {
   columnsNames <- c("time", "", "bpm", "", "", "", "", "", "")
   
   data <- load_csv(file, columnsNames, header = FALSE)
   data_by_sec <- data.table(time = floor(data$time/1000) + timeShift, bpm = data$bpm)[, mean(bpm), by=time]
   data.table(time = data_by_sec$time, bpm = data_by_sec$V1) # TODO
}

load_csv <- function(file, columnsNames, header = FALSE) {
   if(is.null(file)) return(NULL)
   
   read.csv(file, header = header, col.names = columnsNames)
}

data_sampling <- function(data, startTime, endTime, timeInterval){
   sum <- 0
   counter <- 0
   sumStartTime <- startTime
   i <- 1L
   sequence <- seq(startTime, endTime, timeInterval)
   size <- length(sequence)
   DT <- data.table(time = sequence, bpm = numeric(size))
   
   for (n in 1:nrow(data)){
      curTime <- data[n]$time
      if(curTime < startTime) next
      if(curTime > endTime) break
      if(curTime == startTime){
         set(DT, i, "bpm", data[n]$bpm)
         i <- i + 1L
         next
      }
      
      if(sumStartTime + timeInterval < curTime){
         sumStartTime <- sumStartTime + timeInterval
         if (counter != 0){
            set(DT, i, "bpm", sum/counter)
         }
         
         counter <- 0
         sum <- 0
         i <- i + 1L
      }
      while (sumStartTime + timeInterval < curTime){
         sumStartTime <- sumStartTime + timeInterval
         i <- i + 1L
      }
      
      sum <- sum + data[n]$bpm
      counter <- counter + 1
   }
   if (size <= i){
      if(counter != 0){
         value <- sum/counter
      }else{
         value <- data[n]$bpm
      }
      set(DT, i, "bpm", value)
   }
   DT
}

# =================================================================================================================================
# ========================================================== CALCULATION ==========================================================
# =================================================================================================================================

calculate_calculation <- function(time_line1, time_line2, residua){
   if(is.null(time_line1) || is.null(time_line2) || is.null(residua)) return(NULL)
   
   dispersion <- calculate_dispersion(residua)
   std_dev <- sqrt(dispersion)
   corelation <- cor(x = time_line1$bpm, y = time_line2$bpm, method = c("pearson"))
   
   data.table(
      c("Error dispersion", "Error SD", "Corelation"),
      c(dispersion, std_dev, corelation)
   )
}

calculate_quantile <- function(time_line1, time_line2, time_line3, time_line4, name1, name2, name3, name4){
   quantile1 <- quantile(time_line1)
   quantile2 <- quantile(time_line2)
   quantile3 <- quantile(time_line3)
   quantile4 <- quantile(time_line4)
   
   mean1 <- mean(time_line1)
   mean2 <- mean(time_line2)
   mean3 <- mean(time_line3)
   mean4 <- mean(time_line4)
   
   table <- data.table(
      c("Min", "1st Qu.", "Mean", "Median", "3rd Qu.", "Max"),
      c(quantile1[1:2], mean1, quantile1[3:5]),
      c(quantile2[1:2], mean2, quantile2[3:5]),
      c(quantile3[1:2], mean3, quantile3[3:5]),
      c(quantile4[1:2], mean4, quantile4[3:5])
   )
   
   setnames(table, c("V1","V2","V3","V4","V5"), c("", name1, name2, name3, name4))
   table
}

calculate_dispersion <- function(residua){
   if(is.null(residua)) return(NULL)
   
   sum(residua$bpm ^ 2)/nrow(residua)
}

get_time <- function(strTime) {
   as.numeric(format(strptime(x = strTime, format = "%d.%m.%Y %H:%M:%S"), format = "%s"))
}

# =================================================================================================================================
# ============================================================ GRAPHS =============================================================
# =================================================================================================================================

create_plot <- function(time_line1, time_line2, name1, name2){
   validate(need(!is.null(time_line1) && !is.null(time_line2),'Can not create a plot. No input data.'))
   
   max <- max(c(max(time_line1$bpm), max(time_line2$bpm)))
   min <- min(c(min(time_line1$bpm), min(time_line2$bpm)))
   
   mer <- merge(x = time_line1, y = time_line2, by = "time")
   
   plot_ly(y = mer$bpm.x, x = anytime(mer$time), name = name1, type = 'scatter', mode = 'lines')%>%
      add_trace(y = mer$bpm.y, name = name2, mode = 'lines')%>%
      layout(xaxis = list(title = "Time"), yaxis = list(title = "BPM"))
}

create_resi_plot <- function(residua){
   validate(need(!is.null(residua),'Can not create a plot. No input data.'))
   
   plot_ly(residua, y = ~bpm, x = anytime(residua$time), name = 'trace 0', type = 'scatter', mode = 'lines')%>%
      layout(xaxis = list(title = "Time"), yaxis = list(title = "BPM"))
}

create_bland_altman_plot <- function(time_line1, time_line2, residua){
   validate(need(!is.null(time_line2) && !is.null(time_line1) && !is.null(residua),'Can not create a Bland-Altman plot. No input data.'))
   
   data.sd <- 1.96 * sqrt(calculate_dispersion(residua))
   data.mean <- mean(residua$bpm)
   data.x <- (time_line1$bpm + time_line2$bpm)/2
   data.y <- time_line1$bpm - time_line2$bpm
   
   plot_ly(x = ~data.x, y = ~data.y, name = 'data', type = 'scatter', mode = 'markers') %>%
      add_trace(y = ~data.mean + data.sd, name = 'Mean + 1.96SD', mode = 'lines') %>%
      add_trace(y = ~data.mean, name = 'Mean', mode = 'lines') %>%
      add_trace(y = ~data.mean - data.sd, name = 'Mean - 1.96SD', mode = 'lines') %>%
      layout(xaxis = list(zeroline = FALSE, title = '(A + B)/2'), yaxis = list(zeroline = FALSE, title = 'A - B'))
}

create_box_plot <- function(residua){
   plot_ly(y = ~residua$bpm, type = "box")
}

# =================================================================================================================================
# ============================================================ SERVER =============================================================
# =================================================================================================================================
function(input, output, session) {
   
   # observe submit button to switch on next panel in navbar
   observeEvent(input$submitBtn, {
      if(is.null(inputLow1()) || is.null(inputLow2()) || is.null(inputMed1()) || is.null(inputMed2()) || is.null(inputHig1()) || is.null(inputHig2())){
         return(NULL)
      }
      updateNavbarPage(session, "navbar", selected = "tabSummary")
   })
   
   # load files when change associated fileInput
   inputLow1 <- reactive({
      load_data(file = input$inFileLow1$datapath, deviceSelect = input$deviceSelect1, input$startMeasLow, input$endMeasLow, input$timeIntervalInput,  input$timeZone1*3600)
   })
   inputMed1 <- reactive({
      load_data(file = input$inFileMed1$datapath, deviceSelect = input$deviceSelect1, input$startMeasMed, input$endMeasMed, input$timeIntervalInput,  input$timeZone1*3600)
   })
   inputHig1 <- reactive({
      load_data(file = input$inFileHig1$datapath, deviceSelect = input$deviceSelect1, input$startMeasHig, input$endMeasHig, input$timeIntervalInput,  input$timeZone1*3600)
   })
   inputLow2 <- reactive({
      load_data(file = input$inFileLow2$datapath, deviceSelect = input$deviceSelect2, input$startMeasLow, input$endMeasLow, input$timeIntervalInput, input$timeShiftLow + (input$timeZone2*3600))
   })
   inputMed2 <- reactive({
      load_data(file = input$inFileMed2$datapath, deviceSelect = input$deviceSelect2, input$startMeasMed, input$endMeasMed, input$timeIntervalInput, input$timeShiftMed + (input$timeZone2*3600))
   })
   inputHig2 <- reactive({
      load_data(file = input$inFileHig2$datapath, deviceSelect = input$deviceSelect2, input$startMeasHig, input$endMeasHig, input$timeIntervalInput, input$timeShiftHig + (input$timeZone2*3600))
   })
   
   residuaLow <- reactive({
      time_line1 <- inputLow1()
      time_line2 <- inputLow2()
      if(is.null(time_line1) || is.null(time_line2)) return(NULL)
      
      data.table(time = time_line1$time, bpm = time_line1$bpm - time_line2$bpm)
   })
   residuaMed <- reactive({
      time_line1 <- inputMed1()
      time_line2 <- inputMed2()
      if(is.null(time_line1) || is.null(time_line2)) return(NULL)
      
      data.table(time = time_line1$time, bpm = time_line1$bpm - time_line2$bpm)
   })
   residuaHig <- reactive({
      time_line1 <- inputHig1()
      time_line2 <- inputHig2()
      if(is.null(time_line1) || is.null(time_line2)) return(NULL)
      
      data.table(time = time_line1$time, bpm = time_line1$bpm - time_line2$bpm)
   })
   
   
   output$calculationLow <-renderTable(colnames = FALSE,{
      calculate_calculation(inputLow1(), inputLow2(), residuaLow())
   })
   output$quantileLow <- renderTable(align = "lcccc", {
      calculate_quantile(inputLow1()$bpm, inputLow2()$bpm, residuaLow()$bpm, abs(residuaLow()$bpm), input$deviceSelect1, input$deviceSelect2, 'error', 'abs_error')
   })
   
   
   # render plots with two lines
   output$plotLow <- renderPlotly({
      create_plot(inputLow1(), inputLow2(), input$deviceSelect1, input$deviceSelect2)
   })
   output$plotMed <- renderPlotly({
      create_plot(inputMed1(), inputMed2(), input$deviceSelect1, input$deviceSelect2)
   })
   output$plotHig <- renderPlotly({
      create_plot(inputHig1(), inputHig2(), input$deviceSelect1, input$deviceSelect2)
   })
   
   # render plots with residuas
   output$plotResiLow <- renderPlotly({
      create_resi_plot(residuaLow())
   })
   output$plotResiMed <- renderPlotly({
      create_resi_plot(residuaMed())
   })
   output$plotResiHig <- renderPlotly({
      create_resi_plot(residuaHig())
   })
   
   output$boxPlotLow <- renderPlotly({
      create_box_plot(residuaLow())
   })
   output$boxPlotMed <- renderPlotly({
      create_box_plot(residuaMed())
   })
   output$boxPlotHig <- renderPlotly({
      create_box_plot(residuaHig())
   })
   
   output$BAPlotLow <- renderPlotly({
      create_bland_altman_plot(inputLow1(), inputLow2(), residuaLow())
   })
   output$BAPlotMed <- renderPlotly({
      create_bland_altman_plot(inputMed1(), inputMed2(), residuaMed())
   })
   output$BAPlotHig <- renderPlotly({
      create_bland_altman_plot(inputHig1(), inputHig2(), residuaHig())
   })
}