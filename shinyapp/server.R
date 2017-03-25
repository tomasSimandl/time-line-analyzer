library(shiny)
library(anytime)
library(data.table)
library(plotly)
# =================================================================================================================================
# ========================================================= DATA READING ==========================================================
# =================================================================================================================================

# decide which method use for read data ===========================================================================================
load_data <- function(file, deviceSelect, startTime, endTime, timeInterval, timeShift){
   switch(deviceSelect,
      chest_strap = load_chest_strap_csv(file, startTime, endTime, timeInterval, timeShift),
      garmin = load_garmin_tcx(file, startTime, endTime, timeInterval, timeShift),
      basis = load_basis_csv(file, startTime, endTime, timeInterval, timeShift),
      fitbit = load_fitbit_csv(file, startTime, endTime, timeInterval, timeShift),
      return(NULL))
   
} # ==============================================================================================================================

load_garmin_tcx <- function(file, startTime, endTime, timeInterval, timeShift){
   if(is.null(file)) return(NULL)
   
   soubor <- xmlParse(file)
   data <- xmlToDataFrame(getNodeSet(soubor, "//ns:Trackpoint", "ns"))
   formated_data <- data.table(time = as.numeric(format(strptime(x = data$Time, format = "%Y-%m-%dT%H:%M:%S"), format = "%s")) + timeShift, bpm = as.numeric(as.character(data$HeartRateBpm)))
   
   data_sampling(formated_data, get_time(startTime), get_time(endTime), timeInterval)
}

load_basis_csv <- function(file, startTime, endTime, timeInterval, timeShift){
   columnsNames <- c("", "time", "", "", "bpm", "", "")
   
   data <- load_csv(file, columnsNames, header = TRUE)
   formated_data <- data.table(time = as.numeric(format(as.POSIXct(data$time), format = "%s"))  + timeShift, bpm = data$bpm)
 
   data_sampling(formated_data, get_time(startTime), get_time(endTime), timeInterval)  
}

load_fitbit_csv <- function(file, startTime, endTime, timeInterval, timeShift){
   columnsNames <- c("date", "time", "bpm")
   
   data <- load_csv(file, columnsNames, header = FALSE)
   formated_data <- data.table(time = as.numeric(format(as.POSIXct(paste(data$date, data$time, sep = " ")), format = "%s"))  + timeShift, bpm = data$bpm)
   
   data_sampling(formated_data, get_time(startTime), get_time(endTime), timeInterval)  
}

# load data from csv for chest strap and create sampling =========================================================================
load_chest_strap_csv <- function(file, startTime, endTime, timeInterval, timeShift) {
   columnsNames <- c("time", "", "bpm", "", "", "", "", "", "")
   
   data <- load_csv(file, columnsNames, header = FALSE)
   data_by_sec <- data.table(time = floor(data$time/1000) + timeShift, bpm = data$bpm)[, mean(bpm), by=time]
   end <- data.table(time = data_by_sec$time, bpm = data_by_sec$V1) # TODO
   
   data_sampling(end, get_time(startTime), get_time(endTime), timeInterval)
} # ==============================================================================================================================

# load data from csv with specific names of columns ==============================================================================
load_csv <- function(file, columnsNames, header = FALSE) {
   if(is.null(file)) return(NULL)
   
   read.csv(file, header = header, col.names = columnsNames)
} # ==============================================================================================================================










# calculate average for data by timeInterval. ====================================================================================
data_sampling <- function(data, startTime, endTime, timeInterval){
   sum <- 0
   counter <- 0
   sumStartTime <- startTime
   i <- 1L
   sequence <- seq(startTime, endTime, timeInterval)
   DT <- data.table(time = sequence, bpm = numeric(length(sequence)))

   for (n in 1:nrow(data)){
      curTime <- data[n]$time
      print(data[n]$bpm)
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
   if (counter != 0 && sumStartTime + timeInterval <= endTime){
      set(DT, i, "bpm", sum/counter)
   }
   DT
} # ==============================================================================================================================


calculate_sd <- function(time_line1, time_line2){
   
}


get_time <- function(strTime) {
   as.numeric(format(strptime(x = strTime, format = "%d.%m.%Y %H:%M:%S"), format = "%s"))
}

# =================================================================================================================================
# ============================================================ GRAPHS =============================================================
# =================================================================================================================================

# create plot from two input time lines
create_plot <- function(time_line1, time_line2, name1, name2){
   validate(need(!is.null(time_line1) && !is.null(time_line2),'Can not create a plot. No input data.'))
   
   max <- max(c(max(time_line1$bpm), max(time_line2$bpm)))
   min <- min(c(min(time_line1$bpm), min(time_line2$bpm)))
   
   plot_ly(y = time_line1$bpm, x = anytime(time_line1$time), name = name1, type = 'scatter', mode = 'lines')%>%
      add_trace(y = time_line2$bpm, name = name2, mode = 'lines')%>%
      layout(xaxis = list(title = "Time"), yaxis = list(title = "BPM"))
}

# create plot of residuas from two input time lines
create_resi_plot <- function(residua){
   validate(need(!is.null(residua),'Can not create a plot. No input data.'))
   
   plot_ly(residua, y = ~bpm, x = anytime(residua$time), name = 'trace 0', type = 'scatter', mode = 'lines')%>%
      layout(xaxis = list(title = "Time"), yaxis = list(title = "BPM"))
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
      time_line1 <- inputLow1
      time_line2 <- inputLow2
      if(is.null(time_line1) || is.null(time_line2)) return(NULL)
      
      data.table(time = time_line1$time, bpm = time_line1$bpm - time_line2$bpm)
   })
   residuaMid <- reactive({
      time_line1 <- inputMid1
      time_line2 <- inputMid2
      if(is.null(time_line1) || is.null(time_line2)) return(NULL)
      
      data.table(time = time_line1$time, bpm = time_line1$bpm - time_line2$bpm)
   })
   residuaHig <- reactive({
      time_line1 <- inputHig1
      time_line2 <- inputHig2
      if(is.null(time_line1) || is.null(time_line2)) return(NULL)
      
      data.table(time = time_line1$time, bpm = time_line1$bpm - time_line2$bpm)
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
      create_resi_plot(residuaMid())
   })
   output$plotResiHig <- renderPlotly({
      create_resi_plot(residuaHig())
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