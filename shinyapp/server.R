library(shiny)
library(anytime)
library(data.table)
library(plotly)
library(shinyjs)

# =================================================================================================================================
# ========================================================= DATA READING ==========================================================
# =================================================================================================================================

load_data <- function(file, deviceSelect, startTime, endTime, timeInterval, timeShift){
   if(is.null(file) || is.null(startTime) || is.null(endTime) || is.null(timeInterval)) return(NULL)
   
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
   
   data <- data[time >= startTime]
   
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

filter_data <- function(data, options, outliers = TRUE){
   if(is.null(data)) return(NULL)
   if(is.null(options)) return(data)
   
   ignoreZeroValues  <- 'izv' %in% options
   ignoreOutliers    <- 'io'  %in% options
   
   if (ignoreZeroValues) {
      data <- data[apply(data[,.(time, bpm.x, bpm.y)], 1, function(row) all(row != 0)),]
   }
   
   if (ignoreOutliers && outliers) {
      data <- data[!data$residues %in% boxplot.stats(data$residues)$out]
   }
   data
}

# =================================================================================================================================
# ========================================================== CALCULATION ==========================================================
# =================================================================================================================================

calculate_calculation <- function(time_lines){
   if(is.null(time_lines)) return(NULL)
   
   dispersion <- calculate_dispersion(time_lines$residues)
   std_dev <- sqrt(dispersion)
   corelation <- cor(x = time_lines$bpm.x, y = time_lines$bpm.y, method = c("pearson"))
   
   data.table(
      c("Dispersion", "Standard deviation", "Corelation", "Error SD"),
      c(dispersion, std_dev, corelation, sd(time_lines$residues))
   )
}

calculate_quantile_mean <- function(time_line){
   quantile <- quantile(time_line)
   mean <- mean(time_line)
   
   c(quantile[1:2], mean, quantile[3:5])
}

calculate_quantile <- function(time_lines, name1, name2){
   
   tm1 <- time_lines$bpm.x
   tm2 <- time_lines$bpm.y
   tm3 <- time_lines$residues
   tm4 <- abs(time_lines$residues)
   tm5 <- (time_lines$residues / time_lines$bpm.x) * 100
   tm6 <- abs(time_lines$residues / time_lines$bpm.x) * 100
   
   table <- data.table(
      c("Min", "1st Qu.", "Mean", "Median", "3rd Qu.", "Max"),
      calculate_quantile_mean(tm1),
      calculate_quantile_mean(tm2),
      calculate_quantile_mean(tm3),
      calculate_quantile_mean(tm4),
      calculate_quantile_mean(tm5),
      calculate_quantile_mean(tm6)
   )
   
   setnames(table, c("V1","V2","V3","V4","V5", "V6", "V7"),
            c(
               "",
               paste0(name1, " [BPM]"),
               paste0(name2, " [BPM]"),
               'error [BPM]',
               'absolute error [BPM]',
               'relative error [%]',
               'absolute relative error [%]'
               )
            )
   table
}

calculate_dispersion <- function(residues){
   if(is.null(residues)) return(NULL)
   
   sum(residues ^ 2)/length(residues)
}

get_time <- function(strTime) {
   as.numeric(format(strptime(x = strTime, format = "%d.%m.%Y %H:%M:%S"), format = "%s"))
}

create_table <- function(data, device1, device2){
   if(is.null(data)) return(NULL)
   
   table <- data.table(format(anytime(data$time), format = "%d.%m.%Y %H:%M:%S"), data$bpm.x, data$bpm.y, data$residues)
   setnames(table, c("V1", "V2", "V3", "V4"), c("Time", paste0(device1, " [BPM]"), paste0(device2, " [BPM]"), "Residuas [BPM]"))
   table
}

# =================================================================================================================================
# =========================================================== VALIDATION ==========================================================
# =================================================================================================================================

checkInput <- function(inputElement, inputId, fail){
   if(is.null(inputElement)){
      shinyjs::addClass(id = inputId, class = "color_red")
      return(TRUE)
   }
   return(fail)
}

checkNumericInput <- function(inputElement, inputId, inputName, minValue, maxValue){
   message <- ""
   if(!is.numeric(inputElement)) {
      message <- paste("- Element", inputName, "contains non numeric value.")
   } else if(inputElement > maxValue || inputElement < minValue){
      message <- paste("- Value of element", inputName, "is out of range. Expected: <", minValue, ",", maxValue ,">")
   }
   if(message != ""){
      shinyjs::addClass(id = inputId, class = "color_red")
   }
   return(message)
}

checkTimeInput <- function(inputStartElement, inputEndElement, inputStartId, inputEndId, inputsName){
   fail <- FALSE
   fail <- checkInput(inputStartElement, inputStartId, fail)
   fail <- checkInput(inputEndElement, inputEndId, fail)
   if(fail){
      return(paste("- Please fill", inputsName))
   }
   
   startTime <- get_time(inputStartElement)
   endTime <- get_time(inputEndElement)
   
   if(is.na(startTime)){
      shinyjs::addClass(id = inputStartId, class = "color_red")
      fail <- TRUE
   }
   if(is.na(endTime)){
      shinyjs::addClass(id = inputEndId, class = "color_red")
      fail <- TRUE
   }
   if(fail){
      return(paste("-", inputsName, "have incorrect format. Expected: dd.mm.yyyy HH:MM:SS"))
   }
   
   if(startTime > endTime){
      shinyjs::addClass(id = inputStartId, class = "color_red")
      shinyjs::addClass(id = inputEndId, class = "color_red")
      return(paste("-", inputsName, "is incorrect. First value is bigger then second."))
   }
   return("")
}

toggleTabs <- function(show = TRUE){
   if(show){
      show(selector = "#navbar *")
   } else {
      hide(selector = "#navbar li a[data-value=tabLow]")
      hide(selector = "#navbar li a[data-value=tabMedium]")
      hide(selector = "#navbar li a[data-value=tabHigh]")
      hide(selector = "#navbar li a[data-value=tabSummary]")
      hide(selector = "#navbar li a[data-value=tabTables]")
   }
}

paste1 <- function(message1, message2){
   if (message1 != "" && message2 != ""){
      return(paste(message1, message2, sep = "\n"))
   } else{
      if (message1 != ""){
         return(message1)
      } else {
         return(message2)
      }
   }
}

# =================================================================================================================================
# ============================================================ GRAPHS =============================================================
# =================================================================================================================================

create_plot <- function(time_lines, name1, name2){
   validate(need(!is.null(time_lines),'Can not create a plot. No input data.'))
   
   plot_ly(y = time_lines$bpm.x, x = anytime(time_lines$time), name = name1, type = 'scatter', mode = 'lines')%>%
      add_trace(y = time_lines$bpm.y, name = name2, mode = 'lines')%>%
      layout(xaxis = list(title = "Time"), yaxis = list(title = "BPM"))
}

create_resi_plot <- function(time_lines){
   validate(need(!is.null(time_lines),'Can not create a plot. No input data.'))
   
   plot_ly(time_lines, y = ~residues, x = anytime(time_lines$time), type = 'scatter', mode = 'lines')%>%
      layout(xaxis = list(title = "Time"), yaxis = list(title = "A - B [BPM]"))
}

create_bland_altman_plot <- function(time_lines){
   validate(need(!is.null(time_lines),'Can not create a Bland-Altman plot. No input data.'))
   
   data.sd <- 1.96 * sd(time_lines$residues)
   data.mean <- mean(time_lines$residues)
   data.x <- (time_lines$bpm.x + time_lines$bpm.y)/2
   data.y <- time_lines$bpm.x - time_lines$bpm.y
   
   plot_ly(x = ~data.x, y = ~data.y, name = 'data', type = 'scatter', mode = 'markers') %>%
      add_trace(y = ~data.mean + data.sd, name = 'Mean + 1.96SD', mode = 'lines') %>%
      add_trace(y = ~data.mean, name = 'Mean', mode = 'lines') %>%
      add_trace(y = ~data.mean - data.sd, name = 'Mean - 1.96SD', mode = 'lines') %>%
      layout(xaxis = list(zeroline = FALSE, title = '(A + B)/2'), yaxis = list(zeroline = FALSE, title = 'A - B'))
}

create_box_plot <- function(time_lines){
   plot_ly(y = ~time_lines$residues, type = "box", name = '') %>%
      layout(xaxis = list(title = "", showticklabels = FALSE, zeroline = FALSE), yaxis = list(title = "A - B [BPM]", zeroline = FALSE))
}

# =================================================================================================================================
# ============================================================ SERVER =============================================================
# =================================================================================================================================
function(input, output, session) {
   
   toggleTabs(FALSE)
   print("--------------------------------------------- start --------------------------------------------------------")
   
   # observe submit button to switch on next panel in navbar
   observeEvent(input$submitBtn, {
      shinyjs::removeClass(selector = ".divs", class = "color_red")
      
      message <- ""
      fail <- FALSE
      
      fail <- checkInput(input$inFileLow1, "divInputLow1", fail)
      fail <- checkInput(input$inFileLow2, "divInputLow2", fail)
      fail <- checkInput(input$inFileMed1, "divInputMed1", fail)
      fail <- checkInput(input$inFileMed2, "divInputMed2", fail)
      fail <- checkInput(input$inFileHig1, "divInputHig1", fail)
      fail <- checkInput(input$inFileHig2, "divInputHig2", fail)
      if(fail){
         message <- "- Please fill empty inputs!"
      }
      message <- paste1(message, checkNumericInput(input$timeZone1, "divTimeZone1", success, -12, 14))
      message <- paste1(message, checkNumericInput(input$timeZone2, "divTimeZone2", success, -12, 14))
      message <- paste1(message, checkNumericInput(input$timeShiftLow, "divTimeShiftLow", "Time Shift Low", -3600, 3600))
      message <- paste1(message, checkNumericInput(input$timeShiftMed, "divTimeShiftMed", "Time Shift Medium", -3600, 3600))
      message <- paste1(message, checkNumericInput(input$timeShiftHig, "divTimeShiftHig", "Time Shift High", -3600, 3600))
      message <- paste1(message, checkNumericInput(input$timeIntervalInput, "divTimeIntervalInput", "Time Sampling Interval", 1, 60))
      message <- paste1(message, checkTimeInput(input$startMeasLow, input$endMeasLow, "divStartMeasLow", "divEndMeasLow", "Measurement interval for low load"))
      message <- paste1(message, checkTimeInput(input$startMeasMed, input$endMeasMed, "divStartMeasMed", "divEndMeasMed", "Measurement interval for medium load"))
      message <- paste1(message, checkTimeInput(input$startMeasHig, input$endMeasHig, "divStartMeasHig", "divEndMeasHig", "Measurement interval for high load"))
      
      if(message == ""){
         toggleTabs()
         updateNavbarPage(session, "navbar", selected = "tabSummary")
      } else {
         toggleTabs(FALSE)
         session$sendCustomMessage(type = 'allertMessage', message = message)
      }
   })
   
   observeEvent({
      input$inFileLow1
      input$inFileLow2
      input$inFileMed1
      input$inFileMed2
      input$inFileHig1
      input$inFileLow2
      input$deviceSelect1
      input$deviceSelect2
      input$timeZone1
      input$timeZone2
      input$timeIntervalInput
      input$startMeasLow
      input$endMeasLow
      input$startMeasMed
      input$endMeasMed
      input$startMeasHig
      input$endMeasHig
      input$timeShiftLow
      input$timeShiftMed
      input$timeShiftHig
   }, {
      toggleTabs(FALSE)
   })
   
   
   # load files when change associated fileInput
   inputLow1 <- reactive({
      validate(
         need(!is.null(input$inFileLow1) && !is.null(input$inFileLow2), "Please select data sets for low load"),
         need(input$startMeasLow != "", "Please select start of measurement for low load"),
         need(input$endMeasLow != "", "Please select end of measurement for low load"),
         need(input$timeIntervalInput != "", "Please select time interval for data sampling"),
         need(input$timeZone1 != "", "Please select time zone shift for first data set"),
         need(input$timeZone2 != "", "Please select time zone shift for second data set")
      )
      
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
   
   filteredInputLow <- reactive({
      filter_data(inputLowOutliers(), input$otherOptions)
   })
   filteredInputMed <- reactive({
      filter_data(inputMedOutliers(), input$otherOptions)
   })
   filteredInputHig <- reactive({
      filter_data(inputHigOutliers(), input$otherOptions)
   })
   filteredInputSummary <- reactive({
      filter_data(inputSummaryOutliers(), input$otherOptions) 
   })
   
   inputLowOutliers <- reactive({
      time_line1 <- inputLow1()
      time_line2 <- inputLow2()
      if(is.null(time_line1) || is.null(time_line2)) return(NULL)
      
      data <- merge(x = time_line1, y = time_line2, by = "time")
      filter_data(data[, residues := (data$bpm.x - data$bpm.y)], options = input$otherOptions, outliers = FALSE)
   })
   inputMedOutliers <- reactive({
      time_line1 <- inputMed1()
      time_line2 <- inputMed2()
      if(is.null(time_line1) || is.null(time_line2)) return(NULL)
      
      data <- merge(x = time_line1, y = time_line2, by = "time")
      filter_data(data[, residues := (data$bpm.x - data$bpm.y)], options = input$otherOptions, outliers = FALSE)
   })
   inputHigOutliers <- reactive({
      time_line1 <- inputHig1()
      time_line2 <- inputHig2()
      if(is.null(time_line1) || is.null(time_line2)) return(NULL)
      
      data <- merge(x = time_line1, y = time_line2, by = "time")
      filter_data(data[, residues := (data$bpm.x - data$bpm.y)], options = input$otherOptions, outliers = FALSE)
   })
   inputSummaryOutliers <- reactive({
      rbind(inputLowOutliers(), inputMedOutliers(), inputHigOutliers())
   })
   
   otherOptions <- reactive({
      input$otherOptions
   })
   
   output$calculationLow <-renderTable(colnames = FALSE,{
      calculate_calculation(filteredInputLow())
   })
   output$calculationMed <-renderTable(colnames = FALSE,{
      calculate_calculation(filteredInputMed())
   })
   output$calculationHig <-renderTable(colnames = FALSE,{
      calculate_calculation(filteredInputHig())
   })
   output$calculationSum <-renderTable(colnames = FALSE,{
      calculate_calculation(filteredInputSummary())
   })
   
   output$quantileLow <- renderTable(align = "lcccccc", width = "100%", {
      calculate_quantile(filteredInputLow(), input$deviceSelect1, input$deviceSelect2)
   })
   output$quantileMed <- renderTable(align = "lcccccc", width = "100%", {
      calculate_quantile(filteredInputMed(), input$deviceSelect1, input$deviceSelect2)
   })
   output$quantileHig <- renderTable(align = "lcccccc", width = "100%", {
      calculate_quantile(filteredInputHig(), input$deviceSelect1, input$deviceSelect2)
   })
   output$quantileSum <- renderTable(align = "lcccccc", width = "100%", {
      calculate_quantile(filteredInputSummary(), input$deviceSelect1, input$deviceSelect2)
   })
   
   # render plots with two lines
   output$plotLow <- renderPlotly({
      create_plot(filteredInputLow(), input$deviceSelect1, input$deviceSelect2)
   })
   output$plotMed <- renderPlotly({
      create_plot(filteredInputMed(), input$deviceSelect1, input$deviceSelect2)
   })
   output$plotHig <- renderPlotly({
      create_plot(filteredInputHig(), input$deviceSelect1, input$deviceSelect2)
   })
   
   # render plots with residues
   output$plotResiLow <- renderPlotly({
      create_resi_plot(filteredInputLow())
   })
   output$plotResiMed <- renderPlotly({
      create_resi_plot(filteredInputMed())
   })
   output$plotResiHig <- renderPlotly({
      create_resi_plot(filteredInputHig())
   })
   
   output$BAPlotLow <- renderPlotly({
      create_bland_altman_plot(filteredInputLow())
   })
   output$BAPlotMed <- renderPlotly({
      create_bland_altman_plot(filteredInputMed())
   })
   output$BAPlotHig <- renderPlotly({
      create_bland_altman_plot(filteredInputHig())
   })
   
   output$boxPlotLow <- renderPlotly({
      create_box_plot(inputLowOutliers())
   })
   output$boxPlotMed <- renderPlotly({
      create_box_plot(inputMedOutliers())
   })
   output$boxPlotHig <- renderPlotly({
      create_box_plot(inputHigOutliers())
   })
   output$boxPlotWarning1 <- output$boxPlotWarning2 <- output$boxPlotWarning3 <- renderText({
      if (!is.null(input$otherOptions) && 'io' %in% input$otherOptions){
         "Box plot is independent on 'ignore outliers' option"
      } else {
         ""
      }
   })
   
   output$tableLow <- renderTable(align = "cccc", width = "100%", {
      create_table(filteredInputLow(), input$deviceSelect1, input$deviceSelect2)
   })
   output$tableMed <- renderTable(align = "cccc", width = "100%", {
      create_table(filteredInputMed(), input$deviceSelect1, input$deviceSelect2)
   })
   output$tableHig <- renderTable(align = "cccc", width = "100%", {
      create_table(filteredInputHig(), input$deviceSelect1, input$deviceSelect2)
   })
}