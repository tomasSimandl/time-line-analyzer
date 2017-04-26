library(shiny)
library(data.table)
library(plotly)
library(shinyjs)
library(XML)

# =================================================================================================================================
# ==================================================== DATA READING AND EDITING ===================================================
# =================================================================================================================================

# Check input values and decide which function will be used for loading a file. After success file load call function for data
# sampling.
load_data <- function(file, deviceSelect, timeShift){
   if(is.null(file) || !is.numeric(timeShift)) return(NULL)
   
   data <- switch(deviceSelect,
                  Chest_strap = load_chest_strap_csv(file, timeShift),
                  Garmin = load_garmin_tcx(file, timeShift),
                  Basis = load_basis_csv(file, timeShift),
                  Fitbit = load_fitbit_csv(file, timeShift),
                  return(NULL))
   
   data[apply(data[,.(time, bpm)], 1, function(row) all(!is.na(row))),]
}

# Load file in tcx format. Verify loaded data and return data.table with columns 'time' and 'bpm'.
load_garmin_tcx <- function(file, timeShift){
   soubor <- xmlParse(file)
   
   data <- xmlToDataFrame(getNodeSet(soubor, "//ns:Trackpoint", "ns"))
   time_vector <- as.numeric(format(strptime(x = data$Time, format = "%Y-%m-%dT%H:%M:%S"), format = "%s"))  + timeShift
   bpm_vector <- as.numeric(as.character(data$HeartRateBpm))
   
   data.table(time = time_vector, bpm = bpm_vector)
}

# Load file in csv format created in Basis Peak watches. Verify loaded data and return data.table with columns 'time' and 'bpm'.
load_basis_csv <- function(file, timeShift){
   columnsNames <- c("", "time", "", "", "bpm", "", "")
   
   data <- read.csv(file, col.names = columnsNames, header = TRUE)
   if(length(data$bpm[!is.numeric(data$bpm)]) != 0){
      return(NULL)
   }
   data.table(time = as.numeric(format(as.POSIXct(data$time), format = "%s")) + timeShift, bpm = data$bpm)
}

# Load file in csv format where columns are 'date', 'time' and 'bpm' divided by comma. Verify loaded data and return data.table
# with columns 'time' and 'bpm'.
load_fitbit_csv <- function(file, timeShift){
   columnsNames <- c("date", "time", "bpm")
   
   data <- read.csv(file, col.names = columnsNames, header = FALSE)
   if(length(data$bpm[!is.numeric(data$bpm)]) != 0){
      return(NULL)
   }
   
   data.table(time = as.numeric(format(as.POSIXct(paste(data$date, data$time, sep = " ")), format = "%s")) + timeShift, bpm = data$bpm)
}

# Load file in csv format where are nine columns divided by comma. First column is 'time' and third column is 'bpm'. Verify loaded
# data and return data.table with two columns 'time' and 'bpm'.
load_chest_strap_csv <- function(file, timeShift) {
   columnsNames <- c("time", "", "bpm", "", "", "", "", "", "")
   
   data <- read.csv(file, col.names = columnsNames, header = FALSE)
   
   if(length(data$bpm[!is.numeric(data$bpm)]) + length(data$time[!is.numeric(data$time)]) != 0){
      return(NULL)
   }
   
   data_by_sec <- data.table(time = floor(data$time/1000) + timeShift, bpm = data$bpm)[, mean(bpm), by=time]
   setnames(data_by_sec, c("time", "V1"), c("time", "bpm"))
   data_by_sec
}

# Samples input data. Return data.table with time from startTime to endTime with spacing timeInterval. Sampling is create by averaging.
data_sampling <- function(data, startTime, endTime, timeInterval){
   if(is.null(data) || nrow(data) == 0 || is.null(data$time)  || is.null(data$bpm) || is.null(startTime) || is.null(endTime) || !is.numeric(timeInterval)) return(NULL)
   
   startTime <- get_time(startTime)
   endTime <- get_time(endTime)
   
   if(is.na(startTime) || is.na(endTime) || startTime > endTime) return(NULL)
   
   sequence <- seq(startTime, endTime, timeInterval)
   size <- length(sequence)
   DT <- data.table(time = sequence, bpm = numeric(size))
   
   for (n in 1:nrow(DT)){
      frame <- data[time <= startTime]
      frame <- frame[time > startTime - timeInterval]
      data_mean <- mean(frame$bpm)
      if(!is.na(data_mean)){
         set(DT, n, "bpm", data_mean)
      }
      startTime <- startTime + timeInterval
   }
   DT
}

# Filter data based on options value.
# If option contains izv, delete all values where is zero. 
# If option contains io, delete all value which are outliers.
filter_data <- function(data, options, outliers = TRUE){
   if(is.null(data) || is.null(data$residues) || is.null(data$bpm.x) || is.null(data$bpm.y) || is.null(data$time)) return(NULL)
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

calculate_calculation <- function(time_lines, isCorelation = TRUE){
   if(is.null(time_lines) || is.null(time_lines$residues)) return(NULL)
   
   dispersion <- calculate_dispersion(time_lines$residues)
   std_dev <- sqrt(dispersion)
   
   if (isCorelation){
      if(is.null(time_lines$bpm.x) || is.null(time_lines$bpm.y)) return(NULL)
      corelation <- cor(x = time_lines$bpm.x, y = time_lines$bpm.y, method = c("pearson"))
      result <- data.table(c("Dispersion", "Standard deviation", "Corelation", "Error SD"),c(dispersion, std_dev, corelation, sd(time_lines$residues)))
   } else {
      result <- data.table(c("Dispersion", "Standard deviation", "Error SD"),c(dispersion, std_dev, sd(time_lines$residues)))
   }
   result
}

calculate_dispersion <- function(residues){
   if(is.null(residues)) return(NULL)
   
   sum(residues ^ 2)/length(residues)
}

calculate_quantile <- function(time_lines, name1 = 'a', name2 = 'b'){
   if(is.null(time_lines) || is.null(time_lines$residues) || is.null(time_lines$bpm.x) || is.null(time_lines$bpm.y) || is.null(name1) || is.null(name2)) return(NULL)
   
   tm1 <- time_lines$bpm.x
   tm2 <- time_lines$bpm.y
   tm3 <- time_lines$residues
   tm4 <- abs(time_lines$residues)
   tm5 <- (time_lines$residues / time_lines$bpm.x) * 100
   tm6 <- abs(time_lines$residues / time_lines$bpm.x) * 100
   
   tm5[is.nan(tm5)] <- 0
   tm6[is.nan(tm6)] <- 0
   
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
               'Error [BPM]',
               'Absolute error [BPM]',
               'Relative error [%]',
               'Absolute relative error [%]'
               )
            )
   table
}

calculate_quantile_mean <- function(time_line){
   quantile <- quantile(time_line)
   mean <- mean(time_line)
   
   c(quantile[1:2], mean, quantile[3:5])
}

get_time <- function(strTime) {
   as.numeric(format(strptime(x = strTime, format = "%d.%m.%Y %H:%M:%S"), format = "%s"))
}

get_formated_time <- function(longTime){
   format(strptime(longTime, format = '%s'), format = "%d.%m.%Y %H:%M:%S")
}

# =================================================================================================================================
# =========================================================== VALIDATION ==========================================================
# =================================================================================================================================

# If inputElement is null, then element with inputId is assigned to color_red class and TRUE is returned. Otherwise fail is returned.
checkInput <- function(inputElement, inputId, fail){
   if(is.null(inputElement)){
      shinyjs::addClass(id = inputId, class = "color_red")
      return(TRUE)
   }
   return(fail)
}

# If inputElement is non numeric or out of range(minValue, maxValue), element with inputId is assigned to color_red class and
# error message is returned. Otherwise empty string is returned. 
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

# If inputStartElement is NULL or value can not be parse, element with inputStartId is assigned to color_red class and error 
# message is returned. Same behavior is on inputEndElement and inputEndId. If inputStartElement have bigger value than 
# inputEnd element, elements with inputStartId and inputEndId are assigned to color_red class and error message is returned.
# Otherwise return empty string.
checkTimeInput <- function(inputStartElement, inputEndElement, inputStartId, inputEndId, inputsName, timeInterval){
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
   
   if((endTime - startTime)/timeInterval < 10){
      shinyjs::addClass(id = inputStartId, class = "color_red")
      shinyjs::addClass(id = inputEndId, class = "color_red")
      return(paste("- Measurement interval must contains  minimal 10 samples."))
   }
   return("")
}

# If input parametr is TRUE or missing, all tabs in navbar are shown. Othrewise hide tabs: LOW, MEDIUM, HIGH, SUMMARY and TABLES.
toggleTabs <- function(show = TRUE){
   if(show){
      shinyjs::show(selector = "#navbar *", anim = TRUE)
   } else {
      shinyjs::hide(selector = "#navbar li a[data-value=tabLow]")
      shinyjs::hide(selector = "#navbar li a[data-value=tabMedium]")
      shinyjs::hide(selector = "#navbar li a[data-value=tabHigh]")
      shinyjs::hide(selector = "#navbar li a[data-value=tabSummary]")
      shinyjs::hide(selector = "#navbar li a[data-value=tabTables]")
   }
}

# Concate input messages with new line as separator. If one of messages is empty, return only second message. If both messages
# are empty, return empty message.
paste1 <- function(message1, message2){
   if(is.null(message1) || is.null(message2)){
      return("")
   }
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

set_time_Limits <- function(file1, file2, session, startTextInput, endTextInput, device1, device2){
   updateTextInput(session, startTextInput, value = "---")
   updateTextInput(session, endTextInput, value = "---")
   
   if(is.null(file1) || is.null(file2)) return("")
   
   start <- max(file1$time[1L], file2$time[1L])
   end <- min(tail(file1$time, n = 1) , tail(file2$time, n = 1))
   
   if(start >= end){
      return(paste0(get_formated_time(file1$time[1L]),
             " - ",
             get_formated_time(tail(file1$time, n = 1)),
             " - ",
             device1,
             "<br/>",
             get_formated_time(file2$time[1L]),
             " - ",
             get_formated_time(tail(file2$time, n = 1)),
             " - ",
             device2))
   }
   
   strTimeStart <- get_formated_time(start)
   strTimeEnd <- get_formated_time(end)
   
   if(is.na(strTimeStart) || is.na(strTimeEnd)) return("Ivalid time format")
   
   updateTextInput(session, startTextInput, value = strTimeStart)
   updateTextInput(session, endTextInput, value = strTimeEnd)
   return("")
}

# =================================================================================================================================
# ======================================================= GRAPHS AND TABLES =======================================================
# =================================================================================================================================

# Create plot with two lines.
create_plot <- function(time_lines, name1, name2){
   validate(
      need(!is.null(time_lines),'Can not create a plot. No input data.'),
      need(nrow(time_lines) != 0,'Can not create a plot. No input data.')
   )
   
   plot_ly(y = time_lines$bpm.x, x = time_lines$time - time_lines$time[1], name = name1, type = 'scatter', mode = 'lines')%>%
      add_trace(y = time_lines$bpm.y, name = name2, mode = 'lines')%>%
      layout(xaxis = list(title = "Time [s]"), yaxis = list(title = "BPM"))
}

# Create plot of residuas.
create_resi_plot <- function(time_lines){
   validate(
      need(!is.null(time_lines),'Can not create a plot. No input data.'),
      need(nrow(time_lines) != 0,'Can not create a plot. No input data.')
   )
   
   plot_ly(time_lines, y = ~residues, x = time_lines$time - time_lines$time[1], type = 'scatter', mode = 'lines')%>%
      layout(xaxis = list(title = "Time [s]"), yaxis = list(title = "A - B [BPM]"))
}

# Create Bland&Altman plot from input data.
create_bland_altman_plot <- function(time_lines){
   validate(
      need(!is.null(time_lines),'Can not create Bland-Altman plot. No input data.'),
      need(nrow(time_lines) != 0,'Can not create Bland-Altman plot. No input data.')
   )
   
   data.sd <- 1.96 * sd(time_lines$residues)
   data.mean <- mean(time_lines$residues)
   data.x <- (time_lines$bpm.x + time_lines$bpm.y)/2
   data.y <- time_lines$bpm.x - time_lines$bpm.y
   
   plot_ly(x = ~data.x, y = ~data.y, name = 'Data', type = 'scatter', mode = 'markers') %>%
      add_trace(y = ~data.mean + data.sd, name = 'Mean + 1.96SD', mode = 'lines') %>%
      add_trace(y = ~data.mean, name = 'Mean', mode = 'lines') %>%
      add_trace(y = ~data.mean - data.sd, name = 'Mean - 1.96SD', mode = 'lines') %>%
      layout(xaxis = list(zeroline = FALSE, title = '(A + B)/2'), yaxis = list(zeroline = FALSE, title = 'A - B'))
}

# Create box plot showing outliers.
create_box_plot <- function(time_lines){
   plot_ly(y = ~time_lines$residues, type = "box", name = '') %>%
      layout(xaxis = list(title = "", showticklabels = FALSE, zeroline = FALSE), yaxis = list(title = "A - B [BPM]", zeroline = FALSE))
}

# Create table with time, bpm value form first device, bpm value from second device and bpm resiues.
create_table <- function(data, device1, device2){
   if(is.null(data) || nrow(data) == 0) return(NULL)
   
   table <- data.table(format(strptime(data$time, format = "%s"), format = "%d.%m.%Y %H:%M:%S"), data$bpm.x, data$bpm.y, data$residues)
   setnames(table, c("V1", "V2", "V3", "V4"), c("Time", paste0(device1, " [BPM]"), paste0(device2, " [BPM]"), "Residuas [BPM]"))
   table
}

# =================================================================================================================================
# ============================================================ SERVER =============================================================
# =================================================================================================================================

input_validation <- function(input, session, filteredInputLow, filteredInputMed, filteredInputHig, fullCheck = TRUE){
   shinyjs::removeClass(selector = ".divs", class = "color_red")
   
   shinyjs::disable(id = "timeButton")
   shinyjs::disable(id = "submitBtn")
   
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
   message <- paste1(message, checkNumericInput(input$timeZone1, "divTimeZone1", "Time zone shift 1", -12, 14))
   message <- paste1(message, checkNumericInput(input$timeZone2, "divTimeZone2", "Time zone shift 2", -12, 14))
   message <- paste1(message, checkNumericInput(input$timeShiftLow, "divTimeShiftLow", "Time Shift Low", -3600, 3600))
   message <- paste1(message, checkNumericInput(input$timeShiftMed, "divTimeShiftMed", "Time Shift Medium", -3600, 3600))
   message <- paste1(message, checkNumericInput(input$timeShiftHig, "divTimeShiftHig", "Time Shift High", -3600, 3600))
   
   timeIntervalInputMessage <- checkNumericInput(input$timeIntervalInput, "divTimeIntervalInput", "Time Sampling Interval", 1, 60) 
   message <- paste1(message, timeIntervalInputMessage)
   if(fullCheck){
      if(timeIntervalInputMessage == ""){
         message <- paste1(message, checkTimeInput(input$startMeasLow, input$endMeasLow, "divStartMeasLow", "divEndMeasLow", "Measurement interval for low load", input$timeIntervalInput))
         message <- paste1(message, checkTimeInput(input$startMeasMed, input$endMeasMed, "divStartMeasMed", "divEndMeasMed", "Measurement interval for medium load", input$timeIntervalInput))
         message <- paste1(message, checkTimeInput(input$startMeasHig, input$endMeasHig, "divStartMeasHig", "divEndMeasHig", "Measurement interval for high load", input$timeIntervalInput))
      }
      
      if(is.null(filteredInputLow) || nrow(filteredInputLow) == 0) {
         message <- paste(message, "- Input data are incorrect. Can not make the calculations for low load", sep = "\n")
      }
      if(is.null(filteredInputMed) || nrow(filteredInputMed) == 0) {
         message <- paste(message, "- Input data are incorrect. Can not make the calculations for medium load", sep = "\n")
      }
      if(is.null(filteredInputHig) || nrow(filteredInputHig) == 0) {
         message <- paste(message, "- Input data are incorrect. Can not make the calculations for high load", sep = "\n")
      }
   }
   
   shinyjs::enable(id = "timeButton")
   shinyjs::enable(id = "submitBtn")
   
   if(message == ""){
      return(TRUE)
   } else {
      toggleTabs(FALSE)
      session$sendCustomMessage(type = 'allertMessage', message = message)
      return(FALSE)
   }
}

function(input, output, session) {
   print("--------------------------------------------- start --------------------------------------------------------")
   
   toggleTabs(FALSE) # hide tabs
   
   # ======================================================== OBSERVATION =========================================================
   
   # On click on submitBtn input data are validated. On success summary tab is displayed. Otherwise alert is shown.
   observeEvent(input$submitBtn, {
      
      result = input_validation(input, session, filteredInputLow(), filteredInputMed(), filteredInputHig())
      
      if(result){
         toggleTabs()
         updateNavbarPage(session, "navbar", selected = "tabSummary")
      }
   })
   
   observeEvent(input$timeButton, {
      result = input_validation(input, session, filteredInputLow(), filteredInputMed(), filteredInputHig(), fullCheck = FALSE)
      
      if(result){
         shinyjs::disable(id = "timeButton")
         shinyjs::disable(id = "submitBtn")
         
         result <- set_time_Limits(inputLow1(), inputLow2(), session, "startMeasLow", "endMeasLow", input$deviceSelect1, input$deviceSelect2)
         output$lowTimeWarning <- renderUI(HTML(result))
         
         result <- set_time_Limits(inputMed1(), inputMed2(), session, "startMeasMed", "endMeasMed", input$deviceSelect1, input$deviceSelect2)
         output$medTimeWarning <- renderUI(HTML(result))
         
         result <- set_time_Limits(inputHig1(), inputHig2(), session, "startMeasHig", "endMeasHig", input$deviceSelect1, input$deviceSelect2)
         output$higTimeWarning <- renderUI(HTML(result))
         
         shinyjs::enable(id = "timeButton")
         shinyjs::enable(id = "submitBtn")
      }
   })
   
   # When value is changed of some element in list below, tabs with results are hiden.
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
   
   # ===================================================== DATA MANIPULATION ======================================================
   
   # load files on inputs change
   inputLow1 <- reactive({
      tryResult <- try({
         data <- load_data(file = input$inFileLow1$datapath, deviceSelect = input$deviceSelect1, input$timeZone1*3600)
      }, silent = TRUE)
      
      if("try-error" %in% class(tryResult)) return(NULL)
      if(is.null(data) || nrow(data) == 0) return(NULL)
      data
   })
   inputMed1 <- reactive({
      tryResult <- try({
         data <- load_data(file = input$inFileMed1$datapath, deviceSelect = input$deviceSelect1,  input$timeZone1*3600)
      }, silent = TRUE)
      
      if("try-error" %in% class(tryResult)) return(NULL)
      if(is.null(data) || nrow(data) == 0) return(NULL)
      data
   })
   inputHig1 <- reactive({
      tryResult <- try({
         data <- load_data(file = input$inFileHig1$datapath, deviceSelect = input$deviceSelect1,  input$timeZone1*3600)
      }, silent = TRUE)
      
      if("try-error" %in% class(tryResult)) return(NULL)
      if(is.null(data) || nrow(data) == 0) return(NULL)
      data
   })
   inputLow2 <- reactive({
      tryResult <- try({
         data <- load_data(file = input$inFileLow2$datapath, deviceSelect = input$deviceSelect2, input$timeShiftLow + (input$timeZone2*3600))
      }, silent = TRUE)
      
      if("try-error" %in% class(tryResult)) return(NULL)
      if(is.null(data) || nrow(data) == 0) return(NULL)
      data
   })
   inputMed2 <- reactive({
      tryResult <- try({
         data <- load_data(file = input$inFileMed2$datapath, deviceSelect = input$deviceSelect2, input$timeShiftMed + (input$timeZone2*3600))
      }, silent = TRUE)
      
      if("try-error" %in% class(tryResult)) return(NULL)
      if(is.null(data) || nrow(data) == 0) return(NULL)
      data
   })
   inputHig2 <- reactive({
      tryResult <- try({
         data <- load_data(file = input$inFileHig2$datapath, deviceSelect = input$deviceSelect2, input$timeShiftHig + (input$timeZone2*3600))
      }, silent = TRUE)
      
      if("try-error" %in% class(tryResult)) return(NULL)
      if(is.null(data) || nrow(data) == 0) return(NULL)
      data
   })
   
   # sample loaded files
   sampledLow1 <- reactive({
      data <- data_sampling(inputLow1(), input$startMeasLow, input$endMeasLow, input$timeIntervalInput)
      if(is.null(data) || nrow(data) == 0) return(NULL)
      data
   })
   sampledMed1 <- reactive({
      data <- data_sampling(inputMed1(), input$startMeasMed, input$endMeasMed, input$timeIntervalInput)
      if(is.null(data) || nrow(data) == 0) return(NULL)
      data
   })
   sampledHig1 <- reactive({
      data <- data_sampling(inputHig1(), input$startMeasHig, input$endMeasHig, input$timeIntervalInput)
      if(is.null(data) || nrow(data) == 0) return(NULL)
      data
   })
   sampledLow2 <- reactive({
      data <- data_sampling(inputLow2(), input$startMeasLow, input$endMeasLow, input$timeIntervalInput)
      if(is.null(data) || nrow(data) == 0) return(NULL)
      data
   })
   sampledMed2 <- reactive({
      data <- data_sampling(inputMed2(), input$startMeasMed, input$endMeasMed, input$timeIntervalInput)
      if(is.null(data) || nrow(data) == 0) return(NULL)
      data
   })
   sampledHig2 <- reactive({
      data <- data_sampling(inputHig2(), input$startMeasHig, input$endMeasHig, input$timeIntervalInput)
      if(is.null(data) || nrow(data) == 0) return(NULL)
      data
   })
   
   # merge data of same measurement from two devices and filter with outliers disabled
   inputLowOutliers <- reactive({
      tryResult <- try({
         time_line1 <- sampledLow1()
         time_line2 <- sampledLow2()
      }, silent = TRUE)
      if("try-error" %in% class(tryResult)) return(NULL)
      if(is.null(time_line1) || is.null(time_line2)) return(NULL)
      
      
      data <- merge(x = time_line1, y = time_line2, by = "time")
      data <- filter_data(data[, residues := (data$bpm.x - data$bpm.y)], options = input$otherOptions, outliers = FALSE)
      if(is.null(data) || nrow(data) == 0) return(NULL)
      data
   })
   inputMedOutliers <- reactive({
      tryResult <- try({
         time_line1 <- sampledMed1()
         time_line2 <- sampledMed2()
      }, silent = TRUE)
      if("try-error" %in% class(tryResult)) return(NULL)
      if(is.null(time_line1) || is.null(time_line2)) return(NULL)
      
      data <- merge(x = time_line1, y = time_line2, by = "time")
      data <- filter_data(data[, residues := (data$bpm.x - data$bpm.y)], options = input$otherOptions, outliers = FALSE)
      if(is.null(data) || nrow(data) == 0) return(NULL)
      data
   })
   inputHigOutliers <- reactive({
      tryResult <- try({
         time_line1 <- sampledHig1()
         time_line2 <- sampledHig2()
      }, silent = FALSE)
      if("try-error" %in% class(tryResult)) return(NULL)
      if(is.null(time_line1) || is.null(time_line2)) return(NULL)
      
      data <- merge(x = time_line1, y = time_line2, by = "time")
      data <- filter_data(data[, residues := (data$bpm.x - data$bpm.y)], options = input$otherOptions, outliers = FALSE)
      if(is.null(data) || nrow(data) == 0) return(NULL)
      data
   })
   
   # filter data from input*Outliers based on input$otherOptions
   filteredInputLow <- reactive({
      data <- filter_data(inputLowOutliers(), input$otherOptions)
      if(is.null(data) || nrow(data) == 0) return(NULL)
      data
   })
   filteredInputMed <- reactive({
      data <- filter_data(inputMedOutliers(), input$otherOptions)
      if(is.null(data) || nrow(data) == 0) return(NULL)
      data
   })
   filteredInputHig <- reactive({
      data <- filter_data(inputHigOutliers(), input$otherOptions)
      if(is.null(data) || nrow(data) == 0) return(NULL)
      data
   })
   filteredInputSummary <- reactive({
      low <- filteredInputLow()
      med <- filteredInputMed()
      hig <- filteredInputHig()
      
      if(is.null(low) || is.null(med) || is.null(hig)){
         return(NULL)
      }
      tryResult <- try({
         data <- rbind(low, med, hig)
      }, silent = FALSE)
      
      if("try-error" %in% class(tryResult)) return(NULL)
      if(is.null(data) || nrow(data) == 0) return(NULL)
      data
   })
   
   # ======================================================= DATA RENDERING =======================================================
   
   # render table of basic calculatinos(corelation, standard deviation,...)
   output$calculationLow <-renderTable(colnames = FALSE,{
      data <- calculate_calculation(filteredInputLow())
      req(data)
      data
   })
   output$calculationMed <-renderTable(colnames = FALSE,{
      data <- calculate_calculation(filteredInputMed())
      req(data)
      data
   })
   output$calculationHig <-renderTable(colnames = FALSE,{
      data <- calculate_calculation(filteredInputHig())
      req(data)
      data
   })
   output$calculationSum <-renderTable(colnames = FALSE,{
      data <- calculate_calculation(filteredInputSummary(), isCorelation = FALSE)
      req(data)
      data
   })
   
   # render table of quartiles and mean from bpms, residues, errors.
   output$quantileLow <- renderTable(align = "lcccccc", width = "100%", {
      data <- calculate_quantile(filteredInputLow(), input$deviceSelect1, input$deviceSelect2)
      req(data)
      data
   })
   output$quantileMed <- renderTable(align = "lcccccc", width = "100%", {
      data <- calculate_quantile(filteredInputMed(), input$deviceSelect1, input$deviceSelect2)
      req(data)
      data
   })
   output$quantileHig <- renderTable(align = "lcccccc", width = "100%", {
      data <- calculate_quantile(filteredInputHig(), input$deviceSelect1, input$deviceSelect2)
      req(data)
      data
   })
   output$quantileSum <- renderTable(align = "lcccccc", width = "100%", {
      data <- calculate_quantile(filteredInputSummary(), input$deviceSelect1, input$deviceSelect2)
      req(data)
      data
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
   
   # render Bland&Altman plot
   output$BAPlotLow <- renderPlotly({
      create_bland_altman_plot(filteredInputLow())
   })
   output$BAPlotMed <- renderPlotly({
      create_bland_altman_plot(filteredInputMed())
   })
   output$BAPlotHig <- renderPlotly({
      create_bland_altman_plot(filteredInputHig())
   })
   
   # render box plot showing outliers
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
         "The box plot is independent of 'ignore outliers' option"
      } else {
         ""
      }
   })
   
   # render tables of input data
   output$tableLow <- renderTable(align = "cccc", width = "100%", {
      data <- create_table(filteredInputLow(), input$deviceSelect1, input$deviceSelect2)
      req(data)
      data
   })
   output$tableMed <- renderTable(align = "cccc", width = "100%", {
      data <- create_table(filteredInputMed(), input$deviceSelect1, input$deviceSelect2)
      req(data)
      data
   })
   output$tableHig <- renderTable(align = "cccc", width = "100%", {
      data <- create_table(filteredInputHig(), input$deviceSelect1, input$deviceSelect2)
      req(data)
      data
   })
}