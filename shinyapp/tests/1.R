test.calculate_dispersion <- function(){
   checkEquals(target = NULL, current = calculate_dispersion(NULL))
   checkEquals(target = NULL, current = calculate_dispersion(c()))
   checkEqualsNumeric(target = 320, current = calculate_dispersion(c(10,20,30,10,10)))
   checkEquals(target = 0, current = calculate_dispersion(c(0)))
   checkEquals(target = 100, current = calculate_dispersion(c(-10)))
   checkEquals(target = 0, current = calculate_dispersion(0))
   checkEquals(target = 441, current = calculate_dispersion(21))
}

test.get_time <- function() {
   checkEquals(target = 123456789, current = get_time("29.11.1973 22:33:09"))
   checkEquals(target = 123456789, current = get_time("29.11.197322:33:09"))
   checkEquals(target = NaN, current = get_time("29.11.1973"))
   checkEquals(target = NaN, current = get_time(""))
   checkEquals(target = NaN, current = get_time(29111973))
   checkEquals(target = NaN, current = get_time("test"))
}

test.get_formated_time <- function() {
   checkEquals(target = "29.11.1973 22:33:09", current = get_formated_time(123456789))
   checkEquals(target = "29.11.1973 22:33:09", current = get_formated_time("123456789"))
   checkEquals(target = as.character(NA), current = get_formated_time(-1))
   checkEquals(target = as.character(NA), current = get_formated_time("test"))
}

test.paste1 <- function() {
   checkEquals(target = "abc\ndef", current  = paste1("abc", "def"))
   checkEquals(target = "def", current  = paste1("", "def"))
   checkEquals(target = "abc", current  = paste1("abc", ""))
   checkEquals(target = "", current  = paste1("abc", NULL))
   checkEquals(target = "", current  = paste1(NULL, "abc"))
   checkEquals(target = "", current  = paste1("", ""))
}

# calculate_quantile ===========================

test.calculate_quantile_invalid_input <- function() {
   checkEquals(target = NULL, current = calculate_quantile(data.table(10,20,30)))
   checkEquals(target = NULL, current = calculate_quantile(NULL,"a","b"))
}

test.calculate_quantile <- function() {   
   table <- data.table(
      c(1,1,1,1,1,-1,-1,-1,-1,-1),
      c(1,1,1,1,1,1,1,1,1,1),
      c(1,1,1,1,1,1,1,1,1,1)
   )
   setnames(table, c("V1", "V2", "V3"), c("residues", "bpm.x", "bpm.y"))
   
   exp <- data.table(
      c("Min", "1st Qu.", "Mean", "Median", "3rd Qu.", "Max"),
      c(1,1,1,1,1,1),
      c(1,1,1,1,1,1),
      c(-1,-1,0,0,1,1),
      c(1,1,1,1,1,1),
      c(-100,-100,0,0,100,100),
      c(100,100,100,100,100,100)
   )
   setnames(exp, c("V1","V2","V3","V4","V5", "V6", "V7"),
            c("", "a [BPM]", "b [BPM]", 'error [BPM]', 'absolute error [BPM]', 'relative error [%]', 'absolute relative error [%]')
   )
   
   checkEquals(target = exp, current = calculate_quantile(table))
   checkEquals(target = NULL, current = calculate_quantile(table,NULL,"b"))
   checkEquals(target = NULL, current = calculate_quantile(table,"a",NULL))
}

# calculate_calculation ========================

test.calculate_calculation_no_columns <- function(){
   checkEquals(target = NULL, current = calculate_calculation(data.table(10,20,30)))
}

test.calculate_calculation_bad_input <- function(){
   checkEquals(target = NULL, current = calculate_calculation(NULL))
}

test.calculate_calculation1 <- function(){
   table <- data.table(
      c(1,2,3,4,5,6,7,8,9,10),
      c(1,2,3,4,5,6,7,8,9,10),
      c(10,9,8,7,6,5,4,3,2,1)
   )
   setnames(table, c("V1", "V2", "V3"), c("residues", "bpm.x", "bpm.y"))
   
   expected <- data.table(
      c("Dispersion", "Standard deviation", "Corelation", "Error SD"),
      c(38.5, 6.204837, -1, 3.027650)
   )
   checkEquals(target = expected, current = calculate_calculation(table), tolerance = 0.0000001)
}
   
test.calculate_calculation2 <- function(){
   table <- data.table(
      c(1,2,3,4,5,6,7,8,9,10),
      c(1,2,3,4,5,6,7,8,9,10),
      c(10,9,8,7,6,5,4,3,2,1)
   )
   setnames(table, c("V1", "V2", "V3"), c("residues", "bpm.x", "bpm.y"))
   
   expected <- data.table(
      c("Dispersion", "Standard deviation", "Error SD"),
      c(38.5, 6.204837, 3.027650)
   )
   checkEquals(target = expected, current = calculate_calculation(table, FALSE), tolerance = 0.0000001)
   
}

# calculate_quantile_mean =======================

test.calculate_quantile_mean1 <- function() {
   list <- c(1,2,3,4,5,6,7,8,9,10)
   expected <- c(1, 3.25, 5.5, 5.5, 7.75, 10)
   checkEquals(target = expected, current = as.vector(calculate_quantile_mean(list)))
}

test.calculate_quantile_mean2 <- function() {
   list <- c(0)
   expected <- c(0, 0, 0, 0, 0, 0)
   checkEquals(target = expected, current = as.vector(calculate_quantile_mean(list)))
}

test.calculate_quantile_mean3 <- function() {
   list <- c(1)
   expected <- c(1, 1, 1, 1, 1, 1)
   checkEquals(target = expected, current = as.vector(calculate_quantile_mean(list)))
}

test.calculate_quantile_mean4 <- function() {
   expected <- c(NaN, NaN, NaN, NaN, NaN, NaN)
   checkEquals(target = expected, current = as.vector(calculate_quantile_mean(NULL)))
}

test.calculate_quantile_mean5 <- function() {
   checkException(expr = as.vector(calculate_quantile_mean(NA)))
}

# filter_data ===================================

create_table <- function(){
   table <- data.table(
      c(123, 124, 125, 126, 127, 128, 129, 130, 0, 132),
      c(10,3,0,2,1,1,3,3,2,0),
      c(1,2,3,4,0,6,7,8,9,10),
      c(10,9,0,7,6,5,4,3,2,1)
   )
   setnames(table, c("V1", "V2", "V3", "V4"), c("time", "residues", "bpm.x", "bpm.y"))
   table
}

test.filter_data_inputs <- function() {
   options <- c('izv','io')
   table <- create_table()
   # bad columns
   checkEquals(target = NULL, current = filter_data(data = data.table(10,20,30), options = options))
   # bad options
   checkEquals(target = table, current = filter_data(data = table, options = NULL))
}

test.filter_data_no_options <- function(){
   table <- create_table()
   checkEquals(target = table, current = filter_data(data = table, options = c("")))
}

test.filter_data_izv_io <- function(){
   options <- c('izv', 'io')
   table <- create_table()
   
   expected <- data.table( # izv & io
      c(124, 126, 128, 129, 130, 132),
      c(3,2,1,3,3,0),
      c(2,4,6,7,8,10),
      c(9,7,5,4,3,1)
   )
   setnames(expected, c("V1", "V2", "V3", "V4"), c("time", "residues", "bpm.x", "bpm.y"))
   checkEquals(target = expected, current = filter_data(data = table, options = options))
}

test.filter_data_izv <- function(){
   table <- create_table()
   
   expected <- data.table(# izv
      c(123, 124, 126, 128, 129, 130, 132),
      c(10,  3,   2,   1,   3,   3,   0),
      c(1,   2,   4,   6,   7,   8,   10),
      c(10,  9,   7,   5,   4,   3,   1)
   )
   setnames(expected, c("V1", "V2", "V3", "V4"), c("time", "residues", "bpm.x", "bpm.y"))
   
   checkEquals(target = expected, current = filter_data(data = table, options = c('izv', 'io'), outliers = FALSE))
   checkEquals(target = expected, current = filter_data(data = table, options = c('izv')))
}
   
test.filter_data_io <- function(){
   table <- create_table()
   
   expected <- data.table(# io
      c(124, 125, 126, 127, 128, 129, 130, 0, 132),
      c(3,0,2,1,1,3,3,2,0),
      c(2,3,4,0,6,7,8,9,10),
      c(9,0,7,6,5,4,3,2,1)
   )
   setnames(expected, c("V1", "V2", "V3", "V4"), c("time", "residues", "bpm.x", "bpm.y"))
   
   checkEquals(target = expected, current = filter_data(data = table, options = c('io')))
}

# data_sampling =================================

test.data_sampling_inputs <- function() {
   checkEquals(target = NULL, current = data_sampling(NULL, "", "", 10))
   checkEquals(target = NULL, current = data_sampling(data.table(), "", "", 10))
   
   table <- data.table(c(10), c(20))
   checkEquals(target = NULL, current = data_sampling(table, "12.12.2012 12:12:12", "12.12.2012 12:12:13", 1))
   setnames(table, c("V1", "V2"), c("time", "bpm"))
   checkEquals(target = NULL, current = data_sampling(table, NULL, "12.12.2012 12:12:12", 10))
   checkEquals(target = NULL, current = data_sampling(table, "12.12.2012 12:12:12", NULL, 10))
   checkEquals(target = NULL, current = data_sampling(table, "12.12.2012 12:12:12", "12.12.2012 12:12:12", "a"))
   checkEquals(target = NULL, current = data_sampling(table, "", "12.12.2012 12:12:12", 10))
   checkEquals(target = NULL, current = data_sampling(table, "12.12.2012 12:12:12", "", 10))
   checkEquals(target = NULL, current = data_sampling(table, 10, 10, 10))
   checkEquals(target = NULL, current = data_sampling(table, "12.12.2012 12:12:12", "12.12.2012 12:12:11", 1))
}

test.data_sampling1 <- function() {
   table <- data.table(c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16), c(20,30,30,22,21,27,25,31,24,20,26,18,20,24,29,27))
   setnames(table, c("V1", "V2"), c("time", "bpm"))
   
   startTime = "01.01.1970 01:00:01"
   endTime = "01.01.1970 01:00:16"
   
   expected <- data.table(c(1,3,5,7,9,11,13,15), c(20,30,21.5,26,27.5,23,19,26.5))
   setnames(expected, c("V1", "V2"), c("time", "bpm"))
   
   checkEquals(target = expected, current = data_sampling(table, startTime, endTime, 2))
}

test.data_sampling2 <- function() {
   table <- data.table(c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16), c(20,30,30,22,21,27,25,31,24,20,26,18,20,24,29,27))
   setnames(table, c("V1", "V2"), c("time", "bpm"))
   
   startTime = "01.01.1970 01:00:00"
   endTime = "01.01.1970 01:00:17"
   
   expected <- data.table(c(0,2,4,6,8,10,12,14,16), c(0,25,26,24,28,22,22,22,28))
   setnames(expected, c("V1", "V2"), c("time", "bpm"))
   
   checkEquals(target = expected, current = data_sampling(table, startTime, endTime, 2))
}

test.data_sampling3 <- function() {
   table <- data.table(c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16), c(20,30,30,22,21,27,25,31,24,20,26,18,20,24,29,27))
   setnames(table, c("V1", "V2"), c("time", "bpm"))
   
   startTime = "01.01.1970 01:00:30"
   endTime = "01.01.1970 01:00:59"
   
   expected <- data.table(c(30,32,34,36,38,40,42,44,46,48,50,52,54,56,58), c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0))
   setnames(expected, c("V1", "V2"), c("time", "bpm"))
   
   checkEquals(target = expected, current = data_sampling(table, startTime, endTime, 2))
}

test.data_sampling4 <- function() {
   table <- data.table(c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16), c(20,30,30,22,21,27,25,31,24,20,26,18,20,24,29,27))
   setnames(table, c("V1", "V2"), c("time", "bpm"))
   
   startTime = "01.01.1970 01:00:01"
   endTime = "01.01.1970 01:00:16"
   
   checkEquals(target = table, current = data_sampling(table, startTime, endTime, 1))
}

# load_garmin_tcx ================================

test.load_garmin_tcx <- function() {
   
   result <- load_garmin_tcx("shinyapp/tests/test_data/garmin.tcx", 0)
   
   checkTrue(!is.null(result$time))
   checkTrue(!is.null(result$bpm))
   
   checkTrue(is.numeric(result$time))
   checkTrue(is.numeric(result$bpm))
 
   checkTrue(result$time[1] == 1489751199)
   checkTrue(result$bpm[1] == 123)
   
   checkTrue(nrow(result) == 800)
}

test.load_garmin_tcx_time_shift <- function() {
   
   result <- load_garmin_tcx("shinyapp/tests/test_data/garmin.tcx", 300)
   checkTrue(result$time[1] == 1489751499)
   checkTrue(nrow(result) == 800)
   
   result <- load_garmin_tcx("shinyapp/tests/test_data/garmin.tcx", -300)
   checkTrue(result$time[1] == 1489750899)
   checkTrue(nrow(result) == 800)
}

# load_basis_csv =================================

test.load_basis_csv <- function() {
   
   result <- load_basis_csv("shinyapp/tests/test_data/basis.csv", 0)
   
   checkTrue(!is.null(result$time))
   checkTrue(!is.null(result$bpm))
   
   checkTrue(is.numeric(result$time))
   checkTrue(is.numeric(result$bpm))
   
   checkTrue(result$time[1] == 1477954800)
   checkTrue(result$bpm[1] == 135)
   
   checkTrue(nrow(result) == 16352)
}

test.load_basis_csv_time_shift <- function() {
   
   result <- load_basis_csv("shinyapp/tests/test_data/basis.csv", 200)
   checkTrue(result$time[1] == 1477955000)
   checkTrue(nrow(result) == 16352)
   
   result <- load_basis_csv("shinyapp/tests/test_data/basis.csv", -200)
   checkTrue(result$time[1] == 1477954600)
   checkTrue(nrow(result) == 16352)
}

test.load_basis_csv_bad <- function() {
   
   result <- load_basis_csv("shinyapp/tests/test_data/basis_bad.csv", 0)
   checkTrue(is.null(result))
}

# load_fitbit_csv ===============================

test.load_fitbit_csv <- function() {
   
   result <- load_fitbit_csv("shinyapp/tests/test_data/fitbit.csv", 0)
   
   checkTrue(!is.null(result$time))
   checkTrue(!is.null(result$bpm))
   
   checkTrue(is.numeric(result$time))
   checkTrue(is.numeric(result$bpm))
   
   checkTrue(result$time[1] == 1490165095)
   checkTrue(result$bpm[1] == 71)
   
   checkTrue(nrow(result) == 2215)
}

test.load_fitbit_csv_time_shift <- function() {
   
   result <- load_fitbit_csv("shinyapp/tests/test_data/fitbit.csv", 605)
   checkTrue(result$time[1] == 1490165700)
   checkTrue(nrow(result) == 2215)
   
   result <- load_fitbit_csv("shinyapp/tests/test_data/fitbit.csv", -95)
   checkTrue(result$time[1] == 1490165000)
   checkTrue(nrow(result) == 2215)
}

test.load_fitbit_csv_bad <- function() {
   
   result <- load_fitbit_csv("shinyapp/tests/test_data/fitbit_bad.csv", 0)
   checkTrue(is.null(result))
}

# load_chest_strap_csv ==========================


test.load_chest_strap_csv <- function() {
   
   result <- load_chest_strap_csv("shinyapp/tests/test_data/strap.csv", 0)
   
   checkTrue(!is.null(result$time))
   checkTrue(!is.null(result$bpm))
   
   checkTrue(is.numeric(result$time))
   checkTrue(is.numeric(result$bpm))
   
   checkTrue(result$time[1] == 1490257836)
   checkTrue(result$bpm[1] == 79.5)
   
   checkTrue(nrow(result) == 1981)
}

test.load_chest_strap_csv_time_shift <- function() {
   
   result <- load_chest_strap_csv("shinyapp/tests/test_data/strap.csv", 300)
   checkTrue(result$time[1] == 1490258136)
   checkTrue(nrow(result) == 1981)
   
   result <- load_chest_strap_csv("shinyapp/tests/test_data/strap.csv", -220)
   checkTrue(result$time[1] == 1490257616)
   checkTrue(nrow(result) == 1981)
}

test.load_chest_strap_csv_bad <- function() {
   
   result <- load_chest_strap_csv("shinyapp/tests/test_data/strap_bad.csv", 0)
   checkTrue(is.null(result))
}
