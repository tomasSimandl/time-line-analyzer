library(shiny)
library(shinythemes)
library(plotly)
library(shinyTime)
library(shinyjs)


shinyUI(list(
   tags$head(
      useShinyjs(),
      tags$script(
         "Shiny.addCustomMessageHandler(\"allertMessage\",
            function(message) {
               alert(message);
            }
         );"
      ),
      tags$style(".color_red{ color: red }")
   ),
   navbarPage("Data Analyzer", id = "navbar", theme = shinytheme("united"),
              # Panel for file input
              tabPanel("File input", value = "tabFileInput",
                       wellPanel(
                          h2("First data set"),
                          fluidRow(
                             column(width = 6, selectInput(inputId = "deviceSelect1", label = "Choose a device", c("Chest strap" = "chest_strap", "Garmin" = "garmin", "Basis" = "basis", "Fitbit" = "fitbit"))),
                             column(width = 6,
                                    div(id = "divTimeZone1", class = "divs",
                                        numericInput(inputId = "timeZone1", label = "Time zone shift [h]", value = 0, min = -12, max = 14, step = 0.25)))
                          ),
                          fluidRow(
                             column(width = 4,
                                    div(id = "divInputLow1", class = "divs",
                                        fileInput(inputId = "inFileLow1", label = "Low", multiple = FALSE))),
                             column(width = 4,
                                    div(id = "divInputMed1", class = "divs",
                                        fileInput(inputId = "inFileMed1", label = "Medium", multiple = FALSE))),
                             column(width = 4,
                                    div(id = "divInputHig1", class = "divs",
                                        fileInput(inputId = "inFileHig1", label = "High", multiple = FALSE)))
                          )
                       ), 
                       wellPanel(
                          h2("Second data set"),
                          fluidRow(
                             column(width = 6, selectInput(inputId = "deviceSelect2", label = "Choose a device", c("Chest strap" = "chest_strap", "Garmin" = "garmin", "Basis" = "basis", "Fitbit" = "fitbit"))), 
                             column(width = 6,
                                    div(id = "divTimeZone2", class = "divs",
                                        numericInput(inputId = "timeZone2", label = "Time zone shift [h]", value = 0, min = -12, max = 14, step = 0.25)))
                          ),
                          fluidRow(
                             column(width = 4,
                                    div(id = "divInputLow2", class = "divs",
                                       fileInput(inputId = "inFileLow2", label = "Low", multiple = FALSE))),
                             column(width = 4,
                                    div(id = "divInputMed2", class = "divs",
                                        fileInput(inputId = "inFileMed2", label = "Medium", multiple = FALSE))),
                             column(width = 4,
                                    div(id = "divInputHig2", class = "divs",
                                        fileInput(inputId = "inFileHig2", label = "High", multiple = FALSE)))
                          )
                       ),
                       wellPanel(
                          h2("Setting"),
                          fluidRow(
                             column(width = 4,
                                    div(id = "divTimeIntervalInput", class = "divs",
                                        numericInput(inputId = "timeIntervalInput", label = "Choose interval for data sampling [s]", value = 1, min = 1, max = 60))),
                             column(width = 4, checkboxGroupInput("otherOptions", "Options:", inline = TRUE, c("Ignore zero values" = "izv", "Ignore outliers" = "io")))
                          ),
                          fluidRow(
                             column(width = 4, h4("Measurement interval for low load")),
                             column(width = 4, h4("Measurement interval for medium load")),
                             column(width = 4, h4("Measurement interval for high load"))
                          ),
                          fluidRow(
                             column(width = 2,
                                    div(id = "divStartMeasLow", class = "divs",
                                        textInput(inputId = "startMeasLow", "From", value = format(Sys.time(), format = "%d.%m.%Y %H:%M:%S")))),
                             column(width = 2,
                                    div(id = "divEndMeasLow", class = "divs",
                                        textInput(inputId = "endMeasLow", "To", value = format(Sys.time(), format = "%d.%m.%Y %H:%M:%S")))),
                             column(width = 2,
                                    div(id = "divStartMeasMed", class = "divs",
                                        textInput(inputId = "startMeasMed", "From", value = format(Sys.time(), format = "%d.%m.%Y %H:%M:%S")))),
                             column(width = 2,
                                    div(id = "divEndMeasMed", class = "divs",
                                        textInput(inputId = "endMeasMed", "To", value = format(Sys.time(), format = "%d.%m.%Y %H:%M:%S")))),
                             column(width = 2,
                                    div(id = "divStartMeasHig", class = "divs",
                                        textInput(inputId = "startMeasHig", "From", value = format(Sys.time(), format = "%d.%m.%Y %H:%M:%S")))),
                             column(width = 2,
                                    div(id = "divEndMeasHig", class = "divs",
                                        textInput(inputId = "endMeasHig", "To", value = format(Sys.time(), format = "%d.%m.%Y %H:%M:%S"))))
                          ),
                          fluidRow(
                             column(width = 4,
                                    div(id = "divTimeShiftLow", class = "divs",
                                        numericInput(inputId = "timeShiftLow", label = "Time shift for low load [s]", value = 0, min = -3600, max = 3600))),
                             column(width = 4,
                                    div(id = "divTimeShiftMed", class = "divs",
                                        numericInput(inputId = "timeShiftMed", label = "Time shift for low load [s]", value = 0, min = -3600, max = 3600))),
                             column(width = 4,
                                    div(id = "divTimeShiftHig", class = "divs",
                                        numericInput(inputId = "timeShiftHig", label = "Time shift for low load [s]", value = 0, min = -3600, max = 3600)))
                          )
                       ),
                       wellPanel(
                          actionButton(inputId = "submitBtn", label = "Submit", width = "100%")
                       )
              ),
              
              tabPanel("LOW", value = "tabLow",
                       h1("Compare of two time lines for low load"),
                       wellPanel(
                          h2("Calculations"),
                          tableOutput("quantileLow"),
                          tableOutput("calculationLow")
                       ),
                       wellPanel(
                          h2("Plot comparing low BPMs"),
                          plotlyOutput("plotLow")
                       ),
                       wellPanel(
                          h2("Plot of residues for low BPMs"),
                          plotlyOutput("plotResiLow")
                       ),
                       wellPanel(
                          h2("Bland-altman plot"),
                          plotlyOutput("BAPlotLow")
                       ),
                       wellPanel(
                          h2("Box plot showing outliers"),
                          plotlyOutput("boxPlotLow"),
                          span(textOutput("boxPlotWarning1"), style="color:red")
                       )
              ),
              
              tabPanel("MEDIUM", value = "tabMedium",
                       h1("Compare of two time lines for medium load"),
                       wellPanel(
                          h2("Calculations"),
                          tableOutput("quantileMed"),
                          tableOutput("calculationMed")
                       ),
                       wellPanel(
                          h2("Plot comparing medium BPMs"),
                          plotlyOutput("plotMed")
                       ),
                       wellPanel(
                          h2("Plot of residues for medium BPMs"),
                          plotlyOutput("plotResiMed")
                       ),
                       wellPanel(
                          h2("Bland-altman plot"),
                          plotlyOutput("BAPlotMed")
                       ),
                       wellPanel(
                          h2("Box plot showing outliers"),
                          plotlyOutput("boxPlotMed"),
                          span(textOutput("boxPlotWarning2"), style="color:red")
                       )
              ),
              
              tabPanel("HIGH", value = "tabHigh",
                       h1("Compare of two time lines for high load"),
                       wellPanel(
                          h2("Calculations"),
                          tableOutput("quantileHig"),
                          tableOutput("calculationHig")
                       ),
                       wellPanel(
                          h2("Plot comparing hight BPMs"),
                          plotlyOutput("plotHig")
                       ),
                       wellPanel(
                          h2("Plot of residues for high BPMs"),
                          plotlyOutput("plotResiHig")
                       ),
                       wellPanel(
                          h2("Bland-altman plot"),
                          plotlyOutput("BAPlotHig")
                       ),
                       wellPanel(
                          h2("Box plot showing outliers"),
                          plotlyOutput("boxPlotHig"),
                          span(textOutput("boxPlotWarning3"), style="color:red")
                       )
              ),
              
              tabPanel("Summary", value = "tabSummary",
                       h1("Compare of two time lines for every measurement load"),
                       wellPanel(
                          h2("Calculations"),
                          tableOutput("quantileSum"),
                          tableOutput("calculationSum")
                       )
              ),
              
              # panel for tables with input files data
              tabPanel("Tables", value = "tabTables",
                       tabsetPanel(
                          tabPanel("Low", tableOutput('tableLow')), 
                          tabPanel("Medium", tableOutput('tableMed')), 
                          tabPanel("High", tableOutput('tableHig'))
                       )
              )
   )
   )
) 