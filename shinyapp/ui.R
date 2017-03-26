library(shiny)
library(shinythemes)
library(plotly)
library(shinyTime)

shinyUI(
  navbarPage("Data Analyzer", id = "navbar", theme = shinytheme("united"),
             
    # Panel for file input
    tabPanel("File input", value = "tabFileInput",
      wellPanel(
        h2("First data set"),
        fluidRow(
           column(width = 6, selectInput(inputId = "deviceSelect1", label = "Choose a device", c("Chest strap" = "chest_strap", "Garmin" = "garmin", "Basis" = "basis", "Fitbit" = "fitbit"))),
           column(width = 6, numericInput(inputId = "timeZone1", label = "Time zone shift [h]", value = 0, min = -12, max = 14, step = 0.25))
        ),
        fluidRow(
          column(width = 4,fileInput(inputId = "inFileLow1", label = "Low", multiple = FALSE)),
          column(width = 4,fileInput(inputId = "inFileMed1", label = "Medium", multiple = FALSE)),
          column(width = 4,fileInput(inputId = "inFileHig1", label = "High", multiple = FALSE))
        )
      ), 
      wellPanel(
        h2("Second data set"),
        fluidRow(
           column(width = 6, selectInput(inputId = "deviceSelect2", label = "Choose a device", c("Chest strap" = "chest_strap", "Garmin" = "garmin", "Basis" = "basis", "Fitbit" = "fitbit"))), 
           column(width = 6, numericInput(inputId = "timeZone2", label = "Time zone shift [h]", value = 0, min = -12, max = 14, step = 0.25))
        ),
        fluidRow(
          column(width = 4,fileInput(inputId = "inFileLow2", label = "Low", multiple = FALSE)),
          column(width = 4,fileInput(inputId = "inFileMed2", label = "Medium", multiple = FALSE)),
          column(width = 4,fileInput(inputId = "inFileHig2", label = "High", multiple = FALSE))
        )
      ),
      wellPanel(
         h2("Setting"),
         fluidRow(
            column(width = 4, numericInput(inputId = "timeIntervalInput", label = "Choose interval for data sampling [s]", value = 1, min = 1, max = 60)),
            column(width = 4, checkboxGroupInput("otherOptions", "Options:", inline = TRUE, c("Ignore zero values" = "izv", "Ignore outliers" = "io")))
         ),
         fluidRow(
            column(width = 4, h4("Measurement interval for low load")),
            column(width = 4, h4("Measurement interval for medium load")),
            column(width = 4, h4("Measurement interval for high load"))
         ),
         fluidRow(
            column(width = 2, textInput(inputId = "startMeasLow", "From", value = format(Sys.time(), format = "%d.%m.%Y %H:%M:%S"))),
            column(width = 2, textInput(inputId = "endMeasLow", "To", value = format(Sys.time(), format = "%d.%m.%Y %H:%M:%S"))),
            column(width = 2, textInput(inputId = "startMeasMed", "From", value = format(Sys.time(), format = "%d.%m.%Y %H:%M:%S"))),
            column(width = 2, textInput(inputId = "endMeasMed", "To", value = format(Sys.time(), format = "%d.%m.%Y %H:%M:%S"))),
            column(width = 2, textInput(inputId = "startMeasHig", "From", value = format(Sys.time(), format = "%d.%m.%Y %H:%M:%S"))),
            column(width = 2, textInput(inputId = "endMeasHig", "To", value = format(Sys.time(), format = "%d.%m.%Y %H:%M:%S")))
         ),
         fluidRow(
            column(width = 4, numericInput(inputId = "timeShiftLow", label = "Time shift for low load [s]", value = 0, min = -1000, max = 1000)),
            column(width = 4, numericInput(inputId = "timeShiftMed", label = "Time shift for low load [s]", value = 0, min = -1000, max = 1000)),
            column(width = 4, numericInput(inputId = "timeShiftHig", label = "Time shift for low load [s]", value = 0, min = -1000, max = 1000))
         )
      ),
      actionButton(inputId = "submitBtn", label = "Submit")
    ),
    
    tabPanel("LOW", value = "tabLow",
             h1("Compare of two time lines for low load"),
             wellPanel(
                h2("Calculations"),
                tableOutput("calculationLow"),
                tableOutput("quantileLow")
             ),
             wellPanel(
                h2("Plot comparing low BPMs"),
                plotlyOutput("plotLow")
             ),
             wellPanel(
                h2("Plot of residuals for low BPMs"),
                plotlyOutput("plotResiLow")
             ),
             wellPanel(
                h2("Box plot showing outliers"),
                plotlyOutput("boxPlotLow"),
                textOutput("boxPlotWarning")
             ),
             wellPanel(
                h2("Bland-altman plot"),
                plotlyOutput("BAPlotLow")
             )
    ),
    
    tabPanel("MEDIUM", value = "tabMedium",
             h1("Compare of two time lines for medium load"),
             wellPanel(
                "Calculations"
             ),
             wellPanel(
                h2("Plot comparing medium BPMs"),
                plotlyOutput("plotMed")
             ),
             wellPanel(
                h2("Plot of residuals for medium BPMs"),
                plotlyOutput("plotResiMed")
             ),
             wellPanel(
                h2("Box plot showing outliers"),
                plotlyOutput("boxPlotMed")
             ),
             wellPanel(
                h2("Bland-altman plot"),
                plotlyOutput("BAPlotMed")
             )
    ),
    
    tabPanel("HIGH", value = "tabHigh",
             h1("Compare of two time lines for high load"),
             wellPanel(
                "Calculations"
             ),
             wellPanel(
                h2("Plot comparing hight BPMs"),
                plotlyOutput("plotHig")
             ),
             wellPanel(
                h2("Plot of residuals for high BPMs"),
                plotlyOutput("plotResiHig")
             ),
             wellPanel(
                h2("Box plot showing outliers"),
                plotlyOutput("boxPlotHig")
             ),
             wellPanel(
                h2("Bland-altman plot"),
                plotlyOutput("BAPlotHig")
             )
    ),
    
    tabPanel("Summary", value = "tabSummary",
             h1("Compare of two time lines for low, medium and height load"),
             wellPanel(
                "Calculations"
             )
    ),
    
    # panel for tables with input files data
    tabPanel("Tables", value = "tabTables",
      tableOutput('tableLow'),
      tableOutput('tableMed'),
      tableOutput('tableHig')
    )
  )
) 