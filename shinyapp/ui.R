library(shiny)
library(shinythemes)
library(plotly)
library(shinyjs)

shinyUI(list(
   tags$head(
      useShinyjs(),
      tags$script(src ="scripts.js"),
      tags$link(rel = "stylesheet", type = "text/css", href = "style.css")
   ),
   navbarPage("Data Analyzer", id = "navbar", theme = shinytheme("united"),
              # Panel for file input
              tabPanel("File input", value = "tabFileInput",
                       wellPanel(
                          h2("First data set"),
                          fluidRow(
                             column(width = 6, selectInput(inputId = "deviceSelect1", label = "Choose a device", c("Chest strap" = "Chest_strap", "Garmin" = "Garmin", "Basis" = "Basis", "Fitbit" = "Fitbit"))),
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
                             column(width = 6, selectInput(inputId = "deviceSelect2", label = "Choose a device", c("Chest strap" = "Chest_strap", "Garmin" = "Garmin", "Basis" = "Basis", "Fitbit" = "Fitbit"))), 
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
                          h2("Settings"),
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
                                        textInput(inputId = "startMeasLow", "From", value = "---"))),
                             column(width = 2,
                                    div(id = "divEndMeasLow", class = "divs",
                                        textInput(inputId = "endMeasLow", "To", value = "---"))),
                             column(width = 2,
                                    div(id = "divStartMeasMed", class = "divs",
                                        textInput(inputId = "startMeasMed", "From", value = "---"))),
                             column(width = 2,
                                    div(id = "divEndMeasMed", class = "divs",
                                        textInput(inputId = "endMeasMed", "To", value = "---"))),
                             column(width = 2,
                                    div(id = "divStartMeasHig", class = "divs",
                                        textInput(inputId = "startMeasHig", "From", value = "---"))),
                             column(width = 2,
                                    div(id = "divEndMeasHig", class = "divs",
                                        textInput(inputId = "endMeasHig", "To", value = "---")))
                          ),
                          fluidRow(
                             column(width = 4, span(class = "color_red", htmlOutput("lowTimeWarning"))),
                             column(width = 4, span(class = "color_red", htmlOutput("medTimeWarning"))),
                             column(width = 4, span(class = "color_red", htmlOutput("higTimeWarning")))
                          ),
                          fluidRow(
                             column(width = 12, actionButton(inputId = "timeButton", label = "Fill time automatically", width = "100%"))
                          ),
                          fluidRow(
                             column(width = 4,
                                    div(id = "divTimeShiftLow", class = "divs",
                                        numericInput(inputId = "timeShiftLow", label = "Time shift for low load [s]", value = 0, min = -3600, max = 3600))),
                             column(width = 4,
                                    div(id = "divTimeShiftMed", class = "divs",
                                        numericInput(inputId = "timeShiftMed", label = "Time shift for medium load [s]", value = 0, min = -3600, max = 3600))),
                             column(width = 4,
                                    div(id = "divTimeShiftHig", class = "divs",
                                        numericInput(inputId = "timeShiftHig", label = "Time shift for high load [s]", value = 0, min = -3600, max = 3600)))
                          )
                       ),
                       wellPanel(
                          actionButton(inputId = "submitBtn", label = "Submit", width = "100%")
                       )
              ),
              
              tabPanel("Low", value = "tabLow",
                       h1("Comparison of two timelines for low load"),
                       wellPanel(
                          h2("Calculations"),
                          tableOutput("quantileLow"),
                          tableOutput("calculationLow")
                       ),
                       wellPanel(
                          h2("Comparison plot"),
                          p("The plot shows comparison of measurements from two devices"),
                          plotlyOutput("plotLow")
                       ),
                       wellPanel(
                          h2("Plot of residues"),
                          p("The plot shows differences between measurements using two different devices."),
                          plotlyOutput("plotResiLow")
                       ),
                       wellPanel(
                          h2("Bland-Altman plot"),
                          p("The Bland-Altman plot shows comparison of two measurements A and B."),
                          plotlyOutput("BAPlotLow")
                       ),
                       wellPanel(
                          h2("Box plot"),
                          p("The box plot shows outliers of residues as separate points."),
                          plotlyOutput("boxPlotLow"),
                          span(textOutput("boxPlotWarning1"), class = "color_red")
                       )
              ),
              
              tabPanel("Medium", value = "tabMedium",
                       h1("Comparison of two timelines for medium load"),
                       wellPanel(
                          h2("Calculations"),
                          tableOutput("quantileMed"),
                          tableOutput("calculationMed")
                       ),
                       wellPanel(
                          h2("Comparison plot"),
                          p("The plot shows comparison of measurements from two devices"),
                          plotlyOutput("plotMed")
                       ),
                       wellPanel(
                          h2("Plot of residues"),
                          p("The plot shows differences between measurements using two different devices."),
                          plotlyOutput("plotResiMed")
                       ),
                       wellPanel(
                          h2("Bland-Altman plot"),
                          p("The Bland-Altman plot shows comparison of two measurements A and B."),
                          plotlyOutput("BAPlotMed")
                       ),
                       wellPanel(
                          h2("Box plot"),
                          p("The box plot shows outliers of residues as separate points."),
                          plotlyOutput("boxPlotMed"),
                          span(textOutput("boxPlotWarning2"), class = "color_red")
                       )
              ),
              
              tabPanel("High", value = "tabHigh",
                       h1("Comparison of two timelines for high load"),
                       wellPanel(
                          h2("Calculations"),
                          tableOutput("quantileHig"),
                          tableOutput("calculationHig")
                       ),
                       wellPanel(
                          h2("Comparison plot"),
                          p("The plot shows comparison of measurements from two devices"),
                          plotlyOutput("plotHig")
                       ),
                       wellPanel(
                          h2("Plot of residues"),
                          p("The plot shows differences between measurements using two different devices."),
                          plotlyOutput("plotResiHig")
                       ),
                       wellPanel(
                          h2("Bland-Altman plot"),
                          p("The Bland-Altman plot shows comparison of two measurements A and B."),
                          plotlyOutput("BAPlotHig")
                       ),
                       wellPanel(
                          h2("Box plot"),
                          p("The box plot shows outliers of residues as separate points."),
                          plotlyOutput("boxPlotHig"),
                          span(textOutput("boxPlotWarning3"), class = "color_red")
                       )
              ),
              
              tabPanel("Summary", value = "tabSummary",
                       h1("Comparison of two timelines for every measurement load"),
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
              ),
              
              # panel about
              tabPanel("About", value = "tabAbout",
                       div(id = "divAbout",
                           wellPanel(
                              h2("Quick overview"),
                              p("This tool allows you to compare two measurements of heart rate divided in to three
                             parts with difrent physical load. Comparison includes computing of corelation, standard
                             deviation, means, medians, quartils and minimum and maximaum values of measured data 
                             and errors. Measured data are visualizated in graphs."),
                              h2("More informaton"),
                              p("This tool was developed in the R programing language."),
                              p("This work is part of bachelor thesis."),
                              p("Title of bachelor thesis: Software tools for verification of heart rate measurement accuracy."),
                              p("Author: Tomáš Šimandl"),
                              p("Student at University of West Bohemia")
                           )
                       )
              )
   )
)
) 