library(shiny)
library(shinythemes)
library(plotly)

shinyUI(
  navbarPage("Data Analyzer", id = "navbar", theme = shinytheme("yeti"),
             
    # Panel for file input
    tabPanel("File input", value = "tabFileInput",
      wellPanel(
        h2("First data set"),
        fluidRow(
          column(width = 4,fileInput(inputId = "inFileLow1", label = "Low", multiple = FALSE)),
          column(width = 4,fileInput(inputId = "inFileMed1", label = "Medium", multiple = FALSE)),
          column(width = 4,fileInput(inputId = "inFileHig1", label = "High", multiple = FALSE))
        )
      ), 
      wellPanel(
        h2("Second data set"),
        fluidRow(
          column(width = 4,fileInput(inputId = "inFileLow2", label = "Low", multiple = FALSE)),
          column(width = 4,fileInput(inputId = "inFileMed2", label = "Medium", multiple = FALSE)),
          column(width = 4,fileInput(inputId = "inFileHig2", label = "High", multiple = FALSE))
        )
      ),
      actionButton(inputId = "submitBtn", label = "Submit")
    ),
    
    # panel for compare plots
    tabPanel("Plot", value = "tabPlot",
      wellPanel(
         h2("Plot comparing low BPMs"),
         plotlyOutput("plotLow", width = "100%")
      ),
      wellPanel(
         h2("Plot comparing medium BPMs"),
         plotlyOutput("plotMed", width = "100%")
      ),
      wellPanel(
         h2("Plot comparing hight BPMs"),
         plotlyOutput("plotHig", width = "100%")
      )
    ),
    
    # panel for residuas plot
    tabPanel("Residues plot", value = "tabResiduasPlot",
      plotOutput("plotResiLow", width = "100%"),
      plotOutput("plotResiMed", width = "100%"),
      plotOutput("plotResiHig", width = "100%")
    ),
    
    # panel for box plost for outliers visualization
    tabPanel("Outliers", value = "tabOutliers",
      plotOutput("boxPlot", width = "100%")
    ),
    
    # panel for statistic calculation
    tabPanel("SD, Mean, Cov", value = "tabStatisticCalc",
      actionButton(inputId = "submitBtn3", label = "Submit")
    ),
    
    # panel for Bland Altman plot
    tabPanel("Bland&Altman plot", value = "tabBAPlot",
      actionButton(inputId = "submitBtn2", label = "Submit")
    ),
    
    # panel for tables with input files data
    tabPanel("Tables", value = "tabTables",
      tableOutput('tableLow'),
      tableOutput('tableMed'),
      tableOutput('tableHig')
    )
  )
) 
