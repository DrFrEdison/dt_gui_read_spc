library(shiny)
library(plotly)
library(hyperSpec)
library(htmlwidgets)

source("read_spc.R")

options(shiny.maxRequestSize=10*1024^2)
# Define the UI
ui <- fluidPage(
  
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "style.css")
  ),
  
  titlePanel(title=div(img(src="dt_logo.png", height="5%", width="5%"), "DT:Spec - Graphische Darstellung von Spektren")
             , windowTitle = "DT:Spec")
  
  , sidebarPanel(
    fileInput("file_upload"
              , ""
              , multiple = T
              , accept = ".spc"
              , buttonLabel = paste0("Browse oder drag & drop .spc-Dateien")
              , placeholder = NA
              , width = "25%")
    
    , downloadButton("download_spc", "Absorptionsspektren als .html herunterladen")
    , downloadButton("download_ref", "Referenzspektren als .html herunterladen")
    , downloadButton("download_drk", "Dunkelwertspektren als .html herunterladen")
    , downloadButton("download_trans", "Transmissionsspektren als .html herunterladen")
    
    , width = 2)
  
  , mainPanel(
    
    tabsetPanel(type = "tabs",
                tabPanel("Plot .spc"
                         , br()
                         , plotlyOutput("plot_spc", width = "90%", height = "70%"), br()
                         , plotlyOutput("plot_ref", width = "90%", height = "70%"), br()
                         , plotlyOutput("plot_drk", width = "90%", height = "70%"), br()
                         , plotlyOutput("plot_trans", width = "90%", height = "70%"), br()
                         , br()
                ),
                tabPanel("App-Info"
                         , br()
                         , paste0("DT:Spec - Lese und plotte .spc-Dateien,", br(), 
                         "Version: V0.0.1-")
                         , br()
                )
    )
  )
)

# Define the server
server <- function(input, output) {
  
  # Read the uploaded CSV file and create the plot
  
  uploaded_files <- reactiveValues(paths = NULL)
  
  observeEvent(input$file_upload, {
    paths <- uploaded_files$paths
    new_paths <- c(paths, input$file_upload$datapath)
    uploaded_files$paths <- new_paths
  })
  
  plot_upload <- reactive({
    upload_file <- uploaded_files$paths
    if (!is.null(upload_file)) {
      
      plot_data <- read_spc_files(upload_file
                                  , baseline = NA
                                  , pngplot = F
                                  , plotlyplot = T
                                  , recursive = F
                                  , colp = NA
                                  , shinyoutput  = T)
      return(plot_data)
      
    }
    
  })
  
  # Display the plot
  output$plot_spc <- renderPlotly({plot_upload()$spc})
  
  output$plot_ref <- renderPlotly({plot_upload()$ref})
  
  output$plot_drk <- renderPlotly({plot_upload()$drk})
  
  output$plot_trans <- renderPlotly({plot_upload()$trans})
  
  output$download_spc <- downloadHandler(
    filename = paste0( substr(gsub("-", "", Sys.Date()), 3, 8), "_spc.html"),
    
    content = function(file = filename) {
      saveWidget( plot_upload()$spc, file, selfcontained = TRUE)
    }
  )
  
  output$download_ref <- downloadHandler(
    filename = paste0( substr(gsub("-", "", Sys.Date()), 3, 8), "_ref.html"),
    
    content = function(file = filename) {
      saveWidget( plot_upload()$ref, file, selfcontained = TRUE)
    }
  )
  
  output$download_drk <- downloadHandler(
    filename = paste0( substr(gsub("-", "", Sys.Date()), 3, 8), "_drk.html"),
    
    content = function(file = filename) {
      saveWidget( plot_upload()$drk, file, selfcontained = TRUE)
    }
  )
  
  output$download_trans <- downloadHandler(
    filename = paste0( substr(gsub("-", "", Sys.Date()), 3, 8), "_trans.html"),
    
    content = function(file = filename) {
      saveWidget( plot_upload()$trans, file, selfcontained = TRUE)
    }
  )
}

# Run the app
shinyApp(ui, server)