library(shiny)
library(plotly)
library(hyperSpec)
library(htmlwidgets)
library(markdown)
library(data.table)
source("read_spc.R")
source("dotspc_names.R")
source("transfer_csv.R")

options(shiny.maxRequestSize=10*1024^2)
# Define the UI
ui <- fluidPage(
  
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "style.css")
  ),
  
  titlePanel(title=div(img(src="dt_logo.png", height="5%", width="5%"), "DT:GUI_READ_SPC - Graphische Darstellung von Spektren")
             , windowTitle = "DT:GUI_READ_SPC")
  
  , sidebarPanel(
    fileInput("file_upload"
              , ""
              , multiple = T
              , accept = ".spc"
              , buttonLabel = paste0(".spc/csv-Dateien")
              , placeholder = NA
              , width = "100%")
    
    , downloadButton("download_spc", "Absorptionsspektren")
    , downloadButton("download_ref", "Referenzspektren")
    , downloadButton("download_drk", "Dunkelwertspektren")
    , downloadButton("download_trans", "Transmissionsspektren")
    
    , width = 2)
  
  , mainPanel(
    
    tabsetPanel(type = "tabs",
                tabPanel("Spektren plotten"
                         , br()
                         , plotlyOutput("plot_spc", width = "90%", height = "100%"), br()
                         , plotlyOutput("plot_ref", width = "90%", height = "70%"), br()
                         , plotlyOutput("plot_drk", width = "90%", height = "70%"), br()
                         , plotlyOutput("plot_trans", width = "90%", height = "70%"), br()
                         , br()
                ),
                
                tabPanel("Über",
                         fluidRow(
                           column(6,
                                  includeMarkdown("about.Rmd")# Markdown("about.md")
                           )
                         )
                ),
                
                tabPanel("Versionsverlauf",
                         fluidRow(
                           column(6,
                                  includeMarkdown("Version.Rmd")# Markdown("about.md")
                           )
                         )
                ),
                
                tabPanel("Github",
                         fluidRow(
                           column(6,
                                  includeMarkdown("Github.Rmd")# Markdown("about.md")
                           )
                         )
                )
                
                # ,
                #            column(3,
                #                   img(class="img-polaroid",
                #                       src=paste0("http://upload.wikimedia.org/",
                #                                  "wikipedia/commons/9/92/",
                #                                  "1919_Ford_Model_T_Highboy_Coupe.jpg")),
                #                   tags$small(
                #                     "Source: Photographed at the Bay State Antique ",
                #                     "Automobile Club's July 10, 2005 show at the ",
                #                     "Endicott Estate in Dedham, MA by ",
                #                     a(href="http://commons.wikimedia.org/wiki/User:Sfoskett",
                #                       "User:Sfoskett")
                #                   )
                #            )
                #          )
                # )
                
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
