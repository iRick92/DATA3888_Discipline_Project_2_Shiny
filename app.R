#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
library(shinydashboardPlus)

# File size limit increased
options(shiny.maxRequestSize = 100*1024^2)

# Define UI for application that draws a histogram
ui <- dashboardPage(
    dashboardHeader(title = "Predictor"),
    dashboardSidebar(
        sidebarMenu(
            menuItem("Calculator", tabName = "calculator", icon = icon("calculator")),
            menuItem("Model Details", tabName = "model_details", icon = icon("project-diagram"))
        )
    ),
    dashboardBody(
        tabItems(
            # First tab content
            tabItem(tabName = "calculator",
                fluidRow(
                    box(
                        title = "GENE EXPRESSION DATA",
                        width = 12,
                        status = "primary",
                        fluidRow(
                            valueBox("GSE107509", "MICROARRAY DATA", icon = icon("database"), color = "light-blue"),
                            valueBox(659, "SAMPLES", icon = icon("user"), color = "light-blue"),
                            valueBox(54715, "GENES", icon = icon("dna"), color = "light-blue"),
                            valueBox("GENE EXPRESSION", "DESCRIPTION", icon = icon(""), color = "light-blue"),
                            valueBox(166, "OUTCOME: SUBCLINICAL ACUTE REJECTION", icon = icon("times"), color = "light-blue"),
                            valueBox(493, "OUTCOME: TRANSPLANT EXCELLENCE", icon = icon("check"), color = "light-blue")
                        )
                    )
                ),
                fluidRow(
                    box(
                        title = "INPUT",
                        width = 4,
                        status = "primary",
                                
                        # Input: Select a file ----
                        fileInput("file1", "Upload Patient CSV File",
                                  multiple = TRUE,
                                  accept = c("text/csv",
                                             "text/comma-separated-values,text/plain",
                                             ".csv")),
                        
                        # Horizontal line ----
                        tags$hr(),
                        
                        # Input: Checkbox if file has header ----
                        checkboxInput("header", "Header", TRUE),
                        
                        # Input: Select separator ----
                        radioButtons("sep", "Separator",
                                     choices = c(Comma = ",",
                                                 Tab = "\t"),
                                     selected = "\t"),
                        
                        # Horizontal line ----
                        tags$hr(),
                        
                        h3("Data Preview"),
                        DT::dataTableOutput("contents") 
                                
                            
                    ),
                    box(
                        title = "OUTPUT",
                        width = 8,
                        status = "primary",
                        
                        valueBoxOutput("name"),  # --------------
                        valueBoxOutput("test1"),  # --------------
                        valueBoxOutput("test2"),  # --------------
                        valueBoxOutput("test3"),  # --------------
                        valueBoxOutput("test4"),  # --------------
                        valueBoxOutput("test5")  # --------------
                    )
                )
            ), # End Calculator Tab
            tabItem(tabName = "model_details",
            )
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    fetch_data_frame = reactive({
        
        # input$file1 will be NULL initially. After the user selects
        # and uploads a file, head of that data file by default,
        # or all rows if selected, will be shown.
        req(input$file1)
        
        df <- read.csv(input$file1$datapath,
                       header = input$header,
                       sep = input$sep)
        
        return(df)
    })

    output$contents <- DT::renderDataTable({
        return(fetch_data_frame())
    })
    
    output$name <- renderValueBox({
        valueBox(
            as.character(colnames(fetch_data_frame())[2]), "Patient Name", icon = icon("thumbs-up", lib = "glyphicon"),
            color = "red"
        )
    })
    
    output$test1 <- renderValueBox({
        valueBox(
            as.character(fetch_data_frame()[22,1]), as.character(fetch_data_frame()[22,2]), icon = icon("thumbs-up", lib = "glyphicon"),
            color = "red"
        )
    })
    output$test2 <- renderValueBox({
        valueBox(
            as.character(fetch_data_frame()[23,1]), as.character(fetch_data_frame()[23,2]), icon = icon("thumbs-up", lib = "glyphicon"),
            color = "green"
        )
    })
    output$test3 <- renderValueBox({
        valueBox(
            as.character(fetch_data_frame()[24,1]), as.character(fetch_data_frame()[24,2]), icon = icon("thumbs-up", lib = "glyphicon"),
            color = "black"
        )
    })
    output$test4 <- renderValueBox({
        valueBox(
            as.character(fetch_data_frame()[25,1]), as.character(fetch_data_frame()[25,2]), icon = icon("thumbs-up", lib = "glyphicon"),
            color = "black"
        )
    })
    output$test5 <- renderValueBox({
        valueBox(
            as.character(fetch_data_frame()[26,1]), as.character(fetch_data_frame()[26,2]), icon = icon("thumbs-up", lib = "glyphicon"),
            color = "black"
        )
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
