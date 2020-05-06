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
library(shinybusy)

library(corrplot)



# File size limit increased
options(shiny.maxRequestSize = 100*1024^2)



# Define UI for application that draws a histogram
ui <- dashboardPage(
    dashboardHeader(title = "Predictor"),
    dashboardSidebar(
        sidebarMenu(
            menuItem("Calculator", tabName = "calculator", icon = icon("calculator")),
            menuItem("Data Explorer", tabName = "data_explorer", icon = icon("project-diagram")),
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
                        
                        
                        # Load Gene Data
                        actionButton(inputId = "load_gene_data", label = "Load Gene Data"),
                                
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
                        valueBoxOutput("test5"),  # --------------
                        DT::dataTableOutput("gse_table")   # --- Need to add this back in if want to use gse
                        
                    )
                )
            ), # End Calculator Tab
            tabItem(tabName = "data_explorer",
                    
                fluidRow(
                    box(
                        title = "CORRELATIONS",
                        width = 12,
                        status = "primary",
                        
                        # Select Number of Correlations 
                        sliderInput("corr_number", "Number correlations (First n/54715):",
                                    min = 0, max = 100, value = 10),
                        
                        plotOutput(outputId = "corrplot")
                    ),
                    
                    box(
                      title = "BOXPLOT GENE EXPRESSION",
                      width = 12,
                      status = "primary",
                        # Issue with step? It is always 2x whatever it is set to 
                        numericInput("sample_number1", "Sample Number 1:", min = 1, max = 659, value = 1,  width = '25%'),
                        numericInput("sample_number2", "Sample Number 2:", min = 1, max = 659, value = 2, width = '25%'),
                        plotOutput(outputId = "gene_expression_comparison_boxplot")
                    )
                )
                    
            ),
            tabItem(tabName = "model_details",
                    DT::dataTableOutput("gse_table_mean")
            )
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    gse = reactive({
        
        # Load gse data
        print("Loading")
        progress <- shiny::Progress$new()
        progress$set(message = "Loading Gene Expression Data", value = 0)
        Sys.sleep(1)
        progress$inc(0.25, detail = "(May take a min)")
        
        gse = readr::read_csv("data/GSE107509_RAW/GSE107509_Matrix.txt")
        
        # Get gene names
        gene_names = gse$Gene
        progress$inc(0.25, detail = "Getting Gene Names")
        Sys.sleep(1)
        
        # Remove first column - The X column we saved
        gse = gse[,-1]
        progress$inc(0.25, detail = "Removing X Column")
        Sys.sleep(1)
        
        # Set row names as gene names
        rownames(gse) = gene_names
        progress$inc(0.25, detail = "Setting Row Names")
        Sys.sleep(1)
        
        progress$inc(1, detail = "Done")
        #progress$close()
        print("Done")
        return(gse)
    })
    
    
    gse_mean = reactive({
      return(read.csv("data/GSE107509_mean.csv"))
    })
  
    
    gse_var = reactive({
      return(read.csv("data/GSE107509_var.csv"))
    })
    
    output$gse_table_mean <- DT::renderDataTable({
      return(data.frame(gse_mean(), Var = gse_var()$Var))
    })
    
    
    
    observeEvent(input$load_gene_data, {
        print("Clicked Button")
        gse()
    })
    
    
    fetch_data_frame = reactive({
        
        # input$file1 will be NULL initially. After the user selects
        # and uploads a file, head of that data file by default,
        # or all rows if selected, will be shown.
        req(input$file1)
        
        progress <- shiny::Progress$new()
        progress$set(message = "Loading Data", value = 0)
        
        df <- read.csv(input$file1$datapath,
                       header = input$header,
                       sep = input$sep)
        progress$inc(1, detail = "Done")
        #progress$close()
        
        return(df)
    })

    output$contents <- DT::renderDataTable({
        print("Loading Selected File")
        return(fetch_data_frame())
    })
    
    output$gse_table <- DT::renderDataTable({
        print("Loading GSE File")
        return(gse())
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
    
    
    output$corrplot = renderPlot({
        cor_matrix<-cor(gse_t[1:input$corr_number])
        diag(cor_matrix)<-0
        return(corrplot(cor_matrix, method="square"))
    })
    
    
    output$gene_expression_comparison_boxplot = renderPlot({
      
      p <- ggplot(melt(gse()[,c(input$sample_number1, input$sample_number2)]), aes(x=variable, y=value)) +  
        geom_boxplot(outlier.colour="black", outlier.shape=16,
                     outlier.size=0.5, notch=FALSE) +
        theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
        labs (x = "Sample", y = "Expression Value") + theme_minimal() + coord_flip()
      return(p)
      
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
