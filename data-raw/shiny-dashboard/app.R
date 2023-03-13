library(shiny)
library(shinydashboard)

comp.data <- load("data/competition-dataset.rda")
dat.allyears <- na.omit(dat.allyears)
msa.name <- read.csv("data/msa-names-full.csv")
msa.names <- get_msanech(msa.name)

# Define UI for app that draws a histogram ----
ui <- fluidPage(
  
  titlePanel(div("Nonprofit Sector Competition Dashboard", style = "color: #1696d2" )),

  sidebarLayout(
    sidebarPanel(
      selectInput("msa", label = "Select MSA:", 
                  choices = msa.names$NAME, 
                  selected = NULL),
    
      selectInput(
                  "metric", "Metric:",
                  c("HHI" = "hhi",
                    "Normalized HHI" = "nhhi",
                    "Concentration Ratio-1" = "CR1",
                    "Concentration Ratio-2" = "CR2",
                    "Concentration Ratio-3" = "CR3",
                    "Concentration Ratio-4" = "CR4",
                    "Concentration Ratio-5" = "CR5",
                    "Concentration Ratio-6" = "CR6",
                    "Concentration Ratio-7" = "CR7",
                    "Concentration Ratio-8" = "CR8",
                    "Kwoka Index" = "kindex",
                    "Gini Coefficient" = "gini",
                    "Density" = "densityper100000",
                    "Density-Small" = "densitysmallper100000",
                    "Density-Big" = "densitybigper100000",
                    "Density-Commercial" = "density_commercial"),
                  selected = "hhi"),
      
      conditionalPanel(condition = "input.tabselected==2", 
                       selectInput("second_metric", "Second Metric:",
                                   c("HHI" = "hhi",
                                     "Normalized HHI" = "nhhi",
                                     "Concentration Ratio-1" = "CR1",
                                     "Concentration Ratio-2" = "CR2",
                                     "Concentration Ratio-3" = "CR3",
                                     "Concentration Ratio-4" = "CR4",
                                     "Concentration Ratio-5" = "CR5",
                                     "Concentration Ratio-6" = "CR6",
                                     "Concentration Ratio-7" = "CR7",
                                     "Concentration Ratio-8" = "CR8",
                                     "Kwoka Index" = "kindex",
                                     "Gini Coefficient" = "gini",
                                     "Density" = "densityper100000",
                                     "Density-Small" = "densitysmallper100000",
                                     "Density-Big" = "densitybigper100000",
                                     "Density-Commercial" = "density_commercial"),
                                   selected = "density_commercial")),
      
      selectInput("subsector", "Sub Sector:",
                  c("Arts" = "Arts",
                    "Education" = "Education",
                    "Environmental" = "Environmental",
                    "Health" = "Health",
                    "Human Services" = "Human Services",
                    "International" = "International",
                    "Mutual Benefit" = "Mutual Benefit",
                    "Public Benefit" = "Public Benefit",
                    "Religion" = "Religion",
                    "Universities" = "Universities",
                    "Unknown" = "Unknown"
                    ),
                  selected = "Arts"),
      
      
      
      numericInput("year", "Year:", 2019, min = 1990, max = 2019)
    
      
    ),
    
    
    mainPanel(
      
      
      tabsetPanel(type = "tabs",
                  tabPanel("Histogram", br(), plotlyOutput("display_metric_hist"),textOutput("display_desc")),
                  tabPanel("Scatter", br(), plotlyOutput("display_metric_scatter"), value=2),
                  tabPanel("Grid", br(), plotlyOutput("display_metric_grid")),
                  tabPanel("Boxplot", br(), plotlyOutput("display_metric_box")),
                  tabPanel("Table", DTOutput('table')),
                  tabPanel("Resource", uiOutput("url")),
                  id='tabselected'
      ),
      #span(textOutput("display_msa"), style="color:red"),
      
    )
  
  )
)


server <- function(input, output) {
  
  # output$display_msa <- renderText({
  #   if(!check_if_exists(dat.allyears, get_msacode(msa.names, input$msa),  input$metric, input$subsector, input$year)){
  #     paste("The data for selected MSA does not exist!!! MSA: ", input$msa)
  #   }
  # })
  
  output$display_metric_hist <- renderPlotly({
    get_distribution(dat.allyears, get_msacode(msa.names, input$msa),  input$metric, input$subsector, input$year)
    
  })
  
  output$display_metric_scatter <- renderPlotly({
    get_scatter(dat.allyears, get_msacode(msa.names, input$msa),  input$metric, input$second_metric, input$subsector, input$year)
  })
  
  output$display_metric_grid <- renderPlotly({
     get_gridplot_hist(dat.allyears, get_msacode(msa.names, input$msa),  input$metric, input$year) %>% layout(height = 800, width = 1200)
  })
  output$display_metric_box <- renderPlotly({
    get_boxplot(dat.allyears, get_msacode(msa.names, input$msa),  input$subsector, input$year) %>% layout(height = 800, width = 1200)
  })
  
  output$display_desc <- renderText({
    get_desc(input$metric)
  })
  
  output$table <- renderDT({
    # get_table(dat.allyears, get_msacode(msa.names, input$msa),  input$metric, input$subsector, input$year)
    datatable(get_table(dat.allyears, get_msacode(msa.names, input$msa),  input$metric, input$subsector, input$year),
              extensions = 'Buttons', 
              options = list(scrollX=TRUE, lengthMenu = c(5,10,15),
                             paging = TRUE, searching = TRUE,
                             fixedColumns = TRUE, autoWidth = TRUE,
                             ordering = TRUE, dom = 'tB',
                             buttons = list( 
                               list(extend = 'csv',   filename =  paste("npcompete", input$metric, sep = "-")),
                               list(extend = 'excel', filename =  paste("npcompete", input$metric, sep = "-")))))
  })
  
  output$url <- renderUI({
    fluidRow(
      h4("npcompete Package: ", a("Click here", href = "https://github.com/Nonprofit-Open-Data-Collective/npcompete")),
      h4("Competition Dataset: ", a("Click here", href = "https://github.com/Nonprofit-Open-Data-Collective/npcompete/tree/main/data-raw"))
    )
    
  })
  
}

shinyApp(ui=ui, server = server)