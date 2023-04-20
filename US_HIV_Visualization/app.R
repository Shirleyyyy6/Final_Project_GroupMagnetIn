#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
source("overview.R")
year_var <- unique(overview_map$Year)
select_var <- c("Morbidity", "Mortality")

# Define UI for application that draws a histogram
ui <- fluidPage(
  navbarPage(
  title = "US HIV Visualization",
  tabPanel("Overview",
           sidebarLayout(
             sidebarPanel(selectInput("year",
                                      "Year",
                                      choices = year_var),
                          selectInput("dataset",
                                      "By",
                                      choices = select_var)),
             mainPanel( 
               tabsetPanel(
                 tabPanel("Plot",plotlyOutput("OverviewPlot")),
                 tabPanel("Barplot",plotlyOutput("OverviewBar")),
                 tabPanel("Table",tableOutput("OverviewTable"))
           )
             
           ))),
  tabPanel("Trend",
           sidebarLayout(
             sidebarPanel(
               sliderInput("bins",
                           "Number of bins:",
                           min = 1,
                           max = 50,
                           value = 30)
             ),
             
             # Show a plot of the generated distribution
             mainPanel(
               plotOutput("distPlot")
             )
           )
           ),
  tabPanel("About",
           includeMarkdown("about.rmd"))
  )
  

)


# Define server logic required to draw a histogram
server <- function(input, output) {
  source_overview <- reactive({
    if(input$dataset == "Morbidity"){
      data <- overview_map}
    if(input$dataset == "Mortality"){
      data <- mortality_map
    }
    return(data)
  })
    
  output$OverviewPlot <- renderPlotly({
    # generate bins based on input$bins from ui.R
    create_overview(source_overview(),input$year)
  })
  output$OverviewBar <- renderPlotly({
    overview_barplot(source_overview(),input$year)
  })  
  output$OverviewTable <- renderTable({
    overview_table(source_overview(),input$year)
  })  
}

# Run the application 
shinyApp(ui = ui, server = server)
