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
           fluidRow(
             column(width =4,
               
                 selectInput("year",
                             "Year",
                             choices = year_var,
                             width ="100%"),
                 selectInput("dataset",
                             "By",
                             choices = select_var,width ="100%")
               ),
             column(width = 8,
                    plotOutput("OverviewTitle",height='200px'))
             ),
           fluidRow(

               mainPanel( 
      
                 # tags$head(tags$style("#OverviewPlot, #OverviewBar,  
                 #     #OverviewTable {margin: 0 auto;}")),
                 plotlyOutput("OverviewPlot",width="800px"),
                 plotlyOutput("OverviewBar",width="800px"),
                 tableOutput("OverviewTable")
               
             ))
           ),
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
           includeMarkdown("README.md"))
  )
)
  




# Define server logic required to draw a histogram
server <- function(input, output) {
  source_overview <- reactive({
    if(input$dataset == "Morbidity"){
      data <- overview_map}
    if(input$dataset == "Mortality"){
      data <- mortality_map}
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
  }, align='c',width='500px') 
  output$OverviewTitle <- renderPlot(
    overview_title(source_overview(),input$year)
    )
}


# Run the application 
shinyApp(ui = ui, server = server)
