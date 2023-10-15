#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(dplyr)
library(metaviz)
source('Forest_plot_function.R')

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Meta-analysis Results"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
          helpText("Get summary results and plot the distribution of your gene of interest"),
          selectInput(inputId = 'selected_protein',label = "Protein",choices = rownames(IDS),selectize = T),
          h4('GO:BP description'),
          textOutput("GO_terms"),
          h4('GO:CC description'),
          textOutput("GO_CC_terms")
),

        # Show a plot of the generated distribution
        mainPanel(
            shiny::plotOutput("forestPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  key_id=reactive(input$selected_protein)
    output$forestPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
      ENSEMBL_id=IDS[key_id(),"ENSEMBL"]
        # draw the histogram with the specified number of bins
      forest_plot_function(ENSEMBL_id)
    })
    
    output$GO_terms=renderText(IDS[key_id(),"Gene Ontology (biological process)"])
    output$GO_CC_terms=renderText(IDS[key_id(),"Gene Ontology (cellular component)"])
    
}

# Run the application 
shinyApp(ui = ui, server = server)
