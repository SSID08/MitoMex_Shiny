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
#IDS=readRDS('ID_mapping_file')
#meta_objects=readRDS('Meta_objects')
#summary_meta_ENSEMBL=readRDS('Summary_table_for_metaAnalysis')
# Define UI for application that draws a histogram
ui <- fluidPage(
  #theme = bslib::bs_theme(bootswatch = "darkly"),
    # Application title
    titlePanel("Meta-analysis Results"),

    # Sidebar with a slider input for number of bins 
    tabsetPanel(
      tabPanel(title = "Forest plots",
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
            shiny::plotOutput("forestPlot"),
            shiny::tableOutput('table')
        )
    )
),
tabPanel('Volcano Plot',verticalLayout(sidebarLayout(sidebarPanel = sidebarPanel(helpText("Select parameters to customise your volcanoPlot"),
                                      sliderInput(inputId = 'num_studies_VP',label = 'Minimum Number of studies',min = min(Meta_results$`Num. Studies`),
                                                  max=max(Meta_results$`Num. Studies`),value = 5,step = 1,ticks = T),
                                      sliderInput(inputId = 'fc_threshold',label = 'Fold-change threshold',min = 0,max = round(max(abs(Meta_results$LogFC),1)),round = -1,step = 0.2,value = 0.25)),
                                      mainPanel = mainPanel(plotOutput("Volcano_plot")))))))

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
    output$table=renderTable({ENSEMBL_id=IDS[key_id(),"ENSEMBL"]
    out_table=Meta_results[ENSEMBL_id,c("Gene","Uniprot","LogFC","Confidence Interval(lower)","Confidence Interval(Upper)","Num. Participants","Num. Studies","adj.P","sig")]
    out_table})
    
    output$Volcano_plot=renderPlot({
      out_data=Meta_results%>%filter(`Num. Studies`>=input$num_studies_VP,abs(LogFC)>input$fc_threshold)
      out_plot=EnhancedVolcano(out_data,x = 'LogFC',y = 'adj.P',lab = out_data$Gene,title = '',subtitle = '',drawConnectors = T,labSize = 5,pCutoffCol = "P-value",pCutoff = 0.01,
                               FCcutoff = 0.2,legendDropLevels = T)
      plot(out_plot)
    },height = 500)
    
}

# Run the application 
shinyApp(ui = ui, server = server)
