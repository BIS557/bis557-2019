#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(glmnet)

X <- readRDS("X.rds")
prop_helpful <- readRDS("prop_helpful.rds")

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Old Faithful Geyser Data"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
         sliderInput("alpha",
                     "Alpha:",
                     min = 0,
                     max = 1,
                     value = 1,
                     0.01)      ),
      
      # Show a plot of the generated distribution
      mainPanel(
         plotOutput("distPlot"),
         dataTableOutput("table_output")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   
   output$distPlot <- renderPlot({
     plot(cv.glmnet(X, prop_helpful, alpha = input$alpha))
   })
   
   output$table_output <- renderDataTable({
     fit <- cv.glmnet(X, prop_helpful, alpha = input$alpha)
     betas <- fit$glmnet.fit$beta
     row_sum <- apply(betas, 1, sum)
     betas <- betas[row_sum != 0,]
     col_sum <- apply(betas, 2, sum)
     betas <- betas[col_sum != 0,]
     betas
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

