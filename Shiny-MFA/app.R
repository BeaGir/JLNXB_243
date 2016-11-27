#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(MFA)

wines <- read.csv("wines.csv", stringsAsFactors = FALSE)
sets <- list(2:7, 8:13, 14:19, 20:24, 25:30, 31:35, 36:39, 40:45, 46:50, 51:54)
scaling_vec <- apply(subset(wines, select = unlist(sets)), 2, function(x) sqrt(sum((x - mean(x))^2)))
mymfa <- mfa(wines, sets, ncomps = 2, T, scaling_vec)
loading_labels <- data.frame(a = c("Cat Pee", "Passion Fruit", "Green Pepper", "Mineral", "Smoky", "Citrus"),
                             b = c("Cat Pee", "Passion Fruit", "Green Pepper", "Mineral","Tropical", "Leafy"),
                             c = c("Cat Pee", "Passion Fruit", "Green Pepper", "Mineral","Grassy", "Flinty"),
                             d = c("Cat Pee", "Passion Fruit", "Green Pepper", "Mineral","Leafy", NA),
                             e = c("Cat Pee", "Passion Fruit", "Green Pepper", "Mineral","Vegetal", "Hay"),
                             f = c("Cat Pee", "Passion Fruit", "Green Pepper", "Mineral","Melon", NA),
                             g = c("Cat Pee", "Passion Fruit", "Green Pepper", "Mineral",NA, NA),
                             h = c("Cat Pee", "Passion Fruit", "Greeng Pepper", "Mineral","Grass", "Smoky"),
                             i = c("Cat Pee", "Passion Fruit", "Green Pepper", "Mineral","Peach", NA),
                             j = c("Cat Pee", "Passion Fruit", "Green Pepper", "Mineral",NA, NA))

# Define UI for application
ui <- shinyUI(fluidPage(
   
   # Application title
   titlePanel("MFA wine Data"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
        numericInput("type",
                     label = "type of plot:",
                     min = 1,
                     max = 5,
                     value = 1)
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
         plotOutput("distPlot")
      )
   )
))

# Define server logic required to draw a histogram
server <- shinyServer(function(input, output) {
   
   output$distPlot <- renderPlot({
      # generate a plot
      plot(mymfa, type = input$type, X = 1, d1 = 1, d2 = 2, loading_labels = NULL)
#4 things to fix:
     #1- in type 3, there shall be 10 ouputs, however only 1 is shown
     #2- in type 2 and 5, adding labels might cause some error
     #3- adding more slide bars/etc to make this app fancier
     #4- a plot for eigenvalue
     
        })
})

# Run the application 
shinyApp(ui = ui, server = server)

