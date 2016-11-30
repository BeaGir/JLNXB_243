
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

loading_label1 <- c("Cat Pee", "Passion Fruit", "Green Pepper", "Mineral", "Smoky", "Citrus")
loading_label2 <- c("Cat Pee", "Passion Fruit", "Green Pepper", "Mineral","Tropical", "Leafy")
loading_label3 <- c("Cat Pee", "Passion Fruit", "Green Pepper", "Mineral","Grassy", "Flinty")
loading_label4 <- c("Cat Pee", "Passion Fruit", "Green Pepper", "Mineral","Leafy")
loading_label5 <- c("Cat Pee", "Passion Fruit", "Green Pepper", "Mineral","Vegetal", "Hay")
loading_label6 <- c("Cat Pee", "Passion Fruit", "Green Pepper", "Mineral","Melon")
loading_label7 <- c("Cat Pee", "Passion Fruit", "Green Pepper", "Mineral")
loading_label8 <- c("Cat Pee", "Passion Fruit", "Greeng Pepper", "Mineral","Grass", "Smoky")
loading_label9 <- c("Cat Pee", "Passion Fruit", "Green Pepper", "Mineral","Peach")
loading_label10 <- c("Cat Pee", "Passion Fruit", "Green Pepper", "Mineral")
loading_label_list <- list(loading_label1, loading_label2, loading_label3, loading_label4, loading_label5,
                           loading_label6, loading_label7, loading_label8, loading_label9, loading_label10)


# Define UI for application
ui <- shinyUI(fluidPage(

  # Application title
  titlePanel("Multiple Factor Analysis for the Wine Data"),

  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      tags$head(
        tags$style(type="text/css", "select { max-width: 240px; }"),
        tags$style(type="text/css", ".span4 { max-width: 290px; }"),
        tags$style(type="text/css", ".well { max-width: 280px; }")
      ),
      selectInput("type",
                  label = "Type of Plot:",
                  choices = list("Common Factor Scores" = 1,
                                 "Biplot of Partial Factor Scores & Partial Loadings"= 2,
                                 "All 10 Biplots" = 3,
                                 "Partial Factor Scores" = 4,
                                 "Partial Loadings" = 5,
                                 "Eigenvalues" = 6), selected = 2),
      sliderInput("cex",
                  "Size of Text and Points",
                  min = 1,
                  max = 5,
                  value = 3),
      h6("For partial factor scores, loadings, and biplot:"),
      sliderInput("X",
                  label = "Accessor ID", min = 1, max = 10, step = 1, value = 1),
      h6("For eigenvalue plot only:"),
      sliderInput("ncomps",
                  label = "Number of Components", min = 1, max = 10, step =1 , value =10)
    ),



    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("distPlot")
    )
  )
))

# Define server logic required to draw plots
server <- shinyServer(function(input, output) {

  output$distPlot <- renderPlot({
    if (input$type == 6){
      mymfa2 <- mfa(wines, sets, ncomps = input$ncomps, T, scaling_vec)
      par(mfrow = c(5,2))
      barplot(mymfa2$eigen_values, main = "Eigenvalues in Descending Order",
              xlab = "Eigenvalue", border=NA, col = "#1e62cecc" , cex.main = 1.5*(2+0.3*(input$cex-2)), cex.lab = 2+0.3*(input$cex-2),
              ylim = c(0,max(mymfa2$eigen_values)+0.1), names.arg = seq(1:input$ncomps))
    }
    if(input$type == 2){
      par(mfrow = c(5,2))
      plot(mymfa, type = input$type, X = input$X, d1 = 1, d2 = 2, loading_labels = loading_labels[,input$X], cex = 2+0.3*(input$cex-2))
      title(sub = paste("Accessor",input$X), cex.sub = 2)
    }
    if(input$type == 5){
      par(mfrow = c(5,2))
      plot(mymfa, type = input$type, X = input$X, d1 = 1, d2 = 2, loading_labels = loading_label_list[[input$X]], cex = 2+0.3*(input$cex-2))
      title(sub = paste("Accessor",input$X), cex.sub = 2)
    }
    if(input$type == 1){
      par(mfrow = c(5,2))
      plot(mymfa, type = input$type, X = input$X, d1 = 1, d2 = 2, loading_labels = loading_labels, cex = 2+0.3*(input$cex-2))
    }
    if(input$type == 4){
      par(mfrow = c(5,2))
      plot(mymfa, type = input$type, X = input$X, d1 = 1, d2 = 2, loading_labels = loading_labels, cex = 2+0.3*(input$cex-2))
      title(sub = paste("Accessor",input$X), cex.sub = 2)
    }
    if(input$type == 3){
      par(mfrow = c(5,2))
      plot(mymfa, type = 2, X = 1, d1 = 1, d2 = 2, loading_labels = loading_labels[,1], cex = 2+0.3*(input$cex-2))
      title(sub = "Accessor 1 \n \n \n", cex.sub = 2)
      plot(mymfa, type = 2, X = 2, d1 = 1, d2 = 2, loading_labels = loading_labels[,2], cex = 2+0.3*(input$cex-2))
      title(sub = "Accessor 2 \n \n \n", cex.sub = 2)
      plot(mymfa, type = 2, X = 3, d1 = 1, d2 = 2, loading_labels = loading_labels[,3], cex = 2+0.3*(input$cex-2))
      title(sub = "Accessor 3 \n \n \n", cex.sub = 2)
      plot(mymfa, type = 2, X = 4, d1 = 1, d2 = 2, loading_labels = loading_labels[,4], cex = 2+0.3*(input$cex-2))
      title(sub = "Accessor 4 \n \n \n", cex.sub = 2)
      plot(mymfa, type = 2, X = 5, d1 = 1, d2 = 2, loading_labels = loading_labels[,5], cex = 2+0.3*(input$cex-2))
      title(sub = "Accessor 5 \n \n \n", cex.sub = 2)
      plot(mymfa, type = 2, X = 6, d1 = 1, d2 = 2, loading_labels = loading_labels[,6], cex = 2+0.3*(input$cex-2))
      title(sub = "Accessor 6 \n \n \n", cex.sub = 2)
      plot(mymfa, type = 2, X = 7, d1 = 1, d2 = 2, loading_labels = loading_labels[,7], cex = 2+0.3*(input$cex-2))
      title(sub = "Accessor 7 \n \n \n", cex.sub = 2)
      plot(mymfa, type = 2, X = 8, d1 = 1, d2 = 2, loading_labels = loading_labels[,8], cex = 2+0.3*(input$cex-2))
      title(sub = "Accessor 8 \n \n \n", cex.sub = 2)
      plot(mymfa, type = 2, X = 9, d1 = 1, d2 = 2, loading_labels = loading_labels[,9], cex = 2+0.3*(input$cex-2))
      title(sub = "Accessor 9 \n \n \n", cex.sub = 2)
      plot(mymfa, type = 2, X = 10, d1 = 1, d2 = 2, loading_labels = loading_labels[,10], cex = 2+0.3*(input$cex-2))
      title(sub = "Accessor 10 \n \n \n", cex.sub = 2)
    }
  }
  , height = 2700, width = 900
  )
})

# Run the application
shinyApp(ui = ui, server = server)
