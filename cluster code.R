## Clustering
library(dplyr)
library(cluster)
library(skimr)
library(factoextra)

setwd("/Users/ben/Documents/Data Science/Assignment/ASDM/TASK3")
wine_data <- read.csv("wine-clustering.csv")

nrow(wine_data)
ncol(wine_data)
skim(wine_data)

wine_scal <- scale(wine_data)

fviz_nbclust(wine_data, kmeans, method = "wss")

set.seed(2599)

wine_km <- kmeans(wine_scal, 3, nstart=1)
print(wine_km)

fviz_cluster(list(data=wine_scal, clusters=wine_km$cluster))

distance <- dist(wine_scal, method = "euclidean") 
wine_data.hclust <- hclust(distance)
fviz_dend(wine_data.hclust, k = 3,
          k_colors = c("#2E9FDF", "#00AFBB", "#E7B800", "#FC4E07"),
          color_labels_by_k = TRUE)
print(wine_data.hclust)


## Shiny Dashboard
library(shiny)
library(shinydashboard)

ui <- dashboardPage(
      dashboardHeader(title = "Cluster Plots"),
      dashboardSidebar(),
      dashboardBody(
        
   fluidRow(
      box(plotOutput("plot1", height = 300)),
      box(plotOutput("plot2", height = 300)),
      box(
        title = "Controls",
        sliderInput("slider", "Number of Clusters:", 1, 5, 3), width = 12
      )
    )
  )
)

server <- function(input, output) {
  set.seed(2599)
  
  output$plot1 <- renderPlot({
    wine_km <- kmeans(wine_scal, input$slider, nstart=1)
    fviz_cluster(list(data=wine_scal, clusters=wine_km$cluster))
  })
    output$plot2 <- renderPlot({
      fviz_dend(wine_data.hclust, input$slider,
      
                color_labels_by_k = TRUE)
  })
}

shinyApp(ui, server)

