
#' Define the UI
#' @return UI object
library(shiny)
library(shinyjs)
ui <- fluidPage(
  titlePanel("Picross Game"),
  
  selectInput("gridSize", "Taille de la Grille",
              choices = c(5, 6, 7, 8, 9, 10),  # Options de taille de grille
              selected = 5),  # Taille de grille par défaut
  
  selectInput("difficultyLevel", "Niveau de difficulté",
              choices = c("Facile", "Moyen", "Difficile"),
              selected = "Facile"),
  
  actionButton("generateButton", "Générer une nouvelle grille"),
  actionButton("checkSolutionButton", "Vérifier la solution"),
  
  fluidRow(
    column(3, align = "center", 
           uiOutput("rowIndicesTable")
    ),
    column(6, align = "center", 
           uiOutput("picrossGrid")
    ),
    column(3, align = "center", 
           uiOutput("columnIndicesTable")
    )
  )
)
