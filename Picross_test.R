library(shiny)
library(ggplot2)

ui <- fluidPage(
  titlePanel("Picross Game"),

  # Slider pour régler la taille de la grille
  sliderInput("gridSize", "Taille de la Grille", min = 5, max = 10, value = 5),

  # Grille de jeu Picross
  uiOutput("picrossGrid"),

  # Ajouter le code CSS personnalisé pour définir la largeur et la hauteur des boutons
  tags$head(
    tags$style(HTML("
      .square-button {
        width: 50px;
        height: 50px;
      }
    "))
  )
)

server <- function(input, output) {
  # Fonction pour créer la grille de jeu Picross
  output$picrossGrid <- renderUI({
    # Obtenir la taille de la grille depuis le slider
    gridSize <- input$gridSize

    # Créer une matrice de boutons en fonction de la taille de la grille
    picrossMatrix <- matrix(
      data = lapply(1:(gridSize^2), function(i) {
        actionButton(
          inputId = paste0("cell", i),
          label = "",
          class = "square-button",  # Ajouter la classe CSS
          width = 50,
          height = 50
        )
      }),
      nrow = gridSize,
      byrow = TRUE
    )

    # Créer la grille de jeu Picross en utilisant la fonction do.call
    picrossGrid <- do.call(tagList, lapply(1:gridSize, function(i) {
      fluidRow(
        column(width = 2, align = "center", textOutput(paste0("rowHint", i))),
        column(width = 8, tagList(picrossMatrix[i, ])),
        column(width = 2, align = "center", textOutput(paste0("colHint", i)))
      )
    }))

    # Retourner la grille complète
    picrossGrid
  })

  # Générer des indices pour les lignes et les colonnes
  observe({
    gridSize <- input$gridSize
    output_rows <- lapply(1:gridSize, function(i) sample(0:1, gridSize, replace = TRUE))
    output_cols <- lapply(1:gridSize, function(i) sample(0:1, gridSize, replace = TRUE))

    for (i in 1:gridSize) {
      output[[paste0("rowHint", i)]] <- renderText(paste(output_rows[[i]], collapse = " "))
      output[[paste0("colHint", i)]] <- renderText(output_cols[[i]])
    }
  })
}

shinyApp(ui, server)
