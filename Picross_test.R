library(shiny)
library(ggplot2)

# Fonction pour créer une grille Picross avec indices
creer_grille_picross <- function(taille, puzzle) {
  indices_lignes <- apply(puzzle, 1, function(row) {
    paste(rle(row)$lengths[rle(row)$values == 1], collapse = " ")
  })

  indices_colonnes <- apply(puzzle, 2, function(col) {
    paste(rle(col)$lengths[rle(col)$values == 1], collapse = " ")
  })

  ggplot() +
    geom_tile(aes(x = rep(1:taille, each = taille),
                  y = rep(taille:1, times = taille),
                  fill = factor(puzzle)),
              color = "white", size = 1) +
    geom_text(aes(x = 0, y = taille:1, label = indices_lignes),
              hjust = 1.1, vjust = 0.5, size = 4, color = "black") +
    geom_text(aes(x = 1:taille, y = 0, label = indices_colonnes),
              hjust = 0.5, vjust = -0.5, size = 4, color = "black") +
    scale_fill_manual(values = c("white", "black"),
                      name = "Case",
                      labels = c("Blanc", "Noir")) +
    coord_fixed(ratio = 1) + theme_void() +
    theme(legend.position = "bottom",
          plot.title = element_text(hjust = 0.5, size = 20, face = "bold"),
          axis.text = element_blank(),
          axis.ticks = element_blank(),
          panel.grid = element_blank(),
          legend.text = element_text(size = 12),
          legend.title = element_text(size = 14, face = "bold"),
          plot.margin = unit(c(1, 1, 1, 1), "cm"))
}

ui <- fluidPage(
  titlePanel("Picross Game"),

  # Slider pour régler la taille de la grille
  sliderInput("gridSize", "Taille de la Grille", min = 5, max = 10, value = 5),

  # Grille de jeu Picross
  uiOutput("picrossGrid")
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
          width = 50,  # Largeur fixe pour chaque bouton
          height = 50  # Hauteur fixe pour chaque bouton
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
