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

# Définition de l'interface Shiny
ui <- fluidPage(
  titlePanel("Jeu Picross"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("taille", "Taille de la grille", min = 5, max = 15, value = 10)
    ),
    mainPanel(
      plotOutput("grillePicross", width = "100%", height = "600px")
    )
  )
)

# Définition du serveur Shiny
server <- function(input, output) {
  output$grillePicross <- renderPlot({
    # Générer une matrice de Picross aléatoire pour l'exemple
    puzzle <- matrix(sample(0:1, input$taille^2, replace = TRUE),
                     nrow = input$taille, ncol = input$taille)

    # Créer et afficher la grille Picross avec indices
    creer_grille_picross(input$taille, puzzle)
  })
}

# Lancer l'application Shiny
shinyApp(ui, server)



