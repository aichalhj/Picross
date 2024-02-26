library(shiny)
library(ggplot2)

# Fonction pour créer une grille Picross
creer_grille_picross <- function(taille, puzzle) {
  ggplot() +
    geom_tile(aes(x = rep(1:taille, each = taille),
                  y = rep(taille:1, times = taille),
                  fill = factor(puzzle)),
              color = "white", size = 1) +
    scale_fill_manual(values = c("white", "black"),
                      name = "Case",
                      labels = c("Blanc", "Noir")) +
    theme_void() +
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

    # Créer et afficher la grille Picross
    creer_grille_picross(input$taille, puzzle)
  })
}

# Lancer l'application Shiny
shinyApp(ui, server)
