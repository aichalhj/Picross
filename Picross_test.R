library(ggplot2)
library(shiny)

# Fonction pour générer aléatoirement une grille de 0 et 1
generer_grille_aleatoire <- function(taille) {
  matrix(rbinom(taille^2, 1, 0.5), nrow = taille)
}

# Fonction pour calculer les indices de colonnes
calculer_indices_colonnes <- function(grille) {
  nb_colonnes = ncol(grille)
  indices_colonnes = rep("", nb_colonnes)
  
  for (j in 1:nb_colonnes) {
    col = grille[, j]
    indices <- rle(col)$lengths[col == 1]
    indices_colonnes[j] <- paste(indices[indices > 0], collapse = " ")
  }
  
  return(indices_colonnes)
}

# Fonction pour créer une grille Picross avec indices
creer_grille_picross <- function(grille) {
  indices_lignes <- apply(grille, 1, function(row) {
    indices <- rle(row)$lengths[row == 1]
    paste(indices[indices > 0], collapse = " ")
  })
  
  indices_colonnes <- calculer_indices_colonnes(grille)
  
  list(indices_lignes = indices_lignes, indices_colonnes = indices_colonnes)
}

ui <- fluidPage(
  titlePanel("Picross Game"),
  
  # Slider pour régler la taille de la grille
  sliderInput("gridSize", "Taille de la Grille", min = 5, max = 10, value = 5),
  
  # Bouton pour générer une nouvelle grille
  actionButton("generateButton", "Générer une nouvelle grille"),
  
  # Grille de jeu Picross et indices
  uiOutput("picrossGrid"),
  
  # Tableau pour afficher la matrice de 0 et 1
  tableOutput("grille01"),
  
  # Ajouter le code CSS personnalisé pour définir la largeur et la hauteur des boutons
  tags$head(
    tags$style(HTML("
      .square-button {
        width: 50px;
        height: 50px;
        margin: 2px;
      }
      .indices {
        font-size: 18px;
        text-align: center;
      }
    "))
  )
)

server <- function(input, output) {
  # Fonction réactive pour créer la grille de jeu Picross
  picrossGridData <- reactiveVal(NULL)
  
  observeEvent(input$generateButton, {
    # Générer une nouvelle grille de 0 et 1
    nouvelle_grille <- generer_grille_aleatoire(input$gridSize)
    
    # Calculer les indices
    indices <- creer_grille_picross(nouvelle_grille)
    
    # Stocker les données dans la reactiveVal
    picrossGridData(list(
      picrossMatrix = nouvelle_grille,
      indices_lignes = indices$indices_lignes,
      indices_colonnes = indices$indices_colonnes
    ))
  })
  
  # Afficher la grille de jeu Picross et les indices
  output$picrossGrid <- renderUI({
    picrossGridDataValue <- picrossGridData()
    
    if (is.null(picrossGridDataValue)) return(NULL)
    
    # Créer la grille de jeu Picross en utilisant la fonction do.call
    picrossGrid <- do.call(tagList, lapply(1:input$gridSize, function(i) {
      fluidRow(
        column(width = 8, tagList(
          lapply(1:input$gridSize, function(j) {
            actionButton(
              inputId = paste0("cell", i, j),
              label = "",
              class = "square-button",  # Ajouter la classe CSS
              width = 50,
              height = 50
            )
          })
        )),
        column(width = 2, align = "center", div(picrossGridDataValue$indices_lignes[i], class = "indices"))
      )
    }))
    
    # Ajouter une ligne avec les indices de colonnes
    indices_colonnes <- do.call(fluidRow, lapply(1:input$gridSize, function(i) {
      div(picrossGridDataValue$indices_colonnes[i], class = "indices")
    }))
    
    # Retourner la grille complète
    tagList(picrossGrid, indices_colonnes)
  })
  
  # Afficher la matrice de 0 et 1
  output$grille01 <- renderTable({
    picrossGridDataValue <- picrossGridData()
    
    if (is.null(picrossGridDataValue)) return(NULL)
    
    grille01 <- picrossGridDataValue$picrossMatrix
    
    # Retourner la matrice
    grille01
  })
}

shinyApp(ui, server)
