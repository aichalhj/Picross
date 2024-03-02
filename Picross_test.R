library(shiny)

# Fonction pour générer aléatoirement une grille de 0 et 1
generer_grille_aleatoire <- function(taille) {
  matrix(rbinom(taille^2, 1, 0.5), nrow = taille)
}

# Fonction pour obtenir les indices d'une ligne donnée
obtenir_indices_ligne <- function(ligne) {
  consecutive_ones <- rle(ligne)$lengths[rle(ligne)$values == 1]
  if (length(consecutive_ones) == 0) {
    return(NULL)
  } else {
    return(consecutive_ones)
  }
}

ui <- fluidPage(
  titlePanel("Picross Game"),
  
  # Slider pour régler la taille de la grille
  sliderInput("gridSize", "Taille de la Grille", min = 5, max = 10, value = 5),
  
  # Bouton pour générer une nouvelle grille
  actionButton("generateButton", "Générer une nouvelle grille"),
  
  # Grille de jeu Picross
  uiOutput("picrossGrid"),
  
  # Indices des lignes
  uiOutput("ligneIndices"),
  
  # Tableau pour afficher la matrice de 0 et 1
  tableOutput("grille01"),
  
  # Ajouter le code CSS personnalisé pour définir la largeur et la hauteur des boutons
  tags$head(
    tags$style(HTML("
      .square-button {
        width: 30px;
        height: 30px;
        margin: 0px;
        font-size: 8px;
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
    
    # Stocker les données dans la reactiveVal
    picrossGridData(list(
      picrossMatrix = nouvelle_grille
    ))
  })
  
  # Afficher la grille de jeu Picross
  output$picrossGrid <- renderUI({
    picrossGridDataValue <- picrossGridData()
    
    if (is.null(picrossGridDataValue)) return(NULL)
    
    # Créer la grille de jeu Picross en utilisant la fonction do.call
    picrossGrid <- do.call(tagList, lapply(1:(input$gridSize + 1), function(i) {
      if (i == 1) {
        return(fluidRow(column(width = 2), column(width = 8)))
      } else {
        fluidRow(
          column(width = 2, align = "center"),
          column(width = 8, tagList(
            lapply(1:input$gridSize, function(j) {
              actionButton(
                inputId = paste0("cell", i - 1, j),
                label = "",
                class = "square-button",
                width = 30,
                height = 30
              )
            })
          ))
        )
      }
    }))
    
    # Retourner la grille complète
    picrossGrid
  })
  
  # Afficher les indices des lignes
  output$ligneIndices <- renderUI({
    picrossGridDataValue <- picrossGridData()
    
    if (is.null(picrossGridDataValue)) return(NULL)
    
    indices_lignes <- apply(picrossGridDataValue$picrossMatrix, 1, obtenir_indices_ligne)
    
    # Créer les éléments HTML pour afficher les indices
    indices_elements <- lapply(indices_lignes, function(indices) {
      if (!is.null(indices)) {
        paste(indices, collapse = " ")
      } else {
        ""
      }
    })
    
    # Afficher les indices dans une colonne
    indices_column <- column(width = 2, align = "right", indices_elements)
    
    # Retourner la colonne des indices
    indices_column
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
