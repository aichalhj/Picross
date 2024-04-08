library(shiny)
library(shinyjs)

# Définir la fonction pour générer une grille aléatoire 
generer_grille_aleatoire <- function(taille) {
  matrix(rbinom(taille^2, 1, 0.5), nrow = taille)
}

# Fonctions pour obtenir les indices de ligne et de colonne
obtenir_indices_ligne <- function(ligne) {
  consecutive_ones <- rle(ligne)$lengths[rle(ligne)$values == 1]
  if (length(consecutive_ones) == 0) {
    return(NULL)
  } else {
    return(consecutive_ones)
  }
}

obtenir_indices_colonne <- function(colonne) {
  consecutive_ones <- rle(colonne)$lengths[rle(colonne)$values == 1]
  if (length(consecutive_ones) == 0) {
    return(NULL)
  } else {
    return(consecutive_ones)
  }
}

# Définir l'interface utilisateur (UI)
ui <- fluidPage(
  titlePanel("Jeu Picross"),
  
  sliderInput("gridSize", "Taille de la Grille", min = 5, max = 10, value = 5),
  
  actionButton("generateButton", "Générer une nouvelle grille"),
  
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
  ),
  
  
  tags$head(
    tags$style(HTML("
      .square-button {
        width: 30px;
        height: 30px;
        margin: 0px;
        font-size: 12px; /* Taille de la police ajustée pour une meilleure visibilité */
      }
      
      .grid-container {
        display: grid;
        grid-template-columns: auto 2fr auto;
        grid-template-rows: auto;
        gap: 10px;
      }

      .row-indices {
        grid-column: 1 / span 1;
        grid-row: 1 / span 1;
      }

      .grid {
        grid-column: 2 / span 1;
        grid-row: 1 / span 1;
      }

      .column-indices {
        grid-column: 3 / span 1;
        grid-row: 1 / span 1;
      }

      .black-cell {
        background-color: black !important;
      }

      .cross-cell {
        color: red;
        font-size: 18px;
        line-height: 30px;
      }
    ")),
    tags$script(HTML('
      $(document).on("click", ".cell-button", function() {
        if ($(this).hasClass("row-indices") || $(this).hasClass("column-indices")) {
          return;  // Ignorer les clics sur les indices de ligne et de colonne
        }
        
        var cellId = $(this).attr("id");
        var cellValue = parseInt($(this).val());
        if (cellValue === 1) {
          $(this).toggleClass("black-cell"); // Basculer la cellule en noir
        } else if (cellValue === 0) {
          $(this).empty().append("&#10006;").toggleClass("cross-cell");  // Ajouter une croix rouge
        }
      });
    '))
  )
)

server <- function(input, output) {
  picrossGridData <- reactiveVal(NULL)
  coefficient <- reactiveVal(NULL)  # Variable réactive pour stocker le coefficient
  taille_grille <- reactiveVal(NULL)  # Variable réactive pour stocker la taille de la grille
  
  observeEvent(input$generateButton, {
    taille_grille_val <- input$gridSize  # Stocker la taille de la grille
    coefficient_val <- floor(taille_grille_val / 2) + 1  # Calcul du coefficient
    total_taille_grille <- taille_grille_val + coefficient_val  
    nouvelle_grille <- generer_grille_aleatoire(taille_grille_val)
    indices_lignes <- apply(nouvelle_grille, 1, obtenir_indices_ligne)
    indices_colonnes <- apply(nouvelle_grille, 2, obtenir_indices_colonne)
    picrossGridData(list(
      picrossMatrix = nouvelle_grille,
      indicesLignes = indices_lignes,
      indicesColonnes = indices_colonnes,
      selectedCells = matrix(FALSE, nrow = total_taille_grille, ncol = total_taille_grille)
    ))
    coefficient(coefficient_val)  # Stockage du coefficient
    taille_grille(taille_grille_val)  # Stockage de la taille de la grille
  })
  
  output$rowIndicesTable <- renderTable({
    picrossGridDataValue <- picrossGridData()
    if (is.null(picrossGridDataValue)) return(NULL)
    t(sapply(picrossGridDataValue$indicesLignes[1:taille_grille()], function(indices) {
      paste(indices, collapse = " ")
    }))
  })
  
  output$columnIndicesTable <- renderTable({
    picrossGridDataValue <- picrossGridData()
    if (is.null(picrossGridDataValue)) return(NULL)
    t(sapply(picrossGridDataValue$indicesColonnes, function(indices) {
      paste(indices, collapse = " ")
    }))
  })
  
  output$picrossGrid <- renderUI({
    picrossGridDataValue <- picrossGridData()
    if (is.null(picrossGridDataValue)) return(NULL)
    
    coefficient_val <- coefficient()  # Obtenir le coefficient actuel
    grid_size <- input$gridSize  # Taille de la grille
    taille_grille_val <- taille_grille()  # Obtenir la taille de la grille
    
    # Extraire les indices de toutes les colonnes de la matrice d'indices
    indices_colonnes <- lapply(picrossGridDataValue$indicesColonnes, function(indices) {
      indices
    })
    
    # Générer la grille de Picross avec les indices de colonne au-dessus de chaque colonne
    picrossGrid <- tagList(
      lapply(1:(grid_size + coefficient_val), function(i) {
        div(
          class = "cell-container",
          lapply(1:(grid_size + coefficient_val), function(j) {
            if (j <= length(indices_colonnes) && i <= length(indices_colonnes[[j]]) && j > coefficient_val && j <= (grid_size + coefficient_val)) {
              # Si c'est une case pour afficher les indices de colonne
              if (j == coefficient_val + 1) {
                span(class = "square-button", indices_colonnes[[j]][i])
              } else {
                # Pour les autres cases, laisser vide
                span(class = "square-button")
              }
            } else if (i > coefficient_val && i <= (grid_size + coefficient_val) && j > coefficient_val && j <= (grid_size + coefficient_val)) {
              # Si c'est une case de la grille de jeu
              actionButton(
                inputId = paste0("cell", i, j),
                label = "",
                class = c("square-button", "cell-button"),
                value = picrossGridDataValue$picrossMatrix[i-1 - coefficient_val, j-1 - coefficient_val]
              )
            } else {
              # Si c'est une case vide
              span(class = "square-button empty-cell")
            }
          })
        )
      })
    )
    
    picrossGrid
  })
}

# Exécuter l'application
shinyApp(ui, server)
