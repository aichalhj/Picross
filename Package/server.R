# server.R

# Import des bibliothèques nécessaires
library(shiny)


# Import des fonctions définies dans functions.R
source("functions.R")

# Import du contenu de ui.R
source("ui.R")

# Définition de la fonction server
server <- function(input, output) {
  
  # Logique du serveur pour générer une nouvelle grille
  observeEvent(input$generateButton, {
    taille_grille <- as.numeric(input$gridSize)
    niveau_difficulte <- input$difficultyLevel
    
    # Déterminez la probabilité en fonction du niveau de difficulté
    p <- if (niveau_difficulte == "Facile") {
      0.3
    } else if (niveau_difficulte == "Moyen") {
      0.5
    } else {                                   # Niveau de difficulté "Difficile"
      0.7
    }
    
    randomGrid <- generer_grille_aleatoire(taille_grille, p)
    indices_lignes <- apply(randomGrid, 1, obtenir_indices_ligne)
    indices_colonnes <- apply(randomGrid, 2, obtenir_indices_colonne)
    
    picrossGridData <- list(
      picrossMatrix = randomGrid,
      indicesLignes = indices_lignes,
      indicesColonnes = indices_colonnes
    )
    
    # Mettre à jour la grille de l'utilisateur
    userGrid <- matrix(0, nrow = taille_grille, ncol = taille_grille)
    
    # Mettre à jour l'interface utilisateur avec les nouvelles données de la grille
    update_ui(picrossGridData, userGrid)
  })
  
  # Logique du serveur pour vérifier la solution
  observeEvent(input$checkSolutionButton, {
    picrossGridDataValue <- picrossGridData()
    if (is.null(picrossGridDataValue)) {
      showModal(modalDialog(
        title = "Erreur",
        "Veuillez générer une grille aléatoire avant de vérifier la solution."
      ))
      return()
    }
    
    randomGrid <- picrossGridDataValue$picrossMatrix
    comparison_result <- compare_matrices(userGrid(), randomGrid)
    if (all(comparison_result)) {
      showModal(modalDialog(
        title = "Bravo !",
        "Votre solution est correcte !"
      ))
    } else {
      showModal(modalDialog(
        title = "Réessayer",
        "Désolé, votre solution n'est pas correcte. Veuillez réessayer."
      ))
    }
  })
  
  # Logique du serveur pour mettre à jour les données utilisateur lorsqu'une cellule est sélectionnée
  observeEvent(input$selected_cell, {
    selected_cell <- input$selected_cell
    userGridValue <- userGrid()
    userGridValue[selected_cell$row, selected_cell$col] <- 1
    userGrid(userGridValue)
  })
  
  # Fonction pour mettre à jour l'interface utilisateur avec les données de la grille
  update_ui <- function(picrossGridData, userGrid) {
    output$rowIndicesTable <- renderTable({
      t(sapply(picrossGridData$indicesLignes, function(indices) {
        paste(indices, collapse = " ")
      }))
    })
    
    output$columnIndicesTable <- renderTable({
      t(sapply(picrossGridData$indicesColonnes, function(indices) {
        paste(indices, collapse = " ")
      }))
    })
    
    output$picrossGrid <- renderUI({
      picrossGrid <- tagList(
        lapply(1:input$gridSize, function(i) {
          div(
            class = "cell-container",
            lapply(1:input$gridSize, function(j) {
              actionButton(
                inputId = paste0("cell", i, j),
                label = "",
                class = c("square-button", "cell-button"),
                value = picrossGridData$picrossMatrix[i, j],
                onclick = paste("Shiny.setInputValue('selected_cell', {row: ", i, ", col: ", j, "});")
              )
            })
          )
        })
      )
      
      picrossGrid
    })
  }
}

shinyApp(ui = ui, server = server)