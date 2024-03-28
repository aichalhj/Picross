

#' Define the server logic
#' @param input Input values
#' @param output Output values
server <- function(input, output) {
  
  picrossGridData <- reactiveVal(NULL)
  userGrid <- reactiveVal(matrix(0, nrow = 5, ncol = 5))  # Initialisation de userGrid
  
  observeEvent(input$generateButton, {
    taille_grille <- as.numeric(input$gridSize)
    
    niveau_difficulte <- input$difficultyLevel
    
    # Déterminez la probabilité en fonction du niveau de difficulté
    p <- if (niveau_difficulte == "Facile") {
      0.3
    } else if (niveau_difficulte == "Moyen") {
      0.5
    } else {                                   # Niveau de difficulté "Difficile"
      print("Difficile")  # Vérifier si cette partie est atteinte
      0.7
    }
    
    randomGrid <- generer_grille_aleatoire(taille_grille, p)
    indices_lignes <- apply(randomGrid, 1, obtenir_indices_ligne)
    indices_colonnes <- apply(randomGrid, 2, obtenir_indices_colonne)
    picrossGridData(list(
      picrossMatrix = randomGrid,
      indicesLignes = indices_lignes,
      indicesColonnes = indices_colonnes
    ))
    print(randomGrid)
    
    # Mettre à jour la taille de userGrid pour qu'elle corresponde à randomGrid
    userGrid(matrix(0, nrow = taille_grille, ncol = taille_grille))
  })
  
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
  
  output$rowIndicesTable <- renderTable({
    picrossGridDataValue <- picrossGridData()
    if (is.null(picrossGridDataValue)) return(NULL)
    t(sapply(picrossGridDataValue$indicesLignes, function(indices) {
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
    
    picrossGrid <- tagList(
      lapply(1:input$gridSize, function(i) {
        div(
          class = "cell-container",
          lapply(1:input$gridSize, function(j) {
            actionButton(
              inputId = paste0("cell", i, j),
              label = "",
              class = c("square-button", "cell-button"),
              value = picrossGridDataValue$picrossMatrix[i, j],
              onclick = paste("Shiny.setInputValue('selected_cell', {row: ", i, ", col: ", j, "});")
            )
          })
        )
      })
    )
    
    picrossGrid
  })
  
  observeEvent(input$selected_cell, {
    selected_cell <- input$selected_cell
    userGridValue <- userGrid()
    userGridValue[selected_cell$row, selected_cell$col] <- 1
    userGrid(userGridValue)
  })
  
  observe({
    userGridValue <- userGrid()
    print(userGridValue)  # Mettre à jour la matrice userGrid dans la console à chaque changement dans la grille de boutons
  })
}

shinyApp(ui, server)
