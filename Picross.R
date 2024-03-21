library(shiny)
library(shinyjs)

<<<<<<< HEAD
# Function to generate a random grid
generer_grille_aleatoire <- function(taille) {
  matrix(rbinom(taille^2, 1, 0.5), nrow = taille)
=======
# Function to generate a random grid with specified probability
generer_grille_aleatoire <- function(taille, p) {
  matrix(rbinom(taille^2, 1, p), nrow = taille)
>>>>>>> afbb0c03a37e4ec1aa1403702dc9c909d228d2ff
}

# Function to obtain row indices
obtenir_indices_ligne <- function(ligne) {
  consecutive_ones <- rle(ligne)$lengths[rle(ligne)$values == 1]
  if (length(consecutive_ones) == 0) {
    return(NULL)
  } else {
    return(consecutive_ones)
  }
}

# Function to obtain column indices
obtenir_indices_colonne <- function(colonne) {
  consecutive_ones <- rle(colonne)$lengths[rle(colonne)$values == 1]
  if (length(consecutive_ones) == 0) {
    return(NULL)
  } else {
    return(consecutive_ones)
  }
}

# Define the UI
ui <- fluidPage(
  titlePanel("Picross Game"),
  
<<<<<<< HEAD
  sliderInput("gridSize", "Taille de la Grille", min = 5, max = 10, value = 5),
  
  actionButton("generateButton", "Générer une nouvelle grille"),
  
  div(
    class = "grid-container",
    div(
      class = "grid", # La grille est maintenant dans la deuxième colonne
      uiOutput("picrossGrid")
    ),
    div(
      class = "row-indices", # Les indices de lignes sont dans la première colonne de la grille
      uiOutput("ligneIndices")
    ),
    div(
      class = "col-indices", # Les indices de colonnes sont dans la première ligne de la grille
      uiOutput("colonneIndices")
    )
  ),
=======
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
  ),
  
  # Add CSS and JavaScript code...
  
>>>>>>> afbb0c03a37e4ec1aa1403702dc9c909d228d2ff
  tags$head(
    tags$style(HTML("
      .square-button {
        width: 30px;
        height: 30px;
        margin: 0px;
        font-size: 12px; /* Adjusted font size for better visibility */
      }
      
      .grid-container {
        display: grid;
<<<<<<< HEAD
        grid-template-columns: auto 1fr;
=======
        grid-template-columns: auto 2fr auto;
>>>>>>> afbb0c03a37e4ec1aa1403702dc9c909d228d2ff
        grid-template-rows: auto;
        gap: 10px;
      }

<<<<<<< HEAD
=======
      .row-indices {
        grid-column: 1 / span 1;
        grid-row: 1 / span 1;
      }

>>>>>>> afbb0c03a37e4ec1aa1403702dc9c909d228d2ff
      .grid {
        grid-column: 2 / span 1;
        grid-row: 1 / span 1;
      }

<<<<<<< HEAD
      .indices {
              grid-column: 1 / span 1;
              grid-row: 1 / span 1;
              display: grid;
              grid-template-columns: repeat(auto-fill, minmax(30px, 1fr));
              grid-template-rows: repeat(auto-fill, minmax(30px, 1fr));
              gap: 5px;
            }

      .row-indices {
        grid-column: 1 / span 1; /* Les indices de lignes sont sur la première colonne */
        grid-row: 1 / span 1;
        display: flex;
        flex-direction: column;
        align-items: center;
      }

      .col-indices {
        grid-column: 2 / span 1; /* Les indices de colonnes sont sur la première ligne */
        grid-row: 1 / span 1;
        display: flex;
        flex-direction: row;
        justify-content: center;
      }


      .index-box {
        width: 30px;
        height: 30px;
        line-height: 30px;
        text-align: center;
        font-size: 12px;
        font-weight: bold;
        border: 1px solid #ddd;
        background-color: #eee;
=======
      .column-indices {
        grid-column: 3 / span 1;
        grid-row: 1 / span 1;
>>>>>>> afbb0c03a37e4ec1aa1403702dc9c909d228d2ff
      }

      .black-cell {
        background-color: black !important;
      }

      .cross-cell {
<<<<<<< HEAD
       color: red;
=======
        color: red;
>>>>>>> afbb0c03a37e4ec1aa1403702dc9c909d228d2ff
        font-size: 18px;
        line-height: 30px;
      }
    ")),
    tags$script(HTML('
<<<<<<< HEAD
  var isMouseDown = false;
  var isMouseOverCell = false;

  $(document).on("mousedown", ".cell-button", function() {
    isMouseDown = true;
    $(this).toggleClass("maintain-selected-cell");
  });

  $(document).on("mouseup", function() {
    isMouseDown = false;
  });

  $(document).on("mouseenter", ".cell-button", function() {
    if (isMouseDown) {
      $(this).toggleClass("maintain-selected-cell");
    }
  });

  $(document).on("click", ".cell-button", function() {
    var cellId = $(this).attr("id");
    var cellValue = parseInt($(this).val());
    if (cellValue === 1) {
      $(this).toggleClass("black-cell"); // Toggle black cell
    } else if (cellValue === 0) {
      $(this).empty().append("&#10006;").toggleClass("cross-cell");  // Add red cross
    }
  });
'))
    
  )
)

# Define the server logic
server <- function(input, output) {
  picrossGridData <- reactiveVal(NULL)
  
  observeEvent(input$generateButton, {
    nouvelle_grille <- generer_grille_aleatoire(input$gridSize)
    picrossGridData(list(
      picrossMatrix = nouvelle_grille,
      selectedCells = matrix(FALSE, nrow = input$gridSize, ncol = input$gridSize)
    ))
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
              class = c("square-button", "cell-button", ifelse(picrossGridDataValue$selectedCells[i, j], "selected-cell", "")),
              value = picrossGridDataValue$picrossMatrix[i, j]
            )
          })
        )
      })
    )
    
    picrossGrid
  })
  
  output$ligneIndices <- renderUI({
    picrossGridDataValue <- picrossGridData()
    
    if (is.null(picrossGridDataValue)) return(NULL)
    
    indices_lignes <- apply(picrossGridDataValue$picrossMatrix, 1, obtenir_indices_ligne)
    
    indices_text_lignes <- lapply(1:length(indices_lignes), function(i) {
      div(class = "index-box", ifelse(length(indices_lignes[[i]]) > 0, paste(indices_lignes[[i]], collapse = " "), ""))
    })
    
    indices_text_lignes
  })
  
  output$colonneIndices <- renderUI({
    picrossGridDataValue <- picrossGridData()
    
    if (is.null(picrossGridDataValue)) return(NULL)
    
    indices_colonnes <- apply(picrossGridDataValue$picrossMatrix, 2, obtenir_indices_colonne)
    
    indices_text_colonnes <- lapply(1:length(indices_colonnes), function(i) {
      div(class = "index-box", ifelse(length(indices_colonnes[[i]]) > 0, paste(indices_colonnes[[i]], collapse = " "), ""))
    })
    
    indices_text_colonnes
  })
}

# Run the application
shinyApp(ui, server)
=======
  $(document).on("click", ".cell-button", function() {
    if ($(this).hasClass("row-indices") || $(this).hasClass("column-indices")) {
      return;  // Ignore clicks on row and column indices
    }
    
    var cellId = $(this).attr("id");
    var cellValue = parseInt($(this).val());
    
    if ($(this).hasClass("black-cell")) {
      $(this).removeClass("black-cell").addClass("cross-cell").val(""); // Change to cross-cell and remove value
    } else if ($(this).hasClass("cross-cell")) {
      $(this).removeClass("cross-cell").val(""); // Clear cross and value
    } else {
      $(this).addClass("black-cell").val(1); // Toggle black cell and set value to 1
    }
  });
'))
  )
)



compare_matrices <- function(mat1, mat2) {
  if (!identical(dim(mat1), dim(mat2))) {
    stop("Les dimensions des matrices ne correspondent pas.")
  }
  
  rows <- nrow(mat1)
  cols <- ncol(mat1)
  
  comparison <- matrix(NA, nrow = rows, ncol = cols)
  
  for (i in 1:rows) {
    for (j in 1:cols) {
      comparison[i, j] <- ifelse(mat1[i, j] == mat2[i, j], TRUE, FALSE)
    }
  }
  
  return(comparison)
}



# Define the server logic
# Define the server logic
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





>>>>>>> afbb0c03a37e4ec1aa1403702dc9c909d228d2ff
