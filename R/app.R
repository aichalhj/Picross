packageVersion("roxygen2")

#' @title UI
#' @description UI pour le jeu de Picross
#'
#' Cette fonction définit l'interface utilisateur (UI) pour le jeu de Picross en utilisant Shiny.
#'
#' @import shiny
#' @import shinyjs
#' @export

ui <- fluidPage(
  titlePanel("Picross Game"),

  selectInput("gridSize", "Taille de la Grille",
              choices = c(5, 6, 7, 8, 9, 10),
              selected = 5),

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

#' @title server
#' @description
#' Logique serveur pour le jeu de Picross
#'
#' Cette fonction définit la logique serveur pour le jeu de Picross en utilisant Shiny.
#' @import shiny
#' @import shinyjs
#' @export

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


play <- function() {
  shinyApp(ui, server)
  }

