library(shiny)
library(shinyjs)

# Function to generate a random grid
generer_grille_aleatoire <- function(taille) {
  matrix(rbinom(taille^2, 1, 0.5), nrow = taille)
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
    taille_grille <- input$gridSize
    nouvelle_grille <- generer_grille_aleatoire(taille_grille)
    indices_lignes <- apply(nouvelle_grille, 1, obtenir_indices_ligne)
    indices_colonnes <- apply(nouvelle_grille, 2, obtenir_indices_colonne)
    picrossGridData(list(
      picrossMatrix = nouvelle_grille,
      indicesLignes = indices_lignes,
      indicesColonnes = indices_colonnes,
      selectedCells = matrix(FALSE, nrow = input$gridSize, ncol = input$gridSize)
    ))
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
              class = c("square-button", "cell-button", ifelse(picrossGridDataValue$selectedCells[i, j], "selected-cell", "")),
              value = picrossGridDataValue$picrossMatrix[i, j]
            )
          })
        )
      })
    )
    
    picrossGrid
  })
}

# Run the application
shinyApp(ui, server)