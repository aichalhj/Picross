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
  
  div(
    class = "grid-container",
    div(
      class = "grid",
      uiOutput("picrossGrid")
    ),
    div(
      class = "indices",
      div(
        class = "row-indices",
        uiOutput("ligneIndices")
      ),
      div(
        class = "col-indices",
        uiOutput("colonneIndices")
      )
    ),
    div(
      class = "grille",
      tableOutput("grille01")
    )
  ),
  
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
        grid-template-columns: 2fr 1fr 1fr;
        grid-template-rows: auto;
        gap: 10px;
      }

      .grid {
        grid-column: 2 / span 1;
        grid-row: 1 / span 1;
      }

      .indices {
        grid-column: 1 / span 1;
        grid-row: 1 / span 1;
        display: grid;
        grid-template-columns: repeat(auto-fill, minmax(30px, 1fr));
        grid-template-rows: repeat(auto-fill, minmax(30px, 1fr));
        gap: 5px;
      }

      .row-indices, .col-indices {
        display: flex;
        flex-wrap: wrap;
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
      }

      .grille {
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
  
  output$grille01 <- renderTable({
    picrossGridDataValue <- picrossGridData()
    
    if (is.null(picrossGridDataValue)) return(NULL)
    
    grille01 <- picrossGridDataValue$picrossMatrix
    
    grille01
  })
  
  observeEvent(input$picrossGrid, {
    picrossGridDataValue <- picrossGridData()
    
    lapply(1:(input$gridSize), function(i) {
      lapply(1:(input$gridSize), function(j) {
        id <- paste0("cell", i, j)
        shinyjs::runjs(sprintf('$("#%s").toggleClass("selected-cell", %s);', id, tolower(toJSON(input[[id]] %% 2 == 1))))
        picrossGridDataValue$selectedCells[i, j] <- input[[id]] %% 2 == 1
      })
    })
  })
}

# Run the application
shinyApp(ui, server)
