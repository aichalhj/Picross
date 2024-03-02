library(shiny)

generer_grille_aleatoire <- function(taille) {
  matrix(rbinom(taille^2, 1, 0.5), nrow = taille)
}

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
        font-size: 8px;
      }
      
      .grid-container {
        display: grid;
        grid-template-columns: 1fr 2fr 1fr;
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
        grid-template-rows: repeat(auto-fill, minmax(30px, 1fr));
        gap: 5px;
      }
      
      .row-indices {
        grid-row: span 1;
        align-self: end;
      }
      
      .col-indices {
        grid-row: span 1;
        display: grid;
        grid-template-columns: repeat(auto-fill, minmax(30px, 1fr));
        gap: 5px;
      }
      
      .grille {
        grid-column: 3 / span 1;
        grid-row: 1 / span 1;
      }
    "))
  )
)

server <- function(input, output) {
  picrossGridData <- reactiveVal(NULL)
  
  observeEvent(input$generateButton, {
    nouvelle_grille <- generer_grille_aleatoire(input$gridSize)
    picrossGridData(list(
      picrossMatrix = nouvelle_grille
    ))
  })
  
  output$picrossGrid <- renderUI({
    picrossGridDataValue <- picrossGridData()
    
    if (is.null(picrossGridDataValue)) return(NULL)
    
    indices_lignes <- apply(picrossGridDataValue$picrossMatrix, 1, obtenir_indices_ligne)
    indices_colonnes <- apply(picrossGridDataValue$picrossMatrix, 2, obtenir_indices_colonne)
    
    indices_buttons_lignes <- lapply(1:length(indices_lignes), function(i) {
      actionButton(
        inputId = paste0("indiceLigne", i),
        label = ifelse(length(indices_lignes[[i]]) > 0, paste(indices_lignes[[i]], collapse = " "), ""),
        class = "square-button",
        width = 30,
        height = 30
      )
    })
    
    indices_buttons_colonnes <- lapply(1:length(indices_colonnes), function(i) {
      actionButton(
        inputId = paste0("indiceColonne", i),
        label = ifelse(length(indices_colonnes[[i]]) > 0, paste(indices_colonnes[[i]], collapse = " "), ""),
        class = "square-button",
        width = 30,
        height = 30
      )
    })
    
    picrossGrid <- do.call(tagList, lapply(1:(input$gridSize + 1), function(i) {
      if (i == 1) {
        return(fluidRow(column(width = 2), column(width = 8)))
      } else {
        fluidRow(
          column(width = 2, align = "center", indices_buttons_lignes[[i - 1]]),
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
    
    picrossGrid
  })
  
  output$ligneIndices <- renderUI({
    NULL
  })
  
  output$colonneIndices <- renderUI({
    picrossGridDataValue <- picrossGridData()
    
    if (is.null(picrossGridDataValue)) return(NULL)
    
    indices_colonnes <- apply(picrossGridDataValue$picrossMatrix, 2, obtenir_indices_colonne)
    
    indices_buttons_colonnes <- lapply(1:length(indices_colonnes), function(i) {
      actionButton(
        inputId = paste0("indiceColonne", i),
        label = ifelse(length(indices_colonnes[[i]]) > 0, paste(indices_colonnes[[i]], collapse = " "), ""),
        class = "square-button",
        width = 30,
        height = 30
      )
    })
    
    indices_buttons_colonnes
  })
  
  output$grille01 <- renderTable({
    picrossGridDataValue <- picrossGridData()
    
    if (is.null(picrossGridDataValue)) return(NULL)
    
    grille01 <- picrossGridDataValue$picrossMatrix
    
    grille01
  })
}

shinyApp(ui, server)
