library(shiny)
library(igraph)
library(jsonlite)
library(DT)

ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      .app-container {
        padding: 30px;
      }
      .shiny-input-container {
        margin-bottom: 10px;
      }
      #saveJson {
        margin-top: 10px;
      }
      .btn-success {
        width: 100%;
        font-weight: bold;
      }
    "))
  ),

  div(class = "app-container",

    titlePanel("Редактор графа и анализ путей"),

    fluidRow(
      column(4,
        numericInput("size", "Размер матрицы смежности:", 3, min = 2, max = 20),
        actionButton("generate", "Создать матрицу"),
        tags$hr(),
        fluidRow(
          column(6, fileInput("loadJson", "Загрузить JSON", accept = ".json")),
          column(6, br(), downloadButton("saveJson", "Сохранить JSON"))
        ),
        tags$hr(),
        actionButton("calc", "Рассчитать связность и AΣ", class = "btn btn-success")
      )
    ),

    tags$hr(),

    fluidRow(
      column(12,
        tabsetPanel(
          tabPanel("Матрица смежности", DTOutput("adjMatrix")),
          tabPanel("Граф", plotOutput("graphPlot")),
          tabPanel("Матрица AΣ (количество путей)", tableOutput("paths")),
          tabPanel("Матрица связности (1 если есть путь)", tableOutput("reachability"))
        )
      )
    )
  )
)

server <- function(input, output, session) {
  values <- reactiveValues(
    adj = NULL,
    reach = NULL,
    paths = NULL
  )

  observeEvent(input$generate, {
    size <- input$size
    values$adj <- matrix(0, nrow = size, ncol = size)
  })

  output$adjMatrix <- renderDT({
    req(values$adj)
    datatable(
      values$adj,
      editable = TRUE,
      options = list(dom = 't'),
      rownames = paste0("V", 1:nrow(values$adj)),
      colnames = paste0("V", 1:ncol(values$adj))
    )
  }, server = FALSE)

  observeEvent(input$adjMatrix_cell_edit, {
    info <- input$adjMatrix_cell_edit
    i <- info$row
    j <- info$col
    v <- as.numeric(info$value)
    if (!is.na(v) && v %in% c(0, 1)) {
      values$adj[i, j] <- v
    }
  })

  output$graphPlot <- renderPlot({
    req(values$adj)
    g <- graph_from_adjacency_matrix(values$adj, mode = "directed")
    plot(g, vertex.label = V(g)$name, layout = layout_in_circle)
  })

  observeEvent(input$calc, {
    req(values$adj)
    g <- graph_from_adjacency_matrix(values$adj, mode = "directed")

    reachable <- distances(g) != Inf
    # values$reach <- matrix(as.numeric(reachable), nrow = nrow(values$adj))

    A <- values$adj
    A_sum <- matrix(0, nrow = nrow(A), ncol = ncol(A))
    Apow <- A
    for (k in 1:nrow(A)) {
      A_sum <- A_sum + Apow
      Apow <- Apow %*% A
    }
    values$paths <- A_sum
    values$reach <- ifelse(A_sum > 0, 1, 0)
  })

  output$reachability <- renderTable({
    req(values$reach)
    values$reach
  }, rownames = TRUE)

  output$paths <- renderTable({
    req(values$paths)
    values$paths
  }, rownames = TRUE)

  output$saveJson <- downloadHandler(
    filename = function() {
      paste0("adjacency_matrix_", Sys.Date(), ".json")
    },
    content = function(file) {
      write_json(values$adj, file, pretty = TRUE)
    }
  )

  observeEvent(input$loadJson, {
    req(input$loadJson)
    mat <- read_json(input$loadJson$datapath, simplifyVector = TRUE)
    if (is.matrix(mat)) {
      values$adj <- mat
    }
  })
}

shinyApp(ui = ui, server = server)
