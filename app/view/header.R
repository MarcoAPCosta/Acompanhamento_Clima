box::use(
  shiny[moduleServer,
        NS, 
        tags,
        textOutput,
        renderText],
  glue[glue],
  DBI[dbReadTable]
)

box::use(
  app/logic/conexao_database[pool]
)

#' @export
ui <- function(id, nome, tamanho = "xx-large") {
  ns <- NS(id)
  
  tags$div(
    class = "caixa-valores",
    style = "
      background: url(static/images/header.svg);
      background-position: right;
      background-repeat: no-repeat;
      background-size: 48%;
    ",
    tags$h2(
      id = "titulo",
      nome,
      style = glue("
          margin-top: 25px;
          margin-bottom: 0;
          padding-left: 25px;
          font-size:{tamanho}")
    ),
    tags$h5(textOutput(ns("hora")),
            style = "height: 1em;
            color:white;
            position: relative;
            align-content: center;
            padding-left: 25px;
            text-align: left;")
  )
}

#' @export
server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    output$hora <- renderText({
      hora_da_exportação <- dbReadTable(pool,
                                        "anqp24_hora")
      hora_da_exportação <- hora_da_exportação[1,1]
        saida <- glue("Dados atualizados em {hora_da_exportação}")
        
        return(saida)
      
    })
    
  })
}
