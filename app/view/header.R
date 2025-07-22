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
    )
  )
}

#' @export
server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
  
      
    })
    

}
