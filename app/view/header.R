box::use(
  shiny[moduleServer,
        NS, 
        tags,
        textOutput,
        renderText],
  glue[glue],
  DBI[dbReadTable]
)



#' @export
ui <- function(id, nome, tamanho = "xx-large") {
  ns <- NS(id)
  
  tags$div(
    class = "caixa-valores",
    tags$h2(
      id = "titulo",
      nome,
      style = glue("
          padding-left: 25px;
          background: url(static/images/teste_banner.svg);
          background-position: center;
          background-repeat: no-repeat;
          background-size: 100%;
          font-size:{tamanho}")
    )
  )
}

#' @export
server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
  
      
    })
    

}
