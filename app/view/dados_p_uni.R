box::use(
  shiny[moduleServer, NS, reactiveTimer, reactive, icon],
)

box::use(
  f_importar = app/logic/f_import2[f_importar]
)

#' @export
server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    
    dados_p_uni <- reactive({
      saida <- f_importar()
      
      return(saida)
      
    })
    
    
    return(dados_p_uni)
    
  })
}