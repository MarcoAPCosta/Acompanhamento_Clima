box::use(
  shiny[moduleServer,
        NS,
        onStop,
        reactive],
  bslib[bs_theme,
        page_fillable,
        nav_panel,
        navset_tab]
)

box::use(
  app/view/relatorio,
  app/view/relatorio_uni,
  app/view/dados,
  app/view/header,
  app/view/dados1,
)
# 
box::use(
  app/logic/conexao_database
)

#' @export
ui <- function(id) {
  ns <- NS(id)

  page_fillable(
    title = "Painel de Acompanhamento Clima",
    header$ui(ns("titulo"), 
              "",
              "xxx-large"),
    navset_tab(
      id = ns("rede"),
      nav_panel(
        title = "Departamento Regional",
        value = "presencial",
        relatorio$ui(ns("presencial"))
      ),
      nav_panel(
        title = "Unidade",
        value = "presencial",
        relatorio_uni$ui(ns("ead")))
    )
  )
  
  
}

#' @export
server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
   
    
    dados <- dados$server("asdas")
    
    dados1 <- dados1$server("asdasd")
    
    selecao_p <- relatorio$server("presencial", dados, dados1, selecao_e)
    
    selecao_e <- relatorio_uni$server("ead", dados, dados1, selecao_p)
    
    header$server("titulo")
    
    
  })
}
