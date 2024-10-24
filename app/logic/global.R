box::use(
  shiny[h3,
        HTML,
        tags]
)

#' @export
brasil <- readRDS("app/data/br_uf_shape.Rds")
#' @export
opcoes <- readRDS("app/data/opcoes.Rds")

#' @export
dados_p <- readRDS("app/data/dados_p.rds")

#' @export
pop1 <- 400000

#' @export
titulo_mapa <- tags$div(
  h3("Taxa de resposta (%), por Departamento Regional, ANQP 2024", 
     style = " color: black;
    font-weight: bold;
    font-size: 28px;")
)  

#' @export
#' 
hora_da_exportação <- readRDS("app/data/hora.Rds")