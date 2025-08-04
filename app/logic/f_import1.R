box::use(
  dplyr[`%>%`,
        as_tibble,
        bind_rows, 
        case_when,
        filter,
        mutate]
)


f_importar <- function(){
  
  message("Começando a leitura dos dados1")
  dados1 <- readRDS("app/data/dados_p1.rds") %>% 
    as_tibble()
  message("dados1 lido")
  return(dados1)
  
}