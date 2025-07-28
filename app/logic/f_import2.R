box::use(
  dplyr[`%>%`,
        as_tibble,
        bind_rows, 
        case_when,
        filter,
        mutate]
)


f_importar <- function(){
  
  message("Começando a leitura dos dadosp")
  dados_p_uni <- readRDS("app/data/dados_p_uni.rds") %>% 
    as_tibble()
  message("dadosp lido")
  return(dados1)
  
}