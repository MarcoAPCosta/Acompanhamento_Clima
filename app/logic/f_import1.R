box::use(
  dplyr[`%>%`,
        as_tibble,
        bind_rows, 
        case_when,
        filter,
        mutate]
)


f_importar <- function(selecao){
  
  dados1 <- readRDS("app/data/dados_p.rds") %>% 
    as_tibble() %>%
    filter(ead == selecao)
  return(dados1)
  
}