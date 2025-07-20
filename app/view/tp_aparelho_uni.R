box::use(
  shiny[moduleServer, NS, renderPlot, plotOutput, strong],
  bslib[card_header,card_body],
  dplyr[tibble, `%>%`, count, n, summarise,
        group_by, mutate, filter, case_when],
  echarts4r[...],
  stringr[str_detect],
  htmlwidgets[JS]
  
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  
  echarts4rOutput(outputId = ns("grafico_dr_1"))
  
  
  
}

#' @export
server <- function(id, dados,dr_selecionado ,unidade) {
  moduleServer(id, function(input, output, session) {
    
    
    output$grafico_dr_1 <- renderEcharts4r({
  
      dados_aqui <- dados() %>%
        filter(DR == dr_selecionado()) %>%
        filter(unidade == unidade()) %>%
        count(tp.aparelho, name = "Quantidade", sort = T)
      
      
      if(nrow(dados_aqui) > 0){
        grafico <- dados_aqui %>%
          e_charts(tp.aparelho) %>%
          e_pie(Quantidade,
                percentPrecision = 1,
                radius = c("0%", "60%"),
                center = c("50%", "55%"),
                itemStyle = list(borderColor = "rgba(0, 0, 0, 0.30)"),
                labelLine = list(show = TRUE,
                                 length = 15,
                                 length2 = 15,
                                 shadowColor = 'rgba(0, 0, 0, 100)',
                                 shadowBlur = 2)) %>%
          e_color(c("#002a54",
                    "#8aa8ff",
                    "#ffa32a")) %>% 
          e_tooltip(valueFormatter = JS("function(value) {
          saida = value.toString().replace(',', '.');
          return saida
        }")) %>%
          e_labels(formatter = JS("function(params) {
          saida = params.percent.toFixed(1).toString().replace('.', ',');
          saida2 = params.name
          return `${saida2}\n${saida}%`
        }"),
                   position = "outside",
                   fontSize = 16) %>%
          e_legend(show = FALSE,
                   orient = 'vertical',
                   left = "0%",
                   top = "bottom",
                   itemStyle = list(borderColor =  "rgba(0, 0, 0, 1)",
                                    borderWidth =  0.5),
                   selectedMode = FALSE) %>%
          e_title(text = "Distribuição dos acessos, por tipo de aparelho utilizado,\nANQP 2024",
                  #subtext = titulo,
                  textStyle = list(fontSize = 18,
                                   fontStyle = "normal")) %>% 
          e_show_loading(text = "Carregando",
                         color = "#8aa8ff",
                         text_color = "#000",
                         mask_color = "rgba(255, 255, 255, 1)")
      }
      
      if(nrow(dados_aqui) == 0){
        x <- data.frame(Sale = 1, modelo = "A", stringsAsFactors = F)
        
        grafico <- e_charts(x,
                            modelo) %>%
          e_bar(Sale,
                animation = T) %>%
          e_legend(show = FALSE) %>%
          e_color("transparent") %>%
          e_labels(position = "inside",
                   formatter = "DR sem acessos no momento",
                   fontSize = 30,
                   color = "black") %>%
          e_x_axis(show = FALSE) %>%
          e_y_axis(show = FALSE) %>% 
          e_show_loading(text = "Carregando",
                         color = "#8aa8ff",
                         text_color = "#000",
                         mask_color = "rgba(255, 255, 255, 1)")
      }
      
      return(grafico)
      
    })
    
    
  })
}