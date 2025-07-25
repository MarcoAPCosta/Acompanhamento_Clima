box::use(
  shiny[moduleServer, NS, strong, HTML, tags],
  bslib[card_header, card_body],
  leaflet[...],
  dplyr[filter, mutate, count, left_join, select, pull, summarise, n],
  stats[quantile],
  sf[...],
  purrr[map2],
  tidyr[replace_na]
)

box::use(
  app/logic/global[titulo_mapa]
)
#' @export
ui <- function(id) {
  ns <- NS(id)
     leafletOutput(
        outputId = ns("chart_tempo_2"),
        width = "100%"
      )
    
  
}

#' @param id
#'
#' @param brasil objeto mapa .shp
#' @param dados dados da pesquisa
#'
#' @export
server <- function(id, brasil, dados) {
  moduleServer(id, function(input, output, session) {
    output$chart_tempo_2 <- renderLeaflet({
      dados <- filter(dados(), !is.na(valido))
      faixas <- c(0, 0.75, 0.9, 1.1, 1.25, 10)
      media <- dados %>%
        summarise(media = 100 * n()/sum(unique(Total),
                                        na.rm = T)) %>%
        pull(media)
      
      juncao <- dados %>%
        summarise(Total = sum(unique(Total), na.rm = T),
                  Taxa = n(),
                  .by = DR2) %>%
        mutate(
          Taxa = round(100 * Taxa/Total, 1),
          Tx = cut(Taxa,
                   breaks = faixas * media,
                   labels = c(
                     "Abaixo - 25% ou mais",
                     "Abaixo - 10% a 25%",
                     "Entre 10% abaixo e 10% acima",
                     "Acima - 10% a 25%",
                     "Acima - 25% ou mais"
                   ),
                   right = F
          )
        ) %>%
        filter(!is.na(DR2))
      
      valor <- juncao$Tx[juncao$Tx == "Abaixo - 25% ou mais"][1]
      
      brasil <- brasil %>%
        select(-Porc) %>%
        left_join(juncao %>% select(-Total),
                  by = c("CD_GEOCUF" = "DR2")
        ) %>%
        mutate(Tx = replace_na(Tx, valor),
               Taxa = replace_na(Taxa, 0),
               label = map2(DR, Taxa, ~ {
          htmltools::HTML((paste0(
            .x,
            "<br><strong> Taxa: </strong>",
            sprintf("%.1f", .y),
            "%"
          )))
        }))
      
      cores <- colorFactor(
        c("#D7E8F0", "#B0C8F7", "#8AA8FF", "#4569A9", "#002A54"),
        domain = brasil$Tx,
        levels = levels(brasil$Tx),
        na.color = "red"
      )
      
      brasil %>%
        leaflet(
          options = leafletOptions(
            zoomControl = FALSE,
            minZoom = 4.5,
            maxZoom = 4.5
          )
        ) %>%
        addPolygons(
          stroke = TRUE,
          color = "black",
          weight = 0.5,
          fill = TRUE,
          fillColor = ~ cores(Tx),
          fillOpacity = 1,
          label = ~label,
          labelOptions = labelOptions(
            style = list("font-weight" = "normal", padding = "3px 8px"),
            textsize = "15px",
            direction = "auto"
          ),
          highlightOptions = highlightOptions(
            stroke = TRUE,
            color = "#FFF",
            weight = 1.5,
            opacity = 1,
            bringToFront = TRUE
          )
        ) %>%
        addLegend("bottomright",
                  pal = cores,
                  values = ~Tx,
                  title = "Comparativo com a taxa nacional",
                  opacity = 1
        ) %>%
        setView(
          lat = -15.209019860729843,
          lng = -52.121803250871675,
          zoom = 6
        )
    #  %>% 
    #     addControl(tags$div(
    #       tags$p("Taxa de resposta (%), por Departamento Regional, ANQP 2024", 
    #          style = " color: black;
    # font-weight: bold;
    # font-size: 28px;")
    #     )  ,
    #                position = "topleft",
    #                className="map-title")
    })
  })
}