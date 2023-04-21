library(shiny)
library(codec)
library(cincy)
library(bslib)
library(ggiraph)
library(tidyverse)
library(biscale)
library(cowplot)
library(ggExtra)
library(shinyWidgets)
library(leaflet)
library(tmap)

{
  tmap_mode("view")
  
  d <- codec_data("tract_indices") |> 
    left_join(cincy::tract_tigris_2010, by = 'census_tract_id_2010') |> 
    sf::st_as_sf()
  
  d <- d |> 
    select(!where(is.logical))

  var_meta <- glimpse_schema(d) |> 
    relocate(title, .before = name) |> 
    rowwise() |> 
    mutate(title = coalesce(title, name)) |> 
    ungroup()
  
  codec_bi_pal <- c(
    "1-1" = "#eddcc1",
    "2-1" = "#d4aa92",
    "3-1" = "#bb7964",
    "1-2" = "#909992",
    "2-2" = "#81766f",
    "3-2" = "#71544c",
    "1-3" = "#375a66",
    "2-3" = "#31464d",
    "3-3" = "#2b3135"
  )
}

# sidebar_acc <- bslib::accordion(
#   open = c("X Variable", "Y Variable"),
#   accordion_panel(
#     "X Variable",
#     shinyWidgets::pickerInput('x',
#                 label = "",
#                 choices = names(d),
#                 multiple = FALSE,
#                 selected = 'ice',
#                 options = pickerOptions(
#                   liveSearch = TRUE
#                 ))
#     ),
#     accordion_panel(
#       "Y Variable",
#       shinyWidgets::pickerInput('y',
#                      label = "", 
#                      choices = names(d),
#                      multiple = FALSE,
#                      selected = 'dep_index',
#                      options = pickerOptions(
#                        liveSearch = TRUE
#                      ))
#     )
# )
                          

ex_card <- card(
  full_screen = TRUE,
  card_header("Data Explorer"),
  leafletOutput("map"),
  absolutePanel(id = "plot_panel",
                class = "panel panel-default",
                cursor = "inherit",
                draggable = TRUE,
                top = 100,
                height = "400px",
                right = 20,
                width = '400px',
                style =
                  "padding: 5px;
                         border: 1px solid #000;
                         background: #FFFFFF;
                         opacity: .9;
                         margin: auto;
                         border-radius: 5pt;
                         box-shadow: 0pt 0pt 6pt 0px rgba(61,59,61,0.48);",
                fixedRow(girafeOutput("scatter", height = "350px", width = "350px"))),
  
  absolutePanel(id = "legend_panel",
                bottom = 20,
                left = 20,
                height = "160px",
                width = "160px",
                style =
                  "padding: 5px;
                         border: 1px solid #000;
                         background: #FFFFFF;
                         opacity: .7;
                         margin: auto;
                         border-radius: 5pt;
                         box-shadow: 0pt 0pt 0pt 0px rgba(61,59,61,0.48);",
                plotOutput("legend", width = "145px", height = "145px"))
)

data_card <- card(
  card_header("Data Catalog"),
  h2("Hamilton County Landcover and Built Environment Characteristics"),
  DT::dataTableOutput("table")
)

ui <- page_navbar(
  theme = bs_theme(version = 5,
                  # bootswatch = "litera",
                  "bg" = "#FFFFFF",
                  "fg" = "#396175",
                  "primary" = "#C28273",
                  "grid-gutter-width" = "0.0rem",
                  "border-radius" = "0.5rem",
                  "btn-border-radius" = "0.25rem" ),

  
  title = " CoDEC Explorer",
  
  fillable = TRUE,
  
  sidebar = sidebar(shinyWidgets::pickerInput('x',
                                              label = "X Variable",
                                              choices = var_meta$title,
                                              multiple = FALSE,
                                              selected = 'Racial Economic Index of Concentration at the Extremes',
                                              options = pickerOptions(
                                                liveSearch = TRUE
                                              )),
                    shinyWidgets::pickerInput('y',
                                              label = "Y Variable", 
                                              choices = var_meta$title,
                                              multiple = FALSE,
                                              selected = 'Material Deprivation Index',
                                              options = pickerOptions(
                                                liveSearch = TRUE
                                              )),
                    width = '15%'
  ), # sidebar(sidebar_acc,
            #        width = '15%'),
  
  nav("Showcase",
      ex_card
  ),
  
  nav("Data Catalog",
      data_card)
  
)


server <- function(input, output, session) {
  
  xvar <- reactive({
    
    xvar <- var_meta |> 
      filter(title == input$x) |> 
      pull(name)
    
    xvar
    
    })
  yvar <- reactive({
    
    yvar <- var_meta |> 
      filter(title == input$y) |> 
      pull(name)
    
    yvar
    
    })
  
  output$map <- renderLeaflet({
    
    bins_x <- pull(d, xvar())
    bins_y <- pull(d, yvar())
    
    bins_x <- classInt::classIntervals(bins_x, n = 3, style = "quantile")
    bins_y <- classInt::classIntervals(bins_y, n = 3, style = "quantile")
    
    bins_x <- bins_x$brks
    bins_y <- bins_y$brks
    
    # cut into groups defined above
    out <- d |> 
      mutate(bi_x = cut(get(xvar()), breaks = bins_x, include.lowest = TRUE))
    out <- out |> 
      mutate(bi_y = cut(get(yvar()), breaks = bins_y, include.lowest = TRUE))
    out <- out|> 
      mutate(bi_class = paste0(as.numeric(bi_x), "-", as.numeric(bi_y)))
    
    
    map <- 
      tm_basemap("CartoDB.Positron") +
      tm_shape(out, unit = 'miles') +
      tm_polygons(col ="bi_class", alpha = 0.7, palette = codec_bi_pal, legend.show = FALSE,
                  popup.vars = c(xvar(), yvar()))
    
    map |> 
      tmap_leaflet(in.shiny = TRUE) |> 
      removeLayersControl() 
  })
  
 
  
  output$scatter <- renderGirafe({
    
    scat <- ggplot(d) +
      geom_point_interactive(aes_string(x = xvar(), y = yvar(),
                                        data_id = "census_tract_id_2010"), 
                             color = codec_colors()[7]) +
      theme_light() +
      theme(aspect.ratio = 1, title = element_text(size = 8),
            axis.title = element_text(size = 6),
            legend.key.size = unit(3,"mm")) +
      labs(x = paste0(input$x), y = paste0(input$y))
    
    hist1 <- ggplot(d) +
      geom_histogram_interactive(aes_string(x = xvar(), tooltip = "census_tract_id_2010", 
                                            data_id = "census_tract_id_2010"), 
                                 fill = codec_colors()[2], bins = 20, color = codec_colors()[3]) +
      theme_minimal()
    
    hist2 <- ggplot(d) +
      geom_histogram_interactive(aes_string(x = yvar(), tooltip = "census_tract_id_2010", 
                                            data_id = "census_tract_id_2010"), 
                                 fill = codec_colors()[2], bins = 20, color = codec_colors()[3]) +
      coord_flip() + 
      theme_minimal()
    
    scat1 <- insert_xaxis_grob(scat, hist1, position = "bottom")
    scat2 <- insert_yaxis_grob(scat1, hist2, position = "right")
    #scat_full <- ggdraw(scat2)
    
    finalScat <- ggdraw() +
      draw_plot(scat2) + #, 0, 0, 1, 1, vjust = -.2) 
      theme(plot.margin = margin(0,0,0,0))#
    
    gir_join <- girafe(ggobj = finalScat, width_svg = 3, height_svg = 3,
                       options = list(opts_sizing(width = 1, rescale = T)))
    gir_join
    
  })
  
  d_selected <- reactiveVal()
  
  observeEvent(input$map_shape_click, {
    
    map_click <- reactiveVal()
    map_click <- input$map_shape_click
    #print(map_click)
    
    click <- tibble(lng = map_click$lng, lat = map_click$lat, 
                    tract_id = str_sub(map_click$id, 2))
    #print(click)
    
    d_selected <- d |> 
      filter(census_tract_id_2010 == click$tract_id)
    
    output$scatter <- renderGirafe({
      scat <- ggplot() +
        geom_point_interactive(data = d, aes_string(x = xvar(), y = yvar(),
                                          data_id = "census_tract_id_2010"),
                               color = codec_colors()[7]) +
        geom_point_interactive(data = d_selected,
                               aes_string(x = xvar(), y = yvar(),
                                          data_id = "census_tract_id_2010"),
                               #  tooltip = paste0(
                               #   input$x, ": ", xvar(), "\n",
                               #    input$y, ": ", yvar()
                               #   )),
                               color = codec_colors()[1], size = 3, alpha = .6) +
        theme_light() +
        theme(aspect.ratio = 1, title = element_text(size = 8),
              axis.title = element_text(size = 6),
              legend.key.size = unit(3,"mm")) +
        labs(x = paste0(input$x), y = paste0(input$y))
      
      hist1 <- ggplot(d) +
        geom_histogram_interactive(aes_string(x = xvar(), tooltip = "census_tract_id_2010",
                                              data_id = "census_tract_id_2010"),
                                   fill = codec_colors()[2], bins = 20, color = codec_colors()[3]) +
        theme_minimal()
      
      hist2 <- ggplot(d) +
        geom_histogram_interactive(aes_string(x = yvar(), tooltip = "census_tract_id_2010",
                                              data_id = "census_tract_id_2010"),
                                   fill = codec_colors()[2], bins = 20, color = codec_colors()[3]) +
        coord_flip() +
        theme_minimal()
      
      scat1 <- insert_xaxis_grob(scat, hist1, position = "bottom")
      scat2 <- insert_yaxis_grob(scat1, hist2, position = "right")
      #scat_full <- ggdraw(scat2)
      
      finalScat <- ggdraw() +
        draw_plot(scat2) + #, 0, 0, 1, 1, vjust = -.2)
        theme(plot.margin = margin(0,0,0,0))#
      
      gir_join <- girafe(ggobj = finalScat, width_svg = 3, height_svg = 3,
                         options = list(opts_sizing(width = 1, rescale = T)))
      gir_join
    })
    
    
  })
  
  output$legend <- renderPlot({
    
    legend <- bi_legend(pal = codec_bi_pal,
                        dim = 3,
                        xlab = paste0("Greater X Variable"),
                        ylab = paste0("Greater Y Variable"),
                        size = 8)
    
    legend
  })
  
  output$table <- DT::renderDataTable({
    
    d_table <- read_rds('2020_all_codec_nosf.rds')
    
    
    DT::datatable(head(d_table[[2]]),
                  rownames = FALSE,
                  options = list(dom = 't',
                                 ordering = FALSE,
                                 paging = FALSE,
                                 searching = FALSE),
                  selection = 'none',
                  class = 'row-border',
                  escape = FALSE,
                  filter = "none")
    
    # codec_glimpses <-
    #   fs::path_package("codec") |>
    #   fs::path("codec_data") |>
    #   fs::dir_ls(glob = "*tabular-data-resource.yaml", recurse = TRUE) |>
    #   purrr::map(read_tdr_csv) |>
    #   purrr::map(glimpse_tdr)
    # 
    # make_md <- function(.x) {
    #   ttl <- .x$attributes[.x$attributes$name == "title", "value"][[1]]
    #   nm <- .x$attributes[.x$attributes$name == "name", "value"][[1]]
    #   options(knitr.kable.NA = "")
    #   c(
    #     glue::glue("## {{ttl}} {.tabset}\n\n", .open = "{{", .close = "}}"),
    #     "\n\n### Attributes \n\n",
    #     paste0(knitr::kable(.x$attributes), sep = "\n"),
    #     "\n\n### Schema \n\n",
    #     paste0(knitr::kable(.x$schema), sep = "\n"),
    #     "\n\n### Download \n\n",
    #     glue::glue("read in R: `codec::codec_data(\"{nm}\")`"),
    #     "\n\n"
    #   ) |>
    #     paste(sep = "\n\n")
    # }
    # 
    # #purrr::map(codec_glimpses, make_md)
    # # codec_glimpses[[1]] |> 
    # #   make_md()
  })

}

shinyApp(ui, server)
