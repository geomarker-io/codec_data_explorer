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
  
  source('dataset_prep.R')
  
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
  
  pal <- colorFactor(codec_bi_pal, factor(out$bi_class, levels = c("1-1","2-1","3-1",
                                                                   "1-2","2-2","3-2",
                                                                   "1-3","2-3","3-3")))
}

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
  
  sidebar = sidebar(
     shinyWidgets::prettyCheckboxGroup(inputId = "core",
                                       label = strong("Select the CoDEC cores you would like to include:"),
                                       choices = core_names$title,
                                       selected = "Census Tract-Level Neighborhood Indices"),
     uiOutput("x_sel"),
     uiOutput("y_sel"),
     hr(),
     htmlOutput('x_desc'),
     hr(),
     htmlOutput('y_desc'),
     width = '18%'
  ), 
  
  nav("Showcase",
      ex_card
  ),
  
  nav("Data Catalog",
      data_card)
  
)


server <- function(input, output, session) {
  
  d_sel_cores <- reactive({
    core_names |> 
      filter(title %in% input$core)
  })
  
  d_sel_metrics <- reactive({
    d_names |> 
      filter(core %in% d_sel_cores()$name)
  })
  
  d <- reactive({
    d_all #|> 
      #select(matches(d_sel_cores()$name))
  })
  
  output$x_sel <- renderUI({
    shinyWidgets::pickerInput(inputId = 'x',
                              label = "X Variable",
                              choices = d_sel_metrics()$title,
                              multiple = FALSE,
                              selected = 'Racial Economic Index of Concentration at the Extremes',
                              options = pickerOptions(
                                liveSearch = TRUE
                              ))
  })
  
  output$y_sel <- renderUI({
    shinyWidgets::pickerInput(inputId = 'y',
                              label = "Y Variable", 
                              choices = d_sel_metrics()$title,
                              multiple = FALSE,
                              selected = 'Material Deprivation Index',
                              options = pickerOptions(
                                liveSearch = TRUE
                              ))
  })
  
  
  
  xvar <- reactive({
    req(input$x)
    
    xvar <- d_names |> 
      filter(title == input$x) |> 
      pull(name)
    
    xvar
    
    })
  
  yvar <- reactive({
    req(input$y)
    
    yvar <- d_names |> 
      filter(title == input$y) |> 
      pull(name)
    
    yvar
    
    })
  
  output$map <- renderLeaflet({
    req(input$x)
    
    bins_x <- pull(d(), xvar())
    bins_y <- pull(d(), yvar())
    
    bins_x <- classInt::classIntervals(bins_x, n = 3, style = "quantile")
    bins_y <- classInt::classIntervals(bins_y, n = 3, style = "quantile")
    
    bins_x <- bins_x$brks
    bins_y <- bins_y$brks
    
    # cut into groups defined above
    out <- d() |> 
      mutate(bi_x = cut(get(xvar()), breaks = bins_x, include.lowest = TRUE))
    out <- out |> 
      mutate(bi_y = cut(get(yvar()), breaks = bins_y, include.lowest = TRUE))
    out <- out|> 
      mutate(bi_class = paste0(as.numeric(bi_x), "-", as.numeric(bi_y)))
    
    out <- out |> 
      mutate(out_lab = paste(xvar(), ": ", get(xvar()), "<br>",
                             yvar(), ": ", get(yvar())))
    
    map <- 
      leaflet(out) |> 
      setView(-84.55, 39.18, zoom = 11.5) |> 
      addProviderTiles(provider = providers$CartoDB.Positron) |>
      addPolygons(fillColor = ~pal(bi_class), fillOpacity = 0.7, stroke = T, 
                  label = ~lapply(out$out_lab, HTML), 
                  weight = .5, color = "#333333") |> 
      removeLayersControl()
    
    map  
    #   tm_basemap("CartoDB.Positron") +
    #   tm_shape(out, unit = 'miles') +
    #   tm_polygons(col ="bi_class", alpha = 0.7, palette = codec_bi_pal, legend.show = FALSE,
    #               popup.vars = c(xvar(), yvar()))
    # 
    # map |> 
    #   tmap_leaflet(in.shiny = TRUE) |> 
    #   removeLayersControl() 
  })
  
 
  
  output$scatter <- renderGirafe({
    req(input$x)
    
    scat <- ggplot(d()) +
      geom_point_interactive(aes_string(x = xvar(), y = yvar(),
                                        data_id = "census_tract_id_2010"), 
                             color = codec_colors()[7]) +
      theme_light() +
      theme(aspect.ratio = 1, title = element_text(size = 8),
            axis.title = element_text(size = 6),
            legend.key.size = unit(3,"mm")) +
      labs(x = paste0(input$x), y = paste0(input$y))
    
    hist1 <- ggplot(d()) +
      geom_histogram_interactive(aes_string(x = xvar(), tooltip = "census_tract_id_2010", 
                                            data_id = "census_tract_id_2010"), 
                                 fill = codec_colors()[2], bins = 20, color = codec_colors()[3]) +
      theme_minimal()
    
    hist2 <- ggplot(d()) +
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
                       options = list(opts_sizing(width = 1, rescale = T),
                       opts_selection(type = "single")))
    
    gir_join
    
  })
  
  d_scat_click <- reactiveVal()
  scat_click <- reactiveVal()
  
  observeEvent(input$scatter_selected, {
    
    scat_click <- c(input$scatter_selected)
    
    d_scat_click <- d() |> 
      filter(census_tract_id_2010 == scat_click) 
      
      bins_x <- pull(d(), xvar())
      bins_y <- pull(d(), yvar())
      
      bins_x <- classInt::classIntervals(bins_x, n = 3, style = "quantile")
      bins_y <- classInt::classIntervals(bins_y, n = 3, style = "quantile")
      
      bins_x <- bins_x$brks
      bins_y <- bins_y$brks
      
      # cut into groups defined above
      out <- d() |> 
        mutate(bi_x = cut(get(xvar()), breaks = bins_x, include.lowest = TRUE))
      out <- out |> 
        mutate(bi_y = cut(get(yvar()), breaks = bins_y, include.lowest = TRUE))
      out <- out|> 
        mutate(bi_class = paste0(as.numeric(bi_x), "-", as.numeric(bi_y)))
      
      out <- out |> 
        mutate(out_lab = paste(xvar(), ": ", get(xvar()), "<br>",
                               yvar(), ": ", get(yvar())))
      
      map <- 
        leafletProxy("map", data = out) |> 
        clearShapes() |> 
        setView(-84.55, 39.18, zoom = 11.5) |> 
        addProviderTiles(provider = providers$CartoDB.Positron) |>
        addPolygons(fillColor = ~pal(bi_class), fillOpacity = 0.7, stroke = T, 
                    label = ~lapply(out$out_lab, HTML), 
                    weight = .5, color = "#333333") |> 
        addPolygons(data = d_scat_click, color = "#FFF", stroke = T, weight = 5, opacity = 1) |> 
        removeLayersControl()
        
      map
      #   tm_basemap("CartoDB.Positron") +
      #   tm_shape(out, unit = 'miles') +
      #   tm_polygons(col ="bi_class", alpha = 0.7, palette = codec_bi_pal, legend.show = FALSE,
      #               popup.vars = c(xvar(), yvar())) +
      #   tm_shape(d_scat_click, unit = 'miles') +
      #   tm_borders(col = "white", lwd = 2)
      # 
      # map_leaflet <- map |> 
      #   tmap_leaflet(in.shiny = TRUE) 
      # 
      # leafletProxy("map") |> 
      #   clearShapes() |> 
      #   removeLayersControl() 
      # 
  })
  
  d_selected <- reactiveVal()
  
  observeEvent(input$map_click, {
    
    map_click <- reactiveVal()
    map_click <- input$map_shape_click
    
    click <- tibble(lng = map_click$lng, lat = map_click$lat) |> 
      sf::st_as_sf(coords= c('lng', 'lat'), crs = sf::st_crs(d_all))
    print(click)
    
    d_selected <- d() |> 
      sf::st_join(click, left = FALSE)
    
    print(d_selected)
    
    output$scatter <- renderGirafe({
      scat <- ggplot() +
        geom_point_interactive(data = d(), aes_string(x = xvar(), y = yvar(),
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
      
      hist1 <- ggplot(d()) +
        geom_histogram_interactive(aes_string(x = xvar(), tooltip = "census_tract_id_2010",
                                              data_id = "census_tract_id_2010"),
                                   fill = codec_colors()[2], bins = 20, color = codec_colors()[3]) +
        theme_minimal()
      
      hist2 <- ggplot(d()) +
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
                         options = list(opts_sizing(width = 1, rescale = T),
                         opts_selection(type = "single")))
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
  
  output$x_desc <- renderText({
    req(input$x)
    
    paste0(strong(input$x), ": ", var_meta |> filter(title == input$x) |> pull(description))
  })
  
  output$y_desc <- renderText({
    req(input$y)
    
    paste0(strong(input$y), ": ", var_meta |> filter(title == input$y) |> pull(description))
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
