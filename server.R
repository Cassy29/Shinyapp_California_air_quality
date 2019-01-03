library(shiny)
library(shinydashboard)
library(shinyjs)
library(dashboardthemes)
library(shinyWidgets)

library(tidyverse)
library(RColorBrewer)

library(htmltools)
library(leaflet)
library(leaflet.extras)
library(sf)
library(devtools) 
install_github("nik01010/dashboardthemes", force = TRUE)
# set working directory
# upload data
ca_counties <- read_sf("CA_counties.shp")
asthma_data <- read_delim("asthma_2012_2016_Shiny.csv", delim = "\t")
aqi_data <- read_delim("annual_aqi_by_county_2012_2016.csv", delim = "\t")

function(input, output, session) {
  # get_data
  get_data <- reactive({
    #input
    inp_year <- req(input$inp_select_year)

    # asthma data
    asthma_year_sum <- asthma_data %>%
      dplyr::filter(Year == inp_year) %>% 
      dplyr::group_by(Year, County) %>%
      dplyr::summarise(num_visits = sum(`Number of Visits`, na.rm = T)) %>%
      dplyr::ungroup() %>% 
      dplyr::arrange(Year, County)
    
    data <- ca_counties %>% 
      dplyr::left_join(asthma_year_sum, by = c("NAME" = "County"))
    
    return(data)
  })
  
  
  # MAP 
  # create_leaflet_map (leaflet) 
  output$create_leaflet_map <- renderLeaflet({
    # leaflet
    leaflet(options = leafletOptions(minZoom = 5, maxZoom = 12, zoomControl = FALSE)) %>%
      #setView(lng = -119.417931, lat =  36.778259, zoom = 6) %>%
      setView(lng = -119.417931, lat =  37.3, zoom = 6) %>%
      addProviderTiles(providers$CartoDB.Positron)
      
  })
  

  
  # leaflet_proxy (observer) 
  observeEvent(input$inp_select_year, {
    # get data 
    data <- get_data()
    
    # col pal
    gnbu <- brewer.pal(9, "PuBu")
    pal <- colorQuantile(palette = gnbu,
                         n = 30,
                         domain = data$num_visits)
    
    # label
    data$asthma_label <- paste0("<div align = 'center'>",
                                "<h3 style = 'font-family: 'Quicksand', sans-serif; color: black'>", data$NAME, "</h3>",
                                "<h4>Number of Visits</h4>",
                                "<h2><b>",format(round(data$num_visits), big.mark = ","), "</b></h2>",
                                "</div>") %>%
      lapply(htmltools::HTML)
    
    leafletProxy("create_leaflet_map") %>%
      clearShapes() %>%
      addPolygons(data = data,
                  layerId = ~NAME,
                  weight = 1,
                  opacity = 1,
                  color = "white",
                  label = ~asthma_label,
                  fillColor = ~pal(num_visits),
                  fillOpacity = 1,
                  highlight = highlightOptions(
                    weight = 3,
                    color = "orange",
                    bringToFront = TRUE))
  })
  

  # Number of Visists 
  # plot_num_visist (Plot) 
  output$plot_num_visist <- renderPlot(bg = "transparent", {
    #input
    inp_year <- req(input$inp_select_year)
    
    # asthma data
    asthma_year_sum <- asthma_data %>%
      dplyr::filter(Year == inp_year) %>% 
      dplyr::group_by(Year, County) %>%
      dplyr::summarise(num_visits = sum(`Number of Visits`, na.rm = T)) %>%
      dplyr::ungroup() %>% 
      dplyr::arrange(Year, County)
    
    # col pal
    gnbu <- brewer.pal(9, "PuBu")
    pal <- colorQuantile(palette = gnbu,
                         n = 30,
                         domain = asthma_year_sum$num_visits)
    
    asthma_year_sum <- asthma_year_sum %>%
      dplyr::mutate(col_pal = pal(num_visits))
    
    p <- asthma_year_sum %>%
      ggplot(aes(fct_reorder(County, num_visits), num_visits)) +
      geom_col(aes(fill = County), show.legend = F) +
      scale_fill_manual(values = asthma_year_sum$col_pal) +
      labs(x = NULL, y = NULL) +
      coord_flip() +
      theme_minimal() + 
      theme(panel.grid = element_blank(),
            axis.text = element_text(color = "white", size = 9))
    
    return(p)
  })
  
  # 
  # Annual AQI 
  # annual_aqi_ui (UI) 
  output$anual_aqi_ui <- renderUI({
    #input of selected year
    inp_year <- req(input$inp_select_year)
    # clicked county
    # clicked_lga
    clicked_county <- req(input$create_leaflet_map_shape_click)
    clicked_county <- clicked_county$id
    # data
    aqi_data_slice <- aqi_data %>% 
      dplyr::filter(County == clicked_county, 
                    Year == inp_year)
    
    tagList(
      tags$div(align = "right",
               tags$h3(tags$b(aqi_data_slice$County, " - ", aqi_data_slice$Year), style = "color: white"),
               tags$h4("Days with AQI: ", 
                       tags$b(aqi_data_slice$`Days with AQI`), style = "color: white"),
               tags$h4("Good Days: ", 
                       tags$b(aqi_data_slice$`Good Days`), style = "color: white"),
               tags$h4("Moderate Days: ", 
                       tags$b(aqi_data_slice$`Moderate Days`), style = "color: white"),
               tags$h4("Unhealthy for Sensitive Groups Days: ", 
                       tags$b(aqi_data_slice$`Unhealthy for Sensitive Groups Days`), style = "color: white"),
               tags$h4("Unhealthy Days: ", 
                       tags$b(aqi_data_slice$`Unhealthy Days`), style = "color: white"),
               tags$h4("Very Unhealthy Days: ", 
                       tags$b(aqi_data_slice$`Very Unhealthy Days`), style = "color: white"),
               tags$h4("Hazardous Day: ", 
                       tags$b(aqi_data_slice$`Hazardous Days`), style = "color: white"),
               tags$h4("Max AQI: ", 
                       tags$b(aqi_data_slice$`Max AQI`), style = "color: white"),
               tags$h4("90th Percentile AQI: ", 
                       tags$b(aqi_data_slice$`90th Percentile AQI`), style = "color: white"),
               tags$h4("Median AQ: ", 
                       tags$b(aqi_data_slice$`Median AQI`), style = "color: white"),
               tags$h4("Days CO: ", 
                       tags$b(aqi_data_slice$`Days CO`), style = "color: white"),
               tags$h4("Days NO2: ", 
                       tags$b(aqi_data_slice$`Days NO2`), style = "color: white"),
               tags$h4("Days Ozone: ", 
                       tags$b(aqi_data_slice$`Days Ozone`), style = "color: white"),
               tags$h4("Days SO2 ", 
                       tags$b(aqi_data_slice$`Days SO2`), style = "color: white"),
               tags$h4("Days PM2.5: ", 
                       tags$b(aqi_data_slice$`Days PM2.5`), style = "color: white"),
               tags$h4("Days PM10: ", 
                       tags$b(aqi_data_slice$`Days PM10`), style = "color: white")
      )
    )
    
  })
  
  # update number of visits arrow
  num_of_vis_arrow_icon <- reactiveValues(icon = "angle-up")
  observeEvent(input$inp_collapse_bar_panel, {
    if (num_of_vis_arrow_icon$icon == "angle-down") {
      num_of_vis_arrow_icon$icon <- "angle-up"
    } else {
      num_of_vis_arrow_icon$icon <- "angle-down"
    }
    updateActionButton(session, 
                       inputId = "inp_collapse_bar_panel", 
                       icon = icon(num_of_vis_arrow_icon$icon))
  })
  
  # update anual aqi arrow
  anual_aqi_arrow_icon <- reactiveValues(icon = "angle-up")
  observeEvent(input$inp_collapse_anual_aqi, {
    if (anual_aqi_arrow_icon$icon == "angle-down") {
      anual_aqi_arrow_icon$icon <- "angle-up"
    } else {
      anual_aqi_arrow_icon$icon <- "angle-down"
    }
    updateActionButton(session, 
                       inputId = "inp_collapse_anual_aqi", 
                       icon = icon(anual_aqi_arrow_icon$icon))
  })
  
}