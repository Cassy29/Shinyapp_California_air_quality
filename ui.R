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

# creating custom logo object
logo_asthma <- shinyDashboardLogoDIY(
  
  boldText = "CALIFORNIA"
  ,mainText = "Asthma"
  ,textSize = 16
  ,badgeText = NULL
  ,badgeTextColor = NA
  ,badgeTextSize = NA
  ,badgeBackColor = NA
  ,badgeBorderRadius = NA
  
)

dashboardPage(title = "California Asthma",
  # Header 
  dashboardHeader(title = logo_asthma),
  
  # Sidebar 
  dashboardSidebar(collapsed = FALSE, 
                   sidebarMenu(
                     menuItem("Map", 
                              tabName = "tab_map", 
                              icon = icon("map-o")))
  ), # end dashboardSidebar
  
  # Body 
  dashboardBody(
    # map height
    tags$style(type = "text/css", "#create_leaflet_map {height: calc(100vh - 80px) !important;}"),
    
    # CSS
    includeCSS("www/absPanels.css"),
    includeCSS("www/style.css"),
    shinyjs::useShinyjs(),
    
    tags$head(
      tags$style(HTML("@import url('https://fonts.googleapis.com/css?family=Lato');")),
      tags$style(HTML("@import url('https://fonts.googleapis.com/css?family=Work+Sans');")),
      tags$style(HTML("@import url('https://fonts.googleapis.com/css?family=Quicksand');")),
      tags$style(HTML("@import url('https://fonts.googleapis.com/css?family=Montserrat');"))
    ),
    
    tags$head(tags$style(HTML(".small-box {height: 110px}"))),
    
    shinyDashboardThemes(
      theme = "poor_mans_flatly"
    ),
    
    tabItems(
      # | Map 
      tabItem(tabName = "tab_map",
              fluidRow(
                column(width = 12,
                       leafletOutput("create_leaflet_map", width = "100%"),
                       
                       # Select Year panel 
                       absolutePanel(
                         id = "abs_select_year",
                         draggable = TRUE, 
                         top = "1%", left = "auto", right = "58%", bottom = "auto",
                         height = "auto", width = "500px",
                         
                         tags$div(id = 'div_select_year',
                                  radioGroupButtons(inputId = "inp_select_year", 
                                                    label = NULL, 
                                                    choices = c("2012", "2013", "2014", "2015", "2016"), 
                                                    status = "primary", 
                                                    checkIcon = list(yes = icon("ok", lib = "glyphicon"), 
                                                                     no = icon("remove", lib = "glyphicon")),
                                                    direction = "horizontal"
                                  )
                                  
                         )
                         
                       ), # end abs panel
                       
                       # Number of visits Panel
                       absolutePanel(
                         id = "abs_bar_panel", class = "panel panel-default",
                         draggable = TRUE, 
                         top = "15%", left = "auto", right = "68%", bottom = "auto",
                         height = "auto", width = "350px",
                         
                         fluidRow(
                           column(width = 10,
                                  tags$h4(tags$b("NUMBER"), str_to_upper(" of visits"), style = "color: white")
                           ),
                           
                           column(width = 2,
                                  tags$div(align = "right",
                                           actionButton(inputId = "inp_collapse_bar_panel", 
                                                        label = NULL, 
                                                        "data-toggle" = 'collapse', 
                                                        "data-target" = '#div_bar_panel',
                                                        icon = icon("angle-up"), 
                                                        style = "color: white; font-size: large; background: transparent; border-color: transparent;")
                                           
                                  )
                           )
                         ),
                         tags$div(id = 'div_bar_panel', 
                                  class="collapse in",
                                  plotOutput("plot_num_visist", height = "450px")
                         )
                         
                       ), # end abs panel
                       
                       # Annual AQI 
                       absolutePanel(
                         id = "abs_anual_aqi", class = "panel panel-default",
                         draggable = TRUE, 
                         top = "5%", left = "67%", right = "auto", bottom = "auto",
                         height = "auto", width = "380px",
                         
                         fluidRow(
                           column(width = 9,
                                  tags$h4(tags$b("ANNUAL"), str_to_upper("AirQuality (AQI)"), style = "color: white")
                           ),
                           
                           column(width = 3,
                                  tags$div(align = "right",
                                           actionButton(inputId = "inp_collapse_anual_aqi", 
                                                        label = NULL, 
                                                        "data-toggle" = 'collapse', 
                                                        "data-target" = '#div_anual_aqi',
                                                        icon = icon("angle-up"), 
                                                        style = "color: white; font-size: large; background: transparent; border-color: transparent;")
                                           
                                  )
                           )
                         ),
                         tags$div(id = 'div_anual_aqi', 
                                  class="collapse in",
                                  uiOutput("anual_aqi_ui")
                         )
                         
                       ) # end abs panel
                       
                )
              )
              
      ) # end Map tab
    )
    
  ) # end dashboardBody
) # end dashboardPage