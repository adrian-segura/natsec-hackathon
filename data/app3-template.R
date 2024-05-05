# Title: ShinyApp3
# Description: Plotting California Wildfire data with widgets
# Author: Nicholas Perematko
# Date: 04/25/2024


# =======================================================
# Packages (you can use other packages if you want)
# =======================================================
library(shiny)
library(tidyverse)    # data wrangling and graphics
library(lubridate)    # for working with dates
library(sf)           # for working with geospatial vector-data
library(leaflet)      # web interactive maps
library(plotly)       # web interactive graphics


# =======================================================
# Import data
#
# For demo purposes, in this "template" we use storms data
# (but you will have to replace this with the fire perimeters data)
#
# Uncomment the commands below in order to import the data!!!
# We're assuming the shapefile is in the working directory of your app.
# =======================================================
 cal_perims = st_read(dsn = "California_Fire_Perimeters_(1950+)") |>
   st_transform(crs = 4326)
 cal_perims = cal_perims |>
   mutate(MONTH = month(ALARM_DATE),
          SEASON = case_when(
            MONTH %in% c(1, 2, 12) ~ "Winter",
            MONTH %in% 3:5 ~ "Spring", 
            MONTH %in% 6:8 ~ "Summer",
            MONTH %in% 9:11 ~ "Fall"
          ),
          COLOR = case_when(
            SEASON == "Winter" ~ "#4C69C5",
            SEASON == "Spring" ~ "#3ABEA8",
            SEASON == "Summer" ~ "#FF964E",
            SEASON == "Fall" ~ "#FFC84E"
          ),
          LABEL = paste0(FIRE_NAME, ", ", ALARM_DATE))
 cal_perims_2020 = cal_perims |>
   filter(YEAR_ == 2020)

# ===============================================
# Define "ui" for application
# ===============================================
ui <- fluidPage(
  
  # Application title
  titlePanel("Title of your app"),
  
  # -------------------------------------------------------
  # Input widgets 
  # Customize the following dummy widgets with your own inputs
  # -------------------------------------------------------
  sidebarLayout(
    sidebarPanel(
      # ---------------------------------------------
      # input widgets of first tab
      # (adapt code with widgets of your choice)
      # ---------------------------------------------
      conditionalPanel(
        condition = "input.tabselected==1",
        h4("Year Analysis"),
        # replace with your widgets
        numericInput(inputId = "widget1",
                    label = "Widget 1",
                    value = 133),
        numericInput(inputId = "widget2",
                     label = "Widget 2",
                     value = 133),
      ), # closes 1st panel
      
      # ---------------------------------------------
      # input widgets of second tab
      # (adapt code with widgets of your choice)
      # ---------------------------------------------
      conditionalPanel(
        condition = "input.tabselected==2",
        h4("Table in selected year"),
        # replace with your widgets
        numericInput(inputId = "widget3",
                     label = "Widget 3",
                     value = 133),
        numericInput(inputId = "widget4",
                     label = "Widget 4",
                     value = 133),
      ), # closes 2nd panel
      
      # ---------------------------------------------
      # input widgets of third tab
      # (adapt code with widgets of your choice)
      # ---------------------------------------------
      conditionalPanel(
        condition = "input.tabselected==3",
        h4("Summary of all fires"),
        # replace with your widgets
        numericInput(inputId = "widget5",
                     label = "Widget 5",
                     value = 133),
        numericInput(inputId = "widget6",
                     label = "Widget 6",
                     value = 133),
      ) # closes 3rd panel
      
    ), # closes sidebarPanel
    
    
    # -------------------------------------------------------
    # Main Panel with 3 tabsets: 
    # tab1: map of fires in selected year
    # tab2: table of fires in selected year
    # tab3: summary statistics of all fires
    # -------------------------------------------------------
    mainPanel(
      tabsetPanel(
        type = "tabs",
        # first tab (map)
        tabPanel(title = "Map",
                 value = 1,
                 leafletOutput("map", height = 600)),
        # second tab (table)
        tabPanel(title = "Table",
                 value = 2,
                 dataTableOutput(outputId = "table")),
        # third tab (summary plot)
        tabPanel(title = "Summary",
                 value = 3,
                 plotlyOutput(outputId = "plot")),
        id = "tabselected"
        
      ) # closes tabsetPanel
    ) # closes mainPanel
    
  ) # closes sidebarLayout
) # closes fluidPage (UI)



# ===============================================
# Define server logic
# ===============================================
server <- function(input, output) {
  
  # -----------------------------------------------
  # Output for first TAB (i.e. map)
  # (adapt code to make your map of fire perimeters)
  # -----------------------------------------------
  output$map <- renderLeaflet({
    # the following code is for demo purposes only; adapt it!!!
    leaflet(data = cal_perims_2020) |>
      setView(lng = -119, lat = 37, zoom = 6) |>
      addTiles() |> 
      addPolygons(color = "tomato",
                  opacity = 0.9,
                  weight = 1,
                  label = ~FIRE_NAME)
  })
  
  
  # -----------------------------------------------
  # Output for second TAB (i.e. table of fires)
  # (adapt code to show a table of fires in selected year)
  # -----------------------------------------------
  output$table <- renderDataTable({
    # the following code is for demo purposes only; adapt it!!!
    storms |>
      filter(year == 2020) |>
      group_by(name) |>
      summarise(max_wind = max(wind),
                min_pres = min(pressure))
  })
  
  
  # ------------------------------------------------
  # Output for third TAB (i.e. summary plot)
  # (adapt code to graph a summary plot)
  # ------------------------------------------------
  output$plot <- renderPlotly({
    # the following code is for demo purposes only; adapt it!!!
    storms_summary = storms |>
      group_by(year) |>
      summarise(
        num_storms = length(unique(id)),
        med_wind = median(wind)
      )
    
    storms_summary |>
      ggplot() + 
      geom_col(aes(x = year, y = num_storms), fill = "skyblue") +
      labs(title = "Number of Tropical Cyclones per Year",
           y = "Number of Storms") +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size = 5))
  })
  
} # closes server



# ===============================================
# Run the application
# ===============================================
shinyApp(ui = ui, server = server)
