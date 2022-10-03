library(shiny)
library(tidyverse)
library(leaflet)
library(here)
library(sf)
library(spData)
library(htmltools)
library(shinyWidgets)

data <- read_csv(here::here("data", "sgcn_final_data.csv")) %>% select(-1)
spp_choices <- names(data[2:(length(data)-1)])
states <- us_states["NAME"]
df <- data %>% left_join(states, by = c("states_2015"="NAME")) %>% st_as_sf() %>% 
  st_transform("WGS84") %>%
  mutate(map_labels = gsub(",", ",<br>", text))

# Define UI for application
ui <- fluidPage(

    titlePanel("Bat Species of Greatest Conservation Need"),
    br(),
    sidebarLayout(
      sidebarPanel(
        pickerInput('spp', "Select species to see which states consider them species SGCN.", 
                    multiple = TRUE, choices = spp_choices, 
                    options = list(`actions-box` = TRUE), selected = spp_choices[1])
      ),
    
    mainPanel(
      strong("Hover on a state to see which species it considers SGCN"),
      br(),
      p("If multiple species are selected, any state where one or more of those species are 
        SGCN will be highlighted"),
      leafletOutput("spp_map")
      )
    )
  )


server <- function(input, output) {

    map_data_react <- reactive({
      
      if (length(input$spp)==0){
        df
      } else{
      df %>% select(states_2015, any_of(input$spp)) %>% 
        filter_at(input$spp, any_vars(.==1))
      }
    })
    
    output$spp_map <- renderLeaflet({
      
      leaflet(data = map_data_react()) %>% addTiles() %>% 
        setView( lng = -97
                 , lat = 39
                 , zoom = 4 ) %>% 
        addPolygons() %>% 
        addPolygons(data = df, fillOpacity=0, opacity=0, 
        label = ~lapply(paste0("SGCN in ", states_2015, ': ', map_labels), HTML))
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
