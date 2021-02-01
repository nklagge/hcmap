library(dplyr)
library(leaflet)
library(shiny)
library(shinyWidgets)
library(stringr)
#picker group module
# load data
dta <- read.csv("Data/restaurants.csv", stringsAsFactors = FALSE) %>%
  mutate(date = as.Date(date))
# controls settings
cost_choices <- c("$", "$$", "$$$", "$$$$")
min_date <- min(dta$date)
max_date <- max(dta$date)
cuisine_choices <- dta$cuisine %>%
  strsplit(", ") %>% 
  unlist() %>% 
  unique() %>%
  sort()
boro_choices <- dta$political %>%
  unique() %>%
  sort()
# create marker icon
fsicon <- makeIcon(iconUrl = "Static/forkspoon.png", iconWidth = 18, iconHeight = 18)

ui <- fluidPage(
   
   # Application title
   titlePanel("Hungry City Map"),
   sidebarLayout(
      sidebarPanel(
        pickerInput("cost", "Cost:",
                           choices = cost_choices, 
                           selected = cost_choices,
                    multiple = TRUE,
                    options = list(`actions-box` = TRUE,
                                   header = "Choose any range of prices:")),
        pickerInput("cuisines", "Cuisines:",
                           choices = cuisine_choices, 
                           selected = cuisine_choices,
                    multiple = TRUE,
                    options = list(`actions-box` = TRUE,
                                   header = "Choose any number of cuisines:")),
        pickerInput("boro", "Boro:",
                    choices = boro_choices, 
                    selected = boro_choices,
                    multiple = TRUE,
                    options = list(`actions-box` = TRUE,
                                   header = "Choose any number of boros:")),
        uiOutput("nbUI"),
        sliderInput("date_range", 
                    "Choose Date Range:", 
                    min = min_date, max = max_date, 
                    value = c(min_date, max_date)
        )
      ),
      
      mainPanel(
         leafletOutput("hcmap")
      )
   )
)

server <- function(input, output) {
  cuisine_match <- reactive({paste(input$cuisines, collapse = "|")})
  # dynamic neighborhood output
  output$nbUI <- renderUI({
    nbopts <- dta %>%
      filter(political %in% input$boro) %>%
      select(neighborhood) %>%
      unique() %>%
      unlist(use.names = FALSE) %>%
      sort()
    pickerInput("nbhd", "Neighborhood:",
                choices = nbopts, 
                selected = nbopts,
                multiple = TRUE,
                options = list(`actions-box` = TRUE,
                               header = "Choose any number of neighborhoods:"))
  })
  output$hcmap <- renderLeaflet({
    # check inputs
    if (is.null(input$cost) |
        is.null(input$cuisines) |
        is.null(input$boro) |
        is.null(input$nbhd)) return()
    # apply filters based on inputs
    dta <- dta %>%
      filter(cost %in% input$cost) %>%
      filter(date >= min(input$date_range) & date <= max(input$date_range)) %>%
      filter(str_detect(cuisine, cuisine_match())) %>%
      filter(political %in% input$boro) %>%
      filter(neighborhood %in% input$nbhd)
    # verify we still have data
    if (nrow(dta) == 0) return()
    # render map
    dta %>%
      leaflet() %>%
      addProviderTiles("Esri.WorldTopoMap") %>%
      addMarkers(~lon, ~lat, label = ~name, popup = ~popup, icon = fsicon)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)