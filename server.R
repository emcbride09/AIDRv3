# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


library(leaflet.extras)
require(tidyverse)
require(leaflet)
require(shiny)
require(httr)
require(googlesheets)


server <- function(input, output) {
  ###data wrangle
  
  acledurl <- "https://api.acleddata.com/acled/read?terms=accept&limit=0&notes=school:OR:notes=college:OR:notes=university:OR:actor1=student:OR:actor1=teacher:OR:actor2=student:OR:actor2=teacher&year=2019"
  
  acleddata <- GET(acledurl)
  
  accleddata <- content(acleddata)
  
  accleddata <- accleddata$data
  
  accleddata <- bind_rows(accleddata)
  
  #accleddata <- read.csv('2019-acled-education.csv') %>% filter(data_id != '#event+id')
  
  
  accleddata$event_date <- as.Date(accleddata$event_date, format = "%Y-%m-%d")
  accleddata$latitude <- as.numeric(as.character(accleddata$latitude))
  accleddata$longitude <- as.numeric(as.character(accleddata$longitude))
  accleddata$fatalities <- as.numeric(as.character(accleddata$fatalities))
  
  myheatmap <- leaflet(data = accleddata) %>% 
    addProviderTiles(provider = "OpenStreetMap.HOT") %>% 
    addHeatmap(radius = 10, blur = 25)
  
  #barcharts etc
  
  #gs_auth(new_user = TRUE)
  
  #aidr <- gs_title("2019-07-29 education insecurity tweet counts")
  
  #aidrdat <- gs_read(aidr) %>% filter(`Tweet date` != '#date +posted')
  
  aidrdat <- read.csv("data/2019-07-29 education insecurity tweet counts - Sheet1.csv")
  
  aidrdat <- aidrdat %>% filter(Tweet.date != '#date +posted')
  
  aidrdat$Tweet.date <- as.Date(aidrdat$Tweet.date, origin = "1960-10-01")
  aidrdat$Relevant.tweets <- as.numeric(aidrdat$Relevant.tweets)
  
  
  reactive_data_chrono <- reactive({
    accleddata %>%
      filter(event_date >= input$daterange[1] & event_date <= input$daterange[2])
  })
  
  reactive_plot_data <- reactive({
    aidrdat %>% 
      filter(Tweet.date >= input$daterange[1] & Tweet.date <= input$daterange[2])
  })
  
  
  output$myheatmap <- renderLeaflet({
    leaflet(myheatmap, data = reactive_data_chrono()) %>% 
      addProviderTiles(provider = "OpenStreetMap.HOT") %>% 
      addHeatmap(radius = 10, blur = 20)
  })
  
  output$plot <- renderPlot({
    ggplot(data = reactive_plot_data(),
           aes(Tweet.date, Relevant.tweets, fill = Language))+ geom_col() +
      scale_fill_manual(values = c('#9A031E', '#E58F65', '#70877F')) + 
      labs(title = "Education Insecurity - Tweets over time", 
           subtitle = 'Tweets collected by AIDR regarding incidents of education insecurity',
           x = 'Tweet Date',
           y = '# of relevant tweets') +
      theme(plot.background = element_blank(),
            panel.background = element_blank(),
            plot.title = element_text(family = 'Gotham', size = 24),
            plot.subtitle = element_text(family = 'Source Sans', size = 18),
            axis.text = element_text(family = 'Source Sans'))
  
  }, height = 400)
  
  
  
  
  
  
  
}
