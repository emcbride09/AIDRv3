
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
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


#uipage

ui <- fluidPage(
  titlePanel("Attacks Against Education"),
  sidebarLayout(
    sidebarPanel(top = 10, right = 5,
                 
                 sliderInput("daterange", "Date Range", min(accleddata$event_date), max(accleddata$event_date),
                             value = range(accleddata$event_date), step = 1,
                             animate = animationOptions(interval = 1000, loop = TRUE)
                 )
                 
    ), 
    mainPanel(
      leafletOutput("myheatmap"),
      br(), br(),
      
      plotOutput("plot"))
    
  )
)
