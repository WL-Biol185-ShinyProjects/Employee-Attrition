---
title: "Leaflet with Shiny"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

## R Markdown

```{r}
library(shiny)
library(leaflet)
leafletOutput("worldMap")
br(),
actionButton("newButton", "New place!")
)


function(input, output, session) {
  
  output$worldMap <- renderLeaflet({
    
    btn <- input$newButton
    
    lats <- -90:90
    lons <- -180:180
    
    leaflet() %>% 
      setView(lng = sample(lons, 1), lat = sample(lats, 1), zoom = 5) %>% 
      addTiles()
    
  })
}
```




