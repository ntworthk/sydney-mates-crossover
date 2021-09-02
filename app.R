library(shiny)
library(leaflet)
library(sf)

ui <- fluidPage(
  titlePanel("Sydney mates crossover"),
  fluidRow(
    column(8, p("Click on the map to enter the home locations of your friends. The red area is within 5km of all people.")),
    column(4, actionButton("clearMarkers", "Start again"))
  ),
  fluidRow(
    textOutput("msg")
  ),
  fluidRow(
    leafletOutput("map1", height = 800)
  )
)


server <- function(input, output, session) {
  
  polys <- st_sf(st_sfc(st_polygon()), crs = 4326) %>% dplyr::rename(x = 1) %>% head(0)
  
  v <- reactiveValues(polys = polys, msg = "", overlap = TRUE, marker_count = 0)
  
  output$map1 <- renderLeaflet({
    leaflet() %>%
      addProviderTiles("CartoDB") %>%
      addScaleBar(position = "bottomright", options = scaleBarOptions(maxWidth = 200, imperial = FALSE)) %>% 
      fitBounds(151.1037661745509, -33.8186205182385, 151.30555830654728, -33.913499212816504)
  })
  
  output$msg <- renderText(v$msg)
  
  observeEvent(input$map1_click, {
    
    if (v$marker_count < 5) {
      poly <- st_sfc(st_point(c(input$map1_click$lng, input$map1_click$lat)), crs = 4326) %>%
        st_transform(3577) %>%
        st_buffer(5000) %>%
        st_transform(4326)
      
      
      v$polys <- dplyr::bind_rows(v$polys, st_as_sf(poly))
      
      overlappy <- v$polys %>% st_intersection() %>% dplyr::filter(n.overlaps == nrow(v$polys))
      
      print(overlappy)
      
      if (v$overlap) {
        
        leafletProxy("map1") %>%
          addPolygons(data = poly, color = "blue", fillOpacity = 0.1) %>% 
          clearGroup(group = "overlappy") %>% 
          addPolygons(data = overlappy, color = "red", fillOpacity = 0.5, group = "overlappy") %>% 
          addMarkers(lng = input$map1_click$lng, lat = input$map1_click$lat)
        
      }
      
      
      v$overlap <- nrow(overlappy) > 0
      # overlap <- TRUE
      
      
      
      if(!v$overlap) {
        v$msg <- "No overlap! Bugger. Click the button to start again."
      }
      
      v$marker_count <- v$marker_count + 1
      
    } else {
      v$msg <- "Sadly, you can't have more than 5 people! Click the button to start again."
    }
    
    
    
    
  })
  
  observeEvent(input$clearMarkers, {
    
    v$polys <- st_sf(st_sfc(st_polygon()), crs = 4326) %>% dplyr::rename(x = 1) %>% head(0)
    
    leafletProxy("map1") %>% clearMarkers() %>% clearShapes()
    
    v$msg <- ""
    v$overlap <- TRUE
    v$marker_count <- 0
  })
  
}

shinyApp(ui, server)
