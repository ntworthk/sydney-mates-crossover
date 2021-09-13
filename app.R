library(shiny)
library(leaflet)
library(sf)

lgas <- readRDS("lgas_small.rds")
parks <- readRDS("sydney_parks.rds")
ico <- makeAwesomeIcon()

generate_buffer <- function(point, radius) {
  point %>%
    st_transform(3577) %>%
    st_buffer(radius) %>%
    st_transform(4326)
}

generate_allowed_area <- function(point) {
  
  # 5km radius
  poly <- generate_buffer(point, 5000)
  
  # Find LGA of point
  lga_idx <- as.numeric(st_within(point, lgas))
  
  # If point is in an LGA
  if (!is.na(lga_idx)) {
    
    # Get the LGA
    current_lga <- lgas[lga_idx, ]
    
    # If not an LGA of concern, valid area is within 5km of point _or_ within
    # LGA, so take union. If it is an LGA of concern, valid area is only
    # within 5km of point, so don't take the union
    if (!current_lga$of_concern & !grepl("Unin", current_lga$ABB_NAME)) {
      poly <- st_union(poly, current_lga)
    }
    
  }
  
  poly
  
}

calculate_intersections <- function(polygons, marker_count) {
  
  inter <- dplyr::bind_rows(polygons) %>%
    st_transform(3577) %>% 
    st_intersection() %>%
    st_transform(4326) %>% 
    dplyr::filter(n.overlaps == marker_count)
  
  if (nrow(inter) > 0 && st_geometry_type(inter) != "POLYGON") {
    
    inter <- inter %>%
      st_collection_extract(type = "POLYGON") %>%
      st_make_valid() %>% 
      dplyr::filter(st_is_valid(x)) %>% 
      st_sf()
  }
  
  inter <- inter %>% dplyr::filter(st_geometry_type(x) %in% c("MULTIPOLYGON", "POLYGON"))
  
  inter
  
}


ui <- fluidPage(
  tags$head(tags$link(rel="shortcut icon", href="/favicon.ico"), tags$style(HTML(".leaflet-container {
  cursor: auto !important;
}"))),
  titlePanel("Sydney picnic party"),
  fluidRow(
    column(6, p("Click on the map to enter the home locations of your friends. The map will show where each person can travel - within 5km of their home and also anywhere in their LGA (unless it's an LGA of concern). The red area is within 5km of all people. Don't forget, you need to be fully vaccinated for this to apply!")),
    column(2, actionButton("clearMarkers", "Start again")),
    column(2, actionButton("showParks", textOutput("parks_message")))
  ),
  fluidRow(
    column(12, h3(textOutput("msg")))
  ),
  fluidRow(
    column(12, align = "center", leafletOutput("map1", height = 600, width = "80%"))
  ),
  fluidRow(
    column(12, p("This is an unofficial website based on open data. Information provided here should be treated as a guide only and may not be up to date. We strongly recommend users review official sources in additional with consulting this website as a guide. Whilst we endevour to ensure the information provided on this website or application is accurate and up-to-date, we do not guarantee the accuracy or timeliness of information presented on the website or application. You should not rely solely on the information on this website."))
  ),
  fluidRow(
    column(12, p("Final apologies - graphic (and web) design is not my passion."))
  ),
  fluidRow(
    column(12, a(href = "https://twitter.com/nwbort", target = "_blank", "Follow me on Twitter"))
  )
)


server <- function(input, output, session) {
  
  showModal(modalDialog(
    title = "Welcome to the Sydney picnic map!",
    p("This tool can help you plan where you can meet your fully vaccinated friends. Click on the map at each person's place of residence (you can drag the markers around if you make a mistake!)."),
    p("The map will show the area that everyone can reach in red."),
    p("Click the 'show parks' button to find a nice grassy spot to meet!"),
    footer = a(href = "https://www.nsw.gov.au/covid-19/rules/greater-sydney#outdoor-gatherings", target = "_blank", "See the NSW government website for more details."),
    easyClose = TRUE
  ))
  
  v <- reactiveValues(polys = list(), msg = "", overlap = TRUE, overlappy = NA, marker_count = 0, markers = list(), parks = list(), show_parks = FALSE, parks_message = "Show parks")
  
  output$map1 <- renderLeaflet({
    leaflet() %>%
      addProviderTiles("CartoDB") %>%
      addScaleBar(position = "bottomright", options = scaleBarOptions(maxWidth = 200, imperial = FALSE)) %>% 
      fitBounds(151.1037661745509, -33.8186205182385, 151.30555830654728, -33.913499212816504) %>% 
      addPolygons(data = lgas, fill = FALSE, weight = 1, color = "black", opacity = 0.2, group = "lgas", options = pathOptions(clickable = FALSE))
  })
  
  output$msg <- renderText(v$msg)
  
  output$parks_message <- renderText(v$parks_message)
  
  observeEvent(input$map1_click, {
    
    if (v$marker_count < 5) {
      
      if (v$overlap) {
        v$marker_count <- v$marker_count + 1
        
        # Get point at click location
        pt <- st_sfc(st_point(c(input$map1_click$lng, input$map1_click$lat)), crs = 4326)
        
        v$markers <- append(v$markers, list(pt))
        
        # Generate 5km buffer around point and LGA
        poly <- generate_allowed_area(pt)
        
        v$polys <- append(v$polys, list(st_as_sf(poly)))
        
        v$overlappy <- calculate_intersections(v$polys, v$marker_count)
        
        
        leafletProxy("map1") %>%
          addPolygons(data = poly, color = "blue", fillOpacity = 0.1, layerId = paste0("poly_", v$marker_count), group = "areas", options = pathOptions(clickable = FALSE)) %>% 
          clearGroup(group = "parks") %>% 
          clearGroup(group = "overlappy") %>% 
          addPolygons(data = v$overlappy, color = "red", fillOpacity = 0.5, group = "overlappy") %>% 
          addAwesomeMarkers(lng = input$map1_click$lng, lat = input$map1_click$lat, options = markerOptions(draggable = TRUE), layerId = v$marker_count, icon = ico)
        
        v$overlap <- nrow(v$overlappy) > 0
        
        if (v$show_parks) {
          
          if (v$overlap) {
            v$parks <- parks %>%
              dplyr::filter(st_intersects(geometry, v$overlappy, sparse = FALSE))
            
          } else {
            v$parks <- list()
          }
        }
        
      }
      
      if (v$overlap & v$show_parks) {
        
        if (nrow(v$parks) > 0) {
          
          leafletProxy("map1") %>%
            addPolygons(
              data = v$parks,
              color = ~col,
              fillOpacity = ~ifelse(col == "green", 0.6, 1),
              group = "parks",
              label = ~name,
              highlightOptions = highlightOptions(color = "white", weight = 3, bringToFront = TRUE)
            )
          
        } else {
          v$msg <- "Sorry, we couldn't find any parks there!"
        }
        
        
      }
      
      
      
      
      
      v$overlap <- nrow(v$overlappy) > 0
      
      if(!v$overlap) {
        v$msg <- "No overlap! Bugger. Click the button to start again or drag the markers until they overlap."
      }
      
      
    } else {
      v$msg <- "Sadly, you can't have more than 5 people! Click the button to start again."
    }
    
    
    
    
  })
  
  observeEvent(input$clearMarkers, {
    
    
    leafletProxy("map1") %>%
      clearMarkers() %>%
      clearGroup("areas") %>%
      clearGroup("overlappy") %>% 
      clearGroup("parks")
    
    v$polys <- list()
    v$msg <- ""
    v$overlap <- TRUE
    v$marker_count <- 0
    v$markers <- list()
    v$overlappy <- NA
  })
  
  
  observeEvent(input$showParks, {
    
    if (!any(is.na(v$overlappy))) {
      
      
      if (v$show_parks) {
        v$show_parks <- FALSE
        
        v$parks_message <- "Show parks"
        
        v$parks <- NA
        
        leafletProxy("map1") %>%
          clearGroup("parks")
        
      } else {
        
        v$show_parks <- TRUE
        v$parks_message <- "Hide parks"
        
        if (v$overlap) {
          v$parks <- parks %>%
            dplyr::filter(st_intersects(geometry, v$overlappy, sparse = FALSE))
          
          if (nrow(v$parks) > 0) {
            
            leafletProxy("map1") %>%
              addPolygons(
                data = v$parks,
                color = ~col,
                fillOpacity = ~ifelse(col == "green", 0.6, 1),
                group = "parks",
                label = ~name,
                highlightOptions = highlightOptions(color = "white", weight = 3, bringToFront = TRUE)
              )
            
          } else {
            v$msg <- "Sorry, we couldn't find any parks there!"
          }
        }
        
        
        
      }
    } else {
      v$msg <- "You need to add some points on the map first!"
    }
    
  })
  
  observeEvent(input$map1_marker_dragend, {
    
    # Get point at click location
    pt <- st_sfc(st_point(c(input$map1_marker_dragend$lng, input$map1_marker_dragend$lat)), crs = 4326)
    
    v$markers[[input$map1_marker_dragend$id]] <- list(pt)
    
    # Generate 5km buffer around point and LGA
    poly <- generate_allowed_area(pt)
    
    v$polys[[input$map1_marker_dragend$id]] <- st_as_sf(poly)
    
    v$overlappy <- calculate_intersections(v$polys, v$marker_count)
    
    leafletProxy("map1") %>%
      removeShape(layerId = paste0("poly_", input$map1_marker_dragend$id)) %>%
      addPolygons(data = poly, color = "blue", fillOpacity = 0.1, layerId = paste0("poly_", input$map1_marker_dragend$id), group = "areas", options = pathOptions(clickable = FALSE)) %>% 
      clearGroup(group = "overlappy") %>% 
      addPolygons(data = v$overlappy, color = "red", fillOpacity = 0.5, group = "overlappy")
    

    
    if (v$show_parks) {
      
      leafletProxy("map1") %>%
        clearGroup(group = "parks")
      
      if (nrow(v$overlappy) > 0) {
        
        v$parks <- parks %>%
          dplyr::filter(st_intersects(geometry, v$overlappy, sparse = FALSE))
        
        if (nrow(v$parks) > 0) {
          
          leafletProxy("map1") %>%
            addPolygons(
              data = v$parks,
              color = ~col,
              fillOpacity = ~ifelse(col == "green", 0.6, 1),
              group = "parks",
              label = ~name,
              highlightOptions = highlightOptions(color = "white", weight = 3, bringToFront = TRUE)
            )
          
        } else {
          v$msg <- "Sorry, we couldn't find any parks there!"
        }
        
      } else {
        v$parks <- list()
      }
      
      
    }
    
    v$overlap <- nrow(v$overlappy) > 0
    
    if(!v$overlap) {
      v$msg <- "No overlap! Bugger. Click the button to start again or drag the markers until they overlap."
    } else {
      v$msg <- ""
    }
    
    
  })
  
}

shinyApp(ui, server)
