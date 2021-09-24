library(shiny)
library(leaflet)
library(sf)
library(shinythemes)
library(rclipboard)
library(shinyalert)

lgas <- readRDS("lgas_small.rds")
parks <- readRDS("sydney_parks.rds")
ico <- makeAwesomeIcon()
max_people <- 5

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

find_allowed_parks <- function(overall_area) {
  
  parks %>%
    dplyr::filter(st_intersects(geometry, overall_area, sparse = FALSE))
  
}

ui <- fluidPage(
  theme = shinytheme("flatly"),
  rclipboardSetup(),
  useShinyalert(),
  tags$head(
    tags$link(rel="shortcut icon", href="/favicon.ico"), 
    tags$style(
      HTML(
        ".leaflet-container {
  cursor: auto !important;
}")
    ),
    includeHTML(("google-analytics.html")),
    tags$script(HTML(
      "$(document).on('shiny:inputchanged', function(event) {
       if (event.name === 'showParks') {
         gtag('event', 'input', event.name, event.value);
       }
     });"
    )),
    tags$meta(name = "image", property = "og:image", content="https://picnicnear.me/syd-picnic-image.png"),
    tags$meta(name = "author", content = "Nick Twort"),
    tags$meta(name = "title", property = "og:title", content = "Sydney picnic radius"),
    tags$meta(name = "description", property = "og:description", content = "Find out where you can picnic with your fully vaccinated friends"),
    tags$meta(name = "twitter:card", content = "summary")
  ),
  titlePanel("Sydney picnic party"),
  fluidRow(
    column(6, p("Click on the map to enter the home locations of your friends. The map will show where each person can travel - within 5km of their home and also anywhere in their LGA. The red area is where everyone can go. Don't forget, you need to be fully vaccinated for this to apply!")),
    column(2, actionButton("clearMarkers", "Start again")),
    column(2, actionButton("showParks", textOutput("parks_message")))
  ),
  fluidRow(
    column(6, p("Looking for a map for Melbourne? That's", a(href = "https://picnicnear.me/vic", "here!")))
  ),
  fluidRow(
    column(12, h3(textOutput("msg")))
  ),
  fluidRow(
    column(12, align = "center", shinycssloaders::withSpinner(leafletOutput("map1", height = 600, width = "80%"), type = 4, color = "#95a5a6", hide.ui = FALSE))
  ),
  fluidRow(
    column(12, uiOutput("copy_url"), style='padding:10px;')
  ),
  fluidRow(
    column(12, p("This is an unofficial website based on open data. Information provided here should be treated as a guide only and may not be up to date. We strongly recommend users review official sources in addition with consulting this website as a guide. Whilst we endevour to ensure the information provided on this website or application is accurate and up-to-date, we do not guarantee the accuracy or timeliness of information presented on the website or application. You should not rely solely on the information on this website."))
  ),
  fluidRow(
    column(12, p("Final apologies - graphic (and web) design is not my passion."))
  ),
  fluidRow(
    column(12, p(a(href = "https://twitter.com/nwbort", target = "_blank", "Follow me on Twitter", .noWS = "after"), ", find the code on ", a(href = "https://github.com/nwbort/sydney-mates-crossover", target = "_blank", "Github", .noWS = "after"), " and stay safe everyone!"))
  ),
  fluidRow(
    column(12, p(textInput("seed", label = "yo:", value = "searching")), style = "visibility: hidden;")
  )
)


server <- function(input, output, session) {
  
  showModal(
    modalDialog(
      title = "Welcome to the Sydney picnic map!",
      p("This tool can help you plan where you can meet your fully vaccinated friends. Click on the map at each person's place of residence (you can drag the markers around if you make a mistake!)."),
      p("The map will show the area that everyone can reach in red."),
      p("Click the 'show parks' button to find a nice grassy (or beachy) spot to meet!"),
      footer = a(href = "https://www.nsw.gov.au/covid-19/rules/greater-sydney#outdoor-gatherings", target = "_blank", "See the NSW government website for more details."),
      easyClose = TRUE
    )
  )
  

  create_popup_div <- function(id) {
    paste0('<div style="text-align:center;">Drag to move<br/><a id="friend_marker_click" href="#" class="action-button" onclick="{Shiny.onInputChange(&quot;friend_marker_click&quot;, ', id, ');}">Click to delete</button></div>')
  }
  

  output$map1 <- renderLeaflet({
    
    l <- leaflet() %>%
      addProviderTiles("CartoDB") %>%
      addScaleBar(position = "bottomright", options = scaleBarOptions(maxWidth = 200, imperial = FALSE)) %>% 
      fitBounds(151.104, -33.819, 151.306, -33.913) %>% 
      addPolygons(data = lgas, fill = FALSE, weight = 1, color = "black", opacity = 0.2, group = "lgas", options = pathOptions(clickable = FALSE))
    
    l
  })
  
  qsps <- reactive({
    
    tmp <- parseQueryString(session$clientData$url_search)
    tmp
    
  })
  # 
  
  check_latlon_params <- function(qsp_list) {
    
    params <- names(qsp_list)
    
    marker_candidates <- unique(gsub("[A-z]+", "", params))
    
    marker_pts <- lapply(seq_along(marker_candidates), function(i) {
      
      # Get current marker number
      marker_number <- marker_candidates[i]
      
      # If lat AND lng exists, return it
      if (all(paste0(c("lat", "lng"), marker_number) %in% params)) {
        
        lng <- as.numeric(qsp_list[[paste0("lng", marker_number)]])
        lat <- as.numeric(qsp_list[[paste0("lat", marker_number)]])
        
        return(st_sfc(st_point(c(lng, lat)), crs = 4326))
        
      } else {
        return(NULL)
      }
      
    })
    
    # Remove NULLs
    marker_pts <- purrr::compact(marker_pts)
    
    # Take only top 5 points
    marker_pts <- purrr::compact(marker_pts[1:5])
    
    # Return
    return(purrr::compact(marker_pts))
    
  }
  
  parse_qsps <- function(qsp_list) {
    
    check_latlon_params(qsp_list)
    
  }
  
  init_markers <- function() {
    
    markers <- parse_qsps(qsps())
    
    markers
    
  }
  
  
  init_marker_count <- function(markers) {
    
    length(markers)
    
  }
  
  
  init_polys <- function(markers) {
    
    
    poly_list <- list()
    
    if (length(markers) > 0) {
      
      initial_markers <- markers
      
      for (i in seq_along(initial_markers)) {
        
        marker_ <- initial_markers[[i]]
        
        # Generate 5km buffer around point and LGA
        poly <- generate_allowed_area(marker_)
        poly_list <- append(poly_list, list(st_as_sf(poly)))
        
        # leafletProxy("map1") %>%
        # addPolygons(data = poly, color = "blue", fillOpacity = 0.1, layerId = paste0("poly_", i), group = "areas", options = pathOptions(clickable = FALSE))
        
      }
      
    }
    
    poly_list
    
  }
  
  init_overlap <- function(markers, polys) {
    
    overlappy <- NA
    
    if (length(markers) > 0) {
      overlappy <- calculate_intersections(polys, length(markers))
      
      # leafletProxy("map1") %>%
      # addPolygons(data = overlappy, color = "red", fillOpacity = 0.5, group = "overlappy")
    } 
    
    overlappy
  }
  
  
  initial_overlap <- function(markers, polys) {
    is.na(init_overlap(markers, polys)) || nrow(init_overlap(markers, polys)) > 0
  }
  
  v <- reactiveValues(
    polys = list(),
    msg = "",
    overlap = TRUE,
    overlappy = NA,
    marker_count = 0,
    markers = list(),
    parks = list(),
    show_parks = FALSE,
    parks_message = "Show parks",
    qsp = NA,
    qsps = "https://picnicnear.me"
  )
  
  
  
  observeEvent(input$seed, {
    
    updateTextInput(inputId = "seed", value = "")
    
    v$markers <- init_markers()
    
    v$marker_count <- init_marker_count(v$markers)
    
    v$polys <- init_polys(v$markers)
    
    v$overlappy <- init_overlap(v$markers, v$polys)
    
    v$overlap <- initial_overlap(v$markers, v$polys)
    
    if (v$marker_count > 0) {
      for (i in seq_along(v$markers)) {
        leafletProxy("map1") %>% 
          addAwesomeMarkers(data = v$markers[[i]], options = markerOptions(draggable = TRUE), layerId = i, icon = ico) %>% 
          addPolygons(data = v$polys[[i]], color = "blue", fillOpacity = 0.1, layerId = paste0("poly_", i), group = "areas", options = pathOptions(clickable = FALSE))
        
      }
      if (v$overlap) {
        leafletProxy("map1") %>% 
          addPolygons(data = v$overlappy, color = "red", fillOpacity = 0.5, group = "overlappy")
      }
    }
    
    
    
  }, once = TRUE)
  
  
  
  
  generate_qsps <- function(markers) {
    
    
    coords <- unlist(markers)
    
    qsp <- paste0(c("lng", "lat"), sort(rep.int(seq.int(from = 1, to = length(coords)/2, by = 1), 2)), "=", round(coords, digits = 4), collapse = "&")
    
    qsp
    
  }
  
  generate_url <- function() {
    if (v$marker_count > 0) {
      qsp <- generate_qsps(v$markers)
      generated_url <- paste0(session$clientData$url_protocol, "//", session$clientData$url_hostname, ":", session$clientData$url_port, session$clientData$url_pathname, "?", qsp)
    } else {
      generated_url <- paste0(session$clientData$url_protocol, "//", session$clientData$url_hostname, ":", session$clientData$url_port, session$clientData$url_pathname)
    }
    generated_url
  }
  
  # generate_initial_values <- function(markers, polys) {
  #   # v$qsps <- generate_url()
  #   # v$markers <- markers
  #   # v$polys <- polys
  #   return(TRUE)
  # }
  
  # Add clipboard buttons
  output$copy_url <- renderUI({
    rclipButton("clipbtn", " Share link to your map", v$qsps, icon("external-link-alt"))
  })
  
  observeEvent(input$clipbtn, {
    shinyalert(
      title = "",
      text = "Copied to clipboard",
      size = "xs", 
      closeOnEsc = TRUE,
      closeOnClickOutside = TRUE,
      html = FALSE,
      type = "success",
      showConfirmButton = FALSE,
      showCancelButton = FALSE,
      timer = 1500,
      imageUrl = "",
      animation = TRUE
    )
  })
  
  output$msg <- renderText(v$msg)
  
  output$parks_message <- renderText(v$parks_message)
  
  observeEvent(input$map1_click, {
    
    # browser()
    # Check that we can still add points
    if (v$marker_count < max_people) {
      
      # Only add new markers if there is currently an overlap
      if (v$overlap) {
        
        # Indicate that we are adding another marker
        v$marker_count <- v$marker_count + 1
        
        # Get point at click location
        pt <- st_sfc(st_point(c(input$map1_click$lng, input$map1_click$lat)), crs = 4326)
        
        # Add the new point to the markers
        v$markers <- append(v$markers, list(pt))
        
        # Generate 5km buffer around point and LGA
        poly <- generate_allowed_area(pt)
        
        # Add the new polygon to the polys
        v$polys <- append(v$polys, list(st_as_sf(poly)))
        
        # Calculate new overlap
        v$overlappy <- calculate_intersections(v$polys, v$marker_count)
        
        # Add marker, polygon and new overlap to map
        leafletProxy("map1") %>%
          addPolygons(data = poly, color = "blue", fillOpacity = 0.1, layerId = paste0("poly_", v$marker_count), group = "areas", options = pathOptions(clickable = FALSE)) %>% 
          clearGroup(group = "parks") %>% 
          clearGroup(group = "overlappy") %>% 
          addPolygons(data = v$overlappy, color = "red", fillOpacity = 0.5, group = "overlappy") %>% 
          addAwesomeMarkers(lng = input$map1_click$lng, lat = input$map1_click$lat, options = markerOptions(draggable = TRUE), layerId = as.character(v$marker_count), icon = ico, popup = create_popup_div(v$marker_count))
        
        # Check if there is an overlap
        v$overlap <- nrow(v$overlappy) > 0
        
        # If we are showing the parks...
        if (v$show_parks) {
          
          # ...and if there is an overlap
          if (v$overlap) {
            # Calculate the parks in the overlap
            v$parks <- find_allowed_parks(v$overlappy)
            
            # If there are parks, show them...
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
              
              # ...and if not, send a message  
            } else {
              v$msg <- "Sorry, we couldn't find any parks there!"
            }
            
          } else {
            v$parks <- list()
          }
        }
        
      }
      
      # Re-check for an overlap
      v$overlap <- nrow(v$overlappy) > 0
      
      # If no overlap, send a message
      if(!v$overlap) {
        v$msg <- "No overlap! Bugger. Click the button to start again or drag the markers until they overlap."
      }
      
      # If already at max people, send that message
    } else {
      
      v$msg <- glue::glue("Sadly, you can't have more than {max_people} people! Click the button to start again.")
    }
    
    v$qsps <- generate_url()
    
    
  })
  
  
  observeEvent(input$friend_marker_click, {
    
    print(input$friend_marker_click)
    
    leafletProxy("map1") %>% 
      removeMarker(layerId = as.character(input$friend_marker_click))
    
  })
  
  
  
  # Button to clear the map
  observeEvent(input$clearMarkers, {
    
    # Remove markers, areas, overlap and parks from map
    leafletProxy("map1") %>%
      clearMarkers() %>%
      clearGroup("areas") %>%
      clearGroup("overlappy") %>% 
      clearGroup("parks")
    
    # Reset reactive values
    v$polys <- list()
    v$msg <- ""
    v$overlap <- TRUE
    v$overlappy <- NA
    v$marker_count <- 0
    v$markers <- list()
    v$parks <- list()
    
    v$qsps <- generate_url()
    
  })
  
  
  observeEvent(input$showParks, {
    
    # Check for no NAs in overlap
    if (!any(is.na(v$overlappy))) {
      
      # If parks are being shown, turn them off...
      if (v$show_parks) {
        
        v$show_parks <- FALSE
        v$parks_message <- "Show parks"
        
        v$parks <- list()
        
        # Remove parks from the map
        leafletProxy("map1") %>%
          clearGroup("parks")
        
        # ...and if they are not, turn them on
      } else {
        
        v$show_parks <- TRUE
        v$parks_message <- "Hide parks"
        
        if (v$overlap) {
          v$parks <- find_allowed_parks(v$overlappy)
          
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
        
        v$parks <- find_allowed_parks(v$overlappy)
        
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
    
    v$qsps <- generate_url()
    
  })

  
}





shinyApp(ui, server)
