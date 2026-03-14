library(shiny)
library(bslib)
library(dplyr)
library(ggplot2)
library(leaflet)
library(lubridate)
library(readr)

# --- Load data ---
df <- read_tsv("data/raw/gbif-beetle.csv")

YEAR_MIN <- min(df$year, na.rm = TRUE)
YEAR_MAX <- max(df$year, na.rm = TRUE)
REGIONS  <- c("All", sort(unique(na.omit(df$countryCode))))
BASIS    <- c("All", sort(unique(na.omit(df$basisOfRecord))))

# --- UI ---
ui <- page_navbar(
  title = "Japanese Beetle Tracker",
  header = tags$style("
    body { background-color: #e8f5e9; }
    .sidebar { background-color: #c8e6c9; }
    .card { background-color: #f1f8e9; }
    .card-header { background-color: #a5d6a7; color: #1b5e20; }
  "),
  
  nav_panel("Dashboard",
            layout_sidebar(
              sidebar = sidebar(
                width = 300,
                sliderInput("year_range", "Year Range",
                            min = YEAR_MIN, max = YEAR_MAX,
                            value = c(YEAR_MIN, YEAR_MAX), sep = ""),
                selectizeInput("region", "Filter by Region",
                               choices = REGIONS, selected = "All"),
                radioButtons("basis_record", "Basis of Record",
                             choices = BASIS, selected = "All"),
                actionButton("reset_btn", "Reset Filters",
                             class = "btn-warning w-100 mt-2"),
                selectInput("colormap", "Map Color Scale",
                            choices = c("YlOrRd", "Greens", "Blues", "plasma", "viridis"),
                            selected = "YlOrRd")
              ),
              
              # Value boxes
              layout_columns(
                uiOutput("vb_total_obs"),
                uiOutput("vb_first_recorded"),
                uiOutput("vb_status"),
                fill = FALSE
              ),
              
              # Map
              accordion(
                accordion_panel("Geographic Distribution Map",
                                leafletOutput("map", height = "450px")
                ),
                open = TRUE
              ),
              
              # Charts
              accordion(
                accordion_panel("Observation Charts",
                                layout_columns(
                                  card(card_header("Occurrences Over Time"),
                                       plotOutput("plot_timeseries")),
                                  card(card_header("Basis of Record"),
                                       plotOutput("plot_basis")),
                                  col_widths = c(6, 6)
                                ),
                                layout_columns(
                                  card(card_header("Top Rights Holders"),
                                       plotOutput("plot_rights_holder")),
                                  card(card_header("Seasonal Observations by Month"),
                                       plotOutput("plot_monthly")),
                                  col_widths = c(6, 6)
                                )
                ),
                open = TRUE
              )
            )
  )
)

# --- Server ---
server <- function(input, output, session) {
  
  # Reactive filtered dataframe
  filtered_df <- reactive({
    d <- df %>%
      filter(year >= input$year_range[1], year <= input$year_range[2])
    if (input$region != "All")
      d <- d %>% filter(countryCode == input$region)
    if (input$basis_record != "All")
      d <- d %>% filter(basisOfRecord == input$basis_record)
    d
  })
  
  # Reset button
  observeEvent(input$reset_btn, {
    updateSliderInput(session, "year_range", value = c(YEAR_MIN, YEAR_MAX))
    updateSelectizeInput(session, "region", selected = "All")
    updateRadioButtons(session, "basis_record", selected = "All")
  })
  
  # Value boxes
  output$vb_total_obs <- renderUI({
    value_box("Total Observations", format(nrow(filtered_df()), big.mark = ","))
  })
  
  output$vb_first_recorded <- renderUI({
    years <- na.omit(filtered_df()$year)
    val <- if (length(years) > 0) as.character(min(years)) else "N/A"
    value_box("First Recorded", val)
  })
  
  output$vb_status <- renderUI({
    year_max <- input$year_range[2]
    present <- any(filtered_df()$year == year_max, na.rm = TRUE)
    val <- if (present) "Present" else "Not Detected"
    value_box(paste("Status in Region as of", year_max), val)
  })
  
  # Timeseries plot
  output$plot_timeseries <- renderPlot({
    filtered_df() %>%
      count(year) %>%
      ggplot(aes(x = year, y = n)) +
      geom_line(color = "#2e7d32") +
      geom_point(color = "#2e7d32") +
      labs(x = "Year", y = "Observations") +
      theme_minimal()
  })
  
  # Pie chart
  output$plot_basis <- renderPlot({
    filtered_df() %>%
      count(basisOfRecord) %>%
      ggplot(aes(x = "", y = n, fill = basisOfRecord)) +
      geom_col() +
      coord_polar("y") +
      labs(fill = "Basis of Record") +
      theme_void()
  })
  
  # Top rights holders bar chart
  output$plot_rights_holder <- renderPlot({
    filtered_df() %>%
      filter(!is.na(rightsHolder)) %>%
      count(rightsHolder, sort = TRUE) %>%
      slice_head(n = 10) %>%
      ggplot(aes(x = n, y = reorder(rightsHolder, n))) +
      geom_col(fill = "#388e3c") +
      labs(x = "Observations", y = "Rights Holder") +
      theme_minimal()
  })
  
  # Monthly bar chart
  output$plot_monthly <- renderPlot({
    filtered_df() %>%
      mutate(month = month(ymd_hms(eventDate, quiet = TRUE))) %>%
      filter(!is.na(month)) %>%
      count(month) %>%
      ggplot(aes(x = factor(month, labels = month.abb), y = n)) +
      geom_col(fill = "#66bb6a") +
      labs(x = "Month", y = "Observations") +
      theme_minimal()
  })
  
  # Leaflet map
  output$map <- renderLeaflet({
    pts <- filtered_df() %>%
      filter(!is.na(decimalLatitude), !is.na(decimalLongitude),
             between(decimalLatitude, -90, 90),
             between(decimalLongitude, -180, 180))
    
    pal <- colorNumeric(input$colormap, domain = NULL)
    
    # Bin into grid cells (~H3 equivalent using rounding)
    pts <- pts %>%
      mutate(
        lat_bin = round(decimalLatitude, 1),
        lng_bin = round(decimalLongitude, 1)
      ) %>%
      count(lat_bin, lng_bin)
    
    leaflet() %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      setView(lng = 0, lat = 20, zoom = 2) %>%
      addCircleMarkers(
        data = pts,
        lng = ~lng_bin, lat = ~lat_bin,
        radius = ~scales::rescale(n, to = c(3, 15)),
        color = ~pal(n),
        fillOpacity = 0.7,
        stroke = FALSE,
        popup = ~paste("Observations:", format(n, big.mark = ","))
      ) %>%
      addLegend("bottomleft", pal = pal, values = pts$n,
                title = "Observations")
  })
}

shinyApp(ui, server)