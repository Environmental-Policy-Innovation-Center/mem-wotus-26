library(shiny)
library(leaflet)
library(plotly)
library(tidyverse)
library(gt)
library(sf)
library(dplyr)
library(terra)
library(here)
library(htmltools)

# =====================================================
# DATA PREPARATION
# =====================================================

# -------------------------
# Initial Step: Load Data
# -------------------------

# Check working directory
#setwd()
getwd()

# Load CSV file from personal desktop folder into R
clean_orm_Wilmington <- read.csv("C:/Duke_MEM_MP_2026_2027_CWA_Data_Viz/clean1625_Wilmington.csv")

# Filter for last RIA period (2021-2025) to reflect updates from ORM SOP 2021
NC_post_2021 <- clean_orm_Wilmington %>%
  filter(fiscal_year %in% c(2021, 2022, 2023, 2024, 2025))

# Filter more columns
NC_post2021 <- NC_post_2021 %>%
  select(
    da_number,
    action,
    action_type,
    pnn,
    permit_authority,
    compensatory_mitigation_rqd,
    mitigation_permittee_rsp,
    end_date,
    federally_complete_date,
    proj_latitude,
    proj_longitude,
    impact_duration,
    impact_perm_loss,
    impact_type,
    waters_latitude,
    waters_longitude,
    auth_fill_acres,
    auth_struc_acres,
    auth_drg_fill_acres,
    auth_drg_remvl_acres,
    mit_req_acres,
    fiscal_year
  )

NC_proj <- NC_post2021 %>%
  filter(!is.na(proj_longitude),
         !is.na(proj_latitude)) %>%
  st_as_sf(
    coords = c("proj_longitude", "proj_latitude"),
    crs = 4269
  )

# Load county and HUC8 boundaries from repository Data folder
# Transform to WGS84 (EPSG:4326) for Leaflet
NC_county <- vect(
  here("Data", "NC_County_Boundary", "County_Boundary.shp")) %>%
  st_as_sf() %>%
  st_transform(4326)

NC_HUC8 <- vect(
  here("Data", "NC_HUC_8_Boundary", "NC_HUC_8.shp")) %>%
  st_as_sf() %>%
  st_transform(4326)

NC_proj <- st_transform(NC_proj, 4326)

# -------------------------
# TAB 1: Watershed Summary
# -------------------------

huc8_counts <- permits_huc8 %>%
  st_drop_geometry() %>%
  group_by(HUC_8) %>%
  summarise(
    n_projects = n_distinct(da_number),
    .groups = "drop"
  )

NC_HUC8_projects <- NC_HUC8 %>%
  left_join(
    huc8_counts,
    by = "HUC_8"
  )

NC_HUC8_projects$n_projects[
  is.na(NC_HUC8_projects$n_projects)
] <- 0

pal_huc8 <- colorNumeric(
  palette = "YlOrRd",
  domain = NC_HUC8_projects$n_projects
)

# -------------------------
# TAB 2: Impact Metrics
# -------------------------

acre_bins <- c(
  0,
  0.1,
  0.5,
  1,
  2,
  5,
  10,
  25,
  50,
  100,
  200
)

all_acres <- c(
  NC_proj$auth_fill_acres,
  NC_proj$auth_struc_acres,
  NC_proj$auth_drg_fill_acres,
  NC_proj$auth_drg_remvl_acres,
  NC_proj$mit_req_acres
)

all_acres <- all_acres[
  !is.na(all_acres) &
    all_acres > 0
]

pal_acres <- colorBin(
  palette = "viridis",
  domain = all_acres,
  bins = acre_bins,
  reverse = TRUE,
  pretty = FALSE
)

# -------------------------
# TAB 5: Action Type Map
# -------------------------

pal_action <- colorFactor(
  palette = "Set1",
  domain = sort(
    unique(
      na.omit(NC_proj$action_type))))

# -------------------------
# TAB 6: PNN Type Map
# -------------------------

pal_pnn <- colorFactor(
  palette = "Spectral",
  domain = na.omit(NC_proj$pnn))

# -------------------------
# TAB 7: PNN Type Map by Watershed
# -------------------------

# Spatially join projects to watersheds
permits_huc8 <- st_join(
  NC_proj,
  NC_HUC8,
  left = FALSE)

# Count unique PNNs per watershed
huc8_pnn_counts <- permits_huc8 %>%
  st_drop_geometry() %>%
  filter(!is.na(HUC_8)) %>%
  group_by(HUC_8) %>%
  summarise(
    pnn_count = n_distinct(pnn),
    .groups = "drop")

# Join counts back to watershed polygons
NC_HUC8_map <- NC_HUC8 %>%
  left_join(
    huc8_pnn_counts,
    by = "HUC_8")

# Replace missing values
NC_HUC8_map$pnn_count[
  is.na(NC_HUC8_map$pnn_count)] <- 0

# Palette
pal_pnn2 <- colorNumeric(
  palette = "viridis",
  domain = NC_HUC8_map$pnn_count
)

# =====================================================
# UI
# =====================================================

ui <- fluidPage(
  
  titlePanel(
    "CWA Section 404 Permit Dashboard (1/4/2021 - 9/30/2025)"
  ),
  
  tabsetPanel(
    
    # -------------------------------------
    # TAB 1: PERMITS BY WATERSHED
    # -------------------------------------
    
    tabPanel(
      "Permits by Watershed",
      leafletOutput(
        "watershed_map",
        height = "800px")
      ),
    
    # -------------------------------------
    # TAB 2: IMPACT & MITIGATION METRICS
    # -------------------------------------
    
    tabPanel(
      "Impact & Mitigation Metrics",
      leafletOutput(
        "impact_map",
        height = "800px")
    ),
    
    # -------------------------------------
    # TAB 3: PERMIT CHARACTERISTICS
    # -------------------------------------
    
    tabPanel(
      "Permit Characteristics",
      sidebarLayout(
        sidebarPanel(
          
          selectInput(
            inputId = "bar_var",
            label = "Category",
            choices = c(
              "action",
              "action_type",
              "permit_authority",
              "mitigation_permittee_rsp"))
          ),
        
        mainPanel(
          plotOutput(
            outputId = "permit_bar",
            height = "700px"))
        )),
    
    # -------------------------------------
    # TAB 4: IMPACT CHARACTERISTICS
    # -------------------------------------
    
    tabPanel(
      "Impact Characteristics",
      sidebarLayout(
        sidebarPanel(
          
          selectInput(
            inputId = "impact_var",
            label = "Impact Category",
            choices = c(
              "impact_duration",
              "impact_perm_loss",
              "impact_type"))
        ),
        
        mainPanel(
          plotOutput(
            outputId = "impact_bar",
            height = "700px"))
      )),
  # -------------------------------------
  # TAB 5: PERMIT LOCATIONS BY ACTION TYPE
  # -------------------------------------
  tabPanel(
    
    "Permit Locations by Action Type",
    
    leafletOutput(
      "action_type_map",
      height = "800px")),
  
  # -------------------------------------
  # TAB 6: PERMIT LOCATIONS BY PNN TYPE
  # -------------------------------------
  tabPanel(
    
    "Permit Locations by Impact Type",
    
    leafletOutput(
      "pnn_type_map",
      height = "800px")),
  
  # -------------------------------------
  # TAB 7: PERMIT IMPACT TYPE BY WATERSHED
  # -------------------------------------
  tabPanel(
    "PNN Impacts by Watershed",
    
    fluidRow(
      column(
        width = 8,
        leafletOutput(
          "huc8_pnn_map",
          height = 750)),
      column(
        width = 4,
        plotlyOutput(
          "huc8_bar_chart",
          height = 450),
        br(),
        plotlyOutput(
          "huc8_year_chart",
          height = 250))))
))

# =====================================================
# SERVER
# =====================================================

server <- function(input, output) {
  
  # --------------------------------------------------
  # TAB 1: PERMITS BY WATERSHED
  # --------------------------------------------------
  
  output$watershed_map <- renderLeaflet({
    
    leaflet(NC_HUC8_projects) %>%
      addTiles() %>%
      
      addPolygons(
        fillColor = ~pal_huc8(n_projects),
        fillOpacity = 0.8,
        color = "black",
        weight = 1,
        popup = ~paste(
          "<b>HUC8:</b>", HUC_8,
          "<br><b>Unique Projects:</b>", n_projects
        )
      ) %>%
      
      addLegend(
        position = "topright",
        pal = pal_huc8,
        values = ~n_projects,
        title = "Unique DA Numbers"
      ) %>%
      
      fitBounds(
        lng1 = -84.4,
        lat1 = 33.8,
        lng2 = -75.4,
        lat2 = 36.6
      )
    
  })
  
  
  # --------------------------------------------------
  # TAB 2: IMPACT & MITIGATION METRICS
  # --------------------------------------------------
  
  output$impact_map <- renderLeaflet({
    
    leaflet() %>%
      addProviderTiles(providers$OpenStreetMap) %>%
      
      addPolygons(
        data = NC_HUC8,
        fillColor = "grey",
        fillOpacity = 0.1,
        color = "black",
        weight = 1,
        group = "HUC8 Boundaries"
      ) %>%
      
      addCircleMarkers(
        data = NC_proj %>%
          filter(!is.na(auth_fill_acres),
                 auth_fill_acres > 0),
        
        radius = ~pmax(2, sqrt(auth_fill_acres)),
        fillColor = ~pal_acres(auth_fill_acres),
        color = ~pal_acres(auth_fill_acres),
        fillOpacity = 0.5,
        weight = 1,
        
        group = "Authorized Fill Acres",
        
        popup = ~paste(
          "<b>DA Number:</b>", da_number,
          "<br><b>Authorized Fill Acres:</b>",
          round(auth_fill_acres, 2)
        )
      ) %>%
      
      addCircleMarkers(
        data = NC_proj %>%
          filter(!is.na(auth_struc_acres),
                 auth_struc_acres > 0),
        
        radius = ~pmax(2, sqrt(auth_struc_acres)),
        fillColor = ~pal_acres(auth_struc_acres),
        color = ~pal_acres(auth_struc_acres),
        fillOpacity = 0.5,
        weight = 1,
        
        group = "Authorized Structure Acres"
      ) %>%
      
      addCircleMarkers(
        data = NC_proj %>%
          filter(!is.na(auth_drg_fill_acres),
                 auth_drg_fill_acres > 0),
        
        radius = ~pmax(2, sqrt(auth_drg_fill_acres)),
        fillColor = ~pal_acres(auth_drg_fill_acres),
        color = ~pal_acres(auth_drg_fill_acres),
        fillOpacity = 0.5,
        weight = 1,
        
        group = "Authorized Dredge Fill Acres"
      ) %>%
      
      addCircleMarkers(
        data = NC_proj %>%
          filter(!is.na(auth_drg_remvl_acres),
                 auth_drg_remvl_acres > 0),
        
        radius = ~pmax(2, sqrt(auth_drg_remvl_acres)),
        fillColor = ~pal_acres(auth_drg_remvl_acres),
        color = ~pal_acres(auth_drg_remvl_acres),
        fillOpacity = 0.5,
        weight = 1,
        
        group = "Authorized Dredge Removal Acres"
      ) %>%
      
      addCircleMarkers(
        data = NC_proj %>%
          filter(!is.na(mit_req_acres),
                 mit_req_acres > 0),
        
        radius = ~pmax(2, sqrt(mit_req_acres)),
        fillColor = ~pal_acres(mit_req_acres),
        color = ~pal_acres(mit_req_acres),
        fillOpacity = 0.5,
        weight = 1,
        
        group = "Mitigation Required Acres"
      ) %>%
      
      addLayersControl(
        overlayGroups = c(
          "HUC8 Boundaries",
          "Authorized Fill Acres",
          "Authorized Structure Acres",
          "Authorized Dredge Fill Acres",
          "Authorized Dredge Removal Acres",
          "Mitigation Required Acres"
        ),
        options = layersControlOptions(
          collapsed = FALSE
        )
      ) %>%
      
      hideGroup(
        c(
          "Authorized Structure Acres",
          "Authorized Dredge Fill Acres",
          "Authorized Dredge Removal Acres",
          "Mitigation Required Acres"
        )
      ) %>%
      
      addLegend(
        position = "topright",
        pal = pal_acres,
        values = all_acres,
        title = "Acres"
      ) %>%
      
      fitBounds(
        lng1 = -84.4,
        lat1 = 33.8,
        lng2 = -75.4,
        lat2 = 36.6
      )
    
  })
  
  
  # --------------------------------------------------
  # TAB 3: PERMIT CHARACTERISTICS
  # --------------------------------------------------
  
  output$permit_bar <- renderPlot({
    
    plot_data <- NC_proj %>%
      st_drop_geometry() %>%
      select(
        fiscal_year,
        da_number,
        all_of(input$bar_var)
      ) %>%
      filter(
        !is.na(.data[[input$bar_var]])
      )
    
    if (input$bar_var == "mitigation_permittee_rsp") {
      
      plot_data <- plot_data %>%
        filter(
          toupper(mitigation_permittee_rsp) != "NONE"
        )
      
    }
    
    plot_data <- plot_data %>%
      distinct(
        fiscal_year,
        da_number,
        .data[[input$bar_var]]
      ) %>%
      count(
        fiscal_year,
        .data[[input$bar_var]],
        name = "n"
      )
    
    ggplot(
      plot_data,
      aes(
        x = factor(fiscal_year),
        y = n,
        fill = .data[[input$bar_var]]
      )
    ) +
      geom_col() +
      
      geom_text(
        aes(label = n),
        position = position_stack(vjust = 0.5),
        color = "white",
        size = 3
      ) +
      
      scale_fill_viridis_d() +
      
      labs(
        title = paste(
          "Unique Projects by",
          gsub("_", " ", input$bar_var)
        ),
        subtitle = "Unique DA Numbers by Fiscal Year",
        x = "Fiscal Year",
        y = "Unique DA Numbers",
        fill = NULL
      ) +
      
      theme_minimal(base_size = 14) +
      
      theme(
        legend.position = "bottom",
        plot.title = element_text(face = "bold")
      )
    
  })
  
  
  # --------------------------------------------------
  # TAB 4: IMPACT CHARACTERISTICS
  # --------------------------------------------------
  
  output$impact_bar <- renderPlot({
    
    impact_data <- NC_proj %>%
      st_drop_geometry() %>%
      select(
        da_number,
        all_of(input$impact_var)
      ) %>%
      filter(
        !is.na(.data[[input$impact_var]])
      ) %>%
      distinct(
        da_number,
        .data[[input$impact_var]]
      ) %>%
      count(
        .data[[input$impact_var]],
        name = "n"
      ) %>%
      mutate(
        pct = round(
          n / sum(n) * 100,
          1
        )
      ) %>%
      arrange(desc(n))
    
    ggplot(
      impact_data,
      aes(
        x = reorder(
          .data[[input$impact_var]],
          n
        ),
        y = n,
        fill = n
      )
    ) +
      
      geom_col() +
      
      geom_text(
        aes(
          label = paste0(
            n,
            " (",
            pct,
            "%)"
          )
        ),
        hjust = -0.1,
        size = 4
      ) +
      
      coord_flip() +
      
      scale_fill_viridis_c(
        option = "viridis",
        guide = "none"
      ) +
      
      expand_limits(
        y = max(impact_data$n) * 1.15
      ) +
      
      labs(
        title = paste(
          "Distribution of",
          gsub("_", " ", input$impact_var)
        ),
        subtitle = "Unique DA Numbers",
        x = NULL,
        y = "Unique DA Numbers"
      ) +
      
      theme_minimal(base_size = 14) +
      
      theme(
        panel.grid.major.y = element_blank(),
        plot.title = element_text(face = "bold")
      )
    
  })
  
  # --------------------------------------------------
  # TAB 5: PERMIT LOCATIONS BY ACTION TYPE
  # --------------------------------------------------
  
  output$action_type_map <- renderLeaflet({
    
    leaflet(data = NC_proj) %>%
      
      addProviderTiles(
        providers$OpenStreetMap
      ) %>%
      
      addPolygons(
        data = NC_HUC8,
        fillColor = "grey",
        fillOpacity = 0.1,
        color = "black",
        weight = 1
      ) %>%
      
      addCircleMarkers(
        color = ~pal_action(action_type),
        fillColor = ~pal_action(action_type),
        radius = 2,
        stroke = FALSE,
        fillOpacity = 0.5,
        
        popup = ~paste0(
          "<b>Action Type:</b> ", action_type,
          "<br><b>DA Number:</b> ", da_number,
          "<br><b>Fiscal Year:</b> ", fiscal_year
        )
      ) %>%
      
      addLegend(
        position = "topright",
        pal = pal_action,
        values = NC_proj$action_type,
        title = "Action Type"
      ) %>%
      
      fitBounds(
        lng1 = -84.4,
        lat1 = 33.8,
        lng2 = -75.4,
        lat2 = 36.6
      )
    
  })
  
  
  # --------------------------------------------------
  # TAB 6: PERMIT LOCATIONS BY IMPACT TYPE
  # --------------------------------------------------
  
  output$pnn_type_map <- renderLeaflet({
    
    leaflet() %>%
      
      addProviderTiles(
        providers$OpenStreetMap
      ) %>%
      
      addPolygons(
        data = NC_HUC8,
        fillColor = "grey",
        fillOpacity = 0.1,
        color = "black",
        weight = 1
      ) %>%
      
      addCircleMarkers(
        data = NC_proj,
        color = ~pal_pnn(pnn),
        fillColor = ~pal_pnn(pnn),
        radius = 2,
        stroke = FALSE,
        fillOpacity = 0.5,
        
        popup = ~paste0(
          "<b>Impact Type:</b> ", pnn,
          "<br><b>DA Number:</b> ", da_number,
          "<br><b>Fiscal Year:</b> ", fiscal_year
        )
      ) %>%
      
      addLegend(
        position = "topright",
        pal = pal_pnn,
        values = ~pnn,
        data = NC_proj,
        title = "Impact Type (PNN)"
      ) %>%
      
      fitBounds(
        lng1 = -84.4,
        lat1 = 33.8,
        lng2 = -75.4,
        lat2 = 36.6
      )
    
  })
  
  
  # --------------------------------------------------
  # TAB 7: PERMIT IMPACT TYPE BY WATERSHED
  # --------------------------------------------------
  
  output$huc8_pnn_map <- renderLeaflet({
    
    leaflet(NC_HUC8_map) %>%
      
      addProviderTiles(
        providers$OpenStreetMap) %>%
      
      addPolygons(
        layerId = ~HUC_8,
        fillColor = ~pal_pnn2(pnn_count),
        fillOpacity = 0.8,
        color = "black",
        weight = 1,
        popup = ~paste(
          "<b>HUC8:</b>", HUC_8,
          "<br><b>Number of PNNs:</b>", pnn_count),
        highlightOptions = highlightOptions(
          weight = 3,
          color = "yellow",
          bringToFront = TRUE)) %>%
      
      addLegend(
        position = "topright",
        pal = pal_pnn2,
        values = ~pnn_count,
        title = "Unique PNN Impact Count")
  })
  
  # Reactive HUC8 Selection
  selected_huc8 <- reactiveVal(
    NC_HUC8_map$HUC_8[
      which.max(NC_HUC8_map$pnn_count)])
  observeEvent(input$huc8_pnn_map_shape_click, {
    click <- input$huc8_pnn_map_shape_click
    req(click$id)
    selected_huc8(click$id)
  })
  
  # Impact Type Summary
  impact_summary <- reactive({
    req(selected_huc8())
    permits_huc8 %>%
      st_drop_geometry() %>%
      filter(
        as.character(HUC_8) == as.character(selected_huc8()),
        !is.na(impact_type)
      ) %>%
      group_by(impact_type) %>%
      summarise(
        PNNs = n_distinct(pnn),
        .groups = "drop"
      ) %>%
      arrange(desc(PNNs))
  })
  
  # Bar Chart
  output$huc8_bar_chart <- renderPlotly({
    dat <- impact_summary()
    req(nrow(dat) > 0)
    
    plot_ly(
      data = dat,
      x = ~reorder(impact_type, PNNs),
      y = ~PNNs,
      type = "bar") %>%
      
      layout(
        title = paste(
          "Impact Types by HUC8",
          selected_huc8()),
        xaxis = list(
          title = "Impact Type",
          tickangle = -45),
        yaxis = list(
          title = "Unique PNN Count"))
    
  })
  
  # Fiscal Year Summary
  year_summary <- reactive({
    req(selected_huc8())
    permits_huc8 %>%
      st_drop_geometry() %>%
      filter(
        as.character(HUC_8) == as.character(selected_huc8()),
        !is.na(fiscal_year)
      ) %>%
      group_by(fiscal_year) %>%
      summarise(
        PNNs = n_distinct(pnn),
        .groups = "drop"
      ) %>%
      arrange(fiscal_year)
  })
  
  # Fiscal Year Chart
  output$huc8_year_chart <- renderPlotly({
    dat <- year_summary()
    req(nrow(dat) > 0)
    plot_ly(
      data = dat,
      x = ~fiscal_year,
      y = ~PNNs,
      type = "bar") %>%
      
      layout(
        title = paste(
          "Unique PNNs by Fiscal Year",
          selected_huc8()),
        xaxis = list(
          title = "Fiscal Year"),
        yaxis = list(
          title = "Unique PNN Count"))
  })
  
}
    
# =====================================================
# RUN APP
# =====================================================

shinyApp(
  ui = ui,
  server = server
)