library(shiny)
library(leaflet)
library(sf)
library(dplyr)

# =====================================================
# DATA PREPARATION
# =====================================================

# -------------------------
# TAB 1: Watershed Summary
# -------------------------

permits_huc8 <- st_join(
  NC_proj,
  NC_HUC8,
  left = FALSE
)

huc8_counts <- permits_huc8 %>%
  st_drop_geometry() %>%
  group_by(HUC_8) %>%
  summarise(
    n_projects = n_distinct(da_number),
    .groups = "drop"
  )

NC_HUC8_map <- NC_HUC8 %>%
  left_join(
    huc8_counts,
    by = "HUC_8"
  )

NC_HUC8_map$n_projects[
  is.na(NC_HUC8_map$n_projects)
] <- 0

pal_huc8 <- colorNumeric(
  palette = "YlOrRd",
  domain = NC_HUC8_map$n_projects
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
  domain = NC_proj$action_type
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
      height = "800px"))
  )
)

# =====================================================
# SERVER
# =====================================================

server <- function(input, output) {
  
  # --------------------------------------------------
  # TAB 1: PERMITS BY WATERSHED
  # --------------------------------------------------
  
  output$watershed_map <- renderLeaflet({
    
    leaflet(NC_HUC8_map) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      
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
        position = "bottomright",
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
      addProviderTiles(providers$CartoDB.Positron) %>%
      
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
        position = "bottomright",
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
        providers$CartoDB.Positron
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
        position = "bottomright",
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
  
}
    
# =====================================================
# RUN APP
# =====================================================

shinyApp(
  ui = ui,
  server = server
)