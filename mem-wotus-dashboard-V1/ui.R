# Load R Shiny dashboard packages
library(shiny)
library(shinydashboard)
library(shinydashboardPlus)

library(leaflet)
library(leaflet.extras)
library(plotly)
library(tidyverse)
library(highcharter)
library(highr)
library(gt)
library(sf)
library(terra)
library(here)
library(htmltools)
library(readxl)
library(DT)

# =====================================================
# DATA PREPARATION
# =====================================================

# -------------------------
# Initial Step: Read Data
# -------------------------

# Check working directory
#setwd()
#getwd()

# Load CSV file from personal desktop folder into R
clean_orm_Wilmington <- read.csv("D:/MP_Files/clean1625_Wilmington.csv")

# Filter for last wetland permit reporting period (2021-2025) to reflect updates from ORM SOP 2021
NC_post_2021 <- clean_orm_Wilmington %>%
  filter(fiscal_year %in% c(2021, 2022, 2023, 2024, 2025))

#Remove Section 10-only permits from data set
NC_post_2021 <- NC_post_2021 |>
  filter(grepl("Section 103|Section 404|Section 10/404|Section 10/103|Section 10/404/103", permit_authority))

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
    fiscal_year,
    county,
    resource_type
  )

NC_waters <- NC_post2021 %>%
  filter(!is.na(waters_longitude),
         !is.na(waters_latitude)) %>%
  st_as_sf(
    coords = c("waters_longitude", "waters_latitude"),
    crs = 4269)

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

NC_waters <- st_transform(NC_waters, 4326)

#Adjust display names of variables. Will be overridden by labs()
#var_label(NC_post_2021) -> list(original name = "new name", etc.) for more than one variable
library(labelled)
var_label(NC_post_2021$pnn) <- 
  "NWP Number"

# -------------------------
# Page 1: PERMIT LOCATIONS BY ACTION TYPE
# -------------------------
pal_action <- colorFactor(
  palette = "Set1",
  domain = sort(
    unique(
      na.omit(NC_waters$action_type))))

# -------------------------
# Page 2: PNN Type Map by Watershed
# -------------------------

# Spatially join projects to watersheds
permits_huc8 <- st_join(
  NC_waters,
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

# Create page
shinyApp = dashboardPage(
  title = "Clean Water Act Wetland Permit Cumulative Impacts",
  header = dashboardHeader(),
  sidebar = dashboardSidebar(
    collapsed = T,
    minified = T,
    sidebarMenu(
      menuItem("Wilmington District Nationwide Permits", tabName = "page1", icon = icon("map")),
      menuItem("Cumulative Impact Types", tabName = "page2", icon = icon("water")),
      menuItem("Cumulative Impact Analysis", tabName = "page3", icon = icon("house")))
  ),
  
  body = dashboardBody(
    tabItems(
# -------------------------------------
# PAGE 1
# -------------------------------------
      tabItem(
        tabName = "page1",
        
  # -------------------------------------
  # PAGE 1 (Row 1): FILTER DATA PANEL
  # -------------------------------------
        fluidRow(
          box(title = "Filters",
            width = 12,
            collapsible = TRUE,
          fluidRow(
            column(2,
                selectizeInput("fiscal_year_filter",
                  "Fiscal Year",
                  choices = NULL,
                  multiple = TRUE),
                options = list(
                  placeholder = 'Select Year(s)',
                  plugins = list('remove_button')
                )),
            column(2,
                selectizeInput("action_type_filter",
                  "Action Type",
                  choices = NULL,
                  multiple = TRUE),
                options = list(
                  placeholder = 'Select Permit(s)',
                  plugins = list('remove_button')
                )),
            column(2,
                selectizeInput("mitigation_filter",
                  "Compensatory Mitigation Required",
                  choices = NULL,
                  multiple = TRUE),
                options = list(
                  placeholder = 'Mitigation Criteria',
                  plugins = list('remove_button')
                )),
            column(2,
                selectizeInput("authority_filter",
                  "Permit Authority",
                  choices = NULL,
                  multiple = TRUE),
                options = list(
                  placeholder = 'Select Authority',
                  plugins = list('remove_button')
                )),
            column(2,
                selectizeInput("impact_filter",
                  "Impact Type",
                  choices = NULL,
                  multiple = TRUE),
                options = list(
                  placeholder = 'Select Impact(s)',
                  plugins = list('remove_button')
                )),
            column(2,
                selectizeInput("wetland_filter",
                  "Wetland Type",
                  choices = NULL,
                  multiple = TRUE),
                options = list(
                  placeholder = 'Select Wetland(s)',
                  plugins = list('remove_button')
                ))
              ))),

  # -------------------------------------
  # PAGE 1 (Row 2): COUNT VALUE BOXES
  # -------------------------------------       
        fluidRow(
          valueBoxOutput("box1", width = 3),
          valueBoxOutput("box2", width = 3),
          valueBoxOutput("box3", width = 3),
          valueBoxOutput("box4", width = 3)),

  # -------------------------------------
  # PAGE 1 (Row 3): NWPs MAP
  # ------------------------------------- 
      fluidRow(
        box(title = "NWPs Map",
            width = 12,
            collapsible = TRUE,
          leafletOutput("action_type_map",
                        height = 600))),
  # -------------------------------------
  # PAGE 1 (Row 4): NWP CHARTS
  # ------------------------------------- 
    fluidRow(
      box(title = "Action Types",
          width = 6,
          collapsible = TRUE,
        highchartOutput("action_type_pie",
                        height = 500)),
      box(title = "Wetland Types",
          width = 6,
          collapsible = TRUE,
        highchartOutput("wetland_bar",
                        height = 500))),

  # -------------------------------------
  # PAGE 1 (Row 5): NWP TABLE
  # ------------------------------------- 
    fluidRow(
      box(title = "Filtered Permit Records",
          width = 12,
          collapsible = TRUE,
        downloadButton("download_filtered",
                       "Download CSV"),
    br(),
    br(),
    DTOutput("permit_table")))

# -------------------------------------
# PAGE 2
# -------------------------------------
),
tabItem(
  tabName = "page2",
        
  # -------------------------------------
  # PAGE 2 (Row 1): CUMULATIVE IMPACTS BY WATERSHED MAP
  # -------------------------------------
        fluidRow(
          box(title = "HUC8 Watershed Impact Summary",
            width = 12,
            collapsible = TRUE,
          leafletOutput("huc8_pnn_map",
                        height = 600))),

  # -------------------------------------
  # PAGE 2 (Row 2): HUC8 COUNT VALUE BOXES
  # -------------------------------------       
        fluidRow(
          valueBoxOutput("box5", width = 3),
          valueBoxOutput("box6", width = 3),
          valueBoxOutput("box7", width = 3),
          valueBoxOutput("box8", width = 3)),

  # -------------------------------------
  # PAGE 2 (Row 3): HUC8 CHARTS
  # ------------------------------------- 
    fluidRow(
      box(title = "Action Types",
          width = 6,
          collapsible = TRUE,
        highchartOutput("action_type_pie2",
                        height = 450)),
      box(title = "Impact Type Distribution",
          width = 6,
          collapsible = TRUE,
        highchartOutput("impact_bar",
                        height = 450))),

  # -------------------------------------
  # PAGE 2 (Row 4): NWP TABLE
  # ------------------------------------- 
    fluidRow(
      box(title = "Filtered Permit Records by Selected Watershed",
          width = 12,
          collapsible = TRUE,
        downloadButton("download_filtered2",
                       "Download CSV"),
    br(),
    br(),
    DTOutput("permit_table2")))

# -------------------------------------
# PAGE 3
# -------------------------------------
),
tabItem(
  tabName = "page3",
  
  # -------------------------------------
  # PAGE 3 (Row 1): METRIC SELECTION
  # -------------------------------------
  
  fluidRow(
    
    box(
      title = "Impact & Mitigation Metric",
      width = 12,
      collapsible = TRUE,
      
      radioButtons(
        "impact_metric",
        NULL,
        
        choices = c(
          "Authorized Fill Acres" = "auth_fill_acres",
          "Authorized Structure Acres" = "auth_struc_acres",
          "Authorized Dredge Fill Acres" = "auth_drg_fill_acres",
          "Authorized Dredge Removal Acres" = "auth_drg_remvl_acres",
          "Mitigation Required Acres" = "mit_req_acres"
        ),
        
        selected = "auth_fill_acres",
        inline = TRUE
      )
    )
    
  ),
  
  # -------------------------------------
  # PAGE 3 (Row 2): VALUE BOXES
  # -------------------------------------
  
  fluidRow(
    
    valueBoxOutput("impact_box9", width = 3),
    valueBoxOutput("impact_box10", width = 3),
    valueBoxOutput("impact_box11", width = 3),
    valueBoxOutput("impact_box12", width = 3)
    
  ),
  
  # -------------------------------------
  # PAGE 3 (Row 3): MAP
  # -------------------------------------
  
  fluidRow(
    
    box(
      title = "Impact Map",
      width = 12,
      collapsible = TRUE,
      
      leafletOutput(
        "impact_map",
        height = 650
      )
    )
    
  ),
  
  # -------------------------------------
  # PAGE 3 (Row 4): CHARTS
  # -------------------------------------
  
  fluidRow(
    
    box(
      title = "Top Permit Authorities",
      width = 6,
      collapsible = TRUE,
      
      highchartOutput(
        "impact_authority_chart",
        height = 450
      )
    ),
    
    box(
      title = "Impact Type Distribution",
      width = 6,
      collapsible = TRUE,
      
      highchartOutput(
        "impact_type_chart",
        height = 450
      )
    )
    
  ),
  
  # -------------------------------------
  # PAGE 3 (Row 5): TABLE
  # -------------------------------------
  
  fluidRow(
    
    box(
      title = "Permit Records",
      width = 12,
      collapsible = TRUE,
      
      downloadButton(
        "impact_download",
        "Download CSV"
      ),
      
      br(),
      br(),
      
      DTOutput("impact_table")
    )
    
  )
  
)
  
)))
