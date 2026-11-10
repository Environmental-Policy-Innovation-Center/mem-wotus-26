server = function(session, input, output){
  
  # --------------------------------------------------
  # PAGE 1 (Row 1): CREATE FILTER DATA
  # --------------------------------------------------
  observe({
    
    updateSelectizeInput(
      session,
      "fiscal_year_filter",
      choices = sort(unique(NC_proj$fiscal_year)),
      selected = sort(unique(NC_proj$fiscal_year)),
      server = TRUE)
    
    updateSelectizeInput(
      session,
      "action_type_filter",
      choices = sort(unique(NC_proj$action_type)),
      selected = sort(unique(NC_proj$action_type)),
      server = TRUE)
    
    updateSelectizeInput(
      session,
      "mitigation_filter",
      choices = sort(unique(NC_proj$compensatory_mitigation_rqd)),
      selected = sort(unique(NC_proj$compensatory_mitigation_rqd)),
      server = TRUE)
    
    updateSelectizeInput(
      session,
      "authority_filter",
      choices = sort(unique(NC_proj$permit_authority)),
      selected = sort(unique(NC_proj$permit_authority)),
      server = TRUE)
    
    updateSelectizeInput(
      session,
      "impact_filter",
      choices = sort(unique(NC_proj$impact_type)),
      selected = sort(unique(NC_proj$impact_type)),
      server = TRUE)
    
    updateSelectizeInput(
      session,
      "wetland_filter",
      choices = sort(unique(NC_proj$resource_type)),
      selected = sort(unique(NC_proj$resource_type)),
      server = TRUE)
  })
  
  # --------------------------------------------------
  # PAGE 1 (Row 1): APPLY FILTER DATA
  # --------------------------------------------------
  filtered_data <- reactive({
    dat <- NC_proj
    if (!is.null(input$fiscal_year_filter) &&
        length(input$fiscal_year_filter) > 0) {
      dat <- dat %>%
        filter(fiscal_year %in% input$fiscal_year_filter)}
    
    if (!is.null(input$action_type_filter) &&
        length(input$action_type_filter) > 0) {
      dat <- dat %>%
        filter(action_type %in% input$action_type_filter)}
    
    if (!is.null(input$mitigation_filter) &&
        length(input$mitigation_filter) > 0) {
      dat <- dat %>%
        filter(compensatory_mitigation_rqd %in% input$mitigation_filter)}
    
    if (!is.null(input$authority_filter) &&
        length(input$authority_filter) > 0) {
      dat <- dat %>%
        filter(permit_authority %in% input$authority_filter)}
    
    if (!is.null(input$impact_filter) &&
        length(input$impact_filter) > 0) {
      dat <- dat %>%
        filter(impact_type %in% input$impact_filter)}
    
    if (!is.null(input$wetland_filter) &&
        length(input$wetland_filter) > 0) {
      dat <- dat %>%
        filter(resource_type %in% input$wetland_filter)}

    dat
    
  })
  
  # --------------------------------------------------
  # PAGE 1 (Row 2): COUNT VALUE BOXES
  # --------------------------------------------------
  output$box1 <- renderValueBox({
    valueBox(
      value = n_distinct(filtered_data()$da_number),
      subtitle = "Permits",
      icon = icon("file-alt"),
      color = "aqua")
  })
  
  output$box2 <- renderValueBox({
    valueBox(
      value = n_distinct(filtered_data()$action_type),
      subtitle = "Action Types",
      icon = icon("tasks"),
      color = "orange")
  })
  
  output$box3 <- renderValueBox({
    valueBox(
      value = n_distinct(filtered_data()$resource_type),
      subtitle = "Wetland Types",
      icon = icon("leaf"),
      color = "green")
  })
  
  output$box4 <- renderValueBox({
    valueBox(
      value = n_distinct(filtered_data()$impact_type),
      subtitle = "Impact Types",
      icon = icon("water"),
      color = "red")
  })
  
  
  # --------------------------------------------------
  # PAGE 1 (Row 3): NWPs MAP
  # --------------------------------------------------
  output$action_type_map <- renderLeaflet({
    dat <- filtered_data()
    leaflet(dat) %>%
      addProviderTiles(providers$Esri.WorldGrayCanvas) %>%
      addPolygons(data = NC_HUC8,
        fillColor = "grey",
        fillOpacity = 0.1,
        color = "black",
        weight = 1) %>%
      addCircleMarkers(
        radius = 3,
        stroke = FALSE,
        fillOpacity = 0.7,
        color = ~pal_action(action_type),
        fillColor = ~pal_action(action_type),
        
        popup = ~paste0(
          "<b>DA Number:</b> ", da_number,
          "<br><b>Action Type:</b> ", action_type,
          "<br><b>Fiscal Year:</b> ", fiscal_year,
          "<br><b>Wetland Type:</b> ", resource_type)) %>%
      
      addLegend(
        position = "topright",
        pal = pal_action,
        values = dat$action_type,
        title = "Action Type") %>%
      
      fitBounds(
        lng1 = -84.4,
        lat1 = 33.8,
        lng2 = -75.4,
        lat2 = 36.6)
  })
  
  # --------------------------------------------------
  # PAGE 1 (Row 4): NWP CHARTS
  # --------------------------------------------------
  # Action Type Pie Chart
  output$action_type_pie <- renderHighchart({
    pie_data <- filtered_data()  %>%
      count(action_type)
    hchart(pie_data,"pie",
      hcaes(name = action_type, y = n)) %>%
      hc_exporting(enabled = TRUE) %>%
      hc_add_theme(hc_theme_smpl()) %>%
      hc_plotOptions(
        pie = list(
          colorByPoint = TRUE,
          showInLegend = TRUE,
          innerSize = "60%",
          size = 250,
          dataLabels = list(
            enabled = TRUE,
            format = "{point.name}: {point.percentage:.1f}%")))
  })
  # Wetland Type Bar Chart
  output$wetland_bar <- renderHighchart({
    bar_data <- filtered_data() %>%
      count(resource_type)
    hchart(bar_data,"column",
      hcaes(x = resource_type,y = n)) %>%
      hc_yAxis(
        title = list(text = "Count")) %>%
      hc_xAxis(
        title = list(text = "")) %>%
      hc_exporting(enabled = TRUE)
  })
  
  # -------------------------------------
  # PAGE 1 (Row 5): NWP TABLE
  # ------------------------------------- 
  output$permit_table <- renderDT({
    filtered_data() %>%
      st_drop_geometry()},
  options = list(
    pageLength = 10,
    scrollX = TRUE
  ))
  
  output$download_filtered <- downloadHandler(
    filename = function() {
      paste0(
        "NWP_Filtered_",
        Sys.Date(),
        ".csv")},
    content = function(file) {
      write.csv(
        filtered_data() %>%
          st_drop_geometry(),
        file,
        row.names = FALSE)})


  # --------------------------------------------------
  # PAGE 2 (Row 1): CUMULATIVE IMPACTS BY WATERSHED MAP
  # --------------------------------------------------
output$huc8_pnn_map <- renderLeaflet({
  
  leaflet(NC_HUC8_map) %>%
    
    addProviderTiles(
      providers$Esri.WorldGrayCanvas) %>%
    
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

selected_huc8_data <- reactive({
  req(selected_huc8())
  permits_huc8 %>%
    filter(
      as.character(HUC_8) ==
        as.character(selected_huc8()))
})

# Impact Type Summary
impact_summary <- reactive({
  selected_huc8_data() %>%
    st_drop_geometry() %>%
    filter(!is.na(impact_type)) %>%
    group_by(impact_type) %>%
    summarise(
      PNNs = n_distinct(pnn),
      .groups = "drop") %>%
    arrange(desc(PNNs))
})

# Action Type Summary
action_summary <- reactive({
  selected_huc8_data() %>%
    st_drop_geometry() %>%
    filter(!is.na(action_type)) %>%
    group_by(action_type) %>%
    summarise(
      PNNs = n_distinct(pnn),
      .groups = "drop") %>%
    arrange(desc(PNNs))
})


  # --------------------------------------------------
  # PAGE 2 (Row 2): HUC8 COUNT VALUE BOXES
  # --------------------------------------------------
output$box5 <- renderValueBox({
  valueBox(
    value = n_distinct(selected_huc8_data()$da_number),
    subtitle = "Permits",
    icon = icon("file-alt"),
    color = "aqua")
})

output$box6 <- renderValueBox({
  valueBox(
    value = n_distinct(selected_huc8_data()$action_type),
    subtitle = "Action Types",
    icon = icon("tasks"),
    color = "orange")
})

output$box7 <- renderValueBox({
  valueBox(
    value = n_distinct(selected_huc8_data()$resource_type),
    subtitle = "Wetland Types",
    icon = icon("leaf"),
    color = "green")
})

output$box8 <- renderValueBox({
  valueBox(
    value = n_distinct(selected_huc8_data()$impact_type),
    subtitle = "Impact Types",
    icon = icon("water"),
    color = "red")
})


  # --------------------------------------------------
  # PAGE 2 (Row 3): HUC8 CHARTS
  # --------------------------------------------------
# Action Type Pie Chart
output$action_type_pie2 <- renderHighchart({
  pie_data2 <- selected_huc8_data()  %>%
    count(action_type)
  hchart(pie_data2,"pie",
         hcaes(name = action_type, y = n)) %>%
    hc_exporting(enabled = TRUE) %>%
    hc_add_theme(hc_theme_smpl()) %>%
    hc_plotOptions(
      pie = list(
        colorByPoint = TRUE,
        showInLegend = TRUE,
        innerSize = "60%",
        size = 250,
        dataLabels = list(
          enabled = TRUE,
          format = "{point.name}: {point.percentage:.1f}%")))
})
# Wetland Type Bar Chart
output$impact_bar <- renderHighchart({
  bar_data2 <- selected_huc8_data() %>%
    count(resource_type)
  hchart(bar_data2,"column",
         hcaes(x = resource_type,y = n)) %>%
    hc_yAxis(
      title = list(text = "Count")) %>%
    hc_xAxis(
      title = list(text = "")) %>%
    hc_exporting(enabled = TRUE)
})

  # -------------------------------------
  # PAGE 2 (Row 4): HUC8 TABLE
  # ------------------------------------- 
output$permit_table2 <- renderDT({
  selected_huc8_data() %>%
    st_drop_geometry()},
  options = list(
    pageLength = 25,
    scrollX = TRUE
  ))

output$download_filtered2 <- downloadHandler(
  filename = function() {
    paste0(
      "NWP_Filtered_",
      Sys.Date(),
      ".csv")},
  content = function(file) {
    write.csv(
      selected_huc8_data() %>%
        st_drop_geometry(),
      file,
      row.names = FALSE)})
  # --------------------------------------------------
  # PAGE 3: Value Boxes
  # --------------------------------------------------
# Map metric labels
metric_title <- reactive({
  
  switch(
    
    input$impact_metric,
    
    "auth_fill_acres" =
      "Authorized Fill Acres",
    
    "auth_struc_acres" =
      "Authorized Structure Acres",
    
    "auth_drg_fill_acres" =
      "Authorized Dredge Fill Acres",
    
    "auth_drg_remvl_acres" =
      "Authorized Dredge Removal Acres",
    
    "mit_req_acres" =
      "Mitigation Required Acres"
    
  )
  
})

#Reactive Dataset
impact_data <- reactive({
  
  req(input$impact_metric)
  
  NC_proj %>%
    
    filter(
      !is.na(.data[[input$impact_metric]]),
      .data[[input$impact_metric]] > 0
    )
  
})

# Value Boxes
output$box9 <- renderValueBox({
  
  valueBox(
    
    round(
      sum(
        impact_data()[[input$impact_metric]],
        na.rm = TRUE
      ),
      2
    ),
    
    "Total Acres",
    
    icon = icon("water"),
    color = "aqua"
  )
  
})

output$box10 <- renderValueBox({
  
  valueBox(
    
    n_distinct(
      impact_data()$da_number
    ),
    
    "Permits",
    
    icon = icon("file"),
    color = "orange"
  )
  
})

output$box11 <- renderValueBox({
  
  valueBox(
    
    n_distinct(
      impact_data()$NC_HUC8
    ),
    
    "HUC8 Watersheds",
    
    icon = icon("map"),
    color = "green"
  )
  
})

output$box12 <- renderValueBox({
  
  valueBox(
    
    round(
      mean(
        impact_data()[[input$impact_metric]],
        na.rm = TRUE
      ),
      2
    ),
    
    "Average Acres",
    
    icon = icon("chart-bar"),
    color = "red"
  )
  
})
# --------------------------------------------------
# PAGE 3: Impact Map
# --------------------------------------------------
pal_acres <- reactive({
  
  colorNumeric(
    palette = "viridis",
    domain = impact_data()[[input$impact_metric]],
    na.color = "transparent"
  )
  
})

output$impact_map <- renderLeaflet({
  
  dat <- impact_data()
  
  dat$metric_value <- dat[[input$impact_metric]]
  
  leaflet() %>%
    
    addProviderTiles(
      providers$Esri.WorldGrayCanvas) %>%
    
    addPolygons(
      data = NC_HUC8,
      fillColor = "grey",
      fillOpacity = 0.1,
      color = "black",
      weight = 1) %>%
    
    addCircleMarkers(
      data = dat,
      
      radius = ~pmax(2, sqrt(metric_value)),
      
      fillColor = ~pal_acres()(metric_value),
      color = ~pal_acres()(metric_value),
      
      fillOpacity = 0.5,
      weight = 1,
      
      popup = ~paste0(
        "<b>DA Number:</b> ", da_number,
        "<br><b>",
        metric_title(),
        ":</b> ",
        round(metric_value, 2))) %>%
    
    addLegend(
      position = "topright",
      pal = pal_acres(),
      values = dat$metric_value,
      title = metric_title()) %>%
    
    fitBounds(
      lng1 = -84.4,
      lat1 = 33.8,
      lng2 = -75.4,
      lat2 = 36.6
    )
  
})

# --------------------------------------------------
# PAGE 3: Permit Authority Chart
# --------------------------------------------------
output$impact_authority_chart <- renderHighchart({
  
  dat <- impact_data() %>%
    
    st_drop_geometry() %>%
    
    group_by(permit_authority) %>%
    
    summarise(
      Acres = sum(
        .data[[input$impact_metric]],
        na.rm = TRUE
      ),
      .groups = "drop"
    ) %>%
    
    arrange(desc(Acres))
  
  hchart(
    dat,
    "column",
    hcaes(
      x = permit_authority,
      y = Acres
    )
  )
  
})

# --------------------------------------------------
# PAGE 3: Impact Type Chart
# --------------------------------------------------
output$impact_type_chart <- renderHighchart({
  
  dat <- impact_data() %>%
    
    st_drop_geometry() %>%
    
    group_by(impact_type) %>%
    
    summarise(
      Acres = sum(
        .data[[input$impact_metric]],
        na.rm = TRUE
      ),
      .groups = "drop"
    )
  
  hchart(
    dat,
    "bar",
    hcaes(
      x = impact_type,
      y = Acres
    )
  )
  
})

# --------------------------------------------------
# PAGE 3: Table
# --------------------------------------------------
output$impact_table <- renderDT({
  
  impact_data() %>%
    
    st_drop_geometry() %>%
    
    select(
      da_number,
      permit_authority,
      impact_type,
      all_of(input$impact_metric)
    )
  
},
options = list(
  pageLength = 10,
  scrollX = TRUE
))

output$impact_download <- downloadHandler(
  
  filename = function() {
    
    paste0(
      input$impact_metric,
      "_",
      Sys.Date(),
      ".csv"
    )
    
  },
  
  content = function(file) {
    
    write.csv(
      
      impact_data() %>%
        st_drop_geometry(),
      
      file,
      
      row.names = FALSE
      
    )
    
  }
  
)

}
