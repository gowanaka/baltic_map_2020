# libraries
library(shiny)
library(leaflet)
library(htmlwidgets)
library(tidyverse)

# load data
ices_temp_df <- read_csv("data/ices_temp.csv")
ices_psal_df <- read_csv("data/ices_psal.csv")
ices_cphl_df <- read_csv("data/ices_cphl.csv")
ices_doxy_df <- read_csv("data/ices_doxy.csv")
# download
ices_download <- read_csv("data/ices_tot.csv")


# color palettes
temp_pal <- colorNumeric("RdYlBu", domain = ices_temp_df$mean_temp, reverse = TRUE, na.color = "#0c0c0c")
temp_pal_legend <- colorNumeric("RdYlBu", domain = ices_temp_df$mean_temp, reverse = FALSE, na.color = "#0c0c0c")

psal_pal <- colorNumeric("Reds", domain = ices_psal_df$mean_psal, reverse = FALSE, na.color = "#0c0c0c")
psal_pal_legend <- colorNumeric("Reds", domain = ices_psal_df$mean_psal, reverse = TRUE, na.color = "#0c0c0c")

cphl_pal <- colorNumeric("Greens", domain = ices_cphl_df$mean_cphl, reverse = FALSE, na.color = "#0c0c0c")
cphl_pal_legend <- colorNumeric("Greens", domain = ices_cphl_df$mean_cphl, reverse = TRUE, na.color = "#0c0c0c")

doxy_pal <- colorNumeric("Blues", domain = ices_doxy_df$mean_doxy, reverse = FALSE, na.color = "#0c0c0c")
doxy_pal_legend <- colorNumeric("Blues", domain = ices_doxy_df$mean_doxy, reverse = TRUE, na.color = "#0c0c0c")

# about
about_text <- "This Map App displays temperatur, salinity, chlorophyll a and oxygen levels from measurements of oceanographic stations in the Baltic Sea averaged for the year 2019. The dataset was downloaded from the ices.dk website on 16/12/2020. Users can filter a range of stations via slider input and are also able to download this filtered data in CSV format."

ui <- bootstrapPage(
  tags$style("
        #controls {
          background-color: #FFFFFF;
          opacity: 0.3;
          padding-right: 10px;
          padding-left: 10px;
          padding-bottom: 10px;
        }
        #controls:hover{
          opacity: 0.9;
        }
        a {
        color: blue;
        }
               "),
  theme = shinythemes::shinytheme('simplex'),
  titlePanel("ICES Sample Map"),
  h3("By", strong("SR Brunner")),
  h4("27/12/2020"),
  leaflet::leafletOutput('baltic_map', height = 600),
  absolutePanel(id = "controls", bottom = 0, left = 10, draggable = FALSE, fixed = TRUE,
                h4("Filters"),
                sliderInput('temp_ctrl', "Filter Temperature [°C]", 0, 25, value = c(0,25)),
                sliderInput('psal_ctrl', "Filter Salinity [psu]", 0, 30, value = c(0,30)),
                sliderInput('cphl_ctrl', "Filter Chlorophyll A Levels [ug/l]", 0, 90, value = c(0,90)),
                sliderInput('doxy_ctrl', "Filter Oxygen Levels [ml/l]", 0, 11, value = c(0,11)),
                checkboxInput("include_na", "Include missing data in download file?", value = TRUE),
                textOutput("download_text"), br(),
                downloadButton("download_data", "Download filtered data"),
                actionButton("reset", "Reset"),
                actionButton("about", "About")
  ),
  uiOutput("link", align = "right"),
  h5("remy.data@gmx.ch  ", align = 'right')
)

server <- function (input, output, session) {
  
  # about section
  observeEvent(input$about, {
    showModal(modalDialog(about_text))
  })
  
  # link to kaggle
  url <- a("SR Brunner", href="https://www.kaggle.com/reminho")
  output$link <- renderUI({
    tagList("A work by", url)
  })
  
  # reactives
  temp_react <- reactive({
    ices_temp_df %>%
      filter(mean_temp >= input$temp_ctrl[1] & mean_temp <= input$temp_ctrl[2])
  })
  
  psal_react <- reactive({
    ices_psal_df %>%
      filter(mean_psal >= input$psal_ctrl[1] & mean_psal <= input$psal_ctrl[2])
  })
  
  cphl_react <- reactive({
    ices_cphl_df %>%
      filter(mean_cphl >= input$cphl_ctrl[1] & mean_cphl <= input$cphl_ctrl[2])
  })
  
  doxy_react <- reactive({
    ices_doxy_df %>%
      filter(mean_doxy >= input$doxy_ctrl[1] & mean_doxy <= input$doxy_ctrl[2])
  })
  
  download_react <- reactive({
    ices_download %>%
      filter(mean_temp >= input$temp_ctrl[1] & mean_temp <= input$temp_ctrl[2]) %>%
      filter(mean_psal >= input$psal_ctrl[1] & mean_psal <= input$psal_ctrl[2]) %>%
      filter(mean_cphl >= input$cphl_ctrl[1] & mean_cphl <= input$cphl_ctrl[2]) %>%
      filter(mean_doxy >= input$doxy_ctrl[1] & mean_doxy <= input$doxy_ctrl[2])
  })
  
  download_react_with_na <- reactive({
    ices_download %>%
      filter(is.na(mean_temp) | mean_temp >= input$temp_ctrl[1] & mean_temp <= input$temp_ctrl[2]) %>%
      filter(is.na(mean_psal) | mean_psal >= input$psal_ctrl[1] & mean_psal <= input$psal_ctrl[2]) %>%
      filter(is.na(mean_cphl) | mean_cphl >= input$cphl_ctrl[1] & mean_cphl <= input$cphl_ctrl[2]) %>%
      filter(is.na(mean_doxy) | mean_doxy >= input$doxy_ctrl[1] & mean_doxy <= input$doxy_ctrl[2])
  })

  # map output
  output$baltic_map <- leaflet::renderLeaflet({

    leaflet(height = '100%',
            options = leafletOptions(minZoom = 4,
                                     dragging = TRUE)) %>%
      setView(lng = 19.8633, lat = 58.48803, zoom = 5) %>%
      setMaxBounds(lng1 = max(ices_temp_df$lon) + 5,
                   lat1 = max(ices_temp_df$lat) + 5,
                   lng2 = min(ices_temp_df$lon) - 5,
                   lat2 = min(ices_temp_df$lat) - 5) %>%
      addTiles(group = "OSM") %>%
      addProviderTiles(provider = "CartoDB", group = "CartoDB") %>%
      addProviderTiles(provider = "Esri", group = "Esri") %>%
      addCircleMarkers(data = ices_temp_df, 
                       lng = ices_temp_df$lon, 
                       lat = ices_temp_df$lat,
                       group = "Temperature",
                       radius = 3,
                       color = ~temp_pal(mean_temp),
                       popup = ~paste0("Station number: ", 
                                       Station, "<br/>Mean temperature: ", 
                                       round(mean_temp, 2), "°C")) %>%
      addCircleMarkers(data = ices_psal_df, 
                       lng = ices_psal_df$lon, 
                       lat = ices_psal_df$lat,
                       group = "Salinity",
                       radius = 3,
                       color = ~psal_pal(mean_psal),
                       popup = ~paste0("Station number: ", 
                                       Station, "<br/>Mean salinity: ", 
                                       round(mean_psal, 2), "psu")) %>%
      addCircleMarkers(data = ices_cphl_df, 
                       lng = ices_cphl_df$lon, 
                       lat = ices_cphl_df$lat,
                       group = "Chlorophyll A Levels",
                       radius = 3,
                       color = ~cphl_pal(mean_cphl),
                       popup = ~paste0("Station number: ", 
                                       Station, "<br/>Mean chorophyll a: ", 
                                       round(mean_cphl, 2), "ug/l")) %>%
      addCircleMarkers(data = ices_doxy_df,
                       lng = ices_doxy_df$lon,
                       lat = ices_doxy_df$lat,
                       group = "Oxygen Levels",
                       radius = 3,
                       color = ~doxy_pal(mean_doxy),
                       popup = ~paste0("Station number: ",
                                       Station, "<br/>Mean oxygen levels: ",
                                       round(mean_doxy, 2), "ml/l")) %>%
      addLegend(data = ices_temp_df, 
                values = rev(seq(min(ices_temp_df$mean_temp), max(ices_temp_df$mean_temp), 5)),
                title = "[°C]",
                group = "Temperature",
                position = "bottomright",
                pal = temp_pal_legend,
                labFormat = labelFormat(transform = function(x) sort(x, decreasing = TRUE))) %>%
      addLegend(data = ices_psal_df,
                values = seq(min(ices_psal_df$mean_psal), max(ices_psal_df$mean_psal), 5),
                title = "[psu]",
                group = "Salinity",
                position = "bottomright",
                pal = psal_pal_legend,
                labFormat = labelFormat(transform = function(x) sort(x, decreasing = TRUE))) %>%
      addLegend(data = ices_cphl_df, 
                values = seq(min(ices_cphl_df$mean_cphl), max(ices_cphl_df$mean_cphl), 10),
                title = "[ug/l]",
                group = "Chlorophyll A Levels",
                position = "bottomright",
                pal = cphl_pal_legend,
                labFormat = labelFormat(transform = function(x) sort(x, decreasing = TRUE))) %>%
      addLegend(data = ices_doxy_df, 
                values = seq(min(ices_doxy_df$mean_doxy), max(ices_doxy_df$mean_doxy), 2),
                title = "[ul/l]",
                group = "Oxygen Levels",
                position = "bottomright",
                pal = doxy_pal_legend,
                labFormat = labelFormat(transform = function(x) sort(x, decreasing = TRUE))) %>%
      addLayersControl(baseGroups = c("OSM", "CartoDB", "Esri"),
                       overlayGroups = c("Temperature", "Salinity", "Chlorophyll A Levels", "Oxygen Levels"),
                       options = layersControlOptions(collapsed = FALSE)) %>%
      hideGroup(group = c("Temperature", "Chlorophyll A Levels", "Oxygen Levels")) %>%
      htmlwidgets::onRender(("
            function() {
              $('.leaflet-control-layers-list').prepend('<label style=\"text-align:center\">Choose Base Layer</label>');
              $('.leaflet-control-layers-overlays').prepend('<label style=\"text-align:center\">Choose Data Layer</label>');
            }"))
  })
  
  # observers
  observeEvent(input$psal_ctrl, {

    ices_psal_dat <- psal_react()

    leafletProxy("baltic_map", session) %>%
      clearGroup(group = "Salinity") %>%
      addCircleMarkers(data = ices_psal_dat, 
                       lng = ices_psal_dat$lon, 
                       lat = ices_psal_dat$lat,
                       group = "Salinity",
                       radius = 3,
                       color = ~psal_pal(mean_psal),
                       popup = ~paste0("Station number: ", 
                                       Station, "<br/>Mean salinity: ", 
                                       round(mean_psal, 2), "psu")) %>%
      showGroup(group = "Salinity") %>%
      hideGroup(group = c("Temperature", "Chlorophyll A Levels", "Oxygen Levels"))
  
   }, ignoreInit = TRUE)

  observeEvent(input$cphl_ctrl, {

    ices_cphl_dat <- cphl_react()

    leafletProxy("baltic_map", session) %>%
      clearGroup(group = "Chlorophyll A Levels") %>%
      addCircleMarkers(data = ices_cphl_dat, 
                       lng = ices_cphl_dat$lon, 
                       lat = ices_cphl_dat$lat,
                       group = "Chlorophyll A Levels",
                       radius = 3,
                       color = ~cphl_pal(mean_cphl),
                       popup = ~paste0("Station number: ", 
                                       Station, "<br/>Mean chorophyll a: ", 
                                       round(mean_cphl, 2), "ug/l")) %>%
      showGroup(group = "Chlorophyll A Levels") %>%
      hideGroup(group = c("Temperature", "Salinity", "Oxygen Levels"))
    
  }, ignoreInit = TRUE)

  observeEvent(input$doxy_ctrl, {

    ices_doxy_dat <- doxy_react()

    leafletProxy("baltic_map", session) %>%
      clearGroup(group = "Oxygen Levels") %>%
      addCircleMarkers(data = ices_doxy_dat,
                       lng = ices_doxy_dat$lon,
                       lat = ices_doxy_dat$lat,
                       group = "Oxygen Levels",
                       radius = 3,
                       color = ~doxy_pal(mean_doxy),
                       popup = ~paste0("Station number: ",
                                       Station, "<br/>Mean oxygen levels: ",
                                       round(mean_doxy, 2), "ml/l")) %>%
      showGroup(group = "Oxygen Levels") %>%
      hideGroup(group = c("Temperature", "Salinity", "Chlorophyll A Levels"))
    
  }, ignoreInit = TRUE)

  observeEvent(input$temp_ctrl, {

    ices_temp_dat <- temp_react()

    leafletProxy("baltic_map", session) %>%
      clearGroup(group = "Temperature") %>%
      addCircleMarkers(data = ices_temp_dat, 
                       lng = ices_temp_dat$lon, 
                       lat = ices_temp_dat$lat,
                       group = "Temperature",
                       radius = 3,
                       color = ~temp_pal(mean_temp),
                       popup = ~paste0("Station number: ", 
                                       Station, "<br/>Mean temperature: ", 
                                       round(mean_temp, 2), "°C")) %>%
      showGroup(group = "Temperature") %>%
      hideGroup(group = c("Salinity", "Chlorophyll a Levels", "Oxygen Levels"))
    
  }, ignoreInit = TRUE)
  
  # reset
  observeEvent(input$reset, {
    
    updateSliderInput(session, 'temp_ctrl', "Filter Temperature [°C]", 0, 25, value = c(0,25))
    updateSliderInput(session, 'psal_ctrl', "Filter Salinity [psu]", 0, 30, value = c(0,30))
    updateSliderInput(session, 'cphl_ctrl', "Filter Chlorophyll A Levels [ug/l]", 0, 90, value = c(0,90))
    updateSliderInput(session, 'doxy_ctrl', "Filter Oxygen Levels [ml/l]", 0, 11, value = c(0,11))

  }, ignoreInit = TRUE)
  
  # download text
  output$download_text <- renderText({
    if (input$include_na == TRUE) {
      download_file <- download_react_with_na()
      paste0("Your download file will contain ", nrow(download_file), " rows. Continue?")
    } else {
      download_file <- download_react()
      paste0("Your download file will contain ", nrow(download_file), " rows. Continue?") 
    }
  })
  
  # download
  output$download_data <- downloadHandler(
    filename = function() {
      paste0("ices_baltic_2019", ".csv")
    },
    content = function(file) {
      if (input$include_na == TRUE) {
        write.csv(download_react_with_na(), file, row.names = FALSE)
      } else {
        write.csv(download_react(), file, row.names = FALSE)
      }
    }
  )
}
shinyApp(ui = ui, server = server)