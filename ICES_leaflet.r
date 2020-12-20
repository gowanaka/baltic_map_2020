#### explore the leaflet map of the baltic sea for the app
# Author: Remy Brunner
# Date: 16/12/2020

library(leaflet)
library(htmlwidgets)
library(tidyverse)
library(RColorBrewer)

# read in (filtered datasets)
ices <- read_csv("~/Documents/ICES_sample_map_shiny/data/ices_baltic_2019.csv")
ices_temp <- read_csv("~/Documents/ICES_sample_map_shiny/data/ices_temp.csv")
ices_psal <- read_csv("~/Documents/ICES_sample_map_shiny/data/ices_psal.csv")
ices_cphl <- read_csv("~/Documents/ICES_sample_map_shiny/data/ices_cphl.csv")
ices_doxy <- read_csv("~/Documents/ICES_sample_map_shiny/data/ices_doxy.csv")
ices_download <- read_csv("~/Documents/ICES_sample_map_shiny/data/ices_tot.csv")


# glimpse(ices)

ices_latlon <- ices %>%
  select(Station, lat = `Latitude [degrees_north]`, lon = `Longitude [degrees_east]`) %>%
  distinct_all()

### filtering the ices dataset

ices_temp <- ices %>%
  group_by(Station) %>%
  summarise(mean_temp = mean(`TEMP [deg C]`, na.rm = T)) %>%
  inner_join(ices_latlon, by = "Station") %>%
  drop_na(mean_temp)

write_csv(ices_temp, file = "~/Documents/ICES_sample_map_shiny/data/ices_temp.csv")

ices_psal <- ices %>%
  group_by(Station) %>%
  summarise(mean_psal = mean(`PSAL [psu]`, na.rm = T)) %>%
  inner_join(ices_latlon, by = "Station") %>%
  drop_na(mean_psal)

write_csv(ices_psal, file = "~/Documents/ICES_sample_map_shiny/data/ices_psal.csv")

ices_cphl <- ices %>%
  group_by(Station) %>%
  summarise(mean_cphl = mean(`CPHL [ug/l]`, na.rm = T)) %>%
  inner_join(ices_latlon, by = "Station") %>%
  drop_na(mean_cphl)

write_csv(ices_cphl, file = "~/Documents/ICES_sample_map_shiny/data/ices_cphl.csv")

ices_doxy <- ices %>%
  group_by(Station) %>%
  summarise(mean_doxy = mean(`DOXY [ml/l]`, na.rm = T)) %>%
  inner_join(ices_latlon, by = "Station") %>%
  drop_na(mean_doxy)

write_csv(ices_doxy, file = "~/Documents/ICES_sample_map_shiny/data/ices_doxy.csv")

ices_tot <- ices_temp %>%
  left_join(ices_psal, by = c("Station", "lat", "lon")) %>%
  left_join(ices_cphl, by = c("Station", "lat", "lon")) %>%
  left_join(ices_doxy, by = c("Station", "lat", "lon"))

write_csv(ices_tot, file = "~/Documents/ICES_sample_map_shiny/data/ices_tot.csv")

ices_download %>% filter(mean_temp == 25) %>% nrow()


temp_pal <- colorNumeric("RdYlBu", domain = ices_temp$mean_temp, reverse = TRUE)
psal_pal <- colorNumeric("Reds", domain = ices_psal$mean_psal, reverse = FALSE)
cphl_pal <- colorNumeric("Greens", domain = ices_cphl$mean_cphl, reverse = FALSE)
doxy_pal <- colorNumeric("Blues", domain = ices_doxy$mean_doxy, reverse = FALSE)

map <- leaflet(options = leafletOptions(minZoom = 4,
                                 dragging = TRUE)) %>%
  setView(lng = 19.8633, lat = 58.48803, zoom = 5) %>%
  setMaxBounds(lng1 = max(ices_temp$lon) + 2,
               lat1 = max(ices_temp$lat) + 2,
               lng2 = min(ices_temp$lon) - 2,
               lat2 = min(ices_temp$lat) - 2) %>%
  addTiles(group = "OSM") %>%
  addProviderTiles(provider = "CartoDB", group = "CartoDB") %>%
  addProviderTiles(provider = "Esri", group = "Esri") %>%
  addCircleMarkers(data = ices_temp,
                   lng = ices_temp$lon, 
                   lat = ices_temp$lat,
                   group = "Temperature",
                   radius = 3,
                   color = ~temp_pal(mean_temp),
                   popup = ~paste0("Station number: ", 
                                   Station, "<br/>Mean temperature: ", 
                                   round(mean_temp, 2), "°C")) %>%
  addCircleMarkers(data = ices_psal, 
                   lng = ices_psal$lon, 
                   lat = ices_psal$lat,
                   group = "Salinity",
                   radius = 3,
                   color = ~psal_pal(mean_psal),
                   popup = ~paste0("Station number: ", 
                                   Station, "<br/>Mean salinity: ", 
                                   round(mean_psal, 2), "psu")) %>%
  addCircleMarkers(data = ices_cphl, 
                   lng = ices_cphl$lon, 
                   lat = ices_cphl$lat,
                   group = "Chlorophyll a Levels",
                   radius = 3,
                   color = ~cphl_pal(mean_cphl),
                   popup = ~paste0("Station number: ", 
                                   Station, "<br/>Mean chorophyll a: ", 
                                   round(mean_cphl, 2), "ug/l")) %>%
  addCircleMarkers(data = ices_doxy, 
                   lng = ices_doxy$lon, 
                   lat = ices_doxy$lat,
                   group = "Oxygen Levels",
                   radius = 3,
                   color = ~doxy_pal(mean_doxy),
                   popup = ~paste0("Station number: ", 
                                   Station, "<br/>Mean oxygen levels: ", 
                                   round(mean_doxy, 2), "ml/l")) %>%
  addLegend(data = ices_temp, 
            values = rev(seq(min(ices_temp$mean_temp), max(ices_temp$mean_temp), 5)),
            title = "[°C]",
            group = "temp",
            position = "bottomright",
            pal = temp_pal,
            labFormat = labelFormat(transform = function(x) sort(x, decreasing = TRUE))) %>%
  addLayersControl(baseGroups = c("OSM", "CartoDB", "Esri"),
                   overlayGroups = c("Temperature", "Salinity", "Chlorophyll a Levels", "Oxygen Levels"),
                   options = layersControlOptions(collapsed = TRUE)) %>%
  hideGroup(group = c("Temperature", "Chlorophyll a Levels", "Oxygen Levels")) %>%
  htmlwidgets::onRender(("
            function() {
              $('.leaflet-control-layers-list').prepend('<label style=\"text-align:center\">Choose Base Layer</label>');
              $('.leaflet-control-layers-overlays').prepend('<label style=\"text-align:center\">Choose Data Layer</label>');
            }"))

map

map %>% clearGroup(group = "Temperature")

map %>%
  clearGroup(group = "Temperature") %>%
  addCircleMarkers(data = ices_temp,
                   lng = ices_temp$lon, 
                   lat = ices_temp$lat,
                   group = "Temperature",
                   radius = 1,
                   color = ~temp_pal(mean_temp),
                   popup = ~paste0("Station number: ", 
                                   Station, "<br/>Mean temperature: ", 
                                   round(mean_temp, 2), "°C"))
