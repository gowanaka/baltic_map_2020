# Baltic Map Web App

This is the source code for the Map App I created using the leaflet and shiny package in R. <br>
The Web App can be found [here](https://reminho.shinyapps.io/ICES_sample_map_shiny/). <br>
Once I make it available, an R notebook explaining my choices for data filtering and cleaning can be found [here](https://www.kaggle.com/reminho/i-created-a-map-web-app-with-shiny).

## What does it show?

The App will show you temperature, salinity, chlorophyll a and oxygen levels for the Baltic Sea averaged for the year 2019. 
The map includes almost 2500 data points. Users can: <br>

* Choose a base map provider (Open Street Map, CartoDB or Esri tiles).
* Interact with the map (zoom in/out, drag).
* Choose which data layers they wanna look at, laver are stackable.
* Filter the data layers by ranges of the above mentioned variables.
* Reset the filters by clicking the "Reset" button.
* Download the filtered data in CSV format.

## Contents

### Data
* The `data` folder contains all the data I used to build the map. `ices_baltic_2019.csv` contains the raw unfiltered data. 
* `ices_cphl.csv`, `ices_doxy.csv`, `ices_psal.csv` & `ices_temp.csv` contain the data for each data layer. `ices_tot.csv` contains the filtered raw data and is the data downloadable from the Web App. <br>

### Code

* `ICES_leaflet.r` contains the code I used to filter and prepare the raw data. 
* Finally, `ICES_sample_map_fullscreen.r` contains the source code used to build the web app using shiny and leaflet.
