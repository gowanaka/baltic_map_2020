### Script to deploy the ICES App to ShinyApps.io
# Author: Remy
# Date: 16/12/2020

# config the server location
# rsconnect::setAccountInfo(name='reminho',
#                           token='A4F86C4652F4293F9BD2DA30FE139697',
#                           secret='ViOxQGAejVkYbQvGTNC0CeZwh5Ii2sR7K0H7FhIe')

# deploy r markdown app
rsconnect::deployApp("/home/sascha/Documents/ICES_sample_map_shiny/ICES_sample_map.Rmd")

# deploy absolute panel app
rsconnect::deployApp("/home/sascha/Documents/ICES_sample_map_shiny/ICES_sample_map_fullscreen.r")
