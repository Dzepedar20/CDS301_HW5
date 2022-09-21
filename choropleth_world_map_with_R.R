# Spatial Data Visualization
# Choropleth mapping with R

# Great reference examples
# https://geocompr.robinlovelace.net/adv-map.html
# https://mgimond.github.io/Spatial/mapping-data-in-r.html

getwd()

# SET THE LOCATION OF WHERE YOUR R CODE IS SAVED
setwd("~/CDS301_spring_2021/CDS301_DL1/wk6_Mar1_7_spatial_data_visualization_and_color/WK6_Assignments_to_do")



# packages needed for this code
install.packages("sf")
install.packages("tmap")

library(sf)
library(tmap)



## LOAD IN GEOSPATIAL DATASET (this demo is with a single dataset that has thematic attributes and geo data)
## shapefile
world_geo <- st_read("./GIS_data_world_pop/world_countries_with_pop.shp", quiet=TRUE, stringsAsFactors = TRUE)

## Geojson
#world_geo <- st_read("./GIS_data_world_pop/World_countries_w_pop_data_geojson.geojson", quiet=TRUE, stringsAsFactors = TRUE)

## CSV with WKT - note will need to convert strings to values
#world_geo <- st_read("./GIS_data_world_pop/World_countries_w_pop_data_WKT.csv", quiet=TRUE, stringsAsFactors = FALSE)

# SEE METADATA ABOUT YOUR DATASET
# print out the number of features, the coordinate extents, coordinate reference system
# the number of variables (columns), column names, min and max values
world_geo

# set the plot function
current.mode <- tmap_mode("plot")


# QUICK MAP SHOWING POPULATION COUNTS
layer_to_map = world_geo
map = tm_shape(layer_to_map)+ # set up map with dataset
  tm_fill("Population")+ # fill the countries with color based on this column
  tm_borders(alpha=0.5) # country borders
map


# CHOROPLETH MAP OF POPULATION
# Let's change the population categories so the map shows the variation better
# we do this using style to set number of classes and the classification style such as 
# equal, jenks, quantile, fixed
# e.g. n=6, style="jenks",
# or create our own custom class categories e.g. # breaks=c(-Inf, -1.5, -0.25, 0.25, 1.5, Inf), # custom breaks if desired
# and add a legend

layer_to_map = world_geo #we only have one layer and it is the polygons for countries w pop data
column_to_map = "Population" #

# SETUP THE MAP
map_display = 
  tm_shape(layer_to_map)+ # THE LAYERS
  tm_fill(column_to_map, # THE COLOR AND STYLE
          n=6, # number of categories in the legend (try for not more than 7)
          style="jenks")+# The way to divide the data choices are jenks, quantile, cont, equal, fixed
  tm_borders(alpha=0.5)+ # BORDERS OF FEATURES
  tm_layout(legend.position = c("left","bottom"), # CREATE THE LEGEND
            legend.title.size = 0.8,
            legend.text.size=0.5)

# PLOT THE MAP
map_display


# CHOLROPLETH MAP SHOWING COLOR FOR FERTILITY RATE AND CIRCLE FOR POP SIZE

# Note: For some columns the null value was entered as a string N.A.
# because the mixed data types of number values and the string of "N.A." in the column
# lets convert the NA to 0 with the three lines of code below
world_geo$Fert.Rate <- as.character(world_geo$Fert.Rate) # convert the column to strings
world_geo$Fert.Rate[which(world_geo$Fert.Rate=="N.A.")] <- NA # replace the "N.A." with NA 
world_geo$Fert.Rate <- as.numeric(world_geo$Fert.Rate) # convert the column to numbers again

layer_to_map = world_geo
col1_to_map = "Fert.Rate"
col2_to_map = "Population"

# SETUP THE MAP
map_display = 
  tm_shape(layer_to_map)+ # THE LAYERS
  tm_fill(col1_to_map, # THE COLOR AND STYLE
         n=6, style="jenks", # The way to divide the data choices are jenks, quantile, cont, equal, fixed
         palette = "Greens", # SET YOUR COLOR HERE
         colorNA = "white",
         title = "Percent change per Year") +
  tm_borders(alpha=0.5)+ # BORDERS OF FEATURES
  tm_bubbles(col2_to_map)+ # ADD GRADUATED CIRCLES FOR ANOTHER COLUMN'S DATA
  tm_layout(legend.position = c("left","bottom"), # CREATE THE LEGEND
            legend.title.size = 0.8,
            legend.text.size=0.5)
  
# PLOT THE MAP
map_display

# SAVE THE MAP (it automatically saves it at 300 dpi choices are .png or .tiff )
tmap_save(map_display, "World_map_with_R.tiff", width=1920, height=1080, asp=0)





# BASIC SCATTER PLOTS

install.packages("ggplot2")
library(ggplot2)

options(scipen = 999) # disable scientific notation 

# basic scatter plot
plot(world_geo$Population,world_geo$Fert.Rate, log='x', xlab="Population", ylab="Fertility Rate")

# scatter plot plus graduated circle size
ggplot(world_geo, aes(x = Population, y=Land.Area, size=Fert.Rate), log='x', 
  format(x, scientific = FALSE))+
  xlab("Population") + ylab("Land Area (Sq. Km)")+
  geom_point(shape=21)+
  scale_x_log10(oob = scales::squish_infinite, limits = c(100000, 10000000000))+
  scale_y_log10(oob = scales::squish_infinite, limits=c(10,100000000))+
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank())
  

