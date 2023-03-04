# Whitney Wyche
# 12.15.2022
# Final Project DSEE F&M Prof. Howard: Local Food in PA by population

setwd("/Users/whitneywyche/Desktop/School/Fall22/Data_Science/Final_Project/Food")

#install.packages("")

library(dplyr)
library(tidyr)
library(tidyverse)
library(sf)
library(tmap)
library(units)
library(ggplot2)
require(gganimate)
require(gifski)
library(broom)


#import local food (cvs files)
CSA <- read.csv("/Users/whitneywyche/Desktop/School/Fall22/Data_Science/Final_Project/Food/CSA.csv")
Market <- read.csv("/Users/whitneywyche/Desktop/School/Fall22/Data_Science/Final_Project/Food/Market.csv")
Farm <- read.csv("/Users/whitneywyche/Desktop/School/Fall22/Data_Science/Final_Project/Food/Farm.csv")
Hub <- read.csv("/Users/whitneywyche/Desktop/School/Fall22/Data_Science/Final_Project/Food/Hub.csv")
#import cartographic info
County <- st_read("/Users/whitneywyche/Desktop/School/Fall22/Data_Science/Final_Project/Food/cb_2018_us_county_500k/cb_2018_us_county_500k.shp")
States <- st_read("/Users/whitneywyche/Desktop/School/Fall22/Data_Science/Final_Project/Food/tl_2022_us_state/tl_2022_us_state.shp")
#import population info
Population <- read.csv("/Users/whitneywyche/Desktop/School/Fall22/Data_Science/Final_Project/Food/Population.csv")



###
#COUNTY
# set crs of counties to USA (Coordinate reference system, find at https://epsg.io/)
County <- st_transform(County, crs = 4269)  

# read in geojson of county geometry and calculate area of each county in sq km
County$areakm <- County$ALAND/1000000



###
#STATE: PA
# set crs of counties to USA
States <- st_transform(States, crs = 4269)  

#only PA
PA <- filter(States, NAME == "Pennsylvania")

#delete polygons outside of PA 
New_PA <- dplyr::filter(County, STATEFP ==42)
    
#only PA for perimiter line around the state
PA_ = PA %>%
  filter(NAME %in% c("Pennsylvania"))

#Cast geometry to another type that is usable for the line
PA_ <- PA_ %>% sf::st_cast("MULTILINESTRING")



###
#FOOD
#only location columns
CSA <- CSA %>% dplyr::select(location_x, location_y)
Market <- Market %>% dplyr::select(location_x, location_y)
Farm <- Farm %>% dplyr::select(location_x, location_y)
Hub <- Hub %>% dplyr::select(location_x, location_y)

#add names of each dataframe in a new column
CSA$CSA <- rep("CSA",nrow(CSA))
Market$Market <- rep("Market",nrow(Market))
Farm$Farm <- rep("Farm",nrow(Farm))
Hub$Hub <- rep("Hub", nrow(Hub))

#add numbers for food in new column (for ggplot animate sequential order)
CSA$Number <- rep("1",nrow(CSA))
Market$Number <- rep("2",nrow(Market))
Farm$Number <- rep("3",nrow(Farm))
Hub$Number <- rep("4", nrow(Hub))

#delete NA rows
CSA <- na.omit(CSA)
Market <- na.omit(Market)
Farm <- na.omit(Farm)
Hub <- na.omit(Hub)

#rename LAT/LON
CSA <- CSA %>%
  rename(Longitude = location_x,
         Latitude = location_y)
Market <- Market %>%
  rename(Longitude = location_x,
         Latitude = location_y)
Farm <- Farm %>%
  rename(Longitude = location_x,
         Latitude = location_y)
Hub <- Hub %>%
  rename(Longitude = location_x,
         Latitude = location_y)

#convert to shapefile
CSA.sf <- CSA %>%
  st_as_sf( coords = c("Longitude", "Latitude"),
            agr = "constant",
            crs = 4269, #NAD 83
            stringsAsFactors = FALSE,
            remove = TRUE)
Market.sf <- Market %>%
  st_as_sf( coords = c("Longitude", "Latitude"),
            agr = "constant",
            crs = 4269, #NAD 83
            stringsAsFactors = FALSE,
            remove = TRUE)
Farm.sf <- Farm %>%
  st_as_sf( coords = c("Longitude", "Latitude"),
            agr = "constant",
            crs = 4269, #NAD 83
            stringsAsFactors = FALSE,
            remove = TRUE)
Hub.sf <- Hub %>%
  st_as_sf( coords = c("Longitude", "Latitude"),
            agr = "constant",
            crs = 4269, #NAD 83
            stringsAsFactors = FALSE,
            remove = TRUE)

#only food locations within PA
CSA.sf <- CSA.sf[st_within(CSA.sf, New_PA) %>% lengths > 0,]
Market.sf <- Market.sf[st_within(Market.sf, New_PA) %>% lengths > 0,]
Farm.sf <- Farm.sf[st_within(Farm.sf, New_PA) %>% lengths > 0,]
Hub.sf <- Hub.sf[st_within(Hub.sf, New_PA) %>% lengths > 0,]



###
##combine all local food
#only names one column 
Name<-c(CSA$CSA,Farm$Farm, Market$Market, Hub$Hub)

#only longitude column
Longitude<-c(CSA$Longitude, Farm$Longitude, Market$Longitude, Hub$Longitude)

#only latitude column
Latitude<-c(CSA$Latitude, Farm$Latitude, Market$Latitude, Hub$Latitude)

#only number column
Number<-c(CSA$Number, Farm$Number, Market$Number, Hub$Number)

#new dataframe combined name,lat,long
Food <-data.frame(Name,Longitude,Latitude,Number)

#Number numeric
Food$Number <- as.numeric(Food$Number)

#convert to shapefile
Food.sf <- Food %>%
  mutate_at(vars(Latitude, Longitude), as.numeric) %>% 
  # coordinates must be numeric
  st_as_sf(coords = c('Longitude','Latitude'),
           agr = "constant",
           crs = 4269, #NAD 83
           stringsAsFactors = FALSE,
           remove = TRUE)



###
##calculate
# find points within polygons
Food_PA.sf <- st_join(New_PA, Food.sf, st_contains)

# count food per county 
Food_county_count <- count(as_tibble(Food_PA.sf), GEOID)

# join food count with county df
Food_county <- left_join(Food_county_count, New_PA, by = "GEOID")

# calc food density per km
Food_county$Food_per_county <- as.numeric(Food_county$n / Food_county$areakm)

#convert to shapefile
Food_county.sf <- Food_county %>%
  st_as_sf(agr = "constant",
           crs = 4269, #NAD 83
           stringsAsFactors = FALSE,
           remove = TRUE)


###
#POPULATION
#population: GEOD = character, POP = numeric
Population$GEOID <- as.character(Population$GEOID)
Population$POPULATION <- as.numeric(Population$POPULATION)

# join food count with county df
#delete NA rows
#calculate food per capita per 100,000 people
Food_pop <- left_join(Food_county.sf, Population, by = "GEOID") %>%
  mutate(Food_per_capita = as.numeric(n / POPULATION * 100000))

#convert to shapefile
Food_pop.sf <- Food_pop %>%
  st_as_sf(agr = "constant",
           crs = 4269, #NAD 83
           stringsAsFactors = FALSE,
           remove = TRUE)



#for GGPLOT
#colors for ggplot legend
colors <- c("#B36F6F", "#BDA252E9", "#6FC261FD", "#4A79B8")

  

###
#GRAPHS
#Map Local Food density per county 
tmap_mode("plot")
tm_shape(Food_county.sf, bbox = "Pennsylvania", projection = 4269) + 
  #bbox is the boundary of the map
  tm_fill(
    col = "n",
    palette = "Greens",
    style = "cont",
    contrast = c(0.1, 1),
    title = "Local food count\nper county",
    id = "GEOID", 
    showNA = FALSE,
    alpha = 0.8,
    popup.vars = c(
      "Total Local Food" = "n"
    ),
    popup.format = list(
      n = list(format = "f", digits = 0)))+
  tm_borders(col = "darkgray", lwd = 0.7) +  
  tm_layout(legend.outside = TRUE, title.size = 2, legend.text.size = 1)+
tm_shape(PA_) +
  tm_lines(col = "black", lwd = 2) +  
tm_basemap("CartoDB.PositronNoLabels")


#Map Local Food per Capita
tm_shape(Food_pop.sf, bbox = "Pennsylvania", projection = 4269) + 
  #bbox is the boundary of the map
  tm_fill(
    col = "Food_per_capita",
    palette = "Greens",
    style = "cont",
    contrast = c(0.1, 1),
    title = "Local food count per \n100,000 county residents",
    id = "GEOID",
    showNA = FALSE,
    alpha = 0.8,
    popup.vars = c(
      "Total Local Food" = "n",
      "Local food/residents" = "Food_per_capita"
    ),
    popup.format = list(
      n = list(format = "f", digits = 0),
      Food_per_capita = list(format = "f", digits = 0)
      ))+
  tm_borders(col = "darkgray", lwd = 0.7) +  
  tm_layout(legend.outside = TRUE, title.size = 2, legend.text.size = 1)+
  tm_shape(PA_) +
  tm_lines(col = "black", lwd = 2) +  
  tm_basemap("CartoDB.PositronNoLabels")

###
#plot Food using ggplot 
(Food_indiv <- ggplot() +
    geom_sf(data = New_PA)+
    geom_sf(data = CSA.sf, aes(color="CSA"),
            size = 2)+
    geom_sf(data = Market.sf, aes(color="Market"),
             size = 2)+
    geom_sf(data = Farm.sf, aes(color="Farm"), 
             size = 2)+
    geom_sf(data = Hub.sf, aes(color="Hub"), 
             size = 2)+
    xlab("\nLongitude")+
    ylab("Latitude\n")+
    scale_color_manual(values = colors) + 
    theme(axis.text.x = element_text(size = 12),
          axis.text.y = element_text(size = 12),
          axis.title = element_text(size = 14),
          panel.grid = element_blank(),
          legend.title = element_blank(),                                 
          legend.position = "right")+
    transition_manual(factor(Number, levels = c('1', '2', '3', '4'))))

#Animate
animate(Food_indiv, renderer = gifski_renderer())

#saves animation as a .gif to your working directory
anim_save("Food_indiv")

#create a new variable for quadratic regression
Food_county$areakm2 <- Food_county$areakm^2
#quadratic regression
County.quad <- lm(n ~ areakm + areakm2, data = Food_county) 
summary(County.quad)

#linear regression
County.lm <- lm(n ~ areakm, data = Food_county) 
summary(County.lm)

#plot food density by county size  
(Food_county.plot <- ggplot(Food_county, aes(x=areakm, y=n))+
  geom_point(size = 2, color = "#B36F6F") +   
  geom_smooth(method = "lm", aes(fill = "#BDA252E9", color = "#BDA252E9")) + 
  theme_bw() +
  scale_fill_manual(values = "#BDA252E9") +
  scale_color_manual(values = "#BDA252E9") +
  ylab("Local Food Count\n") +                             
  xlab("\nCounty Size")  +
  theme(axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title = element_text(size = 12, face = "plain"),
        legend.position = "none",
        panel.grid = element_blank()))
  
  
#plot food density by population density per county  
(Food_pop.plot <- ggplot(Food_pop, aes(x=POPULATION/100000, y=n))+
    geom_point(size = 2, color = "#6FC261FD") +   
    geom_smooth(method = "lm", aes(fill = "#4A79B8", color = "#4A79B8")) + 
    theme_bw() +
    scale_fill_manual(values = "#4A79B8") +
    scale_color_manual(values = "#4A79B8") +
    ylab("Local Food Count\n") +                             
    xlab("\nCounty Population (per 100,000 residents)")  +
    theme(axis.text.x = element_text(size = 12),
          axis.text.y = element_text(size = 12),
          axis.title = element_text(size = 12, face = "plain"),
          legend.position = "none",
          panel.grid = element_blank()))

#linear regression
Population.reg <- lm(POPULATION ~ n, data = Food_pop) 
summary(Population.reg)


