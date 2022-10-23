###Limpiar 
rm(list = ls())
cat("\014")

## llamar pacman (contiene la función p_load)
require(pacman)

## llamar y/o instalar librerias
p_load(tidyverse,rio,skimr,
       sf, ## datos espaciales
       leaflet, ## visualizaciones
       tmaptools, ## geocodificar
       ggsn, ## map scale bar 
       osmdata) ## packages with census data

#Establacer directorios
#Directorio Daniel
setwd("C:/Users/danie/OneDrive/Escritorio/Uniandes/PEG/Big Data and Machine Learning/BD-ML---PS3/data")
#Crear la base de test
test <- readRDS("test.Rds")
#Crear la base de train
train <- readRDS("train.RDS")

## obtener la caja de coordenada que contiene el polígono de Bogotá
opq(bbox = getbb("Cali Colombia"))

## objeto osm
available_tags("amenity")
#Creamos una lista con los ammenities que a priori consideramos más importantes
lista_amenities <- c("arts_centre", "bank", "bicycle_parking",
                     "bus_station","cafe", "casino", "childcare", "cinema",
                     "clinic", "college", "community_centre", "conference_centre", "dentist", "doctors",
                     "events_venue", "fast_food", "hospital",
                     "kindergarten", "library", "love_hotel", "marketplace", "monastery",
                     "parking", "pharmacy", "place_of_worship", "police", "post_office",
                     "pub", "recycling", "restaurant",
                     "school", "shelter", "social_facility",
                     "theatre", "university", "veterinary")


#El loop crea un objeto osm para cada amenitie y extrae los simple features
for (amenitie in lista_amenities) {
  #Crear objetos osm
  #assign(paste("osm", amenitie, sep = "_"), opq(bbox = getbb("Cali Colombia")) %>%
   #        add_osm_feature(key="amenity" , value= amenitie))
  ## extraer Simple Features Collection (creando objetos sf)
  #assign(paste("osm", amenitie, "sf", sep = "_"), opq(bbox = getbb("Cali Colombia")) %>%
   #        add_osm_feature(key="amenity" , value= amenitie) %>% osmdata_sf())
  assign(paste("osm", amenitie, "sf", sep = "_"), opq(bbox = getbb("Cali Colombia")) %>%
           add_osm_feature(key="amenity" , value= amenitie) %>% osmdata_sf() %>% 
           .$osm_polygons)
}
