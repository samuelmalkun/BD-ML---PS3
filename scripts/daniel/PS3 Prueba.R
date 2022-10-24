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

#Ciudades en el test
test%>%count(city)
#Ciudades en el train
train%>%count(city)

#Convertir datafram a sf
train_sf <- st_as_sf(x = train, ## datos
                     coords=c("lon","lat"), ## coordenadas
                     crs=4326) ## CRS

## obtener la caja de coordenada que contiene el polígono de Cali
opq(bbox = getbb("Cali Colombia"))

## objeto osm
available_tags("amenity")
#Creamos una lista con los ammenities que a priori consideramos más importantes
lista_amenities <- c("restaurant", "police", "school", "theatre", "university", "arts_centre", "bank", "bicycle_parking",
                     "bus_station","cafe", "casino", "childcare", "cinema",
                     "clinic", "college", "community_centre", "conference_centre", "dentist", "doctors",
                     "events_venue", "fast_food", "hospital",
                     "kindergarten", "library", "love_hotel", "marketplace", "monastery",
                     "parking", "pharmacy", "place_of_worship", "post_office",
                     "pub", "recycling",
                     "shelter", "social_facility",
                     "veterinary")

lista_amenities <- c("bank", "restaurant")


#El loop crea un objeto osm para cada amenitie y extrae los simple features
for (amenitie in lista_amenities) {
  #Crear objetos osm
  #assign(paste("osm", amenitie, sep = "_"), opq(bbox = getbb("Cali Colombia")) %>%
   #        add_osm_feature(key="amenity" , value= amenitie))
  ## extraer Simple Features Collection (creando objetos sf)
  #assign(paste("osm", amenitie, "sf", sep = "_"), opq(bbox = getbb("Cali Colombia")) %>%
   #        add_osm_feature(key="amenity" , value= amenitie) %>% osmdata_sf())
  assign(paste("osm", amenitie, "Bogota", "sf", sep = "_"), opq(bbox = getbb("Bogotá Colombia")) %>%
           add_osm_feature(key="amenity" , value= amenitie) %>% osmdata_sf() %>% 
           .$osm_polygons)
  assign(paste("osm", amenitie, "Medellin", "sf", sep = "_"), opq(bbox = getbb("Medellín Colombia")) %>%
           add_osm_feature(key="amenity" , value= amenitie) %>% osmdata_sf() %>% 
           .$osm_polygons)
}

#Parece que tocará hacerlo manual (1 por 1)

#Gráficos de las casas de medellín
housesMedallo <- train_sf %>% subset(city=="Medellín")
leaflet() %>% addTiles() %>% addCircles(data=housesMedallo)

#Gráficos de las casas de Bogotá
housesBogota <- train_sf %>% subset(city=="Bogotá D.C")
leaflet() %>% addTiles() %>% addCircles(data=housesBogota)

## Distancia a muchos polígonos

#Se crea la matriz de distancias con los bancos
matrix_dist_banco <- st_distance(x=housesBogota , y= osm_bank_Bogota_sf)

#matrix_dist_banco[1:5,1:5]

min_dist_banco <- apply(matrix_dist_banco, 1 , min)

min_dist_banco %>% head()

housesBogota$dist_banco = min_dist_banco


#Opción 2
Bog <- getbb(place_name = "Bogotá Colombia", 
                   featuretype = "boundary:administrative", 
                   format_out = "sf_polygon") %>% .$multipolygon

leaflet() %>% addTiles() %>% addPolygons(data=Bog)

house_Bog <- st_intersection(x = housesBogota , y = Bog)

leaflet() %>% addTiles() %>% addPolygons(data=Bog,col="red") %>% addCircles(data=house_Bog)
