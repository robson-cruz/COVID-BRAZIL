## Load Packages
library(dplyr)
library(tidyr)
library(stringr)
library(leaflet)
library(leaflet.extras)
library(rgdal)

## Set the owrk directory
#setwd('C:/Data-Science-Foundations-using-R-Specialization/Data-Products/Peer-graded_Assignment/')

## Load the Brazilian COVID data set
covid_to_map <- read.csv2(
        './data/HIST_PAINEL_COVIDBR_07set2021.csv',
        header = TRUE,
        encoding = 'Windows-1252') %>% 
        filter(regiao != 'Brasil', municipio != '', estado != '') %>%
        rename(geocodigo = codmun)

#write.csv2(covid$municipio, 'D:/namesCitiesBRA.csv', row.names = FALSE)

## Get more information about the data set
#str(covid)

## Read brazilian cities data set
cities <- read.csv(
        './data/spatial/brazilian_cities.csv',
        encoding = 'UTF-8'
) %>% rename(municipio = nome) %>%
        mutate(
                municipio = str_replace(
                        municipio, pattern = "''", replacement = "'"
                )
        ) %>%
        select(id, geocodigo, municipio, lng, lat)

## Merge the data sets
covid_cities <- covid_to_map %>%
        left_join(cities[-c(1, 2)], by = 'municipio', keep = FALSE) 
#head(covid_cities)

## Map
brazil <- readOGR('./data/brazil.shp', verbose = FALSE)

map_covid <- covid_cities %>%
        filter(data == '2021-09-07') %>%
        select(lat, lng, municipio, obitosAcumulado) %>%
        group_by(lng, lat, municipio) %>%
        summarise(totalDeaths = sum(obitosAcumulado, na.rm = TRUE)) %>%
        filter(!is.na(lng)) %>%
        filter(!is.na(lat)) %>%
        leaflet() %>%
        addTiles() %>%
        ## Set heatmap of COVID-19 in Brazil
        addHeatmap(
                lng = ~lng, lat = ~lat, intensity = ~totalDeaths, 
                blur = 20, max = 0.5, radius = 14
        ) %>%
        addCircles(
                lng = ~lng, lat = ~lat,
                label = ~totalDeaths,
                opacity = 0, fill = FALSE 
        ) %>%
        addLegend('bottomright',
                  values = ~totalDeaths, opacity = 1,
                  pal = colorNumeric(palette = 'RdYlBu',
                                     reverse = TRUE,
                                     domain = covid_cities$totalDeaths),
                  title = 'Total Deaths') %>%
        setView(lng = -50.80182849, lat = -11.00090426, zoom = 4) %>%
        ## Add Brazil boundary
        addPolygons(
                data = brazil, color = 'black', 
                fill = NA, 
                fillOpacity = 0.3
        ) %>%
        addTiles()

map_covid
