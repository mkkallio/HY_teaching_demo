# This script reproduces the examples used in my teaching demonstration 
# for the position of university lecturer in geoinformatics.



# check if required packages are installed, and install if not

packages <- c("sf", "dplyr", "ggplot2")
installed_packages <- installed.packages()
for (package in packages) {
    test <- package %in% installed_packages[,1]
    if (!test) {
        install.packages(package)
    }
}
rm(package, installed_packages, test) # remove unnecessary variables


# load required packages
library(sf)
library(dplyr)
library(ggplot2)


# ------------------------------------------------------------------------------
# define functions used

# this function creates voronoi polygons from a set of points, and clips them
# to the provided polygon. This also retains the attribute information of the 
# point data.
create_voronoi <- function(points, polygon) {
    vor <- points %>% 
        sf::st_union() %>% 
        sf::st_voronoi() %>% 
        sf::st_cast() %>% 
        sf::st_sf() %>% 
        sf::st_join(points) %>% 
        sf::st_intersection(st_union(polygon))
    return(vor)
}




# ------------------------------------------------------------------------------
# read in data

# Finnish municipality dataset is from the National Land Survey and obtained
# from Paituli-service, https://paituli.csc.fi/download.html.
helsinki <- read_sf("data/SuomenKuntajako_2021_100k.shp") %>% 
    filter(NAMEFIN == "Helsinki")

# Helsinki buildings dataset is by Helsinki Region Infoshare and was obtained
# on 24 May 2023 from https://hri.fi/data/fi/dataset/helsingin-rakennukset.  
buildings <- read_sf("data/buildings_helsinki.gpkg") %>% 
    select(C_KUNTA)
build_points <- st_centroid(buildings)
build_lines <- st_cast(buildings, "MULTILINESTRING")



# ------------------------------------------------------------------------------
# select random number of buildings, create voronois from them, and 
# compute their intersection. The loop records the time spent in the operation

n_buildings <- c(10, 100, 1000, seq(5000, 50000, by = 5000))

data <- tibble(n = n_buildings,
               polygons = NA,
               lines = NA,
               points = NA)


set.seed(20230502) # for reproducibility
pb <- txtProgressBar(0, length(n_buildings), style = 3)
for(i in seq_along(n_buildings)) {
    n <- n_buildings[i]
    
    # take a random sample of buildings, and create voronoi of the sample
    build1 <- sample(nrow(buildings), n)
    build2 <- sample(nrow(buildings), n)
    set1 <- create_voronoi(build_points[build1,], helsinki)
    set2 <- create_voronoi(build_points[build2,], helsinki)
    
    # compute polygon - polygon intersection
    elapsed <- system.time(st_intersection(set1, set2))
    data$polygons[i] <- elapsed[3]
    
    # compute polygon - linestring intersection
    elapsed <- system.time(st_intersection(set1, build_lines[build2,]))
    data$lines[i] <- elapsed[3]
    
    # compute polygon - point intersection
    elapsed <- system.time(st_intersection(set1, build_points[build2,]))
    data$points[i] <- elapsed[3]
    
    setTxtProgressBar(pb, i)
}
close(pb)


# make a plot of the data
ggplot(data) +
    geom_point(aes(n, polygons, color = "polygons")) +
    geom_smooth(aes(n, polygons, color = "polygons"),
                method = "lm",
                formula = y ~ poly(x, 2)) +
    geom_point(aes(n, lines, color = "lines")) +
    geom_smooth(aes(n, lines, color = "lines"),
                method = "lm",
                formula = y ~ poly(x, 2)) +
    geom_point(aes(n, points, color = "points")) +
    geom_smooth(aes(n, points, color = "points"),
                method = "lm",
                formula = y ~ poly(x, 2)) +
    theme_minimal() +
    labs(x = "Number of features",
         y = "Seconds")






# ------------------------------------------------------------------------------
# select random number of buildings, create voronois from them, and 
# perform a spatial query (st_intersects). The loop records the time 
# spent in the operation

n_buildings <- c(10, 100, 1000, seq(5000, 50000, by = 5000))

data_query <- tibble(n = n_buildings,
               polygons = NA,
               lines = NA,
               points = NA)


set.seed(20230502) # for reproducibility
pb <- txtProgressBar(0, length(n_buildings), style = 3)
for(i in seq_along(n_buildings)) {
    n <- n_buildings[i]
    
    # take a random sample of buildings, and create voronoi of the sample
    build1 <- sample(nrow(buildings), n)
    build2 <- sample(nrow(buildings), n)
    set1 <- create_voronoi(build_points[build1,], helsinki)
    set2 <- create_voronoi(build_points[build2,], helsinki)
    
    # compute polygon - polygon query
    elapsed <- system.time(st_intersects(set1, set2))
    data_query$polygons[i] <- elapsed[3]
    
    # compute polygon - linestring query
    elapsed <- system.time(st_intersects(set1, build_lines[build2,]))
    data_query$lines[i] <- elapsed[3]
    
    # compute polygon - point query
    elapsed <- system.time(st_intersects(set1, build_points[build2,]))
    data_query$points[i] <- elapsed[3]
    
    setTxtProgressBar(pb, i)
}
close(pb)


# make a plot of the data
ggplot(data_query) +
    geom_point(aes(n, polygons, color = "polygons")) +
    geom_smooth(aes(n, polygons, color = "polygons"),
                method = "lm",
                formula = y ~ poly(x, 2)) +
    geom_point(aes(n, lines, color = "lines")) +
    geom_smooth(aes(n, lines, color = "lines"),
                method = "lm",
                formula = y ~ poly(x, 2)) +
    geom_point(aes(n, points, color = "points")) +
    geom_smooth(aes(n, points, color = "points"),
                method = "lm",
                formula = y ~ poly(x, 2)) +
    theme_minimal() +
    labs(x = "Number of features",
         y = "Seconds")


