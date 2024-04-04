
library(tmap)
library(mapview)
library(tigris)
library(tidyverse)
library(sf)
library(htmlwidgets)
library(webshot)

options(tigris_use_cache = TRUE)





ca.zips <- zctas(starts_with = el.school$Zip, cb = TRUE, year = 2020)

ca.counties <- counties(state = "CA" , cb = TRUE )


ca.zips.93 <- zctas(starts_with = "93", cb = TRUE, year = 2020)



mry.crs <- suggest_crs(mry)


mry <- ca.counties %>%
    filter(NAME == "Monterey")



# Snips zips to only include Monterey County portion
monterey_districts <- ca.zips[st_contains(mry, ca.zips, sparse = FALSE),] %>%
    rbind(ca.zips ) %>%
    st_intersection(mry) 

monterey_districts <- ca.zips %>%
    st_intersection(mry) %>%
    select(Zip = NAME20, 
           geometry) # %>%
   # rbind(mry %>% select(Zip = NAME, 
 #                        geometry))




### Plotting ====

ggplot() + 
    geom_sf(data = mry) +
    geom_sf(data = monterey_districts) + 
    theme_void()


mapview::mapview(monterey_districts)
mapview::mapview(ca.zips)
mapview(ca.zips.93)


sum.map <- monterey_districts %>%
    # select(Zip = NAME20, 
    #        geometry
    #        ) %>%
    left_join(summary) 


   map.view.preschool <-  mapview(sum.map, 
            zcol = "priority.preschool")

    
    
    
    mapshot(map.view.preschool, url = "mapview-preschool.html")
    
    
    
    
    
    
    tm_shape(sum.map) +
        tm_polygons(col = "priority.infant") +
        tm_facets(by = "priority.preschool")

    
    tmap_mode("view")

    map.facet <- tm_shape(sum.map) +
        tm_polygons(c("priority.infant", "priority.preschool", "priority.school")) +
        tm_borders() +
        tm_text("Zip", auto.placement = TRUE) +
        tm_view(set.view = c(lat = 36.65 , lon = -121.6 ,  zoom = 10)) +
        tm_facets(sync = TRUE, ncol = 2)
    
    
    
    map.infant <- tm_shape(sum.map) +
        tm_polygons("priority.infant",
                    popup.vars = c("City",
                                   "Infant Priority Level" = "priority.infant",
                                   "Preschool Priority Level" ="priority.preschool",
                                   "School-Age Priority Level" ="priority.school"),
                    title = "Infant Priority Level"
                    ) +
        tm_borders() +
        tm_text("Zip", auto.placement = TRUE) +
        tm_view(set.view = c(lat = 36.2 , lon = -121.1 ,  zoom = 9)) +
        tm_facets(sync = TRUE, ncol = 2)    
    
    map.preschool <- tm_shape(sum.map) +
        tm_polygons("priority.preschool",
                    popup.vars = c("City",
                                   "Infant Priority Level" = "priority.infant",
                                   "Preschool Priority Level" ="priority.preschool",
                                   "School-Age Priority Level" ="priority.school"),
                    title = "Preschool Priority Level"
        ) +
        tm_borders() +
        tm_text("Zip", auto.placement = TRUE) +
        tm_view(set.view = c(lat = 36.2 , lon = -121.1 ,  zoom = 9)) +
        tm_facets(sync = TRUE, ncol = 2)  
    
    map.school <- tm_shape(sum.map) +
        tm_polygons("priority.school",
                    popup.vars = c("City",
                                   "Infant Priority Level" = "priority.infant",
                                   "Preschool Priority Level" ="priority.preschool",
                                   "School-Age Priority Level" ="priority.school"),
                    title = "School-Age Priority Level"
        ) +
        tm_borders() +
        tm_text("Zip", auto.placement = TRUE) +
        tm_view(set.view = c(lat = 36.2 , lon = -121.1 ,  zoom = 9)) +
        tm_facets(sync = TRUE, ncol = 2)  
    
    
    
    
    tmap_save(map.infant,  "map-infant.html")
    tmap_save(map.preschool,  "map-preschool.html")
    tmap_save(map.school,  "map-school.html")
    
    
 #   tmap_save(map.facet,  "map-all-priorities.html")
    
