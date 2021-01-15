# author: Abigail Stamm
# date: 13 January 2021
# purpose: To read in census demo data and isolate age groups and GQ

#---- set up ----
library(tidyverse)
path <- "P:/Sections/EHS/Abigail/DifferentialPrivacy/Census 05272020 demo"
file <- "nhgis_ppdd_20200527_tract.csv"


#---- isolate desired variables ----
#GQ, age groups, total
d <- read.csv(paste(path, file, sep = "/")) %>% 
  rename(total_demo = H7V001_dp, total_real = H7V001_sf, 
         gq_demo = H80001_dp, gq_real = H80001_sf) %>%
  select(gisjoin, name, state, total_demo, total_real, gq_demo, gq_real,
         starts_with("H760")) # sex by age, m->f, total+0-4->85+

d <- d %>% filter(state == 36) %>% 
  mutate(gq_prop_d = gq_demo/total_demo, gq_prop_r = gq_real/total_real, 
         is_gq_demo = gq_prop_d > .5, is_gq_real = gq_prop_r > .5, 
         tract = paste0(36, substr(gisjoin, 5, 7), substr(gisjoin, 9, 14))) 

# testing data
# d %>% filter(!is_gq_demo == is_gq_real) %>% 
#   select(tract, total_demo, gq_demo, total_real, gq_real,  gq_prop_d, gq_prop_r)
# only tract that changes gq status is 36061031900 with ~25 people

p <- d %>% select(tract, total_demo, total_real, gq_prop_d, gq_prop_r)
p <- p %>% rename(geoid10 = tract)

#---- read in tract shapefile ----
# P:\Sections\EHS\Arjita\EPHT\Sub-County\Creating new Agg Areas\tract_mcd_crosswalk_AR.shp

path <- "P:/Sections/EHS/Arjita/EPHT/Sub-County/Creating new Agg Areas"
file <- "tract_mcd_crosswalk_AR"

s <- rgdal::readOGR(dsn = path, layer = file)
s@data <- s@data %>% rename(geoid10 = tract10) %>%
  select(geoid10, county10, mcd16_name, mcd16_ids, mcd_ge15k)


#---- join data ----
s@data <- s@data %>% left_join(p, by = "geoid10")



#---- save shapefile ----
path <- "P:/Sections/EHS/Abigail/DifferentialPrivacy/Census 05272020 demo"
file <- "census_20200527_tract_demo"
rgdal::writeOGR(s, dsn = path, layer = file, driver = "ESRI Shapefile", 
                overwrite_layer = TRUE)




