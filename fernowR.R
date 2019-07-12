library(tidyverse)
library(sf)
library(kableExtra)
library(knitr)
library(leaflet)
library(rgdal)
library(geojsonio)

# Import data
trees <- read_csv("fernow_tree_data.csv", col_types = cols(
  ws = col_character(),
  plot = col_character(),
  tree_id = col_character(),
  species = col_factor(),
  branch = col_integer(),
  dbh = col_double(),
  ba = col_double(),
  dam_class = col_character(),
  dam_gte2 = col_logical(),
  pct_dam = col_double(),
  canopy = col_factor(),
  ba_perc_dam = col_double()
))

fernow_plots <- st_read("geo_data/fernow_plotsv2.shp")
subcompartments <- geojsonio::geojson_read("geo_data/subcompartments_selected_wgs84utm17n.json", what = "sp") %>% 
  spTransform(CRS("+init=epsg:4326"))
fernow_boundary <- geojsonio::geojson_read("geo_data/fernow_boundary.json", what = "sp") %>% 
  spTransform(CRS("+init=epsg:4326"))
streams <- geojsonio::geojson_read("geo_data/fernow_streams.json", what = "sp") %>% 
  spTransform(CRS("+init=epsg:4326"))

# Add a column with the Latin code for all important species
trees <- trees %>% 
  mutate(sp_name = case_when(species == "ACSA" ~ "Acer saccharim", 
                             species == "LITU" ~ "Liriodendron tulipifera",
                             species == "ACPE" ~ "Acer pensylvanicum",
                             species == "ACRU" ~ "Acer rubrum", 
                             species == "AMAR" ~ "Amelanchier arborea", 
                             species == "ARSP" ~ "Aralia spinosa", 
                             species == "BELE" ~ "Betula lenta", 
                             species == "CACO" ~ "Carya cordiformis", 
                             species == "FAGR" ~ "Fagus grandifolia", 
                             species == "FRAM" ~ "Fraxinus americanus", 
                             species == "MAAC" ~ "Magnolia accuminata", 
                             species == "MAFR" ~ "Magnolia fraseri", 
                             species == "NYSY" ~ "Nyssa sylvatica", 
                             species == "OSVI" ~ "Ostrya virginiana", 
                             species == "OXAR" ~ "Oxydendrum arboretum", 
                             species == "PRPE" ~ "Prunus pensylvanica", 
                             species == "PRSE" ~ "Prunus serotina", 
                             species == "QUAL" ~ "Quercus alba", 
                             species == "AMAR" ~ "Amelanchier arborea", 
                             species == "ARSP" ~ "Aralia spinosa", 
                             species == "BEAL" ~ "Betula alleghaniensis", 
                             species == "CACA" ~ "Carya cathayensis", 
                             species == "CAGL" ~ "Carya glabra", 
                             species == "CAOV" ~ "Carya ovata", 
                             species == "QUPR" ~ "Quercus prinus", 
                             species == "QURU" ~ "Quercus rubra", 
                             species == "QUVE" ~ "Quercus velutina", 
                             species == "ROPS" ~ "Robinia pseudoacacia", 
                             species == "SAAL" ~ "Sassafras albidum", 
                             species == "TIAM" ~ "Tilia americana", 
                             species == "ULRU" ~ "Ulmus rubra",
                             TRUE ~ "Unknown"))

# Table of all species where n > 10, with species code and full latin name
kable(trees_tbl <- trees %>% 
        group_by(species, sp_name) %>% 
        count(species) %>% 
        arrange(desc(n)) %>% 
        filter(n > 10)) %>%
  kable_styling(bootstrap_options = c("striped", "hover"))

# Create a factor subset of only important species to report
species_main <- trees %>% 
  group_by(species) %>% 
  count(species) %>% 
  filter(n > 10) %>% 
  filter(species != 'ARSP' & 
           species != 'BEAL' & 
           species != 'CACO' & 
           species != 'PRPE' & 
           species != 'ROPS' & 
           species != 'ULRU')

species_fct <- levels(factor(species_main$species))
species_fct <- fct_relevel(species_fct)

dam_class_plot <- trees %>%
  group_by(species, sp_name, dam_class) %>% 
  count(species) %>% 
  filter(n > 10) %>% 
  filter(dam_class != "Class 1")

# Figure 4 - only important species, not showing damage class 1
ggplot(dam_class_plot, aes(species, n, fill = dam_class, label = n)) +
  geom_col(position = position_fill(reverse = TRUE), color = "white") +
  scale_x_discrete(limits = rev(levels(species_fct))) +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_brewer(palette = "YlGnBu", direction = -1) +
  xlab("Species") +
  ylab("Percent of Trees in Each Damage Class") +
  labs(fill = "Damage Class") +
  coord_flip()

# Create a study area map
leaflet(fernow_plots) %>% 
  addProviderTiles(providers$Esri.WorldTopoMap) %>% 
  addMarkers(~POINT_X, ~POINT_Y, popup = ~PlotName) %>% 
  addPolygons(data = subcompartments, 
              fillOpacity = 0.2, 
              weight = 1) %>% 
  addPolygons(data = fernow_boundary, 
              fillOpacity = 0,
              weight = 2) %>% 
  addPolylines(data = streams,
               weight = 1)
