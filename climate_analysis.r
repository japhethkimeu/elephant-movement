# import required packages
rm(list = ls())
library(stars)      
library(sf)         
library(ggplot2)    
library(patchwork)  
library(rnaturalearth)
library(raster)
library(here)

# set working directory
setwd("C:/Desktop/myfiles/new_dev/climate_analysis")
getwd()

# download observed data from 1960-1990
file_path <- "C:/Desktop/myfiles/new_dev/climate_analysis"
raster::getData(name = 'worldclim', var = 'bio', res = 10,
                path = file_path)

# get projected data for period 2041-2060
raster::getData(name = 'CMIP5', var = 'bio', res = 10,
                rcp = 45, model = 'IP', year = 50,
                path = file_path)

# process data
annual_T <- read_stars("C:/Desktop/myfiles/new_dev/climate_analysis/wc10/bio1.bil")
annual_T <- annual_T/10
annual_T_50 <- read_stars("C:/Desktop/myfiles/new_dev/climate_analysis/cmip5/10m/ip45bi501.tif")
annual_T_50 <- annual_T_50/10

# define a color palette
temp_colors <- colorRampPalette(c("#2c7bb6", "#abd9e9", "#ffffbf", "#fdae61", "#d7191c"))

# plot the global maps
nbreaks <- 20
{
  par(mfrow = c(1,2))
  plot(annual_T, main = "Annual temperature - 1960-1990",
       nbreaks = nbreaks,
       col = temp_colors(nbreaks - 1))
  plot(annual_T_50, main = "Annual temperature - RCP 4.5 projection for 2041-2060",
       nbreaks = nbreaks,
       col = temp_colors(nbreaks - 1))
}

# lets get analysis fro Kenya
kenya_map <- ne_countries(country = "kenya", returnclass = "sf")
{
 #plot(kenya_map)
}

# crop climate data to country kenya
annual_T_KE <- annual_T[kenya_map]
# get and set data crs and make it identical to kenya
st_crs(annual_T_50) <- st_crs(kenya_map)
annual_T_50_KE <- annual_T_50[kenya_map]

# plot
{
  par(mfrow = c(1, 2))
  plot(annual_T_KE, main = "Annual temperature - 1960-1990",
       nbreaks = nbreaks,
       col = temp_colors(nbreaks - 1))
  plot(main = "Annual temperature - RCP 4.5 projection for 2041-2060",
       annual_T_50_KE, nbreaks = nbreaks,
       col = temp_colors(nbreaks - 1))
}

# lets compute temp changes
names(annual_T_KE) <- "recent"
annual_T_KE$projected <- annual_T_50_KE$ip45bi501.tif
annual_T_KE$change <- annual_T_KE$projected  - annual_T_KE$recent
annual_T_KE

# plot the changes
recent_T_plot <- ggplot() + 
  geom_stars(data = annual_T_KE) +
  scale_fill_gradientn(name = "Annual T [°C]",
                       colors = temp_colors(5),
                       limits = c(-7, 32),
                       na.value = "white") +
  geom_sf(data = kenya_map, fill = NA) +
  coord_sf(default_crs = NULL, lims_method = "geometry_bbox") +
  scale_x_discrete(expand = c(0, 0)) +
  scale_y_discrete(expand = c(0, 0)) +
  ggtitle("a) 1960-1990") +
  theme_void() +
  theme(legend.position = "none")

projected_T_plot <- ggplot() + 
  geom_stars(data = annual_T_KE["projected"]) +
  scale_fill_gradientn(name = "Annual T [°C]",
                       colors = temp_colors(5),
                       limits = c(-7, 32),
                       na.value = "white") +
  geom_sf(data = kenya_map, fill = NA) +
  coord_sf(default_crs = NULL, lims_method = "geometry_bbox") +
  scale_x_discrete(expand = c(0, 0)) +
  scale_y_discrete(expand = c(0, 0)) +
  ggtitle("b) 2041-2060 (projected)") +
  theme_void() +
  theme(legend.position = "bottom")

projected_change_T_plot <- ggplot() + 
  geom_stars(data = annual_T_KE["change"]) +
  scale_fill_gradientn(name = "Change in T [°C]",
                       colors = temp_colors(5)[3:5],
                       limits = c(1, 5),
                       na.value = "white") +
  geom_sf(data = kenya_map, fill = NA) + 
  coord_sf(default_crs = NULL, lims_method = "geometry_bbox") +
  scale_x_discrete(expand = c(0, 0)) +
  scale_y_discrete(expand = c(0, 0)) +
  ggtitle("c) Projected change") +
  theme_void() +
  theme(legend.position = "bottom")
(recent_T_plot / projected_T_plot + plot_layout(guides = "keep")) | projected_change_T_plot +
  theme(plot.margin = margin(c(0, 0, 0, 0)))