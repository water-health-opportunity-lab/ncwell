############################################# load packages
library("MASS")
library("dplyr")
library("factoextra")
library("exactextractr")
library("tigris")
library("ggplot2")
library("plotly")
library("sf")
library("tibble")
library("GGally")
library("cluster")
library("mclust")
library("stringr")
library("tigris")
library("tidyverse")
library("ggnewscale")

############################################# read data
index <- st_read('nc_grid_index.gpkg')
well_tests <- st_read('well_tests_filtered.gpkg')

#############################################  target counties
target_counties <- c("Alexander County","Alleghany County","Ashe County","Avery County",
                     "Buncombe County","Burke County","Caldwell County","Catawba County",
                     "Clay County","Cleveland County","Gaston County","Haywood County",
                     "Henderson County","Jackson County","Lincoln County","Macon County",
                     "Madison County","Mcdowell County","Mitchell County","Polk County",
                     "Rutherford County","Transylvania County","Watauga County","Wilkes County",
                     "Yancey County")

############################################# NC counties
NC_counties <- tigris::counties(state = "NC", year = 2024)
western_counties <- NC_counties %>% filter(NAMELSAD %in% target_counties)

############################################# subset of the data that lie in those target counties
index <- index %>% mutate(county_raw = str_extract(block_group_name, "[A-Za-z .'-]+ County"),
                          county_norm = str_squish(str_to_title(county_raw)))
index_subset <- index %>% filter(!is.na(county_norm) & county_norm %in% target_counties)

well_tests <- well_tests %>%
  st_transform(4326) %>%
  mutate(
    lon = st_coordinates(geom)[,1],
    lat = st_coordinates(geom)[,2]
  )

# Filter out points with longitude < 80
well_tests_filtered <- well_tests %>%
  filter(lon <= -80)

######## Capacity index with contaminated well tests overlaid
idx_na <- subset(index_subset,  is.na(index_capacity))
idx_ok <- subset(index_subset, !is.na(index_capacity))

p <- ggplot() +
  # 1) NA polygons as a discrete legend entry
  geom_sf(data = idx_na,
          aes(fill = "NA"),
          color = NA,
          show.legend = nrow(idx_na) > 0) +
  scale_fill_manual(
    name   = NULL,
    values = c("NA" = "grey85"),
    breaks = "NA",
    guide  = guide_legend(order = 1, override.aes = list(fill = "grey85", color = "grey60"))
  ) +
  
  # Reset the fill scale for the gradient
  new_scale_fill() +
  
  # 2) Non-NA polygons with continuous gradient
  geom_sf(data = idx_ok,
          aes(fill = index_capacity),
          color = NA) +
  scale_fill_gradientn(
    colours  = c("white", "lightgreen", "darkgreen"),
    name     = NULL,
    na.value = NA,
    guide    = guide_colourbar(
      order     = 2,
      barwidth  = 18,   # longer color bar
      barheight = 0.6
    )
  ) +
  
  # 3) County boundaries
  geom_sf(data = western_counties, fill = NA, color = "black", linewidth = 0.5) +
  
  # 4) Well test results
  geom_sf(data = filter(well_tests_filtered, Coliform_Result_Binary == "Present"), color = "red", shape = 19, size = 1.5, alpha = 0.5) +

  # 4) Labels & theme
  theme_bw() +
  labs(
    title = "Social capacity",
    x = NULL,  # blank axis titles
    y = NULL
  ) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid       = element_blank(),
    axis.text        = element_blank(),
    axis.ticks       = element_blank(),
    axis.title       = element_blank(),
    
    # Title placement
    plot.title = element_text(size = 40, face = "bold", hjust = 0,
                              margin = margin(t = 5, b = 55)),
    
    # Legends in one horizontal row, outside near top-right
    legend.position      = c(0.26, 1.02),      # tweak first value to move left/right
    legend.justification = c("left", "bottom"),
    legend.direction     = "horizontal",
    legend.box           = "horizontal",
    legend.spacing.x     = unit(0.5, "cm"),
    legend.text          = element_text(size = 22),
    legend.title         = element_text(size = 22, face = "bold"),
    
    # Space for outside legend
    plot.margin = margin(t = 5, r = 5, b = 0, l = 5)
  ) +
  coord_sf(expand = FALSE)

# Export with ragg
ragg::agg_png("capacity_contaminated.png",
              width = 2200, height = 1600, units = "px", res = 300, background = "white")
print(p)
dev.off()



######## Risk score with contaminated well tests overlaid (Figure 4)
idx_na <- subset(index_subset,  is.na(risk))
idx_ok <- subset(index_subset, !is.na(risk))

p <- ggplot() +
  # 1) NA polygons as discrete legend entry
  geom_sf(data = idx_na,
          aes(fill = "NA"),
          color = NA,
          show.legend = nrow(idx_na) > 0) +
  scale_fill_manual(name = NULL,
                    values = c("NA" = "grey85"),
                    breaks = "NA",
                    guide  = guide_legend(order = 1, override.aes = list(color = "grey60"))) +
  
  # Reset the fill scale
  new_scale_fill() +
  
  # 2) Non-NA polygons with gradient
  geom_sf(data  = idx_ok, aes(fill = risk), color = NA) +
  scale_fill_gradientn(colours = c("white", "pink", "purple", "black"),
                       name = NULL,
                       na.value = NA,
                       guide = guide_colourbar(order = 2)) +
  
  # 3) County boundaries
  geom_sf(data = western_counties, fill = NA, color = "black", linewidth = 0.5) +
  
  # 4) Well test results
  geom_sf(data = filter(well_tests_filtered, Coliform_Result_Binary == "Present"), color = "red", shape = 'x', size = 4) +

  # 5) Labels & theme
  theme_bw() +
  labs(title = "Risk score",
       x = bquote("Longitude (" * degree * "W)"),
       y = bquote("Latitude (" * degree * "N)"),) +
  theme(panel.grid  = element_blank(),
        axis.title  = element_text(size = 14, face = "bold"),
        axis.text   = element_text(size = 12, face = "bold"),
        plot.title  = element_text(size = 16, face = "bold", hjust = 0.5),
        plot.margin = margin(t = 15, r = 15, b = 15, l = 15),
        legend.text = element_text(size = 12))

# Export
ragg::agg_png("risk_contaminated.png", width = 2400, height = 1400, units = "px", res = 300)
print(p)
dev.off()


######## Risk score with uncontaminated well tests overlaid (Figure 4)
idx_na <- subset(index_subset,  is.na(risk))
idx_ok <- subset(index_subset, !is.na(risk))

p <- ggplot() +
  # 1) NA polygons as discrete legend entry
  geom_sf(data = idx_na,
          aes(fill = "NA"),
          color = NA,
          show.legend = nrow(idx_na) > 0) +
  scale_fill_manual(name = NULL,
                    values = c("NA" = "grey85"),
                    breaks = "NA",
                    guide  = guide_legend(order = 1, override.aes = list(color = "grey60"))) +
  
  # Reset the fill scale
  new_scale_fill() +
  
  # 2) Non-NA polygons with gradient
  geom_sf(data  = idx_ok, aes(fill = risk), color = NA) +
  scale_fill_gradientn(colours = c("white", "pink", "purple", "black"),
                       name = NULL,
                       na.value = NA,
                       guide = guide_colourbar(order = 2)) +
  
  # 3) County boundaries
  geom_sf(data = western_counties, fill = NA, color = "black", linewidth = 0.5) +
  
  # 4) Well test results
  geom_sf(data = filter(well_tests_filtered, Coliform_Result_Binary == "Absent"), color = "#09C81C", shape = 19, size = 1.5, alpha = 0.5) +
  
  # 5) Labels & theme
  theme_bw() +
  labs(title = "Risk score",
       x = bquote("Longitude (" * degree * "W)"),
       y = bquote("Latitude (" * degree * "N)")) +
  theme(panel.grid  = element_blank(),
        axis.title  = element_text(size = 14, face = "bold"),
        axis.text   = element_text(size = 12, face = "bold"),
        plot.title  = element_text(size = 16, face = "bold", hjust = 0.5),
        plot.margin = margin(t = 15, r = 15, b = 15, l = 15),
        legend.text = element_text(size = 12))

# Export
ragg::agg_png("risk_uncontaminated.png", width = 2400, height = 1400, units = "px", res = 300)
print(p)
dev.off()