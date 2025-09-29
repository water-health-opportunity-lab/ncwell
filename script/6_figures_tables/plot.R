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

############################################# set working directory
setwd('~/Desktop/Research/Contamination risk/')

############################################# read data
index_capacity <- st_read('index_capacity.gpkg')
index_vulnerability <- st_read('index_vulnerability.gpkg')
index_hazard <- st_read('index_hazard.gpkg')

############################################# data manipulation
colnames(index_capacity)[4] <- 'index_capacity'
colnames(index_hazard)[4] <- 'index_hazard'
colnames(index_vulnerability)[4] <- 'index_vulnerability'

############################################# merging
index <- index_vulnerability %>% st_drop_geometry() %>% 
  left_join(index_hazard, by = "grid_id") %>%
  left_join(index_capacity, by = "grid_id")
index <- index[, c(1, 2, 3, 4, 7, 11, 8)]
colnames(index)[c(1, 3, 7)] <- c('ID', 'block_group_name', 'geom')
index <- st_as_sf(index, sf_column_name = "geom")

#############################################  risk score computation
index$risk <- NA_real_
ok <- !is.na(index$index_capacity)
index$risk[ok] <- index$index_vulnerability[ok] * index$index_hazard[ok] / exp(2 * index$index_capacity[ok])
index$risk[ok] <- (index$risk[ok] - min(index$risk[ok])) / (max(index$risk[ok]) - min(index$risk[ok]))
st_write(index, 'nc_grid_index.gpkg', layer = "my_layer", driver = "GPKG", append = FALSE)

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


############################################# Hazard index (Figure 2)
index_subset$hazard_for_plot <- ifelse(is.na(index_subset$index_capacity),
                                       NA, index_subset$index_hazard)
p <- ggplot() +
  # NA polygons
  geom_sf(data = subset(index_subset, is.na(hazard_for_plot)),
          aes(fill = "NA"), color = NA, show.legend = TRUE) +
  scale_fill_manual(
    name = NULL,
    values = c("NA" = "grey85"),
    breaks = "NA",
    guide = guide_legend(order = 1, override.aes = list(fill = "grey85"))
  ) +
  
  new_scale_fill() +
  
  # Non-NA polygons
  geom_sf(data = subset(index_subset, !is.na(hazard_for_plot)),
          aes(fill = hazard_for_plot), color = NA) +
  scale_fill_gradientn(
    colours = c("white", "red", "darkred"),
    name = NULL, na.value = NA,
    guide = guide_colourbar(
      order = 2,
      barwidth = 18,   # make the color bar longer
      barheight = 0.6
    )
  ) +
  
  # County boundaries
  geom_sf(data = western_counties, fill = NA, color = "black", linewidth = 0.5) +
  
  # Labels & theme
  labs(title = "Hazard", x = NULL, y = NULL) +
  theme_bw() +
  theme(
    panel.grid.major = element_blank(),
    axis.text   = element_blank(),
    axis.ticks  = element_blank(),
    panel.grid  = element_blank(),
    axis.title  = element_blank(),
    
    plot.title  = element_text(
      size = 40, face = "bold", hjust = 0,
      margin = margin(t = 5, b = 55)
    ),
    
    # Legends in one line
    legend.position = c(0.25, 1.02),   # top-right, outside
    legend.justification = c("left", "bottom"),
    legend.direction = "horizontal",
    legend.box = "horizontal",         # combine guides in one row
    legend.spacing.x = unit(0.5, "cm"),
    
    legend.text  = element_text(size = 22),
    legend.title = element_text(size = 22, face = "bold"),
    
    plot.margin = margin(t = 10, r = 5, b = 0, l = 5)
  ) +
  coord_sf(expand = FALSE)


ragg::agg_png("hazard_index_dist.png", width = 2200, height = 1600,
              units = "px", res = 300, background = "white")
print(p)
dev.off()


############################################# Vulnerability index (Figure 2)

# Split data once (safer & clearer than subsetting in aes)
index_subset$vulnerability_for_plot <- ifelse(is.na(index_subset$index_capacity), NA, index_subset$index_vulnerability)
idx_na  <- subset(index_subset, is.na(vulnerability_for_plot))
idx_ok  <- subset(index_subset, !is.na(vulnerability_for_plot))

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
  
  # Reset the fill scale so we can add the gradient for non-NA cells
  new_scale_fill() +
  
  # 2) Non-NA polygons with continuous gradient
  geom_sf(data = idx_ok,
          aes(fill = vulnerability_for_plot),
          color = NA) +
  scale_fill_gradientn(
    colours  = c("white", "skyblue", "navy"),
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
  
  # 4) Labels & theme
  theme_bw() +
  labs(
    title = "Physical vulnerability",
    x = NULL,  # blank axis titles
    y = NULL
  ) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid       = element_blank(),
    axis.text        = element_blank(),
    axis.ticks       = element_blank(),
    axis.title       = element_blank(),
    
    # Title position/margins
    plot.title = element_text(size = 40, face = "bold", hjust = 0,
                              margin = margin(t = 5, b = 55)),
    
    # Legends in one horizontal row, outside near top-right
    legend.position      = c(0.26, 1.02),       # move left/right by changing first number
    legend.justification = c("left", "bottom"),
    legend.direction     = "horizontal",
    legend.box           = "horizontal",
    legend.spacing.x     = unit(0.5, "cm"),
    legend.text          = element_text(size = 22),
    legend.title         = element_text(size = 22, face = "bold"),
    
    # Add a little top/right space for the outside legend
    plot.margin = margin(t = 5, r = 5, b = 0, l = 5)
  ) +
  coord_sf(expand = FALSE)

# Export with ragg
ragg::agg_png("vulnerability_index_dist.png",
              width = 2200, height = 1600, units = "px", res = 300, background = "white")
print(p)
dev.off()


############################################# Capacity index (Figure 2)

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
ragg::agg_png("capacity_index_dist.png",
              width = 2200, height = 1600, units = "px", res = 300, background = "white")
print(p)
dev.off()



############################################# Risk score (Figure 2)

######## Spatial distribution
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
  
  # 4) Labels & theme
  theme_bw() +
  labs(title = "Risk score",
       x = bquote("Longitude (" * degree * "W)"),
       y = bquote("Latitude (" * degree * "N)")) +
  theme(panel.grid  = element_blank(),
        axis.title  = element_text(size = 14, face = "bold"),
        axis.text   = element_text(size = 12, face = "bold"),
        plot.title  = element_text(size = 16, face = "bold", hjust = 0.5),
        plot.margin = margin(t = 15, r = 15, b = 15, l = 15))

# Export
ragg::agg_png("risk_dist.png", width = 2400, height = 1400, units = "px", res = 300)
print(p)
dev.off()





######################## FEMA Floodplain (Figure 1)
data_hazard <- st_read('nc_grid_hazard_imputed.gpkg')
index_subset <- index_subset %>% st_drop_geometry() %>% left_join(data_hazard, by = "grid_id")
colnames(index_subset)[1] <- 'ID'
index_subset <- st_as_sf(index_subset, sf_column_name = "geom")


p <- ggplot() +
  geom_sf(data = index_subset, aes(fill = fema_floodplain), color = NA) +
  geom_sf(data = western_counties, fill = NA, color = "black", linewidth = 0.5) +
  theme_bw() +
  labs(title = "FEMA Floodplain",
       x = bquote("Longitude (" * degree * "W)"),
       y = bquote("Latitude (" * degree * "N)")) +
  scale_fill_manual(values = c(`FALSE` = "white", `TRUE` = "#2B8CBE"),  # deeper blue
                    breaks = c(FALSE, TRUE),
                    labels = c("FALSE", "TRUE"),
                    name = NULL,
                    drop = FALSE) +
  guides(fill = guide_legend(override.aes = list(colour = "black", linewidth = 0.8))) +
  theme(
    panel.grid   = element_blank(),
    axis.title   = element_text(size = 32, face = "bold"),
    axis.text    = element_text(size = 15, face = "bold"),
    plot.title   = element_text(size = 32, face = "bold", hjust = 0.5),
    plot.margin  = margin(t = 2, r = 2, b = 2, l = 2),
    legend.text    = element_text(size = 18),   
    legend.title   = element_text(size = 18),
    legend.key.width  = unit(0.9, "cm"),
    legend.key.height = unit(0.5, "cm"))


ragg::agg_png("flood_dist.png", width = 2400, height = 1400, units = "px", res = 300)
print(p)
dev.off()

################ CAFOs (Figure 1)

p <- ggplot() +
  geom_sf(data = index_subset, aes(fill = cafo_impact), color = NA) +
  geom_sf(data = western_counties, fill = NA, color = "black", linewidth = 0.5) +
  theme_bw() +
  labs(
    title = "CAFO Impact",
    x = bquote("Longitude (" * degree * "W)"),
    y = bquote("Latitude (" * degree * "N)")
  ) +
  # Warm sepia ramp: cream -> sand -> khaki -> brown -> dark brown
  scale_fill_gradientn(
    colours = c("#FFF7E6", "#FBE6B1", "#F3C969", "#C08A3D", "#7A4A0C"),
    name = NULL,
    na.value = "grey85"
    # , trans = "log1p"  # uncomment if ag_runoff is highly skewed
  ) +
  theme(
    panel.grid  = element_blank(),
    axis.title  = element_text(size = 32, face = "bold"),
    axis.text   = element_text(size = 15, face = "bold"),
    plot.title  = element_text(size = 32, face = "bold", hjust = 0.5),
    legend.text    = element_text(size = 18),   # legend labels
    legend.title   = element_text(size = 18),
    plot.margin = margin(t = 2, r = 2, b = 2, l = 2)
  )


ragg::agg_png("cafo.png", width = 2400, height = 1400, units = "px", res = 300)
print(p)
dev.off()





######################## Precipitation (Figure 1)
data_vulnerability <- st_read('nc_grid_vulnerability_imputed.gpkg')
data_vulnerability <- data_vulnerability %>% st_drop_geometry()
index_subset <- index_subset %>% left_join(data_vulnerability, by = "grid_id")
colnames(index_subset)[1] <- 'ID'


p <- ggplot() +
  geom_sf(data = index_subset, aes(fill = ppt), color = NA) +
  geom_sf(data = western_counties, fill = NA, color = "black", linewidth = 0.5) +
  theme_bw() +
  labs(
    title = "Precipitation",
    x = bquote("Longitude (" * degree * "W)"),
    y = bquote("Latitude (" * degree * "N)")
  ) +
  # Intuitive light→dark blue for precipitation
  scale_fill_gradientn(
    colours = c("#F7FBFF", "#DEEBF7", "#9ECAE1", "#4292C6", "#08519C"),
    name = NULL,
    na.value = "grey85"
    # , trans = "log1p"  # uncomment if ppt is highly skewed or has zeros
  ) +
  theme(
    panel.grid  = element_blank(),
    axis.title  = element_text(size = 32, face = "bold"),
    axis.text   = element_text(size = 15, face = "bold"),
    legend.text    = element_text(size = 18),   # legend labels
    legend.title   = element_text(size = 18),
    plot.title  = element_text(size = 32, face = "bold", hjust = 0.5),
    plot.margin = margin(t = 2, r = 2, b = 2, l = 2)
  )

# render
ragg::agg_png("ppt.png", width = 2400, height = 1400, units = "px", res = 300)
print(p)
dev.off()




######################## Poverty (Figure 1)
data_capacity <- st_read('nc_grid_capacity_imputed.gpkg')
data_capacity <- data_capacity %>% st_drop_geometry()

index_subset <- index_subset %>% left_join(data_capacity, by = "grid_id")
colnames(index_subset)[1] <- 'ID'


p <- ggplot() +
  geom_sf(data = index_subset, aes(fill = pct_poverty), color = NA) +
  geom_sf(data = western_counties, fill = NA, color = "black", linewidth = 0.5) +
  theme_bw() +
  labs(title = "Poverty (%)",
       x = bquote("Longitude (" * degree * "W)"),
       y = bquote("Latitude (" * degree * "N)")) +
  scale_fill_gradientn(colours  = c("#FFFFFF", "#FFE5CC", "#FFC482", "#F17C1C", "#8C3B00"),
                       name     = NULL,
                       na.value = "grey85",
                       breaks   = scales::breaks_pretty(n = 5),          
                       labels   = scales::label_number(accuracy = 1)) +
  theme(panel.grid  = element_blank(),
    axis.title  = element_text(size = 32, face = "bold"),
    axis.text   = element_text(size = 15, face = "bold"),
    legend.text    = element_text(size = 18),   # legend labels
    legend.title   = element_text(size = 18),
    plot.title  = element_text(size = 32, face = "bold", hjust = 0.5),
    plot.margin = margin(t = 2, r = 2, b = 2, l = 2))


# render
ragg::agg_png("poverty.png", width = 2400, height = 1400, units = "px", res = 300)
print(p)
dev.off()



