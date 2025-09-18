rm(list = ls())
library(data.table)
library(dplyr)
library(ggplot2)
library(ggpubr)
library(patchwork)
library(bit64)
library(maps)
library(rnaturalearth)
library(rnaturalearthdata)
library(rnaturalearthhires)
library(radiant.data)

world <- ne_countries(scale = "medium", returnclass = "sf")

reefmod_metadata <- read.csv("./data/reefmod_metadata.csv")
uniqueids <- reefmod_metadata %>% dplyr::select(Reef_ID, UNIQUE_ID)

raw_rci <- data.table::fread("./data/rci_raw.csv")
raw_rci <- raw_rci[,c(1:20)]

rci <- data.table::fread("./data/rci_km2_counterfactual.csv")


coral_cover <- data.table::fread("./data/coral_cover_cf.csv")
shelter_volume <- data.table::fread("./data/shelter_volume_cf.csv")
juveniles <- data.table::fread("./data/juveniles_cf.csv")
rubble <- data.table::fread("./data/rubble_cf.csv")
#cots <- data.table::fread("./Code_Data/Output_Data/process_rci/cots_cf.csv")


#### FIGURE 2: BUBBLE PLOT OF RAW RCIS, FROM 2010 TO 2020
data_list <- list(coral_cover, shelter_volume, juveniles, rubble, raw_rci)

rci_index <- read.csv("./data/Heneghan_RCI.csv")
rci_index <- rci_index[,2:5]
rci_index$rci <- c(0.9,0.7,0.5,0.3,0.1)

reef_meta <- read.csv("./data/GBR_REEF_POLYGONS_2023_check_210323.csv") 
reef_meta <- reef_meta %>% dplyr::select(UNIQUE_ID, Reference_Area_km2, LAT, LON) %>% dplyr::mutate(UNIQUE_ID = as.numeric(UNIQUE_ID))

world <- ne_countries(scale = "large", country = "australia", returnclass = "sf")

theme_save <- theme(axis.title = element_text(size = 12, face = "bold", colour = "black"),
                    plot.title = element_blank(),
                    axis.text = element_text(size = 12, colour = "black", face = "bold"),
                    plot.subtitle = element_text(hjust = -0.1, face = "bold", vjust = 3, colour = "black", size = 12),
                    plot.margin = unit(c(0.2,0,0,0.2), "lines"),
                    legend.title = element_text(size = 12, face = "bold"),
                    legend.key.width = unit(0.3, "cm"),
                    legend.key.height = unit(0.3, "cm"),
                    legend.text = element_text(size = 10, face = "bold"),
                    legend.key.size = unit(10,"line"),
                    panel.background = element_rect(fill = "#c2e2fc",
                                                    colour = "#c2e2fc"),
                    panel.grid = element_blank())

plot_list <- list()

captions <- c("a", "b", "c", "d", "e")
anns <- c("Coral Cover", "Shelter Volume", "Recruits",  "Rubble","Reef Condition")

for(i in 1:length(data_list)){
  curr_data <- data_list[[i]]
  
  curr_data <- left_join(curr_data, uniqueids) %>% dplyr::rename("reef_uniqueid" = "UNIQUE_ID")
  
  raw_mean <- curr_data %>% dplyr::select(year_absolute, reef_uniqueid, sim_1) %>% 
    filter(year_absolute >= 2040 & year_absolute <= 2040) %>%
    dplyr::group_by(reef_uniqueid) %>% summarise(mean_dat = mean(sim_1, na.rm = TRUE)) %>%
    dplyr::mutate(reef_uniqueid = as.numeric(reef_uniqueid))
  
  raw_mean2 <- left_join(raw_mean, reef_meta, by = c("reef_uniqueid" = "UNIQUE_ID"))
  
  if(i < 5){
  curr_scores <- rci_index[,i]
  raw_mean2 <- raw_mean2 %>% dplyr::mutate(rci = ifelse(mean_dat >= curr_scores[1], 0.9, 
                                                        ifelse(mean_dat < curr_scores[1] & mean_dat >= curr_scores[2], 0.7,
                                                        ifelse(mean_dat < curr_scores[2] & mean_dat >= curr_scores[3], 0.5,
                                                        ifelse(mean_dat < curr_scores[3] & mean_dat >= curr_scores[4], 0.3, 0.1)))))  
  }
  
  if(i == 5){
  rci_scores <- rci_index[,i]
  raw_mean2$rci <- as.factor(sapply(raw_mean2$mean_dat, function(a, b) {b[which.min(abs(a-b))]}, rci_scores))
  }
  
  raw_mean2$point_size <- raw_mean2$Reference_Area_km2/max(raw_mean2$Reference_Area_km2)
  
  plot_list[[i]] <- ggplot(data = world) +
    geom_sf(color = "black", fill = "gray") + geom_point(data = raw_mean2, aes(x=LON, y = LAT, col = as.character(rci)), size = 0.1) +
    coord_sf(xlim = c(142, 153), ylim = c(-24.5, -10), expand = FALSE) + 
    scale_color_viridis_d(name = "Index Score", breaks = c("0.9","0.7","0.5","0.3","0.1"),
                          labels = c("Very Good", "Good", "Fair", "Poor", "Very Poor"),
                          option = "plasma", direction = -1) +
    guides(color = guide_legend(override.aes = list(size = 2)))+
    theme_save +
    labs(x="Longitude",y="Latitude", subtitle = captions[i]) 
  
  if(i < 5){
    plot_list[[i]] <- plot_list[[i]] + theme(legend.position="none", axis.title = element_text(size = 8, face = "bold", colour = "black"),
                                             plot.title = element_blank(),
                                             axis.text = element_text(size = 7, colour = "black", face = "bold"),) +
      annotate(geom= "label", x=142.5, y=-23.5, label=anns[i], size=4, hjust = 0)
  }
  
  if(i== 5){
    plot_list[[i]] <- plot_list[[i]] + geom_point(data = raw_mean2, aes(x=LON, y = LAT, col = as.character(rci)), size = 0.5)+
      annotate(geom= "label", x=142.5, y=-23.5, label=anns[i], size=5, hjust = 0) #+ theme(legend.position="none")
  }
  
}

 
all_plots <- ((plot_list[[1]] + plot_list[[2]] + plot_list[[3]] + plot_list[[4]])|plot_list[[5]])  +
               plot_layout(widths = c(1.3,1), heights = c(1.3,1), guides = "collect")
all_plots

ggsave(plot = all_plots, filename = "./figures/figure2.jpeg", width = 250, height = 150, units = "mm")



##### FIGURE 3, MEAN OF RCI COMPONENTS ACROSS GBR
rci_sum <- rci %>% dplyr::select(year_absolute, sim_1) %>% dplyr::group_by(year_absolute) %>% summarise(total_rci = sum(sim_1, na.rm = TRUE)) 

cover_sum <- coral_cover %>% dplyr::select(year_absolute, total_area_nine_zones, sim_1) %>% dplyr::group_by(year_absolute) %>% summarise(mean_dat = weighted.mean(sim_1, total_area_nine_zones, na.rm = TRUE),
                                                                                                                                         sd_dat = weighted.sd(sim_1, total_area_nine_zones, na.rm = TRUE))
shelter_sum <- shelter_volume %>% dplyr::select(year_absolute, sim_1, total_area_nine_zones) %>% dplyr::group_by(year_absolute) %>% summarise(mean_dat = weighted.mean(sim_1, total_area_nine_zones, na.rm = TRUE),
                                                                                                                                              sd_dat = weighted.sd(sim_1, total_area_nine_zones, na.rm = TRUE))
juv_sum <- juveniles %>% dplyr::select(year_absolute, sim_1, total_area_nine_zones) %>% dplyr::group_by(year_absolute) %>% summarise(mean_dat = weighted.mean(sim_1, total_area_nine_zones, na.rm = TRUE),
                                                                                                                                     sd_dat = weighted.sd(sim_1, total_area_nine_zones, na.rm = TRUE))
rub_sum <- rubble %>% dplyr::select(year_absolute, sim_1, total_area_nine_zones) %>% dplyr::group_by(year_absolute) %>% summarise(mean_dat = weighted.mean(1-sim_1, total_area_nine_zones,na.rm = TRUE),
                                                                                                                                  sd_dat = weighted.sd(1-sim_1, total_area_nine_zones, na.rm = TRUE))

dat_list <- list(cover_sum, shelter_sum, juv_sum, rub_sum)
ynames <- c("Coral Cover", "Relative Shelter Volume", "Relative Recruits",
            "Rubble")
captions <- c("a", "b", "c", "d")

ymaxlim <- c(0.53, 1, 0.41, 0.32)
plot_list <- list()
theme_save <- theme(axis.title = element_text(size = 12, face = "bold"),
                    plot.title = element_blank(),
                    axis.text = element_text(size = 12, colour = "black", face = "bold"),
                    plot.subtitle = element_text(hjust = -0.1, face = "bold", vjust = 3, size = 12),
                    plot.margin = unit(c(1,1.4,0.1,0.5), "lines"),
                    legend.title = element_blank(),
                    legend.key.width = unit(0.3, "cm"),
                    legend.key.height = unit(0.3, "cm"),
                    legend.text = element_text(size = 7, face = "bold"))

for(i in 1:length(dat_list)){
  curr_dat <- dat_list[[i]] %>% dplyr::filter(year_absolute >= 2010)
  
  plot_list[[i]] <- ggplot(curr_dat) +
    geom_ribbon(aes(x = year_absolute, ymin=mean_dat - sd_dat,ymax=mean_dat + sd_dat),  fill = "gray", alpha=0.7) +
    geom_line(aes(x = year_absolute, y = mean_dat), color = "black", linewidth = 1) + theme_bw() + theme_save +
    scale_x_continuous(expand = c(0,0)) + scale_y_continuous(expand = c(0,0)) + xlab("") + ylab("")+
    coord_cartesian(xlim = c(2010,2100), ylim = c(0, ymaxlim[i]), expand = FALSE)+
    labs(title = "", x = "Year", y = ynames[i], color = "", subtitle = captions[i])
}


all_plots <- ggarrange(plotlist = plot_list, ncol = 2, nrow = 2)

ggsave(plot = all_plots, filename = "./figures/figure3.jpeg", width = 170, height = 150, units = "mm")
