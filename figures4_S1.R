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
library(radiant.data)

world <- ne_countries(scale = "medium", returnclass = "sf")

#### FIGURE 4
rci_index <- read.csv("./data/Heneghan_RCI.csv")
rci_index <- rci_index[,2:5]
rci_index$Rubble <- 1 - rci_index$Rubble
rci_index$rci <- c(0.9,0.7,0.5,0.3,0.1)

reefmod_metadata <- read.csv("./data/reefmod_metadata.csv")
uniqueids <- reefmod_metadata %>% dplyr::select(Reef_ID, UNIQUE_ID)

coral_cover <- data.table::fread("./data/coral_cover_cf.csv")
shelter_volume <- data.table::fread("./data/shelter_volume_cf.csv")
juveniles <- data.table::fread("./data/juveniles_cf.csv")

rubble <- data.table::fread("./data/rubble_cf.csv")

n <- 1000

set.seed(1)

all_dat <- data.frame("year" = coral_cover$year_absolute,
                      "total_area" = coral_cover$total_area_nine_zones,
                      "coral_cover" = coral_cover$sim_1, 
                      "shelter_volume" = shelter_volume$sim_1,
                      "juveniles" = juveniles$sim_1,
                      "rubble" = 1-rubble$sim_1)

rm(shelter_volume, juveniles, rubble)

vg_frame <- matrix(NA, nrow = length(unique(coral_cover$year_absolute)), ncol = n)
fair_frame <- vg_frame
p_vp_frame <- vg_frame

for(i in 1:n){
  print(i)
  jitt <- matrix(c(runif(4, min = 0.75, max = 1.25),1), nrow = 5, ncol = 5, byrow = TRUE)
    
  curr_rci <- rci_index*jitt
  curr_rci[curr_rci > 1] <- 1
  
  curr_dat <- all_dat %>% dplyr::mutate(coral_cover_score = ifelse(coral_cover >= curr_rci$RelCover[1], curr_rci$rci[1],
                                                                   ifelse(coral_cover < curr_rci$RelCover[1] & coral_cover >= curr_rci$RelCover[2], curr_rci$rci[2],
                                                                          ifelse(coral_cover < curr_rci$RelCover[2] & coral_cover >= curr_rci$RelCover[3], curr_rci$rci[3],
                                                                                 ifelse(coral_cover < curr_rci$RelCover[3] & coral_cover >= curr_rci$RelCover[4], curr_rci$rci[4],
                                                                                        ifelse(coral_cover < curr_rci$RelCover[4], curr_rci$rci[5], curr_rci$rci[5]))))),
                                        shelter_volume_score = ifelse(shelter_volume >= curr_rci$Sheltervol[1], curr_rci$rci[1],
                                                                      ifelse(shelter_volume < curr_rci$Sheltervol[1] & shelter_volume >= curr_rci$Sheltervol[2], curr_rci$rci[2],
                                                                             ifelse(shelter_volume < curr_rci$Sheltervol[2] & shelter_volume >= curr_rci$Sheltervol[3], curr_rci$rci[3],
                                                                                    ifelse(shelter_volume < curr_rci$Sheltervol[3] & shelter_volume >= curr_rci$Sheltervol[4], curr_rci$rci[4],
                                                                                           ifelse(shelter_volume < curr_rci$Sheltervol[4], curr_rci$rci[5], curr_rci$rci[5]))))),
                                        juveniles_score = ifelse(juveniles >= curr_rci$CoralJuv[1], curr_rci$rci[1],
                                                                      ifelse(juveniles < curr_rci$CoralJuv[1] & juveniles >= curr_rci$CoralJuv[2], curr_rci$rci[2],
                                                                             ifelse(juveniles < curr_rci$CoralJuv[2] & juveniles >= curr_rci$CoralJuv[3], curr_rci$rci[3],
                                                                                    ifelse(juveniles < curr_rci$CoralJuv[3] & juveniles >= curr_rci$CoralJuv[4], curr_rci$rci[4],
                                                                                           ifelse(juveniles < curr_rci$CoralJuv[4], curr_rci$rci[5], curr_rci$rci[5]))))),
                                        rubble_score = ifelse(rubble <= curr_rci$Rubble[1], curr_rci$rci[1],
                                                                      ifelse(rubble > curr_rci$Rubble[1] & rubble <= curr_rci$Rubble[2], curr_rci$rci[2],
                                                                             ifelse(rubble > curr_rci$Rubble[2] & rubble <= curr_rci$Rubble[3], curr_rci$rci[3],
                                                                                    ifelse(rubble > curr_rci$Rubble[3] & rubble <= curr_rci$Rubble[4], curr_rci$rci[4],
                                                                                           ifelse(rubble > curr_rci$Rubble[4], curr_rci$rci[5], curr_rci$rci[5])))))) %>%
                  dplyr::select(-c(coral_cover, shelter_volume, juveniles, rubble))
  
  store_rci <- rep(NA, length = dim(curr_dat)[1])
  
  for(j in 1:dim(curr_rci)[1]){
    curr_crit <- curr_rci$rci[j]
    
    curr_thresh <- curr_dat %>% dplyr::select(-c(year, total_area))
    curr_thresh1 <- curr_thresh
    curr_thresh[curr_thresh1 >= curr_crit] <- 1
    curr_thresh[curr_thresh1 < curr_crit] <- 0
    
    curr_sum <- rowSums(curr_thresh)
    curr_sum[curr_sum < 3] <- NA

    store_rci[!is.na(curr_sum) & is.na(store_rci)] <- curr_crit
  }
  
  curr_dat <- curr_dat %>% dplyr::mutate(curr_rci = store_rci,
                                         g_vg = ifelse(curr_rci >= 0.7, total_area, 0),
                                         fair = ifelse(curr_rci == 0.5, total_area, 0),
                                         p_vp = ifelse(curr_rci < 0.5, total_area, 0)) %>% 
              dplyr::group_by(year) %>% dplyr::summarise(g_vg = sum(g_vg),
                                                         fair = sum(fair),
                                                         p_vp = sum(p_vp))
  
  vg_frame[,i] <- curr_dat$g_vg
  fair_frame[,i] <- curr_dat$fair
  p_vp_frame[,i] <- curr_dat$p_vp
}

save_frame <- data.frame("year" = unique(coral_cover$year_absolute))
save_frame <- save_frame %>% dplyr::mutate(vg_mean = apply(vg_frame, 1, mean),
                                           vg_sd = apply(vg_frame, 1, sd),
                                           fair_mean = apply(fair_frame, 1, mean),
                                           fair_sd = apply(fair_frame, 1, sd),
                                           p_mean = apply(p_vp_frame, 1, mean),
                                           p_sd = apply(p_vp_frame, 1, sd))

theme_save <- theme(axis.title = element_text(size = 12, face = "bold"),
                    plot.title = element_blank(),
                    axis.text = element_text(size = 12, colour = "black", face = "bold"),
                    plot.subtitle = element_text(hjust = -0.1, face = "bold", vjust = 3, size = 12),
                    plot.margin = unit(c(1,1.4,0.1,0.5), "lines"),
                    legend.title = element_blank(),
                    legend.key.width = unit(0.3, "cm"),
                    legend.key.height = unit(0.3, "cm"),
                    legend.text = element_text(size = 10, face = "bold"),
                    legend.position = c(0.8, 0.5))

color_rci <- c("Good/Very Good" = "#332288", "Fair" = "#117733", "Poor/Very Poor" = "#CC6677")

rci_plot <- ggplot(save_frame) + geom_ribbon(aes(x=year, ymin = vg_mean - vg_sd, ymax = vg_mean + vg_sd), alpha = 0.5, fill = "#332288") +
  geom_ribbon(aes(x=year, ymin = fair_mean - fair_sd, ymax = fair_mean + fair_sd), alpha = 0.5, fill = "#117733")+
  geom_ribbon(aes(x=year, ymin = p_mean - p_sd, ymax = p_mean + p_sd), alpha = 0.5, fill = "#CC6677") +
  geom_line(aes(x=year,y=vg_mean, colour = "Good/Very Good"), size = 0.8) +
  geom_line(aes(x=year,y= fair_mean, colour = "Fair"), size = 0.8) +
  geom_line(aes(x=year,y=p_mean, colour = "Poor/Very Poor"), size = 0.8) + theme_bw() + theme_save + 
  xlab("") + ylab("")+
  coord_cartesian(xlim = c(2010,2100), ylim = c(0,14000), expand = FALSE)+
  labs(title = "", x = "Year", y = expression(bold(paste("Area (km"^{2}, ")", sep = ""))), color = "") +
  scale_color_manual(values = color_rci, name = "Condition", breaks = c("Good/Very Good", "Fair", "Poor/Very Poor"))
rci_plot

ggsave(plot = rci_plot, filename = "./figures/figure4.jpeg", width = 140, height = 110, units = "mm")


### PLOT RCI VARIABLES (SUPP FIGURE 1)
plot_list <- list()

theme_save <- theme(axis.title = element_text(size = 12, face = "bold"),
                    plot.title = element_blank(),
                    axis.text = element_text(size = 12, colour = "black", face = "bold"),
                    plot.subtitle = element_text(hjust = -0.1, face = "bold", vjust = 3, size = 12),
                    plot.margin = unit(c(1,1.4,0.1,0.5), "lines"),
                    legend.title = element_blank(),
                    legend.key.width = unit(0.3, "cm"),
                    legend.key.height = unit(0.3, "cm"),
                    legend.text = element_text(size = 10, face = "bold"),
                    legend.position = c(0.8, 0.5))

plot_dat <- all_dat %>% dplyr::mutate(coral_cover = coral_cover*runif(dim(all_dat)[1], 0.97, 1.03),
                                     shelter_volume = shelter_volume*runif(dim(all_dat)[1], 0.97, 1.03),
                                     juveniles = juveniles*runif(dim(all_dat)[1], 0.97, 1.03),
                                     rubble = rubble*runif(dim(all_dat)[1], 0.97, 1.03)) %>% dplyr::select(coral_cover, shelter_volume, juveniles, rubble)

plot_dat[plot_dat > 1] <- 1
plot_dat[plot_dat < 0] <- 0

cor_shelt <- cor(plot_dat$coral_cover, plot_dat$shelter_volume)
plot_list[[1]] <- ggplot(plot_dat) + geom_point(aes(x=coral_cover, y=shelter_volume), size = 0.3) + theme_bw() + theme_save + 
  coord_cartesian(xlim = c(0,0.7), ylim = c(0,1), expand = FALSE)+
  labs(title = "", x = "Coral Cover", y = "Shelter Volume", color = "", subtitle = "a)") +
  annotate("label", x =0.6, y = 0.85, label = paste("R = ", round(cor_shelt,2), sep = ""), size = 4) 

cor_juv <- cor(plot_dat$coral_cover, plot_dat$juveniles)
plot_list[[2]] <- ggplot(plot_dat) + geom_point(aes(x=coral_cover, y=juveniles), size = 0.3) + theme_bw() + theme_save + 
  coord_cartesian(xlim = c(0,0.7), ylim = c(0,0.75), expand = FALSE)+
  labs(title = "", x = "Coral Cover", y = "Relative Juveniles", color = "", subtitle = "b)") +
  annotate("label", x =0.6, y = 0.65, label = paste("R = ", round(cor_juv,2), sep = ""), size = 4) 

cor_rubb <- cor(plot_dat$coral_cover, plot_dat$rubble)
plot_list[[3]] <- ggplot(plot_dat) + geom_point(aes(x=coral_cover, y=rubble), size = 0.3) + theme_bw() + theme_save + 
  coord_cartesian(xlim = c(0,0.7), ylim = c(0,0.6), expand = FALSE)+
  labs(title = "", x = "Coral Cover", y = "Rubble", color = "", subtitle = "c)") +
  annotate("label", x =0.6, y = 0.53, label = paste("R = ", round(cor_rubb,2), sep = ""), size = 4) 

all_plots <- ggarrange(plotlist = plot_list, ncol = 2, nrow = 2)

ggsave(plot = all_plots, filename = "./figures/figure_S1.jpeg", width = 200, height = 170, units = "mm")

