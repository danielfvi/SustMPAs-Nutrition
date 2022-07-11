####MPA and Nutrition Project
####Figure 1
####Author: Daniel Viana
####Date: June 2021

library(tidyverse) 
library(gdata)
library(ggpubr)
library(sf)
library(cowplot)
library(ggnewscale)

# Clear workspace
rm(list = ls())

survey_bio_covariates <- read_csv("data/survey_bio_covariates.csv") %>% 
  dplyr::select(SurveyID, SiteLongitude, SiteLatitude, is_mpa)

predicted_OA_MPA <- read_csv("Outputs/predicted_OA_MPA_bayes_brms.csv") %>% 
  left_join(survey_bio_covariates) %>% 
  mutate(perc_change = if_else(relative_change>1, 100*(relative_change-1), -1*100*(1-relative_change)),
         LRR_1 = log(exp(bio_log)/exp(pred)),
         LRR = bio_log - pred,
         ABS_change = 100*(exp(LRR)-1)) %>% 
  filter(is_mpa == 1)

survey_bio_covariates <- read_csv("data/survey_bio_covariates.csv")

##Filter for sites outside of MPAs
RLS_OA = survey_bio_covariates %>%
  filter(governance_cat=="OpenAccess") %>% 
  select(bio_log, SiteLatitude, SiteLongitude)

#########Maps###########

##Transform to sf 
RLS_sites_st_OA = st_as_sf(RLS_OA, coords = c("SiteLongitude", "SiteLatitude")) %>% 
  st_set_crs(4326)

##Map RLS sites
# World polygons from the maps package
world_shp <- sf::st_as_sf(maps::map("world", plot = FALSE, fill = TRUE))

# ###Open access
# RLS_sites_st_OA = RLS_sites_st %>% 
#   filter(Governance=="Unregulated")

##Simple plot prod per EEEZ
map1 = ggplot() +
  geom_sf(data = world_shp,
          fill = "grey85", 
          color = "grey90",
          size = 0.05) +
  geom_sf(data = RLS_sites_st_OA,
          shape=21, 
          alpha = 0.6,
          aes(fill=bio_log),
          size = 2) +
  scale_fill_gradientn(name=expression(paste("Biomass\nlog (kg ha"^-1,")")), 
                       colors = RColorBrewer::brewer.pal(9, "PuBu"), 
                       na.value = "grey87",
                       breaks = c(2, 6, 9),
                       labels = c("2", "6", "9")) +
  guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black", barwidth = 1, barheight = 3)) +
  labs(title = "Non-MPA sites") +
  theme(panel.grid.major = element_blank(), 
        panel.background = element_rect(fill = "aliceblue"),
        panel.border = element_rect(colour = "black", fill = NA),
        plot.title = element_text(hjust = 0.5,face = "bold", size = 15),
        legend.key = element_rect(fill = NA),
        title = element_text(size = 10),
        legend.title = element_text(size=15),
        legend.text = element_text(size = 15),
        plot.margin = ggplot2::margin(t = 0, r = 0, b = 0, l = 0.04, "cm")) +
  coord_sf(y=c(-28, 30))

#map1

###Restricted vs Open Access
RLS_sites_st_MU = st_as_sf(predicted_OA_MPA, coords = c("SiteLongitude", "SiteLatitude")) %>% 
  st_set_crs(4326)

# ##Stats
# RLS_sites_st_MU_stats = RLS_sites_st_MU %>% 
#   st_drop_geometry() %>% 
#   mutate(is_positive = if_else(ABS_change>0, 1, 0)) %>% 
#   count(is_positive)

rows <- sample(nrow(RLS_sites_st_MU))
RLS_sites_st_MU <- RLS_sites_st_MU[rows, ]


##Simple plot prod per EEEZ
map2 = ggplot() +
  geom_sf(data = world_shp,
          fill = "grey85", 
          color = "grey90",
          size = 0.05) +
  geom_sf(data = RLS_sites_st_MU,
          shape=21, 
          alpha = 0.6,
          aes(fill=bio_log),
          size = 2) +
  scale_fill_gradientn(name=expression(paste("Biomass\nlog (kg ha"^-1,")")), 
                       colors = RColorBrewer::brewer.pal(9, "PuBu"), 
                       na.value = "grey87",
                       breaks = c(2, 6, 9),
                       labels = c("2", "6", "9")) +
  guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black", barwidth = 1, barheight = 3)) +
  labs(title = "Sustainable-use MPAs") +
  theme(panel.grid.major = element_blank(), 
        panel.background = element_rect(fill = "aliceblue"),
        panel.border = element_rect(colour = "black", fill = NA),
        plot.title = element_text(hjust = 0.5,face = "bold", size = 15),
        legend.key = element_rect(fill = NA),
        title = element_text(size = 10),
        legend.title = element_text(size = 15),
        legend.text = element_text(size = 15),
        plot.margin = ggplot2::margin(t = -3, r = 0, b = 0, l = 0.04, "cm")) +
  coord_sf(y=c(-28, 30))

##Boxplots

p1 = ggplot(data = RLS_sites_st_OA)+
  geom_boxplot(aes(y = bio_log))+
  labs(y = "")+
  theme_bw()+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y = element_text(size = 15),
        plot.margin = ggplot2::margin(t = 1.85, r = 0.1, b = 1.53, l = 0.5, "cm"))
#p1

p2 = ggplot(data = RLS_sites_st_MU)+
  geom_boxplot(aes(y = bio_log))+
  labs(y = "")+
  theme_bw()+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y = element_text(size = 15),
        plot.margin = ggplot2::margin(t = 0.35, r = 0.1, b = 3, l = 0.4, "cm"))

#p2

###Posterior distributions
# posterior_dist <- read_csv("Outputs/posterior_dist.csv")
# 
# p3 = ggplot(data = posterior_dist) +
#   geom_density(aes(b_is_mpa), fill = "deepskyblue") + 
#   #labs(x = expression(paste("Change in\nbiomass\nlog (kg ha"^-1,")")), y = "Density") +
#   labs(x = "Change in\nbiomass\nlog (kg/ha)", y = "Density") +
#   geom_vline(xintercept = 0.54, size = 1.2) +
#   scale_x_continuous(breaks = c(0, 0.54, 1), labels = c("0","0.5", "1"))+
#   theme_bw() +
#   theme(axis.title = element_text(size = 15, vjust = 0.8),
#         axis.text = element_text(size = 15),
#         plot.margin = ggplot2::margin(t = 1.9, r = 0, b = 3, l = 0.4, "cm"))

perc_change_OA_MPA <- read_csv("Outputs/predicted_OA_MPA_bayes_perc_change.csv")

p3 = ggplot(data = perc_change_OA_MPA) +
  geom_density(aes(x = pred_change, y = ..scaled..), fill = "deepskyblue") + 
  #labs(x = expression(paste("Change in\nbiomass\nlog (kg ha"^-1,")")), y = "Density") +
  labs(x = "Difference in\nbiomass (%)", y = "Density") +
  geom_vline(xintercept = 0, size = 1.2, linetype="dashed") +
  scale_x_continuous(breaks = c(0, 15, 30), labels = c("0","15", "30"))+
  scale_y_continuous(breaks = c(0, 0.5, 1), labels = c("0.0", "0.5", "1.0")) +
  theme_bw() +
  theme(axis.title = element_text(size = 15, vjust = 0.8),
        axis.text = element_text(size = 15),
        plot.margin = ggplot2::margin(t = 1.9, r = 0, b = 3, l = 0.4, "cm"))

##########Figure 1####
p.final = ggarrange(p1, map1, p2, map2, 
          ncol=2, 
          nrow=2,
          widths = c(0.3, 2), 
          heights = c(2,2), 
          labels = c("", "B", "", "C"), 
          font.label = list(size = 20, color = "black", face = "bold"),
          label.y = c(0.9, 0.85, 1, 1.1), 
          label.x = c(-0.01))

p.final2 = ggarrange(p3, p.final, 
                     ncol=2, 
                     nrow=1,
                     widths = c(0.5, 2), 
                     heights = c(2,2), 
                     labels = c("A", ""), 
                     font.label = list(size = 20, color = "black", face = "bold"),
                     label.y = c(0.91), 
                     label.x = c(0.2))

ggsave(filename = "Figures/Figure1_posterior.pdf", 
       plot = p.final2,
       height = 4.5, 
       width = 8, dpi=600, 
       device=cairo_pdf)

ggsave(filename = "Figures/Figure1_posterior.jpeg", 
       plot = p.final2,
       height = 4.5, 
       width = 10)

