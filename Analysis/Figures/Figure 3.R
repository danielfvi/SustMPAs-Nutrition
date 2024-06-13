####MPA and Nutrition Project
####Figure 3
####Author: Daniel Viana
####Date: June 2023

library(tidyverse) 
library(gdata)
library(ggpubr)
library(cowplot)
library(ggnewscale)
library(ggrepel)
library(sf)
library(scatterpie)

# Clear workspace
rm(list = ls())


###Set of Maps
##1)Unprotected Reef Area / % of reef unprotected / change in catch
##2) # of people around unprotected reefs (10km buffer) / per capita seafood consumption
##3) Current SEV
##3) Combined change in SEV


#Load world map
world <- rnaturalearth::ne_countries("small", returnclass = "sf")

# Extract French Guiana
fguiana <-world %>% 
  sf::st_cast(to="POLYGON") %>% 
  filter(gu_a3=="FRA") %>% 
  mutate(id=1:n()) %>% 
  select(id) %>% 
  filter(id==1)

#World centroids
world_lg <- rnaturalearth::ne_countries(scale="large", returnclass = "sf") %>% 
  mutate(area_sqkm=sf::st_area(.)/(1000*1000)) %>%
  mutate(area_sqkm=as.numeric(area_sqkm)) %>% 
  sf::st_centroid() %>% 
  select(continent, subunit, su_a3, area_sqkm) %>% 
  rename(country=subunit, iso3=su_a3) 

# Small nation centroids
world_tiny <- rnaturalearth::ne_countries(type="tiny_countries", returnclass = "sf") %>% 
  select(continent, subunit, su_a3) %>% 
  rename(country=subunit, iso3=su_a3) %>% 
  mutate(area_sqkm=10)
  
# Merge centroids
world_centers <- bind_rows(world_lg, world_tiny)

# Base theme
base_theme <- theme(axis.text=element_blank(),
                    axis.title=element_blank(),
                    legend.text=element_text(size=10),
                    legend.title=element_text(size=10),
                    strip.text=element_blank(),
                    plot.title=element_text(size=7),
                    panel.grid.major = element_blank(), 
                    panel.grid.minor = element_blank(),
                    panel.background = element_blank(), 
                    axis.line = element_line(colour = "black"),
                    # Legend
                    legend.position=c(0.11,0.35),
                    legend.background = element_rect(fill=alpha('blue', 0)))


##1)Unprotected Reef Area / % of reef unprotected / change in catch
reef_MPA_area_all_spread <- read_csv("data/reef_MPA_area_all_spread.csv") %>% 
  mutate(OA_area_log = log(open_access),
         OA_area_log = if_else(OA_area_log<1, 1, OA_area_log),
         reef_area = multiple_use + no_take + open_access,
         reef_area_log = log(reef_area),
         `Non-MPA area` = perc_OA,
         `MPA area` = 100 - perc_OA)

reef_MPA_benefits <- read_csv("Outputs/reef_MPA_benefits_bayes.csv") %>% 
  mutate(perc_change = 100*(all_catch/OA_all_catch-1)) %>% 
  select(iso3c, perc_change) %>% 
  left_join(reef_MPA_area_all_spread) %>% 
  select(iso3c, perc_change, OA_area_log, perc_OA, `Non-MPA area`, `MPA area`) %>% 
  mutate(perc_change = if_else(perc_change>200, 200, perc_change))

# Format data
mpa_dta_sf <- world %>% 
  left_join(reef_MPA_benefits, by=c("gu_a3"="iso3c"))

# Spatialize tiny
sdata_pt <- world_centers %>% 
  left_join(reef_MPA_benefits, by=c("iso3"="iso3c")) %>% 
  # Reduce to ones with data
  filter(!is.na(perc_change)) %>% 
  arrange(area_sqkm) %>% 
  # Reduce to small
  filter(area_sqkm<=2.5*10^4 & continent!="Europe")

# Open Access reef area
plot1 <- ggplot(mpa_dta_sf) +
  geom_sf(mapping=aes(fill=OA_area_log), lwd=0.1) +
  # Plot small places
  geom_sf(data=sdata_pt, mapping=aes(fill=OA_area_log), shape=21, size=3, stroke=0.3) +
  # Plot French Guiana
  geom_sf(data=fguiana, lwd=0.1, color="grey30", fill="grey80") +
  # Crop out Antarctica
  coord_sf(y=c(-55, NA)) +
  # Legend and labels
  scale_fill_gradient2(name="Non-MPA \nreef area   \n(km2)", 
                       breaks=c(2.5, 5.01, 7.31), labels=c("15", "150", "1500"),
                       low="navy", high="darkred", mid="white", midpoint=0) +
  guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black", barwidth = 0.8, barheight = 3)) +
  # Theme
  labs(title = "A") +
  theme_bw() + base_theme +
  theme(legend.position="left",
        axis.text = element_blank(),
        axis.title=element_blank(), 
        legend.background = element_rect(fill=alpha('blue', 0)),
        plot.title = element_text(face = "bold", size = 13))
#plot1


##Change in catch
plot2 <- ggplot(mpa_dta_sf) +
  geom_sf(mapping=aes(fill=perc_change), lwd=0.1) +
  # Plot small places
  geom_sf(data=sdata_pt, mapping=aes(fill=perc_change), shape=21, size=3, stroke=0.3) +
  # Plot French Guiana
  geom_sf(data=fguiana, lwd=0.1, color="grey30", fill="grey80") +
  # Crop out Antarctica
  coord_sf(y=c(-55, NA)) +
  # Legend and labels
  scale_fill_gradient2(name="Predicted  \nchange\nin catch     \n(%)  ", 
                       #breaks=seq(0, 400, 100), labels=c("0", "100", "200", "300", ">400"),
                       breaks=seq(0, 20, 5), labels=c("0", "5", "10", "15", "20"),
                       low="navy", high="darkred", mid="white", midpoint=0) +
  guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black", barwidth = 0.8, barheight = 3)) +
  # Theme
  labs(title = "B") +
  theme_bw() + base_theme +
  theme(legend.position="right",
        axis.text = element_blank(),
        axis.title=element_blank(), 
        legend.background = element_rect(fill=alpha('blue', 0)),
        plot.title = element_text(face = "bold", size = 13))
#plot2


### of people around unprotected reefs (10km buffer)
sevs = read_csv("data/2017_perc_pop_deficient.csv") %>% 
  group_by(iso3) %>% 
  summarise(intake = mean(perc_deficient))

reef_pop <- read_csv("Outputs/reef_pop_final.csv")

seafood_consump = read_csv("Outputs/reef_MPA_benefits_bayes.csv") %>% 
  left_join(sevs, by=c("iso3c" = "iso3")) %>% 
  left_join(reef_pop) %>%
  mutate(change_seafood = ((all_catch - OA_all_catch)*0.8)/pop,
         change_seafood = if_else(change_seafood>0.3, 0.3, change_seafood),
         change_seafood = if_else(change_seafood<0, 0, change_seafood),
         sev_cat = cut(intake, 
                       breaks = c(0, 25, 100),
                       labels = c("Adequate", "Inadequate")),
         sev_cat = as.character(sev_cat),
         pop_log = log(pop)*100) %>% 
  drop_na(sev_cat) %>% 
  select(iso3c, intake, sev_cat, buffer, all_catch, OA_all_catch, pop, pop_log, change_seafood) %>% 
  filter(buffer == 10)

# Format data
mpa_pop_sf <- world %>% 
  left_join(seafood_consump, by=c("gu_a3"="iso3c"))

# Spatialize tiny
sdata_pop_pt <- world_centers %>% 
  left_join(seafood_consump, by=c("iso3"="iso3c")) %>% 
  # Reduce to ones with data
  filter(!is.na(pop)) %>% 
  arrange(area_sqkm) %>% 
  # Reduce to small
  filter(area_sqkm<=2.5*10^4 & continent!="Europe")

plot3 <- ggplot(mpa_pop_sf) +
  geom_sf(mapping=aes(fill=pop_log), lwd=0.1) +
  # Plot small places
  geom_sf(data=sdata_pop_pt, mapping=aes(fill=pop_log), shape=21, size=3, stroke=0.3) +
  # Plot French Guiana
  geom_sf(data=fguiana, lwd=0.1, color="grey30", fill="grey80") +
  # Crop out Antarctica
  coord_sf(y=c(-55, NA)) +
  # Legend and labels
  scale_fill_gradient2(name="People near\nnon-MPA\nreefs\n(millions) ", 
                       breaks=seq(800, 2000, 300), labels=c("", "0.06", "1", "25", " "),
                       low="navy", high="darkred", mid="white", midpoint=800) +
  guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black", barwidth = 0.8, barheight = 3)) +
  # Theme
  labs(title = "C") +
  theme_bw() + base_theme +
  theme(legend.position="left",
        axis.text = element_blank(),
        axis.title=element_blank(), 
        legend.background = element_rect(fill=alpha('blue', 0)),
        legend.title = element_text(size = 9),
        plot.title = element_text(face = "bold", size = 13))
#plot3

### per capita seafood consumption
plot4 <- ggplot(mpa_pop_sf) +
  geom_sf(mapping=aes(fill=change_seafood), lwd=0.1) +
  # Plot small places
  geom_sf(data=sdata_pop_pt, mapping=aes(fill=change_seafood), shape=21, size=3, stroke=0.3) +
  # Plot French Guiana
  geom_sf(data=fguiana, lwd=0.1, color="grey30", fill="grey80") +
  # Crop out Antarctica
  coord_sf(y=c(-55, NA)) +
  # Legend and labels
  scale_fill_gradient2(name="Change in\nper capita\nseafood\nconsumption\n(kg/p/yr)", 
                       breaks=c(0, 0.1, 0.2, 0.28), labels=c("0.0", "0.1", "0.2", ">0.3"),
                       low="navy", high="darkred", mid="white", midpoint=0) +
  guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black", barwidth = 0.8, barheight = 3)) +
  # Theme
  labs(title = "D") +
  theme_bw() + base_theme +
  theme(legend.position="right",
        axis.text = element_blank(),
        axis.title=element_blank(), 
        legend.background = element_rect(fill=alpha('blue', 0)),
        legend.title = element_text(size = 9),
        plot.title = element_text(face = "bold", size = 13))
#plot4


###Current SEVs
### per capita seafood consumption
plot5 <- ggplot(mpa_pop_sf) +
  geom_sf(mapping=aes(fill=intake), lwd=0.1) +
  # Plot small places
  geom_sf(data=sdata_pop_pt, mapping=aes(fill=intake), shape=21, size=3, stroke=0.3) +
  # Plot French Guiana
  geom_sf(data=fguiana, lwd=0.1, color="grey30", fill="grey80") +
  # Crop out Antarctica
  coord_sf(y=c(-55, NA)) +
  # Legend and labels
  scale_fill_gradient2(name="Inadequate \nintake (%)", 
                       #breaks=seq(0, 10, 2), labels=c("0", "2", "4", "6", "8", ">10"),
                       low="navy", high="darkred", mid="white", midpoint=0) +
  guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black", barwidth = 0.8, barheight = 3)) +
  # Theme
  labs(title = "E") +
  theme_bw() + base_theme +
  theme(legend.position="left",
        axis.text = element_blank(),
        axis.title=element_blank(), 
        legend.background = element_rect(fill=alpha('blue', 0)),
        legend.title = element_text(size = 9),
        plot.title = element_text(face = "bold", size = 13))
#plot5

###Delta SEVs
delta_sevs <- read_csv("Outputs/2017_sevs_base_high_MPA_buffer_all_bayes.csv") %>%
  group_by(iso3, country, buffer, nutrient) %>%
  summarise(sev_base = mean(sev_base),
            sev_high = mean(sev_high)) %>%
  mutate(sev_delta=sev_high - sev_base,
         perc_change_SEV = 100*(sev_base - sev_high)/sev_base) %>% 
  mutate(nutrient = recode(nutrient, 
                           "Omega-3 fatty acids" = "DHA + EPA",
                           "Vitamin A, RAE" = "Vitamin A")) %>% 
  left_join(sevs) %>% 
  mutate(sev_cat = cut(intake, 
                       breaks = c(0, 25, 100),
                       labels = c("Adequate", "Inadequate")),
         sev_cat = as.character(sev_cat)) %>% 
  drop_na(sev_cat) %>% 
  filter(buffer == 10) %>% 
  group_by(iso3) %>% 
  summarise(sev_delta = sum(sev_delta)) %>% 
  mutate(sev_delta = if_else(sev_delta>-0.5, sev_delta, -0.5))

# Format data
mpa_SEV_sf <- world %>% 
  left_join(delta_sevs, by=c("gu_a3"="iso3"))

# Spatialize tiny
sdata_SEV_pt <- world_centers %>% 
  left_join(delta_sevs, by=c("iso3"="iso3")) %>% 
  # Reduce to ones with data
  filter(!is.na(sev_delta)) %>% 
  arrange(area_sqkm) %>% 
  # Reduce to small
  filter(area_sqkm<=2.5*10^4 & continent!="Europe")

### per capita seafood consumption
plot6 <- ggplot(mpa_SEV_sf) +
  geom_sf(mapping=aes(fill=sev_delta), lwd=0.1) +
  # Plot small places
  geom_sf(data=sdata_SEV_pt, mapping=aes(fill=sev_delta), shape=21, size=3, stroke=0.3) +
  # Plot French Guiana
  geom_sf(data=fguiana, lwd=0.1, color="grey30", fill="grey80") +
  # Crop out Antarctica
  coord_sf(y=c(-55, NA)) +
  # Legend and labels
  scale_fill_gradient2(name="Change in   \ninadequate\nintake (%)   ", 
                       breaks=c(-0.5, -0.3, -0.1), labels=c("< -0.5", "-0.3", "0.1"),
                       low="navy", high="darkred", mid="white", midpoint=0) +
  guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black", barwidth = 0.8, barheight = 3)) +
  # Theme
  labs(title = "F") +
  theme_bw() + base_theme +
  theme(legend.position="right",
        axis.text = element_blank(),
        axis.title=element_blank(), 
        legend.background = element_rect(fill=alpha('blue', 0)),
        legend.title = element_text(size = 9),
        plot.title = element_text(face = "bold", size = 13))
#plot6


# Merge maps
g <- gridExtra::grid.arrange(plot1, plot2,
                             plot3, plot4,
                             plot5, plot6, 
                             ncol=2)

# Export
ggsave(g, filename = "Manuscript figures/Fig 3.pdf", 
       width=10, height=6.2, units="in", dpi=600, device=cairo_pdf)

ggsave(g, filename = "Manuscript figures/Fig 3.jpeg", 
       width=9, height=6)

