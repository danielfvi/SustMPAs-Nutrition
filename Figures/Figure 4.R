####MPA and Nutrition Project
####Figure 2
####Author: Daniel Viana
####Date: June 2021

library(tidyverse) 
library(gdata)
library(ggpubr)
library(cowplot)
library(ggnewscale)
library(ggrepel)
library(sf)

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
                    legend.text=element_text(size=5),
                    legend.title=element_text(size=5),
                    strip.text=element_blank(),
                    plot.title=element_text(size=7),
                    panel.grid.major = element_blank(), 
                    panel.grid.minor = element_blank(),
                    panel.background = element_blank(), 
                    axis.line = element_line(colour = "black"),
                    # Legend
                    legend.position=c(0.11,0.35),
                    legend.background = element_rect(fill=alpha('blue', 0)))

reef_pop_iso <- read_csv("Outputs/reef_pop_iso.csv")


##Impacted pop
impacted_pop <- read_csv("Outputs/impacted_pop_SEV_bayes.csv") %>% 
  group_by(iso3, country, buffer, nutrient) %>% 
  summarise(ndeficient_diff = sum(ndeficient_diff, na.rm=T)) %>% 
  #Add reef population
  left_join(reef_pop_iso, by=c("iso3" = "iso3c", "buffer")) %>% 
  mutate(nutrient = recode(nutrient, 
                           "Omega-3 fatty acids" = "DHA+EPA",
                           "Vitamin B-12" = "Vitamin B12"),
         ndeficient_diff2 = if_else(ndeficient_diff<pop, ndeficient_diff, pop),
         ndeficient_diff_log = log(ndeficient_diff2),
         ndeficient_diff_log = if_else(ndeficient_diff_log<1, 1, ndeficient_diff_log)) %>%
  filter(buffer == 10)


### per capita seafood consumption
plot_func = function(nut){
  
  if(nut %in% c("DHA+EPA", "Iron", "Calcium")){
    legend.pos = "left"
  }else{
    legend.pos = "right"
  }
  
  # Format data
  mpa_SEV_sf <- world %>% 
    left_join(impacted_pop %>% filter(nutrient == nut), by=c("gu_a3"="iso3"))
  
  # Spatialize tiny
  sdata_SEV_pt <- world_centers %>% 
    left_join(impacted_pop %>% filter(nutrient == nut), by=c("iso3"="iso3")) %>% 
    # Reduce to ones with data
    filter(!is.na(ndeficient_diff_log)) %>% 
    arrange(area_sqkm) %>% 
    # Reduce to small
    filter(area_sqkm<=2.5*10^4 & continent!="Europe")
  
  # Get breaks and labels
  #breaks <- breaks_list[[nut]]
  #labels <- labels_list[[nut]]
  
  # Build title
  nutr_do_use <- nut
  if(nut=="Vitamin A, RAE"){
    nutr_do_use <- "Vitamin A"
  }
  if(nut=="Vitamin B12"){
    nutr_do_use <- expression("Vitamin B"["12"])
  }
  
  p <- ggplot(mpa_SEV_sf) +
    geom_sf(mapping=aes(fill=ndeficient_diff_log), lwd=0.1) +
    # Plot small places
    geom_sf(data=sdata_SEV_pt, mapping=aes(fill=ndeficient_diff_log), shape=21, size=3, stroke=0.3) +
    # Plot French Guiana
    geom_sf(data=fguiana, lwd=0.1, color="grey30", fill="grey80") +
    # Crop out Antarctica
    coord_sf(y=c(-55, NA)) +
    # Legend and labels
    scale_fill_gradient2(name="Number of\npeople", 
                         breaks=c(2.3, 4.6, 6.9, 9.2, 11.5), 
                         labels=c("10", "100", "1,000", "10,000", "100,000"),
                         low="navy", high="darkred", mid="white", midpoint=0) +
    guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black", barwidth = 0.8, barheight = 3)) +
    # Theme
    labs(title = nutr_do_use) +
    theme_bw() + base_theme +
    theme(legend.position=legend.pos,
          axis.text = element_blank(),
          axis.title=element_blank(), 
          legend.background = element_rect(fill=alpha('blue', 0)),
          legend.title = element_text(size = 10),
          legend.text = element_text(size = 10),
          plot.title = element_text(size = 13))
  
  return(p)
}

plot1 <- plot_func(nut = "DHA+EPA")
plot2 <- plot_func(nut = "Vitamin B12")
plot3 <- plot_func(nut = "Iron")
plot4 <- plot_func(nut = "Zinc")
plot5 <- plot_func(nut = "Calcium")
plot6 <- plot_func(nut = "Vitamin A, RAE")

# Merge maps
g <- gridExtra::grid.arrange(plot1, plot2,
                             plot3, plot4,
                             plot5, plot6, ncol=2)

# Export
ggsave(g, filename = "Figures/All_nutrients_bayes_Npeople_brms.pdf", 
       width=10, height=6.2, units="in", dpi=600, device=cairo_pdf)

ggsave(g, filename = "Figures/All_nutrients_bayes_Npeople_brms.jpeg", 
       height = 6.2, 
       width = 10)
