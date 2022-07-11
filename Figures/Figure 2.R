library(tidyverse)
library(readr)
library(ggpubr)
library(cowplot)
library(ggnewscale)
library(ggrepel)

# Clear workspace
rm(list = ls())

MPA_population_MU <- read_csv("data/MPA_population_sent_buffer_1_30_atlas.csv") %>% 
  mutate(governance = "Fishing-permitted")
MPA_population_NT <- read_csv("data/MPA_population_sent_buffer_1_30_atlas_NT.csv") %>% 
  mutate(governance = "No-fishing")
colnames(MPA_population_NT) = names(MPA_population_MU)

MPA_pop = rbind(MPA_population_MU, MPA_population_NT) %>% 
  reshape2::melt(id.vars = c("iso3c", "governance")) %>% 
  rename(buffer = variable,
         pop = value)

reef_pop <- read_csv("Outputs/reef_pop_final.csv") %>% 
  mutate(governance = "NonMPA")

all_pop = rbind(MPA_pop, reef_pop)

sevs = read_csv("data/2017_perc_pop_deficient.csv") %>% 
  mutate(nutrient = dplyr::recode(nutrient, 
                                  "Vitamin B-12" = "Vitamin B12",
                                  "Vitamin A, RAE" = "Vitamin A")) %>% 
  group_by(iso3) %>% 
  summarise(intake = mean(perc_deficient))

reef_MPA_area_all_spread <- read_csv("data/reef_MPA_area_all_spread.csv") %>% 
  mutate(OA_area_log = log(open_access),
         OA_area_log = if_else(OA_area_log<1, 1, OA_area_log)) %>% 
  select(iso3c, OA_area_log)
  
reef_MPA_benefits <- read_csv("Outputs/reef_MPA_benefits_bayes.csv") %>% 
  left_join(sevs, by=c("iso3c" = "iso3")) %>% 
  left_join(all_pop) %>%
  left_join(reef_MPA_area_all_spread) %>% 
  mutate(perc_change_MU = if_else(relative_catch_MU>1, 100*(relative_catch_MU-1), -1*100*(1-relative_catch_MU)),
         perc_change_MU_low = if_else(relative_catch_MU_low>1, 100*(relative_catch_MU_low-1), -1*100*(1-relative_catch_MU_low)),
         perc_change_MU_high = if_else(relative_catch_MU_high>1, 100*(relative_catch_MU_high-1), -1*100*(1-relative_catch_MU_high)),
         perc_change_OA = 100*(all_catch/OA_all_catch-1),
         log_pop = log(pop),
         log_pop = if_else(log_pop==-Inf, 0, log_pop),
         log10_pop = log10(pop),
         log10_pop = if_else(log10_pop==-Inf, 0, log10_pop),
         reef_area2 = reef_area/4)

fig1 = ggplot(data = reef_MPA_benefits %>% filter(buffer=="10", governance=="Fishing-permitted"), aes(y=perc_change_MU, x=log_pop, size=reef_area, fill=intake, label = iso3c)) +
  geom_linerange(aes(ymin = perc_change_MU_low, ymax = perc_change_MU_high), size = 1, color = "black", alpha = 0.5, position = position_dodge(0.5)) +
  geom_point(shape=21) +
  geom_text_repel(min.segment.length = 0.5, seed = 42, box.padding = 1, size = 5) +
  #geom_vline(xintercept = 0, linetype="dashed") + 
  #geom_hline(yintercept = 1, linetype="dashed") +
  #scale_color_gradientn(name="Inadequate \nintake (%)", colors=RColorBrewer::brewer.pal(9, "Reds"), na.value = "grey", guide = "none") +
  scale_fill_gradientn(name="Inadequate \nintake (%)", colors=RColorBrewer::brewer.pal(9, "Reds"), na.value = "grey") +
  guides(fill = guide_colorbar(ticks.colour = "black", 
                               frame.colour = "black", 
                               barwidth = 1, 
                               barheight = 5, 
                               order = 2),
         #color = guide_legend(show = F),
         size = guide_legend(override.aes = aes(label = ""), title = expression(paste(" Reef area\n  log    (km"^2,")")))) +
  scale_x_continuous(breaks=c(2.3, 4.6, 6.9, 9.2, 11.5, 13.8), 
                     labels=c("10", "100", "1,000", "10,000", "100,000", "1,000,000")) +
  labs(y="Expected change in catch (%)", 
       x = "Population near Sustainable-Use MPAs") +
  #ylim(0, 400) +
  #xlim(0, 20)+
  theme_bw() +
  theme(plot.margin = ggplot2::margin(t = 1.5, r = 0.5, b = 0.1, l = 1.65, "cm"),
        axis.title = element_text(size = 23),
        axis.text = element_text(size = 15),
        plot.title = element_text(hjust = 0.5, size = 17),
        legend.title.align = 0,
        legend.text = element_text(size = 20),
        legend.title = element_text(size = 20),
        legend.margin = margin(t = 0.5, r = 0, b = 1, l = 0, "cm"))
fig1

ggsave(filename = "Figures/Figure2_bayes.jpeg",
       plot = fig1,
       height = 6, 
       width = 11)

ggsave(fig1, filename = "Figures/Figure3_bayes.pdf", 
       width=11, height=6, units="in", dpi=600, device=cairo_pdf)

