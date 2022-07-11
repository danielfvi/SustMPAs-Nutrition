####Sisitivity to harvaest rate

library(tidyverse)

# Clear workspace
rm(list = ls())


sensit_harvest_sev <- read_csv("Outputs/sensit_harvest_sev.csv") %>% 
  mutate(nutrient = recode(nutrient, 
                           "Omega-3 fatty acids" = "DHA+EPA",
                           "Vitamin B-12" = "Vitamin B12",
                           "Vitamin A, RAE" = "Vitamin A"))


ggplot(data = sensit_harvest_sev) + 
  geom_line(aes(y=ndeficient_diff, x = harvest_msy), size = 2) +
  facet_wrap(~nutrient, scales = "free") +
  labs(y = "Millions of people", x = "MSY Harvest rate") +
  theme_classic() +
  theme(axis.title = element_text(size = 15),
        axis.text = element_text(size = 15))

p1 = ggplot(data = sensit_harvest_sev) + 
  geom_line(aes(y=ndeficient_diff, x = harvest_msy*2, color = nutrient), size = 2) +
  #facet_wrap(~nutrient, scales = "free") +
  labs(y = "Millions of people", x = "Population growth rate (r)", color = "") +
  theme_classic() +
  theme(axis.title = element_text(size = 15),
        axis.text = element_text(size = 15),
        legend.text = element_text(size = 15))
p1

# Export
ggsave(p1, filename = "Figures/Sensit_r.pdf", 
       width=10, height=6.2, units="in", dpi=600, device=cairo_pdf)

ggsave(p1, filename = "Figures/Sensit_r.jpeg", 
       width=10, height=6.2)

####Buffer sensitivity

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
  group_by(nutrient, buffer) %>%
  summarise(ndeficient_diff = sum(ndeficient_diff2)/1000000)

p2 = ggplot(data = impacted_pop) + 
  geom_line(aes(y=ndeficient_diff, x = buffer, color = nutrient), size = 2) +
  #facet_wrap(~nutrient, scales = "free") +
  labs(y = "Millions of people", x = "Buffer around MPAs", color = "") +
  theme_classic() +
  theme(axis.title = element_text(size = 15),
        axis.text = element_text(size = 15),
        legend.text = element_text(size = 15))
p2
# Export
ggsave(p2, filename = "Figures/Sensit_pop.pdf", 
       width=10, height=6.2, units="in", dpi=600, device=cairo_pdf)

ggsave(p2, filename = "Figures/Sensit_pop.jpeg", 
       width=10, height=6.2)

impacted_sevs <- read_csv("Outputs/impacted_pop_SEV_bayes.csv") %>% 
  group_by(iso3, nutrient, buffer) %>% 
  summarise(ndeficient_diff = sum(ndeficient_diff, na.rm = T)) %>% 
  ungroup() %>% 
  left_join(reef_pop, by = c("iso3" = "iso3c", "buffer")) %>%
  mutate(final_pop = if_else(ndeficient_diff>pop, pop, ndeficient_diff)) %>% 
  group_by(nutrient, buffer) %>%
  summarise(ndeficient_diff = sum(final_pop)/1000000) %>% 
  spread(buffer, ndeficient_diff) %>% 
  mutate(nutrient = recode(nutrient, 
                           "Omega-3 fatty acids" = "DHA + EPA",
                           "Vitamin A, RAE" = "Vitamin A")) %>% 
  ungroup()

###Bmsy sensitivity
sensit_bmsy_sev <- read_csv("Outputs/sensit_bmsy_sev.csv") %>% 
  mutate(nutrient = recode(nutrient, 
                           "Omega-3 fatty acids" = "DHA + EPA",
                           "Vitamin A, RAE" = "Vitamin A"))

p3 = ggplot(data = sensit_bmsy_sev) + 
  geom_line(aes(y=ndeficient_diff, x = bmsy_q, color = nutrient), size = 2) +
  #facet_wrap(~nutrient, scales = "free") +
  labs(y = "Millions of people", x = "Bmsy biomass quantile", color = "") +
  theme_classic() +
  theme(axis.title = element_text(size = 15),
        axis.text = element_text(size = 15),
        legend.text = element_text(size = 15))
p3
ggsave(p3, filename = "Figures/Sensit_Bmsy.pdf", 
       width=10, height=6.2, units="in", dpi=600, device=cairo_pdf)

ggsave(p3, filename = "Figures/Sensit_Bmsy.jpeg", 
       width=7, height=5)
