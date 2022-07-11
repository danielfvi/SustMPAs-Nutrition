####MPA and Nutrition Project
####Step 2 - Random Forest predictions 
####Author: Daniel Viana
####Date: June 2021

#####Load Packages
library(tidyverse) # for general data wrangling and plotting
library(gdata)

# Clear workspace
rm(list = ls())

##Read SAU data
sau_2014 = read_csv("G:/My Drive/MPAs and Human Nutrition/Data/SAU data/complete data/SAU raw database by EEZ 2014.csv")

#Lead country iso information
country_iso <- read_csv("data/countries_ISO.csv")

##Join SAU with country ISO
sau_2014 = left_join(sau_2014, country_iso, by = c("fishing_entity" = "missing_countries"))

##Load biomass and catch prediction data
reef_MPA_benefits <- read_csv("Outputs/reef_MPA_benefits_bayes.csv")

biomass_catch_restricted = reef_MPA_benefits %>% 
  dplyr::select(iso3c, all_catch, OA_all_catch)

#############################Assign catch to species######################################### 

##Spp composition calculations

##Filter for reef fish species catch
spp_reef = sau_2014 %>% 
  filter(functional_group %in% c("Large reef assoc. fish (>=90 cm)",
                                 "Small reef assoc. fish (<30 cm)",
                                 "Medium reef assoc. fish (30 - 89 cm)")) %>% 
  group_by(iso3c, scientific_name) %>% 
  summarise(tonnes = sum(tonnes)) %>% 
  mutate(scientific_name = recode(scientific_name, "moolgarda seheli" = "crenimugil seheli"))

taxa_table = readRDS("~/AFCD/data-raw/taxa-table/taxa_table.Rds")

all_spp = spp_reef %>%
  ungroup() %>%
  select(scientific_name) %>%
  unique() %>%
  mutate(scientific_name = tolower(scientific_name),
         scientific_name = recode(scientific_name, "moolgarda seheli" = "crenimugil seheli")) %>%
  separate(scientific_name, c("genus", "spp"), " ", remove=FALSE) %>%
  left_join(taxa_table, by = c("genus"))

#Fill taxa by family
missing_family = all_spp %>%
  filter(is.na(class)) %>%
  select(-order, -class, -phylum, -kingdom) %>%
  left_join(taxa_table %>% select(-genus) %>% distinct(family, .keep_all=T), by = c("genus" = "family"))

all_spp = all_spp %>%
  filter(!is.na(class)) %>%
  rbind(missing_family)

#Fill taxa by order
missing_order = all_spp %>%
  filter(is.na(class)) %>%
  select(-class, -phylum, -kingdom) %>%
  left_join(taxa_table %>% select(-genus, -family) %>% distinct(order, .keep_all=T), by = c("genus" = "order"))

all_spp = all_spp %>%
  filter(!is.na(class)) %>%
  rbind(missing_order) %>%
  mutate(genus = recode(genus,
                        "osteichthyes" = "actinopterygii",
                        "elasmobranchii" = "chondrichthyes"))

##Filter with families from RLS/AGGRA
##Load RLS biomass data
RLS_data <- read_csv("Data/RLS_data.csv") %>%
  mutate(family = tolower(family))

families = unique(RLS_data$family)

all_spp = all_spp %>%
  filter(family %in% families)

spp_list = all_spp %>%
  ungroup() %>%
  select(scientific_name) %>%
  unique()

#write.csv(spp_list, "Outputs/SAU_spp_list.csv", row.names = F)

spp_vec = unique(spp_list$scientific_name)

spp_reef = spp_reef %>%
  mutate(scientific_name = tolower(scientific_name)) %>% 
  filter(scientific_name %in% spp_vec)

##Get unique reef countries
SAU_reef_countries = unique(spp_reef$iso3c)

##Calculate the proportion of catch for each species
for(n in 1:length(SAU_reef_countries)){
  reef_country = spp_reef %>% filter(iso3c == SAU_reef_countries[n])
  reef_country_sum = sum(reef_country$tonnes)
  reef_country = reef_country %>% 
    mutate(prop.catch = tonnes/reef_country_sum)
  if(n==1){
    prop_reef = reef_country
  }else{
    prop_reef = rbind(prop_reef, reef_country)
  }
}

#########Assign nutrient content

##Load nutrient composition data for reef species
spp_nutrient <- read_csv("Outputs/SAU_spp_nutrients.csv")

##Join species proportions data and delta catch
prop_reef_delta = left_join(prop_reef, biomass_catch_restricted)

##Assign catch to particular species
prop_reef_delta = prop_reef_delta %>% 
  mutate(spp_all_catch = prop.catch*all_catch,
         spp_OA_all_catch = prop.catch*OA_all_catch)

##Join nutrient composition and catch tables
delta_nutrition = prop_reef_delta %>% 
  mutate(scientific_name = tolower(scientific_name)) %>% 
  left_join(spp_nutrient)

##CAlculate nutrient supply of MPAs, multiplying first by the edible portion (assumed to be 0.8)
delta_nutrition = delta_nutrition %>% 
  mutate(all_nut_supply = value*spp_all_catch*0.7*10/365,
         OA_all_nut_supply = value*spp_OA_all_catch*0.7*10/365) %>% ####0.7 is the assumed edible portion and 10 is the conversion and units are in (g or mg or μg)/100g, so we need to multiply by 10 for kg
  group_by(iso3c, nutrient) %>% 
  summarise(total_supply_all = sum(all_nut_supply),
            total_supply_OA_all = sum(OA_all_nut_supply)) %>%
  mutate(relative_supply_all = total_supply_all/total_supply_OA_all,
         supply_diff_all = total_supply_all - total_supply_OA_all)

write.csv(delta_nutrition, "Outputs/reef_nutrient_supply_bayes.csv", row.names = F)
