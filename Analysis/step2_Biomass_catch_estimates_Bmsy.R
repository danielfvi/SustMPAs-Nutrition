####MPA and Nutrition Project
####Step 2 - Random Forest predictions 
####Author: Daniel Viana
####Date: June 2021

#####Load Packages
library(tidyverse) # for general data wrangling and plotting
library(gdata)

# Clear workspace
rm(list = ls())

rf_predictions <- read_csv("Outputs/rf_predictions_bayes_brms.csv")

reefID_area_final <- read_csv("data/reefID_area_final.csv") %>% 
  mutate(multiple_use = if_else(is.na(multiple_use), 0, multiple_use),
         no_take = if_else(is.na(no_take), 0, no_take),
         open_access = if_else(is.na(open_access), 0, open_access))

#######Calculate biomass inside/outside MPAs
reef_bio = rf_predictions %>% 
  left_join(reefID_area_final, by=c("reef.id" = "ID"))

##Calculate Potential benefits from MPAs
##Units:
#Biomass is in kg/ha
#Reef area is in km2
#1km2 is 100ha
#Therefore, we need to multiply the biomass by 100
x = as.numeric(quantile(reef_bio$pred.restricted, probs = 0.9))
Bmsy = x
K = 2*Bmsy
r = 0.23
#f = r*(1-B/K)

reef_MPA = reef_bio %>% 
  mutate(##Reef area
         MPA_area = multiple_use + no_take,
         reef_area = MPA_area + open_access,
         ##Fishing Morality
         f_restricted = r*(1-pred.restricted/K),
         f_restricted = if_else(f_restricted<0, 0, f_restricted),
         f_OA = r*(1-pred.oa/K),
         f_OA = if_else(f_OA<0, 0, f_OA),
         ##All reef
         #Biomass
         all_bio = open_access*exp(pred.restricted)*100,
         all_bio_low = open_access*exp(pred.restricted-error.mu)*100,
         all_bio_high = open_access*exp(pred.restricted+error.mu)*100,
         OA_all_bio = open_access*exp(pred.oa)*100,
         OA_all_bio_low = open_access*exp(pred.oa-error.oa)*100,
         OA_all_bio_high = open_access*exp(pred.oa+error.oa)*100,
         #Catch
         all_catch = f_restricted*all_bio,
         all_catch_low = f_restricted*all_bio_low,
         all_catch_high = f_restricted*all_bio_high,
         OA_all_catch = f_OA*OA_all_bio,
         OA_all_catch_low = f_OA*OA_all_bio_low,
         OA_all_catch_high = f_OA*OA_all_bio_high,
         ##Multiple use 
         #Biomass 
         MU_bio = multiple_use*exp(pred.restricted)*100,
         MU_bio_low = multiple_use*exp(pred.restricted-error.mu)*100,
         MU_bio_high = multiple_use*exp(pred.restricted+error.mu)*100,
         OA_MU_bio = multiple_use*exp(pred.oa)*100,
         OA_MU_bio_low = multiple_use*exp(pred.oa-error.oa)*100,
         OA_MU_bio_high = multiple_use*exp(pred.oa+error.oa)*100,
         #Catch
         MU_catch = f_restricted*MU_bio,
         MU_catch_low = f_restricted*MU_bio_low,
         MU_catch_high = f_restricted*MU_bio_high,
         OA_MU_catch = f_OA*OA_MU_bio,
         OA_MU_catch_low = f_OA*OA_MU_bio_low,
         OA_MU_catch_high = f_OA*OA_MU_bio_high
         ) %>% 
  filter(!iso3c %in% c("KEN/SOM", "PNG/AUS", "REU/MUS", "SDN/EGY"))

reef_MPA_benefits = reef_MPA %>% 
  group_by(iso3c) %>% 
  summarise(##catch on open access reefs
            all_catch = sum(all_catch),
            all_catch_low = sum(all_catch_low),
            all_catch_high = sum(all_catch_high),
            OA_all_catch = sum(OA_all_catch),
            OA_all_catch_low = sum(OA_all_catch_low),
            OA_all_catch_high = sum(OA_all_catch_high),
            ##catch on MU MPAs
            MU_catch = sum(MU_catch),
            MU_catch_low = sum(MU_catch_low),
            MU_catch_high = sum(MU_catch_high),
            OA_MU_catch = sum(OA_MU_catch),
            OA_MU_catch_low = sum(OA_MU_catch_low),
            OA_MU_catch_high = sum(OA_MU_catch_high),
            ##Areas
            MU_area = log(sum(multiple_use)),
            reef_area = log(sum(reef_area))) %>% 
  ungroup() %>% 
  mutate(relative_catch_all = all_catch/OA_all_catch,
         relative_catch_low = all_catch_low/OA_all_catch_low,
         relative_catch_high = all_catch_high/OA_all_catch_high,
         relative_catch_MU = MU_catch/OA_MU_catch,
         relative_catch_MU_low = MU_catch_low/OA_MU_catch_low,
         relative_catch_MU_high = MU_catch_high/OA_MU_catch_high)

# sevs <- read_csv("data/2017_sevs_base_high_road_country_average_diversity_disagg.csv") %>% 
#   filter(!nutrient=="Omega-3 fatty acids") %>% 
#   group_by(iso3) %>% 
#   summarise(intake = mean(sev_base))
# 
# reef_MPA_benefits = reef_MPA_benefits %>% 
#   left_join(sevs, by=c("iso3c" = "iso3"))

write.csv(reef_MPA_benefits, "Outputs/reef_MPA_benefits_bayes.csv", row.names = F)
write.csv(reef_MPA, "Outputs/reef_MPA_predictions_bayes.csv", row.names = F)

