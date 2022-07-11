####MPA and Nutrition Project
####Step 1 - Predicting Biomass in fished reef sites 
####Author: Daniel Viana
####Date: May 2020

#####Load Packages
library(tidyverse) # for general data wrangling and plotting
library(brms)

# Clear workspace
rm(list = ls())

survey_bio_covariates <- read_csv("data/survey_bio_covariates.csv")

##Filter for sites outside of MPAs
RLS_OA = survey_bio_covariates %>%
  filter(governance_cat=="OpenAccess")

RLS_restricted = survey_bio_covariates %>%
  filter(governance_cat=="Restricted")

RLS_all = rbind(RLS_OA, RLS_restricted)

RLS_fishing = RLS_all %>% 
  dplyr::select(bio_log, is_mpa, mrkt.dist_log, hpop50_log, land50_log,
                wave.exp_log, reef15_log, BO_chlomean_log, BO_nitrate,
                sst_log, BO_sstrange, shore.dist_log, HDI_2018, 
                log_density, mora, gov_effect, iso3c, ECOREGION) %>% 
  drop_na()

RLS_fishing_cov = RLS_fishing %>% 
  dplyr::select(is_mpa, mrkt.dist_log, hpop50_log, land50_log,
                wave.exp_log, reef15_log, BO_chlomean_log, BO_nitrate, 
                sst_log, BO_sstrange, shore.dist_log, HDI_2018, 
                log_density, mora, gov_effect, iso3c, ECOREGION) %>% 
  drop_na()

RLS_fishing_data = RLS_all %>% 
  dplyr::select(SurveyID, bio_log, is_mpa, mrkt.dist_log, hpop50_log, 
                land50_log, wave.exp_log, reef15_log, BO_chlomean_log, BO_nitrate, 
                sst_log, BO_sstrange, shore.dist_log, 
                HDI_2018, log_density, mora, gov_effect, iso3c, ECOREGION) %>% 
  drop_na() %>% 
  dplyr::select(SurveyID, bio_log)

##Reef sites covariates
reef_sites <- read_csv("data/reef_sites_cov.csv")

reef_sites_id = reef_sites %>% 
  select(reef.id, iso3c)

reef_sites_cov_OA = reef_sites %>% select(-reef.id, 
                                          -iso3c, 
                                          -SiteLatitude, 
                                          -SiteLongitude, 
                                          -BO_ph, 
                                          -BO_salinity, 
                                          -BO_parmean,
                                          -BO_sstmin) %>% 
  mutate(is_mpa = 0)

reef_sites_cov_restricted = reef_sites %>% select(-reef.id, 
                                                  -iso3c, 
                                                  -SiteLatitude, 
                                                  -SiteLongitude, 
                                                  -BO_ph, 
                                                  -BO_salinity, 
                                                  -BO_parmean,
                                                  -BO_sstmin) %>% 
  mutate(is_mpa = 1)

########Run multivariate bayesian model

##Model 1 - all covariates
b1 <- 
  brm(data = RLS_fishing, family=gaussian,
      bio_log ~ 
        is_mpa +
        is_mpa*mora +
        mrkt.dist_log +
        hpop50_log +
        wave.exp_log +
        reef15_log +
        BO_chlomean_log +
        BO_nitrate +
        sst_log +
        BO_sstrange +
        shore.dist_log +
        HDI_2018 +
        gov_effect +
        mora,
      iter = 10000, warmup = 500, chains = 4, cores = 4, seed = 5)

###########Predict Biomass in reef sites
pred.oa = b1 %>% 
  predict(reef_sites_cov_OA) %>% 
  cbind(reef_sites_id) %>% 
  rename(pred.oa = Estimate)

pred.oa = pred.oa %>% 
  rename(error.oa = Est.Error,
         Q2.5.oa = Q2.5,
         Q97.5.oa = Q97.5)

##Restricted scenario
pred.mu = b1 %>% 
  predict(reef_sites_cov_restricted) %>% 
  cbind(reef_sites_id) %>% 
  rename(pred.restricted = Estimate)

pred.mu = pred.mu %>% 
  rename(error.mu = Est.Error,
         Q2.5.mu = Q2.5,
         Q97.5.mu = Q97.5)

##Join all predictions
reef_bio = pred.oa %>% 
  left_join(pred.mu) %>% 
  mutate(pred_change = pred.restricted - pred.oa)

write.csv(reef_bio, "Outputs/rf_predictions_bayes_brms.csv", row.names = F)

##############Relative change in data (for Figure 1)############
##Filter for sites outside of MPAs
RLS_OA = survey_bio_covariates %>%
  filter(governance_cat=="OpenAccess")

RLS_restricted = survey_bio_covariates %>%
  filter(governance_cat=="Restricted")

RLS_all = rbind(RLS_OA, RLS_restricted)

RLS_fishing = RLS_all %>% 
  dplyr::select(bio_log, is_mpa, mrkt.dist_log, hpop50_log, land50_log,
                wave.exp_log, reef15_log, BO_chlomean_log, BO_nitrate,
                sst_log, BO_sstrange, shore.dist_log, HDI_2018, 
                log_density, mora, gov_effect, iso3c, ECOREGION) %>% 
  drop_na()

b2 <- 
  brm(data = RLS_fishing, family=gaussian,
      bio_log ~ 
        is_mpa +
        is_mpa*mora +
        mrkt.dist_log +
        hpop50_log +
        wave.exp_log +
        reef15_log +
        BO_chlomean_log +
        BO_nitrate +
        sst_log +
        BO_sstrange +
        shore.dist_log +
        HDI_2018 +
        gov_effect +
        mora +
        (1 | ECOREGION),
      iter = 10000, warmup = 500, chains = 4, cores = 4, seed = 5)

##Plot coeficients
mcmc_plot(b2) +
  theme_bw() +
  theme(panel.grid = element_blank())

##OA sites covariates
RLS_OA_cov = RLS_fishing %>%
  mutate(is_mpa = 0)

RLS_OA_MPA_cov = RLS_fishing %>%
  mutate(is_mpa = 1)

##Predict change in Open Access
pred.oa = b2 %>% 
  predict(RLS_OA_cov) %>% 
  as.data.frame() %>% 
  rename(pred.oa = Estimate)

pred.oa = pred.oa %>% 
  rename(error.oa = Est.Error,
         Q2.5.oa = Q2.5,
         Q97.5.oa = Q97.5)

##Restricted scenario
pred.mu = b2 %>% 
  predict(RLS_OA_MPA_cov) %>% 
  as.data.frame() %>%
  rename(pred.restricted = Estimate)

pred.mu = pred.mu %>% 
  rename(error.mu = Est.Error,
         Q2.5.mu = Q2.5,
         Q97.5.mu = Q97.5)

##Join all predictions
mpa_bio = pred.oa %>% 
  cbind(pred.mu, RLS_OA_MPA_cov) %>% 
  mutate(pred_change = 100*(exp(pred.restricted)/exp(pred.oa)-1))

write.csv(mpa_bio, "Outputs/predicted_OA_MPA_bayes_perc_change.csv", row.names = F)
