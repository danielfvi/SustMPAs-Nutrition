####MPA and Nutrition Project
####Step 2 - Random Forest predictions 
####Author: Daniel Viana
####Date: June 2021

#####Load Packages
library(tidyverse) # for general data wrangling and plotting
library(gdata)
library(tidymodels)
library(corrplot)
library(brms)
library(corrr)
library(PerformanceAnalytics)
library(GGally)
library(car)
library(ggpubr)

# Clear workspace
rm(list = ls())

survey_bio_covariates <- read_csv("data/survey_bio_covariates.csv")

##Filter for sites outside of MPAs
RLS_OA = survey_bio_covariates %>%
  filter(governance_cat=="OpenAccess") %>%
  mutate(is_restricted = if_else(governance_cat == "OpenAccess", 0, 1))

RLS_restricted = survey_bio_covariates %>%
  filter(governance_cat=="Restricted") %>%
  mutate(is_restricted = if_else(governance_cat == "OpenAccess", 0, 1))

RLS_all = rbind(RLS_OA, RLS_restricted)

##1)Check for correlated variables
RLS_covariates = RLS_all %>% 
  select(-c(SurveyID:Effectiveness, SiteLatitude, SiteLongitude, bio_log:ECOREGION))

ggcorr(RLS_covariates)

##Remove Correlated variables (sst_min, gov_effect) and add logged values

RLS_fishing = RLS_all %>% 
  dplyr::select(bio_log, is_restricted, is_mpa, mrkt.dist_log, hpop50_log, land50_log,
                wave.exp_log, reef15_log, BO_chlomean_log, BO_nitrate,
                sst_log, BO_sstrange, shore.dist_log, HDI_2018, 
                log_density, gov_effect, mora, iso3c, ECOREGION) %>% 
  drop_na()

RLS_fishing_cov = RLS_fishing %>% 
  dplyr::select(is_restricted, is_mpa, mrkt.dist_log, hpop50_log, land50_log,
                wave.exp_log, reef15_log, BO_chlomean_log, BO_nitrate, 
                sst_log, BO_sstrange, shore.dist_log, HDI_2018, 
                log_density, gov_effect, mora, iso3c, ECOREGION) %>% 
  drop_na()

RLS_fishing_data = RLS_all %>% 
  dplyr::select(SurveyID, bio_log, is_restricted, is_mpa, mrkt.dist_log, hpop50_log, 
                land50_log, wave.exp_log, reef15_log, BO_chlomean_log, BO_nitrate, 
                sst_log, BO_sstrange, shore.dist_log, 
                HDI_2018, gov_effect, log_density, mora, iso3c, ECOREGION) %>% 
  drop_na() %>% 
  dplyr::select(SurveyID, bio_log)

##2) remove variables with high variance inflation factors (land50 and fisher density)

x = as.data.frame(vif(lm(bio_log ~
         is_mpa +
         mrkt.dist_log +
         hpop50_log +
         #land50_log +
         wave.exp_log +
         reef15_log +
         BO_chlomean_log +
         BO_nitrate +
         sst_log +
         BO_sstrange +
         shore.dist_log +
         HDI_2018 +
         #log_density +
         mora, data = RLS_fishing)))
colnames(x) = "vif"

ggplot(data = x) +
  geom_bar(aes(x = vif, y=rownames(x)), stat = "identity") +
  xlim(0,8)

##3) Compare NULL vs model with all covariates

########Run multivariate bayesian model

##Model 3 - excluding covariates with high correlation (land50, sstmin) and group by country
b1 <- 
  brm(data = RLS_fishing, family=gaussian,
      bio_log ~ 1 + 
        (1 | ECOREGION),
      iter = 10000, warmup = 500, chains = 4, cores = 4, seed = 5)


##Model 2 - excluding covariates with high correlation (land50, sstmin) and group by ecoregion
b2 <- 
  brm(data = RLS_fishing, family=gaussian,
      bio_log ~ 
        #is_restricted +
        is_mpa +
        #is_effective +
        is_mpa*mora +
        mora +
        #is_restricted*gov_effect +
        mrkt.dist_log +
        hpop50_log +
        HDI_2018 +
        gov_effect +
        shore.dist_log +
        #land50_log +
        wave.exp_log +
        reef15_log +
        BO_chlomean_log +
        BO_nitrate +
        sst_log +
        BO_sstrange +
        #log_density +
        (1 | ECOREGION),
      iter = 10000, warmup = 500, chains = 4, cores = 4, seed = 5)


##Model  - excluding covariates with high correlation (land50, sstmin), without interaction and group by ecoregion
b3 <- 
  brm(data = RLS_fishing, family=gaussian,
      bio_log ~ 
        #is_restricted +
        is_mpa +
        #is_effective +
        #is_mpa*mora +
        mora +
        #is_restricted*gov_effect +
        mrkt.dist_log +
        hpop50_log +
        HDI_2018 +
        gov_effect +
        shore.dist_log +
        #land50_log +
        wave.exp_log +
        reef15_log +
        BO_chlomean_log +
        BO_nitrate +
        sst_log +
        BO_sstrange +
        #log_density +
        (1 | ECOREGION),
      iter = 10000, warmup = 500, chains = 4, cores = 4, seed = 5)

w = loo_compare(loo(b1), loo(b2))
w = loo_compare(loo(b2), loo(b3))

##4) Check convergence, R_hat
#Check convergence
#Plot posterior distributions
plot(b2)

##Eport posterior distributions
x = as.data.frame(b2)
write.csv(x, "Outputs/posterior_dist.csv", row.names = F)

#R_hat
s = summary(b2)$fixed
write.table(s, file = "Outputs/summary_stats.csv", sep = ",", quote = FALSE)

##5) Check observed vs predicted
###Posterior Predictive check
p1 = pp_check(b2, ndraws = 1000) +
  labs(x = "Biomass [log(kg/ha)]", 
       y = "Density") +
  #xlim(0, 12) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        axis.title = element_text(size = 20),
        axis.text = element_text(size = 20))

##Fitted vs observed plot
f <- 
  fitted(b2) %>% 
  as_tibble() %>% 
  bind_cols(RLS_fishing) %>% 
  mutate(residuals = Estimate - bio_log)

p2 = ggplot(data = f,
       aes(x = bio_log, y = Estimate)) +
  geom_abline(linetype = 2, color = "grey50", size = .5) +
  geom_point(size = 1.5, color = "firebrick4", alpha = 3/4) +
  geom_linerange(aes(ymin = Q2.5, ymax = Q97.5),
                 size = 1/4, color = "firebrick4") +
  geom_linerange(aes(ymin = Estimate - Est.Error, 
                     ymax = Estimate + Est.Error),
                 size = 1/2, color = "firebrick4") +
  geom_smooth() +
  #xlim(0, 12) +
  labs(x = "Observed biomass [log(kg/ha)]", 
       y = "Predicted biomass [log(kg/ha)]") +
  geom_text(x=9, y=2.5, label="R2 = 0.41", size = 8) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        axis.title = element_text(size = 20),
        axis.text = element_text(size = 20))

p3 = ggplot(data = f, aes(x = Estimate, y = residuals)) +
  geom_point() +
  geom_smooth()+
  geom_hline(yintercept = 0)+
  #xlim(0, 12) +
  labs(x = "Predicted biomass [log(kg/ha)]", 
       y = "Residuals") +
  theme_bw() +
  theme(panel.grid = element_blank(),
        axis.title = element_text(size = 20),
        axis.text = element_text(size = 20))

#Other plots
#hist(resid(b2)) 

#ggplot(data=NULL)+
#  geom_point(aes(y=resid(b2), x=fitted(b2)))

#compute R squared
bayes_R2(b2)

##Export plot
plot.fitted = ggarrange(p1, p3, p2,
                       ncol=3, 
                       labels = "AUTO",
                       font.label = list(size = 20, face = "bold"))
plot.fitted

ggsave(filename = "Figures/Fitted_observed checks - brms.jpeg", 
       plot = plot.fitted,
       height = 5.5, 
       width = 15)

##Plot coeficients
mcmc_plot(b2) +
  theme_bw() +
  theme(panel.grid = element_blank())

estimates_b2=as.data.frame(posterior_summary(b2)) %>% 
  slice(2:15) %>% 
  mutate(variable = c("Sustainable-use MPA", 
                      "Fisheries Management Effectiveness",
                      "Market distance",
                      "Human population",
                      "Human Development Index",
                      "Government Effectiveness",
                      "Shore distance",
                      "Wave exposure",
                      "Reef area",
                      "Chlorophyll",
                      "Nitrate",
                      "Sea Surface Temperature mean",
                      "Sea Surface Temperature range",
                      "MPA:FME interaction"),
         sign = if_else(Q2.5<0 & Q97.5<0, "negative",
                        if_else(Q2.5>0 & Q97.5>0, "positive", "no effect"))) %>% 
  arrange(Estimate)

estimates_b2$order = 1:nrow(estimates_b2)
estimates_b2$variable = factor(estimates_b2$variable, levels = estimates_b2$variable[order(estimates_b2$order)])

p4 = ggplot(estimates_b2,aes(x=variable,y=Estimate,ymin=Q2.5,ymax=Q97.5))+
  geom_pointrange(aes(colour=sign),size=0.7, shape = 19)+
  scale_color_manual(values=c("no effect"="grey60","negative"="red","positive"="blue"),
                     labels=c("Negative","No effect","Positive"), guide="none")+
  #scale_shape_manual(values=c("open"=1,"closed"=19), guide=F)+
  theme(legend.position = "none")+
  coord_flip()+
  geom_hline(aes(yintercept=0),colour="dark grey",linetype="dashed")+
  #scale_y_continuous("",limits=c(-3,3.6))+
  labs(x = "", y="Effect size")+
  theme_classic() +
  theme(axis.text = element_text(size = 12))
p4

ggsave(filename = "Figures/effect size - brms.jpeg", 
       plot = p4,
       height = 5, 
       width = 6)


##Perform cross validation
kfold1 <- kfold(b2, K=10, chains = 4)

x = loo_R2(b2)

##Compare models
loo(b1)
loo(b2)

w = loo_compare(loo(b1), loo(b2), loo(b3))
w = loo_compare(loo(b4), loo(b3))
write.table(w, file = "loo_compare.csv", sep = ",", quote = FALSE)
k = loo_compare(waic(b1), waic(b2), waic(b3), waic(b4))
write.table(k, file = "loo_compare_waic.csv", sep = ",", quote = FALSE)

#############Test models
b2 <- 
  brm(data = RLS_fishing, family=gaussian,
      bio_log ~ 
        #is_restricted +
        is_mpa +
        #is_effective +
        is_mpa*mora +
        #is_restricted*gov_effect +
        mrkt.dist_log +
        hpop50_log +
        #land50_log +
        wave.exp_log +
        reef15_log +
        BO_chlomean_log +
        BO_nitrate +
        sst_log +
        BO_sstrange +
        shore.dist_log +
        HDI_2018 +
        #gov_effect +
        #log_density +
        mora +
        (1 | ECOREGION),
      iter = 10000, warmup = 500, chains = 4, cores = 4, seed = 5)

mcmc_plot(b2) +
  theme_bw() +
  theme(panel.grid = element_blank())

b3 <- 
  brm(data = RLS_fishing, family=gaussian,
      bio_log ~ 
        #is_restricted +
        is_mpa +
        #is_effective +
        is_mpa*mora +
        is_mpa*gov_effect +
        #is_restricted*gov_effect +
        mrkt.dist_log +
        hpop50_log +
        #land50_log +
        wave.exp_log +
        reef15_log +
        BO_chlomean_log +
        BO_nitrate +
        sst_log +
        BO_sstrange +
        shore.dist_log +
        HDI_2018 +
        gov_effect +
        #log_density +
        mora,
      iter = 10000, warmup = 500, chains = 4, cores = 4, seed = 5)

##Plot coeficients
mcmc_plot(b3) +
  theme_bw() +
  theme(panel.grid = element_blank())

b4 <- 
  brm(data = RLS_fishing, family=gaussian,
      bio_log ~ 
        is_restricted +
        is_mpa +
        #is_effective +
        is_mpa*mora +
        #is_mpa*gov_effect +
        #is_restricted*gov_effect +
        mrkt.dist_log +
        hpop50_log +
        #land50_log +
        wave.exp_log +
        reef15_log +
        BO_chlomean_log +
        BO_nitrate +
        sst_log +
        BO_sstrange +
        shore.dist_log +
        HDI_2018 +
        gov_effect +
        #log_density +
        mora,
      iter = 10000, warmup = 500, chains = 4, cores = 4, seed = 5)

mcmc_plot(b4) +
  theme_bw() +
  theme(panel.grid = element_blank())

b5 <- 
  brm(data = RLS_fishing, family=gaussian,
      bio_log ~ 
        #is_restricted +
        is_mpa +
        #is_effective +
        #is_mpa*mora +
        is_mpa*gov_effect +
        #is_restricted*gov_effect +
        mrkt.dist_log +
        hpop50_log +
        #land50_log +
        wave.exp_log +
        reef15_log +
        BO_chlomean_log +
        BO_nitrate +
        sst_log +
        BO_sstrange +
        shore.dist_log +
        HDI_2018 +
        gov_effect +
        #log_density +
        mora,
      iter = 10000, warmup = 500, chains = 4, cores = 4, seed = 5)

mcmc_plot(b5) +
  theme_bw() +
  theme(panel.grid = element_blank())

w = loo_compare(loo(b3), loo(b4), loo(b5))
write.table(w, file = "Outputs/loo_compare_effect.csv", sep = ",", quote = FALSE)

w = loo_compare(loo(b2), loo(b3), loo(b4), loo(b5))
write.table(w, file = "Outputs/loo_compare_effect2.csv", sep = ",", quote = FALSE)

