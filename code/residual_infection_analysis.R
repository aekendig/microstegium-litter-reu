#### goals #### 

# analyze effects of litter and live plant competition on:
# Mv and Ev biomass


#### set up ####

# clear all existing data
rm(list=ls())

# load libraries
library(tidyverse)
library(broom)

# load model
load("output/ev_tot_bio_simplified_model.rda")
load("output/ev_percap_bio_simplified_model.rda")
load("output/ev_establishment_simplified_model.rda")

# import data
EvInfDat <- read_csv("intermediate-data/ev_infection_data.csv")
EvBioDat <- read_csv("intermediate-data/ev_biomass_data.csv")
EvEstDat <- read_csv("intermediate-data/ev_establishment_data.csv")


#### edit data ####

# select columns
# combine data
# add columns
totBioDat <- EvBioDat %>%
  dplyr::select(PotID, Litter, Competition, LogWeight.g) %>%
  filter(!is.na(LogWeight.g)) %>%
  mutate(ResidWeight = resid(ev_tot_bio_mod3)) %>%
  left_join(EvInfDat %>%
              dplyr::select(PotID, Litter, Competition, PropInfEv)) %>%
  mutate(Litter = factor(Litter, levels = c("None", "Low", "Med", "High")),
         Planting = dplyr::recode(Competition, "0" = "alone", "1" = "in competition")) %>%
  filter(!is.na(PropInfEv))

pcBioDat <- EvBioDat %>%
  dplyr::select(PotID, Litter, Competition, LogPerCapWeight.g) %>%
  filter(!is.na(LogPerCapWeight.g)) %>%
  mutate(ResidWeight = resid(ev_bio_mod4)) %>%
  left_join(EvInfDat %>%
              dplyr::select(PotID, Litter, Competition, PropInfEv)) %>%
  mutate(Litter = factor(Litter, levels = c("None", "Low", "Med", "High")),
         Planting = dplyr::recode(Competition, "0" = "alone", "1" = "in competition"))

estDat <- EvEstDat %>%
  dplyr::select(PotID, Litter, Competition, PropEstEv) %>%
  mutate(ResidEst = resid(ev_est_mod4)) %>%
  left_join(EvInfDat %>%
              dplyr::select(PotID, Litter, Competition, PropInfEv)) %>%
  mutate(Litter = factor(Litter, levels = c("None", "Low", "Med", "High")),
         Planting = dplyr::recode(Competition, "0" = "alone", "1" = "in competition")) %>%
  filter(!is.na(PropInfEv))


#### visualizations ####

# raw weight
ggplot(totBioDat, aes(PropInfEv, LogWeight.g)) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ x) +
  facet_wrap(~ Planting)

# residual weight 
# account for effect of competition on weight
ggplot(totBioDat, aes(PropInfEv, ResidWeight)) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ x)

# raw weight
ggplot(pcBioDat, aes(PropInfEv, LogPerCapWeight.g)) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ x) +
  facet_wrap(~ Planting)

# residual weight
# account for effect of competition and litter on weight
ggplot(pcBioDat, aes(PropInfEv, ResidWeight)) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ x)

# raw establishment
ggplot(estDat, aes(PropInfEv, PropEstEv)) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ x)

# residual establishment
# account for effect of litter on weight
ggplot(estDat, aes(PropInfEv, ResidEst)) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ x)


#### stats ####

ev_tot_bio_inf_mod1 <- lm(ResidWeight ~ PropInfEv, totBioDat)
summary(ev_tot_bio_inf_mod1)

ev_pc_bio_inf_mod1 <- lm(ResidWeight ~ PropInfEv, pcBioDat)
summary(ev_pc_bio_inf_mod1)

ev_est_inf_mod1 <- lm(ResidEst ~ PropInfEv, estDat)
summary(ev_est_inf_mod1)


#### output ####

write_csv(tidy(ev_tot_bio_inf_mod1), "output/ev_tot_bio_resid_full_model.csv")
write_csv(tidy(ev_pc_bio_inf_mod1), "output/ev_pc_bio_resid_full_model.csv")
write_csv(tidy(ev_est_inf_mod1), "output/ev_est_resid_full_model.csv")
