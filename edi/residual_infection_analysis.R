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
load("output/ev_percap_bio_full_model.rda")
load("output/ev_establishment_full_model.rda")

# import data
EvInfDat <- read_csv("intermediate-data/ev_infection_data.csv")
EvBioDat <- read_csv("intermediate-data/ev_biomass_data.csv")
EvEstDat <- read_csv("intermediate-data/ev_establishment_data.csv")


#### edit data ####

# select columns
# combine data
# add columns
bioDat <- EvBioDat %>%
  select(PotID, Litter, Competition, Weight_g, LogPerCapWeight.g) %>%
  mutate(ResidWeight = resid(ev_bio_mod2)) %>%
  left_join(EvInfDat %>%
              select(PotID, Litter, Competition, PropInfEv)) %>%
  mutate(Litter = factor(Litter, levels = c("None", "Low", "Med", "High")),
         Planting = dplyr::recode(Competition, "0" = "alone", "1" = "in competition"))

# select columns
# combine data
# add columns
estDat <- EvEstDat %>%
  select(PotID, Litter, Competition, PropEstEv) %>%
  mutate(ResidEst = resid(ev_est_mod2)) %>%
  left_join(EvInfDat %>%
              select(PotID, Litter, Competition, PropInfEv)) %>%
  mutate(Litter = factor(Litter, levels = c("None", "Low", "Med", "High")),
         Planting = dplyr::recode(Competition, "0" = "alone", "1" = "in competition"))


#### visualizations ####

# raw weight
ggplot(bioDat, aes(PropInfEv, Weight_g)) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ x) +
  facet_wrap(~ Planting, scales = "free")

# residual weight
ggplot(bioDat, aes(PropInfEv, ResidWeight)) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ x)

# raw establishment
ggplot(estDat, aes(PropInfEv, PropEstEv)) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ x)

# residual establishment
ggplot(estDat, aes(PropInfEv, ResidEst)) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ x)


#### stats ####

ev_bio_inf_mod1 <- lm(ResidWeight ~ PropInfEv, bioDat)
summary(ev_bio_inf_mod1)

ev_est_inf_mod1 <- lm(ResidEst ~ PropInfEv, estDat)
summary(ev_est_inf_mod1)


#### output ####

write_csv(tidy(ev_bio_inf_mod1), "output/ev_bio_resid_full_model.csv")
write_csv(tidy(ev_est_inf_mod1), "output/ev_est_resid_full_model.csv")
