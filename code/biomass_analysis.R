#### goals #### 

# analyze effects of litter and live plant competition on:
  # Mv and Ev biomass


#### set up ####

# clear all existing data
rm(list=ls())

# load libraries
library(tidyverse)
library(broom)

# import data (all data combined up to post harvest)
dat1 <- read_csv("./data/MvEv_Biomass_110918.csv")
MvEstDat <- read_csv("intermediate-data/mv_establishment_data.csv")
EvEstDat <- read_csv("intermediate-data/ev_establishment_data.csv")


#### edit data ####

# look at notes
unique(dat1$Notes)
filter(dat1, Notes == "intentionally not measured")

# make a shade treatment
# reorder litter
# make litter quantitative
# re-order species present
# make a treatment ID
# make a binary litter variable
# remove unwanted plant
dat2 <- dat1 %>%
  mutate(Shade = ifelse(Treatment == "Shade", "yes", "no"),
         Litter = dplyr::recode(Treatment, Shade = "Med") %>% factor(levels = c("None", "Low", "Med", "High")),
         Litter.g = case_when(Litter == "None" ~ 0,
                              Litter == "Low" ~ 0.91,
                              Litter == "Med" ~ 1.82,
                              Litter == "High" ~ 3.64),
         SpPresent = factor(SpPresent, levels = c("Ev", "Mv", "Ev+Mv")),
         Competition = recode(SpPresent, Ev = 0, Mv = 0, "Ev+Mv" = 1),
         Planting = ifelse(Competition == 0, "alone", "in competition"),
         WeighedSp = dplyr::recode(WeighedSp, MV = "Mv"), 
         TrtID = paste(SpPresent, Litter, Shade, PotID, sep="."),
         LogWeight.g = log(Weight.g)) %>%
  filter(Notes != "intentionally not measured" | is.na(Notes))

# check for NA's
filter(dat2, is.na(Weight.g))

# total weight by treatment
totBioDat <- dat2 %>%
  select(PotID, Treatment, SpPresent, Shade, Litter, WeighedSp, Weight.g) %>%
  pivot_wider(names_from = "WeighedSp",
              values_from = "Weight.g") %>%
  mutate(Mv = replace_na(Mv, 0),
         Ev = replace_na(Ev, 0),
         TotalWeight.g = Mv + Ev) %>%
  select(-c(Mv, Ev))

# split by species and merge with count data
MvBioDat <- dat2 %>% 
  left_join(totBioDat) %>%
  filter(WeighedSp == "Mv" & SpPresent != "Ev") %>%
  full_join(select(MvEstDat, TrtID, GermMv)) %>% 
  mutate(LogPerCapWeight.g = log(Weight.g/GermMv)) %>%
  filter(Shade == "no")

EvBioDat <- dat2 %>%
  left_join(totBioDat) %>%
  filter(WeighedSp == "Ev" & SpPresent != "Mv") %>%
  full_join(select(EvEstDat, TrtID, GermEv)) %>%
  mutate(LogPerCapWeight.g = log(Weight.g/GermEv))

# check total weight
MvBioDat %>%
  filter(SpPresent == "Mv" & Weight.g != TotalWeight.g)

EvBioDat %>%
  filter(SpPresent == "Ev" & Weight.g != TotalWeight.g)

# relative abundance datasets
MvRelDat <- MvBioDat %>%
  filter(SpPresent == "Ev+Mv")

EvRelDat <- EvBioDat %>%
  filter(SpPresent == "Ev+Mv")


#### visualizations ####

# Mv log-transformed
ggplot(MvBioDat, aes(x = Litter.g, y = LogWeight.g, color = SpPresent)) +
  stat_summary(fun.data = "mean_cl_boot", geom = "errorbar", width = 0, position = position_dodge(0.1)) +
  stat_summary(fun = "mean", geom = "point", position = position_dodge(0.1)) +
  theme_bw()

# Mv per capita
ggplot(MvBioDat, aes(x = Litter.g, y = LogPerCapWeight.g, color = SpPresent)) +
  stat_summary(fun.data = "mean_cl_boot", geom = "errorbar", width = 0, position = position_dodge(0.1)) +
  stat_summary(fun = "mean", geom = "point", position = position_dodge(0.1)) +
  theme_bw()

# Mv relative abundance
MvRelDat %>%
  ggplot(aes(x = Litter.g, y = Weight.g/TotalWeight.g)) +
  stat_summary(fun.data = "mean_cl_boot", geom = "errorbar", width = 0) +
  stat_summary(fun = "mean", geom = "point") +
  theme_bw()

# Ev log-transformed
ggplot(EvBioDat, aes(x = Litter.g, y = LogWeight.g, color = SpPresent)) +
  stat_summary(fun.data = "mean_cl_boot", geom = "errorbar", width = 0, position = position_dodge(0.1)) +
  stat_summary(fun = "mean", geom = "point", position = position_dodge(0.1)) +
  theme_bw()

# Ev per capita
ggplot(EvBioDat, aes(x = Litter.g, y = LogPerCapWeight.g, color = SpPresent)) +
  stat_summary(fun.data = "mean_cl_boot", geom = "errorbar", width = 0, position = position_dodge(0.1)) +
  stat_summary(fun = "mean", geom = "point", position = position_dodge(0.1)) +
  theme_bw()

# Ev relative abundance
EvRelDat %>%
  ggplot(aes(x = Litter.g, y = Weight.g/TotalWeight.g)) +
  stat_summary(fun.data = "mean_cl_boot", geom = "errorbar", width = 0) +
  stat_summary(fun = "mean", geom = "point") +
  theme_bw()


#### Mv per capita biomass ####

# continuous litter
mv_bio_mod1 <- lm(LogPerCapWeight.g ~ Litter.g * Competition, data = MvBioDat)
summary(mv_bio_mod1)

# categorical litter
mv_bio_mod2 <- lm(LogPerCapWeight.g ~ Litter * Competition, data = MvBioDat)
summary(mv_bio_mod2)

# compare models
AIC(mv_bio_mod1, mv_bio_mod2)
# continuous is slightly better: delta AIC = 2.1
# plot(mv_bio_mod1)

# remove interaction?
mv_bio_mod3 <- update(mv_bio_mod1, ~. -Litter.g:Competition)
summary(mv_bio_mod3)
anova(mv_bio_mod1, mv_bio_mod3, test = "F") # yes
# F: 0.01, P: 0.92

# remove main effects?
mv_bio_mod4 <- update(mv_bio_mod3, ~. -Competition)
summary(mv_bio_mod4)
anova(mv_bio_mod3, mv_bio_mod4, test = "F") # no
# F: 4.36, P = 0.04

mv_bio_mod5 <- update(mv_bio_mod3, ~. -Litter.g)
summary(mv_bio_mod5)
anova(mv_bio_mod3, mv_bio_mod5, test = "F") # no
# F: 5.90, P = 0.02


#### Relative abundance ####

# continuous litter
rel_mod1 <- lm(LogWeight.g ~ offset(log(TotalWeight.g)) + Litter.g, data = MvRelDat)
summary(rel_mod1)

# categorical litter
rel_mod2 <- lm(LogWeight.g ~ offset(log(TotalWeight.g)) + Litter, data = MvRelDat)
summary(rel_mod2)

# compare models
AIC(rel_mod1, rel_mod2)
# categorical is better: delta AIC 7.8
# plot(rel_mod2)

# litter p-value
rel_mod3 <- update(rel_mod2, ~. -Litter)
summary(rel_mod3)
anova(rel_mod2, rel_mod3, test = "F")
# F: 4.28, P: 0.017


#### Ev per capita biomass ####

# continuous litter
ev_bio_mod1 <- lm(LogPerCapWeight.g ~ Litter.g * Competition, data = EvBioDat)
summary(ev_bio_mod1)

# categorical litter
ev_bio_mod2 <- lm(LogPerCapWeight.g ~ Litter * Competition, data = EvBioDat)
summary(ev_bio_mod2)

# compare models
AIC(ev_bio_mod1, ev_bio_mod2)
# categorical better: delta AIC = 3.4
# plot(ev_bio_mod2)

# remove interaction?
ev_bio_mod3 <- update(ev_bio_mod2, ~. -Litter:Competition)
summary(ev_bio_mod3)
anova(ev_bio_mod2, ev_bio_mod3, test = "F") # yes
# F: 1.33, P: 0.28

# remove main effects?
ev_bio_mod4 <- update(ev_bio_mod3, ~. -Litter)
summary(ev_bio_mod4)
anova(ev_bio_mod3, ev_bio_mod4, test = "F") # yes
# F: 2.20, P = 0.10

ev_bio_mod5 <- update(ev_bio_mod4, ~. -Competition)
summary(ev_bio_mod5)
anova(ev_bio_mod4, ev_bio_mod5, test = "F") # no
# F: 321.3, P < 0.001


#### output ####
save(mv_bio_mod3, file = "output/mv_percap_bio_model.rda")

write_csv(tidy(mv_bio_mod1), "output/mv_percap_bio_full_model.csv")
write_csv(tidy(rel_mod2), "output/mv_relative_abundance_full_model.csv")
write_csv(tidy(ev_bio_mod2), "output/ev_percap_bio_full_model.csv")

write_csv(MvBioDat, "intermediate-data/mv_biomass_data.csv")
write_csv(EvBioDat, "intermediate-data/ev_biomass_data.csv")
