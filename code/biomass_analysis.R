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
dat1 <- read_csv("data/MvEv_Biomass_110918.csv")
MvCountDat <- read_csv("intermediate-data/mv_final_count_data.csv")
EvCountDat <- read_csv("intermediate-data/ev_infection_data.csv")


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
         TrtID = paste(SpPresent, Litter, Shade, PotID, sep="."),
         LogWeight.g = log(Weight_g)) %>%
  filter(Notes != "intentionally not measured" | is.na(Notes))

# check for NA's
filter(dat2, is.na(Weight_g))

# total weight by treatment
totBioDat <- dat2 %>%
  dplyr::select(PotID, Treatment, SpPresent, Shade, Litter, WeighedSp, Weight_g) %>%
  pivot_wider(names_from = "WeighedSp",
              values_from = "Weight_g") %>%
  mutate(Mv = replace_na(Mv, 0),
         Ev = replace_na(Ev, 0),
         TotalWeight.g = Mv + Ev) %>%
  dplyr::select(-c(Mv, Ev)) %>%
  mutate(Litter.g = case_when(Litter == "None" ~ 0,
                              Litter == "Low" ~ 0.91,
                              Litter == "Med" ~ 1.82,
                              Litter == "High" ~ 3.64),
         Competition = recode(SpPresent, Ev = 0, Mv = 0, "Ev+Mv" = 1),
         LogTotalWeight.g = log(TotalWeight.g))

# split by species and merge with count data
MvBioDat <- dat2 %>% 
  left_join(totBioDat) %>%
  filter(WeighedSp == "Mv" & SpPresent != "Ev") %>%
  full_join(dplyr::select(MvCountDat, TrtID, GermMv)) %>% 
  mutate(PerCapWeight.g = Weight_g/GermMv,
         LogPerCapWeight.g = log(PerCapWeight.g),
         LogWeight.g = log(Weight_g)) %>%
  filter(Shade == "no")

EvBioDat <- dat2 %>%
  left_join(totBioDat) %>%
  filter(WeighedSp == "Ev" & SpPresent != "Mv") %>%
  full_join(dplyr::select(EvCountDat, TrtID, GermEv)) %>%
  mutate(PerCapWeight.g = Weight_g/GermEv,
         LogPerCapWeight.g = log(PerCapWeight.g),
         LogWeight.g = log(Weight_g))

# check total weight
MvBioDat %>%
  filter(SpPresent == "Mv" & Weight_g != TotalWeight.g)

EvBioDat %>%
  filter(SpPresent == "Ev" & Weight_g != TotalWeight.g)

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
  ggplot(aes(x = Litter.g, y = Weight_g/TotalWeight.g)) +
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
  ggplot(aes(x = Litter.g, y = Weight_g/TotalWeight.g)) +
  stat_summary(fun.data = "mean_cl_boot", geom = "errorbar", width = 0) +
  stat_summary(fun = "mean", geom = "point") +
  theme_bw()

# total biomass
totBioDat %>%
  ggplot(aes(x = Litter.g, y = LogTotalWeight.g, color = SpPresent)) +
  stat_summary(fun.data = "mean_cl_boot", geom = "errorbar", width = 0) +
  stat_summary(fun = "mean", geom = "point") +
  theme_bw()


#### Mv total biomass ####

# continuous litter
mv_tot_bio_mod1 <- lm(LogWeight.g ~ Litter.g * Competition, data = MvBioDat)
summary(mv_tot_bio_mod1)

# categorical litter
mv_tot_bio_mod2 <- lm(LogWeight.g ~ Litter * Competition, data = MvBioDat)
summary(mv_tot_bio_mod2)

# compare models
AIC(mv_tot_bio_mod1, mv_tot_bio_mod2)
# continuous is better: delta AIC = 5.7
# plot(mv_tot_bio_mod1)

# remove interaction?
mv_tot_bio_mod3 <- update(mv_tot_bio_mod1, ~. -Litter.g:Competition)
summary(mv_tot_bio_mod3)
anova(mv_tot_bio_mod1, mv_tot_bio_mod3, test = "F") # yes
# F: 0.80, P: 0.38

# remove main effects?
mv_tot_bio_mod4 <- update(mv_tot_bio_mod3, ~. -Litter.g)
summary(mv_tot_bio_mod4)
anova(mv_tot_bio_mod3, mv_tot_bio_mod4, test = "F") # no
# F: 5.84, P = 0.02

mv_tot_bio_mod5 <- update(mv_tot_bio_mod3, ~. -Competition)
summary(mv_tot_bio_mod5)
anova(mv_tot_bio_mod3, mv_tot_bio_mod5, test = "F") # no
# F: 11.41, P = 0.002


#### Mv per capita biomass ####

# continuous litter
mv_bio_mod1 <- lm(LogPerCapWeight.g ~ Litter.g * Competition, data = MvBioDat)
summary(mv_bio_mod1)

# categorical litter
mv_bio_mod2 <- lm(LogPerCapWeight.g ~ Litter * Competition, data = MvBioDat)
summary(mv_bio_mod2)

# compare models
AIC(mv_bio_mod1, mv_bio_mod2)
# continuous is better: delta AIC = 5.6
# plot(mv_bio_mod1)

# remove interaction?
mv_bio_mod3 <- update(mv_bio_mod1, ~. -Litter.g:Competition)
summary(mv_bio_mod3)
anova(mv_bio_mod1, mv_bio_mod3, test = "F") # yes
# F: 0.0001, P: 0.99

# remove main effects?
mv_bio_mod4 <- update(mv_bio_mod3, ~. -Litter.g)
summary(mv_bio_mod4)
anova(mv_bio_mod3, mv_bio_mod4, test = "F") # yes
# F: 1.82, P = 0.18

mv_bio_mod5 <- update(mv_bio_mod4, ~. -Competition)
summary(mv_bio_mod5)
anova(mv_bio_mod4, mv_bio_mod5, test = "F") # yes
# F: 3.96, P = 0.05

mv_bio_mod6 <- lm(LogPerCapWeight.g ~ 1, data = MvBioDat)


#### Ev total biomass ####

# continuous litter
ev_tot_bio_mod1 <- lm(LogWeight.g ~ Litter.g * Competition, data = EvBioDat)
summary(ev_tot_bio_mod1)

# categorical litter
ev_tot_bio_mod2 <- lm(LogWeight.g ~ Litter * Competition, data = EvBioDat)
summary(ev_tot_bio_mod2)

# compare models
AIC(ev_tot_bio_mod1, ev_tot_bio_mod2)
# categorical better: delta AIC = 8.1
# plot(ev_tot_bio_mod2)

# remove interaction?
ev_tot_bio_mod3 <- update(ev_tot_bio_mod2, ~. -Litter:Competition)
summary(ev_tot_bio_mod3)
anova(ev_tot_bio_mod2, ev_tot_bio_mod3, test = "F") # yes
# F: 1.24, P: 0.31

# remove main effects?
ev_tot_bio_mod4 <- update(ev_tot_bio_mod3, ~. -Litter)
summary(ev_tot_bio_mod4)
anova(ev_tot_bio_mod3, ev_tot_bio_mod4, test = "F") # no
# F: 4.14, P = 0.01

ev_tot_bio_mod5 <- update(ev_tot_bio_mod3, ~. -Competition)
summary(ev_tot_bio_mod5)
anova(ev_tot_bio_mod3, ev_tot_bio_mod5, test = "F") # no
# F: 388.14, P < 0.001

# post-hoc test
ev_tot_bio_mod6 <- aov(LogWeight.g ~ Litter + Competition, data = EvBioDat)
summary(ev_tot_bio_mod6)
TukeyHSD(ev_tot_bio_mod6)
ev_tot_mult_comp <- TukeyHSD(ev_tot_bio_mod6)$Litter
ev_tot_mult_comp2 <- tibble(comp = rownames(ev_tot_mult_comp),
                           diff = as.numeric(ev_tot_mult_comp[,1]),
                           lwr = as.numeric(ev_tot_mult_comp[,2]),
                           upr = as.numeric(ev_tot_mult_comp[,3]),
                           p = as.numeric(ev_tot_mult_comp[,4]))


#### Ev per capita biomass ####

# continuous litter
ev_bio_mod1 <- lm(LogPerCapWeight.g ~ Litter.g * Competition, data = EvBioDat)
summary(ev_bio_mod1)

# categorical litter
ev_bio_mod2 <- lm(LogPerCapWeight.g ~ Litter * Competition, data = EvBioDat)
summary(ev_bio_mod2)

# compare models
AIC(ev_bio_mod1, ev_bio_mod2)
# categorical better: delta AIC = 3.9
# plot(ev_bio_mod2)

# remove interaction?
ev_bio_mod3 <- update(ev_bio_mod2, ~. -Litter:Competition)
summary(ev_bio_mod3)
anova(ev_bio_mod2, ev_bio_mod3, test = "F") # yes
# F: 0.40, P: 0.75

# remove main effects?
ev_bio_mod4 <- update(ev_bio_mod3, ~. -Litter)
summary(ev_bio_mod4)
anova(ev_bio_mod3, ev_bio_mod4, test = "F") # yes
# F: 0.84, P = 0.48

ev_bio_mod5 <- update(ev_bio_mod4, ~. -Competition)
summary(ev_bio_mod5)
anova(ev_bio_mod4, ev_bio_mod5, test = "F") # no
# F: 110.24, P < 0.001


#### relative abundance ####

# continuous litter
rel_mod1 <- lm(LogWeight.g ~ offset(LogTotalWeight.g) + Litter.g, data = MvRelDat)
summary(rel_mod1)

# categorical litter
rel_mod2 <- lm(LogWeight.g ~ offset(LogTotalWeight.g) + Litter, data = MvRelDat)
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

# post-hoc test
rel_mod4 <- aov(LogWeight.g ~ offset(LogTotalWeight.g) + Litter, data = MvRelDat)
summary(rel_mod4)
TukeyHSD(rel_mod4)
rel_mult_comp <- TukeyHSD(rel_mod4)$Litter
rel_mult_comp2 <- tibble(comp = rownames(rel_mult_comp),
                            diff = as.numeric(rel_mult_comp[,1]),
                            lwr = as.numeric(rel_mult_comp[,2]),
                            upr = as.numeric(rel_mult_comp[,3]),
                            p = as.numeric(rel_mult_comp[,4]))


#### total biomass ####

# continuous litter
tot_mod1 <- lm(LogTotalWeight.g ~ Litter.g * SpPresent, data = totBioDat)
summary(tot_mod1)

# categorical litter
tot_mod2 <- lm(LogTotalWeight.g ~ Litter * SpPresent, data = totBioDat)
summary(tot_mod2)

# compare models
AIC(tot_mod1, tot_mod2)
# categorical is better: delta AIC 5.8
# plot(tot_mod2)

# remove interaction?
tot_mod3 <- update(tot_mod2, ~. -Litter:SpPresent)
summary(tot_mod3)
anova(tot_mod2, tot_mod3, test = "F") # yes
# F: 1.86, P: 0.102

# remove litter?
tot_mod4 <- update(tot_mod3, ~. -Litter)
summary(tot_mod4)
anova(tot_mod3, tot_mod4, test = "F") # no
# F = 2.82, P: 0.045

# remove competition?
tot_mod5 <- update(tot_mod3, ~. -SpPresent)
summary(tot_mod5)
anova(tot_mod3, tot_mod5, test = "F") # no
# F = 303.91, P: < 2.2e-16

# post-hoc test
tot_mod6 <- aov(LogTotalWeight.g ~ Litter + SpPresent, data = totBioDat)
summary(tot_mod6)
TukeyHSD(tot_mod6)

# none of the litter treatments are sig different
# remove litter
tot_mod7 <- update(tot_mod4, ~. -SpPresent)
summary(tot_mod7)
anova(tot_mod4, tot_mod7) # keep SpPresent
# F = 280.31, P: < 2.2e-16

# post-hoc test
tot_mod8 <- aov(LogTotalWeight.g ~ SpPresent, data = totBioDat)
summary(tot_mod8)
TukeyHSD(tot_mod8) # Ev is different from the ones with Mv
tot_mult_comp <- TukeyHSD(tot_mod8)$SpPresent
tot_mult_comp2 <- tibble(comp = rownames(tot_mult_comp),
                         diff = as.numeric(tot_mult_comp[,1]),
                         lwr = as.numeric(tot_mult_comp[,2]),
                         upr = as.numeric(tot_mult_comp[,3]),
                         p = as.numeric(tot_mult_comp[,4]))


#### output ####
write_csv(tidy(mv_tot_bio_mod1), "output/mv_tot_bio_full_model.csv")
write_csv(tidy(mv_bio_mod1), "output/mv_percap_bio_full_model.csv")
write_csv(tidy(ev_tot_bio_mod2), "output/ev_tot_bio_full_model.csv")
write_csv(ev_tot_mult_comp2, "output/ev_tot_bio_mult_comp.csv")
write_csv(tidy(ev_bio_mod2), "output/ev_percap_bio_full_model.csv")
write_csv(tidy(rel_mod2), "output/mv_relative_abundance_full_model.csv")
write_csv(rel_mult_comp2, "output/mv_relative_abundance_mult_comp.csv")

write_csv(MvBioDat, "intermediate-data/mv_biomass_data.csv")
write_csv(EvBioDat, "intermediate-data/ev_biomass_data.csv")

save(mv_tot_bio_mod3, file = "output/mv_tot_bio_simplified_model.rda")
save(mv_bio_mod6, file = "output/mv_percap_bio_simplified_model.rda")
save(ev_tot_bio_mod3, file = "output/ev_tot_bio_simplified_model.rda")
save(ev_bio_mod4, file = "output/ev_percap_bio_simplified_model.rda")
save(rel_mod2, file = "output/relative_biomass_model.rda")