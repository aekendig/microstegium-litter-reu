#### goals #### 

# analyze effects of litter and live plant competition on:
  # Mv and Ev biomass


#### set up ####

# clear all existing data
rm(list=ls())

# load libraries
library(tidyverse)

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

# split by species and merge with count data
MvBioDat <- dat2 %>% 
  filter(WeighedSp == "Mv" & SpPresent != "Ev") %>%
  full_join(select(MvEstDat, TrtID, GermMv)) %>% 
  mutate(LogPerCapWeight.g = log(Weight.g/GermMv)) %>%
  filter(Shade == "no")

EvBioDat <- filter(dat2, WeighedSp == "Ev" & SpPresent != "Mv") %>%
  full_join(select(EvEstDat, TrtID, GermEv)) %>%
  mutate(LogPerCapWeight.g = log(Weight.g/GermEv))


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


#### prediction dataset ####

pred_dat <- tibble(Litter.g = rep(seq(0, 3.64, length.out = 100), 2),
                   Competition = rep(c(0, 1), each = 100)) %>%
  mutate(Planting = ifelse(Competition == 0, "alone", "in competition"))


#### Mv biomass ####

# total biomass
mv_bio_mod1 <- lm(LogWeight.g ~ Litter.g * Competition, data = MvBioDat)
summary(mv_bio_mod1)

# total biomass (categorical)
mv_bio_mod1b <- lm(LogWeight.g ~ Litter * Competition, data = MvBioDat)
summary(mv_bio_mod1b)

# compare models
AIC(mv_bio_mod1, mv_bio_mod1b)
# continuous better
# plot(mv_bio_mod1)

# predicted values
mv_pred_mod1 <- pred_dat %>%
  mutate(pred = predict(mv_bio_mod1, newdata = ., type = "response"),
         pred.se = predict(mv_bio_mod1, newdata = ., type = "response", se.fit = T)$se.fit)

# visualize
ggplot(MvBioDat, aes(x = Litter.g, y = LogWeight.g, color = Planting, fill = Planting)) +
  geom_ribbon(data = mv_pred_mod1, aes(y = pred, ymin = pred - pred.se, ymax = pred + pred.se), alpha = 0.6, color = NA) +
  geom_line(data = mv_pred_mod1, aes(y = pred)) +
  stat_summary(fun.data = "mean_cl_boot", geom = "errorbar", width = 0, position = position_dodge(0.1)) +
  stat_summary(fun = "mean", geom = "point", position = position_dodge(0.1)) +
  theme_bw()


#### Mv per capita biomass ####

# per capita biomass
mv_bio_mod2 <- lm(LogPerCapWeight.g ~ Litter.g * Competition, data = MvBioDat)
summary(mv_bio_mod2)
summary(update(mv_bio_mod2, .~. -Litter.g:Competition))

# per capita biomass (categorical)
mv_bio_mod2b <- lm(LogPerCapWeight.g ~ Litter * Competition, data = MvBioDat)
summary(mv_bio_mod2b)

# compare models
AIC(mv_bio_mod2, mv_bio_mod2b)
# continuous is slightly better
# plot(mv_bio_mod2)

# predicted values
mv_pred_mod2 <- pred_dat %>%
  mutate(pred = predict(mv_bio_mod2, newdata = ., type = "response"),
         pred.se = predict(mv_bio_mod2, newdata = ., type = "response", se.fit = T)$se.fit)

# visualize
ggplot(MvBioDat, aes(x = Litter.g, y = LogPerCapWeight.g, color = Planting, fill = Planting)) +
  geom_ribbon(data = mv_pred_mod2, aes(y = pred, ymin = pred - pred.se, ymax = pred + pred.se), alpha = 0.6, color = NA) +
  geom_line(data = mv_pred_mod2, aes(y = pred)) +
  stat_summary(fun.data = "mean_cl_boot", geom = "errorbar", width = 0, position = position_dodge(0.1)) +
  stat_summary(fun = "mean", geom = "point", position = position_dodge(0.1)) +
  theme_bw()


#### Ev biomass ####

# total biomass
ev_bio_mod1 <- lm(LogWeight.g ~ Litter.g * Competition, data = EvBioDat)
summary(ev_bio_mod1)

# total biomass (categorical)
ev_bio_mod1b <- lm(LogWeight.g ~ Litter * Competition, data = EvBioDat)
summary(ev_bio_mod1b)
summary(update(ev_bio_mod1b, .~. -Litter:Competition))

# compare models
AIC(ev_bio_mod1, ev_bio_mod1b)
# categorical better
# plot(Ev_bio_mod1b)

# predicted values
ev_pred_mod1 <- pred_dat %>%
  mutate(pred = predict(ev_bio_mod1, newdata = ., type = "response"),
         pred.se = predict(ev_bio_mod1, newdata = ., type = "response", se.fit = T)$se.fit)

# visualize
ggplot(EvBioDat, aes(x = Litter.g, y = LogWeight.g, color = Planting, fill = Planting)) +
  geom_ribbon(data = ev_pred_mod1, aes(y = pred, ymin = pred - pred.se, ymax = pred + pred.se), alpha = 0.6, color = NA) +
  geom_line(data = ev_pred_mod1, aes(y = pred)) +
  stat_summary(fun.data = "mean_cl_boot", geom = "errorbar", width = 0, position = position_dodge(0.1)) +
  stat_summary(fun = "mean", geom = "point", position = position_dodge(0.1)) +
  theme_bw()


#### Ev per capita biomass ####

# per capita biomass
ev_bio_mod2 <- lm(LogPerCapWeight.g ~ Litter.g * Competition, data = EvBioDat)
summary(ev_bio_mod2)

# per capita biomass (categorical)
ev_bio_mod2b <- lm(LogPerCapWeight.g ~ Litter * Competition, data = EvBioDat)
summary(ev_bio_mod2b)
summary(update(ev_bio_mod2b, .~. -Litter:Competition))

# compare models
AIC(ev_bio_mod2, ev_bio_mod2b)
# categorical better
# plot(ev_bio_mod2b)

# predicted values
ev_pred_mod2 <- pred_dat %>%
  mutate(pred = predict(ev_bio_mod2, newdata = ., type = "response"),
         pred.se = predict(ev_bio_mod2, newdata = ., type = "response", se.fit = T)$se.fit)

# visualize
ggplot(EvBioDat, aes(x = Litter.g, y = LogPerCapWeight.g, color = Planting, fill = Planting)) +
  geom_ribbon(data = ev_pred_mod2, aes(y = pred, ymin = pred - pred.se, ymax = pred + pred.se), alpha = 0.6, color = NA) +
  geom_line(data = ev_pred_mod2, aes(y = pred)) +
  stat_summary(fun.data = "mean_cl_boot", geom = "errorbar", width = 0, position = position_dodge(0.1)) +
  stat_summary(fun = "mean", geom = "point", position = position_dodge(0.1)) +
  theme_bw()


#### output ####
save(mv_bio_mod1, file = "output/mv_bio_model.rda")
save(mv_bio_mod2, file = "output/mv_percap_bio_model.rda")
save(ev_bio_mod1b, file = "output/ev_bio_model.rda")
save(ev_bio_mod2b, file = "output/ev_percap_bio_model.rda")

write_csv(MvBioDat, "intermediate-data/mv_biomass_data.csv")
write_csv(EvBioDat, "intermediate-data/ev_biomass_data.csv")
