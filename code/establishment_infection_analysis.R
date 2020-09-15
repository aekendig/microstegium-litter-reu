#### goals #### 

# analyze effects of litter and live plant competition on:
  # Mv and Ev establishment
  # Ev infection


#### set up ####

# clear all existing data
rm(list=ls())

# load libraries
library(tidyverse)

# import data
MvEstDat <- read_csv("intermediate-data/mv_establishment_data.csv")
EvEstDat <- read_csv("intermediate-data/ev_establishment_data.csv")
EvInfDat <- read_csv("intermediate-data/ev_infection_data.csv")


#### edit data ####

# Mv establishment
MvEstDat <- MvEstDat %>%
  mutate(Litter = factor(Litter, levels = c("None", "Low", "Med", "High")),
         Planting = ifelse(Competition == 0, "alone", "in competition"))

# Ev establishment
EvEstDat <- EvEstDat %>%
  mutate(Litter = factor(Litter, levels = c("None", "Low", "Med", "High")),
         Planting = ifelse(Competition == 0, "alone", "in competition"))

# Ev infection
EvInfDat <- EvInfDat %>%
  mutate(Litter = factor(Litter, levels = c("None", "Low", "Med", "High")),
         Planting = ifelse(Competition == 0, "alone", "in competition"))


#### visualize ####

# Mv corrected denominator (number planted)
ggplot(MvEstDat, aes(x = Litter.g, y = PropEstMvDenCor, color = SpPresent)) +
  stat_summary(fun.data = "mean_cl_boot", geom = "errorbar", width = 0, position = position_dodge(0.1)) +
  stat_summary(fun = "mean", geom = "point", position = position_dodge(0.1)) +
  theme_bw()

# Mv corrected numerator (number established)
ggplot(MvEstDat, aes(x = Litter.g, y = PropEstMvNumCor, color = SpPresent)) +
  stat_summary(fun.data = "mean_cl_boot", geom = "errorbar", width = 0, position = position_dodge(0.1)) +
  stat_summary(fun = "mean", geom = "point", position = position_dodge(0.1)) +
  theme_bw()

# Ev establishment
ggplot(EvEstDat, aes(x = Litter.g, y = PropEstEv, color = SpPresent)) +
  stat_summary(fun.data = "mean_cl_boot", geom = "errorbar", width = 0, position = position_dodge(0.1)) +
  stat_summary(fun = "mean", geom = "point", position = position_dodge(0.1)) +
  theme_bw()

# Ev infection
ggplot(EvInfDat, aes(x = Litter.g, y = PropInfEv, color = SpPresent)) +
  stat_summary(fun.data = "mean_cl_boot", geom = "errorbar", width = 0, position = position_dodge(0.1)) +
  stat_summary(fun = "mean", geom = "point", position = position_dodge(0.1)) +
  theme_bw()


#### prediction dataset ####

pred_dat <- tibble(Litter.g = rep(seq(0, 3.64, length.out = 100), 2),
                   Competition = rep(c(0, 1), each = 100)) %>%
  mutate(Planting = ifelse(Competition == 0, "alone", "in competition"))


#### Mv establishment ####

# corrected denominator (number planted)
mv_est_mod1 <- glm(cbind(NewGermMv2, UngermMvDen) ~ Litter.g * Competition, data = MvEstDat, family = binomial)
summary(mv_est_mod1)

# corrected denominator (number planted) categorical
mv_est_mod1b <- glm(cbind(NewGermMv2, UngermMvDen) ~ Litter * Competition, data = MvEstDat, family = binomial)
summary(mv_est_mod1b)

# compare models
AIC(mv_est_mod1, mv_est_mod1b)
# continuous fits better
# plot(mv_est_mod1)

# predicted values
mv_pred_mod1 <- pred_dat %>%
  mutate(pred = predict(mv_est_mod1, newdata = ., type = "response"),
         pred.se = predict(mv_est_mod1, newdata = ., type = "response", se.fit = T)$se.fit)

# visualize
ggplot(MvEstDat, aes(x = Litter.g, y = PropEstMvDenCor, color = Planting, fill = Planting)) +
  geom_ribbon(data = mv_pred_mod1, aes(y = pred, ymin = pred - pred.se, ymax = pred + pred.se), alpha = 0.6, color = NA) +
  geom_line(data = mv_pred_mod1, aes(y = pred)) +
  stat_summary(fun.data = "mean_cl_boot", geom = "errorbar", width = 0, position = position_dodge(0.1)) +
  stat_summary(fun = "mean", geom = "point", position = position_dodge(0.1)) +
  theme_bw()

# corrected numerator (number established)
mv_est_mod2 <- glm(cbind(CorGermMv, UngermMvNum) ~ Litter.g * Competition, data = MvEstDat, family = binomial)
summary(mv_est_mod2)

# corrected numerator (number established) categorical
mv_est_mod2b <- glm(cbind(CorGermMv, UngermMvNum) ~ Litter * Competition, data = MvEstDat, family = binomial)
summary(mv_est_mod2b)

# compare models
AIC(mv_est_mod2, mv_est_mod2b)
# continuous fits better
# plot(mv_est_mod2)

# predicted values
mv_pred_mod2 <- pred_dat %>%
  mutate(pred = predict(mv_est_mod2, newdata = ., type = "response"),
         pred.se = predict(mv_est_mod2, newdata = ., type = "response", se.fit = T)$se.fit)

# visualize
ggplot(MvEstDat, aes(x = Litter.g, y = PropEstMvNumCor, color = Planting, fill = Planting)) +
  geom_ribbon(data = mv_pred_mod2, aes(y = pred, ymin = pred - pred.se, ymax = pred + pred.se), alpha = 0.6, color = NA) +
  geom_line(data = mv_pred_mod2, aes(y = pred)) +
  stat_summary(fun.data = "mean_cl_boot", geom = "errorbar", width = 0, position = position_dodge(0.1)) +
  stat_summary(fun = "mean", geom = "point", position = position_dodge(0.1)) +
  theme_bw()


#### Ev establishment ####

# model
ev_est_mod1 <- glm(cbind(NewGermEv, UngermEv) ~ Litter.g * Competition, data = EvEstDat, family = binomial)
summary(ev_est_mod1)

# model (categorical)
ev_est_mod1b <- glm(cbind(NewGermEv, UngermEv) ~ Litter * Competition, data = EvEstDat, family = binomial)
summary(ev_est_mod1b)

# compare models
AIC(ev_est_mod1, ev_est_mod1b)
# categorical fits better
# plot(ev_est_mod1b)

# predicted values
ev_pred_mod1 <- pred_dat %>%
  mutate(pred = predict(ev_est_mod1, newdata = ., type = "response"),
         pred.se = predict(ev_est_mod1, newdata = ., type = "response", se.fit = T)$se.fit)

# visualize
ggplot(EvEstDat, aes(x = Litter.g, y = PropEstEv, color = Planting, fill = Planting)) +
  geom_ribbon(data = ev_pred_mod1, aes(y = pred, ymin = pred - pred.se, ymax = pred + pred.se), alpha = 0.6, color = NA) +
  geom_line(data = ev_pred_mod1, aes(y = pred)) +
  stat_summary(fun.data = "mean_cl_boot", geom = "errorbar", width = 0, position = position_dodge(0.1)) +
  stat_summary(fun = "mean", geom = "point", position = position_dodge(0.1)) +
  theme_bw()


#### Ev infection ####

# model
ev_inf_mod1 <- glm(cbind(InfectedEv, HealthyEv) ~ Litter.g * Competition, data = EvInfDat, family = binomial)
summary(ev_inf_mod1)
# plot(ev_inf_mod1)

# model (categorical)
ev_inf_mod1b <- glm(cbind(InfectedEv, HealthyEv) ~ Litter * Competition, data = EvInfDat, family = binomial)
summary(ev_inf_mod1b)

# compare models
AIC(ev_inf_mod1, ev_inf_mod1b)
# categorical better
# plot(ev_inf_mod1b)

# predicted values
inf_pred_mod1 <- pred_dat %>%
  mutate(pred = predict(ev_inf_mod1, newdata = ., type = "response"),
         pred.se = predict(ev_inf_mod1, newdata = ., type = "response", se.fit = T)$se.fit)

# visualize
ggplot(EvInfDat, aes(x = Litter.g, y = PropInfEv, color = Planting, fill = Planting)) +
  geom_ribbon(data = inf_pred_mod1, aes(y = pred, ymin = pred - pred.se, ymax = pred + pred.se), alpha = 0.6, color = NA) +
  geom_line(data = inf_pred_mod1, aes(y = pred)) +
  stat_summary(fun.data = "mean_cl_boot", geom = "errorbar", width = 0, position = position_dodge(0.1)) +
  stat_summary(fun = "mean", geom = "point", position = position_dodge(0.1)) +
  theme_bw()


#### outputs ####

# save models
save(mv_est_mod1, file = "./output/mv_establishment_model.rda")
save(ev_est_mod1b, file = "./output/ev_establishment_model.rda")
save(ev_inf_mod1b, file = "./output/ev_infection_model.rda")

