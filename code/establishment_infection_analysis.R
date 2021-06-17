#### goals #### 

# analyze effects of litter and live plant competition on:
  # Mv and Ev establishment
  # Ev infection


#### set up ####

# clear all existing data
rm(list=ls())

# load libraries
library(tidyverse)
library(broom)
library(multcomp)

# import data
MvEstDat <- read_csv("intermediate-data/mv_establishment_data.csv")
MvCountDat <- read_csv("intermediate-data/mv_final_count_data.csv")
EvEstDat <- read_csv("intermediate-data/ev_establishment_data.csv")
EvInfDat <- read_csv("intermediate-data/ev_infection_data.csv")


#### edit data ####

# Mv establishment
MvEstDat <- MvEstDat %>%
  mutate(Litter = factor(Litter, levels = c("None", "Low", "Med", "High")),
         Planting = ifelse(Competition == 0, "alone", "in competition"))

# Mv counts
MvCountDat <- MvCountDat %>%
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


#### Mv establishment ####

# continuous litter
mv_est_mod1 <- glm(cbind(NewGermMv2, UngermMvDen) ~ Litter.g * Competition, data = MvEstDat, family = binomial)
summary(mv_est_mod1)

# categorical litter
mv_est_mod2 <- glm(cbind(NewGermMv2, UngermMvDen) ~ Litter * Competition, data = MvEstDat, family = binomial)
summary(mv_est_mod2)

# compare models
AIC(mv_est_mod1, mv_est_mod2)
# continuous fits better: delta AIC = 4.7
# plot(mv_est_mod1)

# remove interaction?
mv_est_mod3 <- update(mv_est_mod1, ~. -Litter.g:Competition)
summary(mv_est_mod3)
anova(mv_est_mod1, mv_est_mod3, test = "Chi") # no
# dev: -4.06, P: 0.04


#### Mv abundance ####

# mean vs. variance
mean(MvCountDat$GermMv)
var(MvCountDat$GermMv) # smaller

# continuous litter
mv_abu_mod1 <- glm(GermMv ~ Litter.g * Competition, data = MvCountDat, family = poisson)
summary(mv_abu_mod1)

# categorical litter
mv_abu_mod2 <- glm(GermMv ~ Litter * Competition, data = MvCountDat, family = poisson)
summary(mv_abu_mod2)

# compare models
AIC(mv_abu_mod1, mv_abu_mod2)
# continuous fits better: delta AIC = 7.1
# plot(mv_abu_mod1)

# remove interaction?
mv_abu_mod3 <- update(mv_abu_mod1, ~. -Litter.g:Competition)
summary(mv_abu_mod3)
anova(mv_abu_mod1, mv_abu_mod3, test = "Chi") # yes
# dev: -0.69, P: 0.45

# remove main effects?
mv_abu_mod4 <- update(mv_abu_mod3, ~. -Competition)
summary(mv_abu_mod4)
anova(mv_abu_mod3, mv_abu_mod4, test = "Chi") # yes
# dev: -0.23, P: 0.63

mv_abu_mod5 <- update(mv_abu_mod4, ~. -Litter.g)
summary(mv_abu_mod5)
anova(mv_abu_mod4, mv_abu_mod5, test = "Chi") # yes
# dev: -0.19, P: 0.66


#### Ev establishment ####

# continuous litter
ev_est_mod1 <- glm(cbind(NewGermEv, UngermEv) ~ Litter.g * Competition, data = EvEstDat, family = binomial)
summary(ev_est_mod1)

# categorical litter
ev_est_mod2 <- glm(cbind(NewGermEv, UngermEv) ~ Litter * Competition, data = EvEstDat, family = binomial)
summary(ev_est_mod2)

# compare models
AIC(ev_est_mod1, ev_est_mod2)
# categorical fits better: delta AIC = 8.4
# plot(ev_est_mod2)

# remove interaction?
ev_est_mod3 <- update(ev_est_mod2, ~. -Litter:Competition)
summary(ev_est_mod3)
anova(ev_est_mod2, ev_est_mod3, test = "Chi") # yes
# dev: -3.74, P: 0.29

# remove main effects?
ev_est_mod4 <- update(ev_est_mod3, ~. -Competition)
summary(ev_est_mod4)
anova(ev_est_mod3, ev_est_mod4, test = "Chi") # yes
# dev: -1.57, P: 0.21

ev_est_mod5 <- update(ev_est_mod4, ~. -Litter)
summary(ev_est_mod5)
anova(ev_est_mod4, ev_est_mod5, test = "Chi") # no
# dev: -22.04, P < 0.001

# post-hoc test
summary(glht(ev_est_mod4, linfct = mcp(Litter = "Tukey")))
ev_est_mult_comp <- summary(glht(ev_est_mod4, linfct = mcp(Litter = "Tukey")))$test
ev_est_mult_comp2 <- confint(glht(ev_est_mod4, linfct = mcp(Litter = "Tukey")))$confint
ev_est_mult_comp3 <- tibble(comp = names(ev_est_mult_comp$coefficients),
                            diff = as.numeric(ev_est_mult_comp$coefficients),
                            lwr = as.numeric(ev_est_mult_comp2[,2]),
                            upr = as.numeric(ev_est_mult_comp2[,3]),
                            p = as.numeric(ev_est_mult_comp$pvalues))


#### Ev infection ####

# continuous litter
ev_inf_mod1 <- glm(cbind(InfectedEv, HealthyEv) ~ Litter.g * Competition, data = EvInfDat, family = binomial)
summary(ev_inf_mod1)

# categorical litter
ev_inf_mod2 <- glm(cbind(InfectedEv, HealthyEv) ~ Litter * Competition, data = EvInfDat, family = binomial)
summary(ev_inf_mod2)

# compare models
AIC(ev_inf_mod1, ev_inf_mod2)
# categorical better: delta AIC = 143.2
# plot(ev_inf_mod2)

# remove interaction?
ev_inf_mod3 <- update(ev_inf_mod2, ~. -Litter:Competition)
summary(ev_inf_mod3)
anova(ev_inf_mod2, ev_inf_mod3, test = "Chi") # yes
# dev: -6.62, P: 0.08

# remove main effects?
ev_inf_mod4 <- update(ev_inf_mod3, ~. -Litter)
summary(ev_inf_mod4)
anova(ev_inf_mod3, ev_inf_mod4, test = "Chi") # no
# dev: -180.64, P: < 0.001

ev_inf_mod5 <- update(ev_inf_mod3, ~. -Competition)
summary(ev_inf_mod5)
anova(ev_inf_mod3, ev_inf_mod5, test = "Chi") # no
# dev: -247.28, P < 0.001

# post-hoc test
summary(glht(ev_inf_mod3, linfct = mcp(Litter = "Tukey")))
ev_inf_mult_comp <- summary(glht(ev_inf_mod3, linfct = mcp(Litter = "Tukey")))$test
ev_inf_mult_comp2 <- confint(glht(ev_inf_mod3, linfct = mcp(Litter = "Tukey")))$confint
ev_inf_mult_comp3 <- tibble(comp = names(ev_inf_mult_comp$coefficients),
                            diff = as.numeric(ev_inf_mult_comp$coefficients),
                            lwr = as.numeric(ev_inf_mult_comp2[,2]),
                            upr = as.numeric(ev_inf_mult_comp2[,3]),
                            p = as.numeric(ev_inf_mult_comp$pvalues))


#### outputs ####

# save models
save(mv_est_mod1, file = "output/mv_establishment_model.rda")
save(ev_est_mod4, file = "output/ev_establishment_simplified_model.rda")
save(ev_inf_mod3, file = "output/ev_infection_simplified_model.rda")

write_csv(tidy(mv_est_mod1), "output/mv_establishment_full_model.csv")
write_csv(tidy(mv_abu_mod1), "output/mv_abundance_full_model.csv")
write_csv(tidy(ev_est_mod2), "output/ev_establishment_full_model.csv")
write_csv(ev_est_mult_comp3, "output/ev_establishment_mult_comp.csv")
write_csv(tidy(ev_inf_mod2), "output/ev_infection_full_model.csv")
write_csv(ev_inf_mult_comp3, "output/ev_infection_mult_comp.csv")
