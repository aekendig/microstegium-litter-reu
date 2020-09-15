#### goals #### 

# create tables and figures


#### set up ####

# clear all existing data
rm(list=ls())

# load libraries
library(car)
library(tidyverse)
library(cowplot)
library(ggsignif)

# import models
load("output/mv_establishment_model.rda")
load("output/ev_establishment_model.rda")
load("output/ev_infection_model.rda")
load("output/mv_bio_model.rda")
load("output/mv_percap_bio_model.rda")
load("output/ev_bio_model.rda")
load("output/ev_percap_bio_model.rda")

# import data
MvEstDat <- read_csv("intermediate-data/mv_establishment_data.csv")
EvEstDat <- read_csv("intermediate-data/ev_establishment_data.csv")
EvInfDat <- read_csv("intermediate-data/ev_infection_data.csv")
estDat <- read_csv("intermediate-data/establishment_infection_data.csv")
MvBioDat <- read_csv("intermediate-data/mv_biomass_data.csv")
EvBioDat <- read_csv("intermediate-data/ev_biomass_data.csv")

# figure theme
fig_theme <- theme_bw() +
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text = element_text(size = 9, color = "black"),
        axis.title = element_text(size = 11),
        legend.text = element_text(size = 9),
        legend.title = element_text(size = 9))

col_pal = c("#92B851","#44A5B6")
line_pal = c("solid", "dashed")
shape_pal = c(21, 22)


#### edit data ####

# Mv establishment
MvEstDat <- MvEstDat %>%
  mutate(Planting = ifelse(Competition == 0, "alone", "in competition"))

# Ev establishment
EvEstDat <- EvEstDat %>%
  mutate(Litter = factor(Litter, levels = c("None", "Low", "Med", "High")),
         Planting = ifelse(Competition == 0, "alone", "in competition"))

# Ev infection
EvInfDat <- EvInfDat %>%
  mutate(Litter = factor(Litter, levels = c("None", "Low", "Med", "High")),
         Planting = ifelse(Competition == 0, "alone", "in competition"))

# Ev biomass
EvBioDat <- EvBioDat %>%
  mutate(Litter = factor(Litter, levels = c("None", "Low", "Med", "High")),
         PerCapWeight.g = Weight.g/GermEv)

# Mv biomass
MvBioDat <- MvBioDat %>%
  mutate(PerCapWeight.g = Weight.g/GermMv)


#### model summaries ####

summary(mv_est_mod1)
Anova(mv_est_mod1, type = 3)

summary(ev_est_mod1b)
Anova(ev_est_mod1b, type = 3)
Anova(ev_est_mod1b, type = 2)

summary(ev_inf_mod1b)
Anova(ev_inf_mod1b, type = 3)
Anova(ev_inf_mod1b, type = 2)

summary(mv_bio_mod1)
Anova(mv_bio_mod1, type = 3)
Anova(mv_bio_mod1, type = 2)

summary(mv_bio_mod2)
Anova(mv_bio_mod2, type = 3)
Anova(mv_bio_mod2, type = 2)

summary(ev_bio_mod1b)
Anova(ev_bio_mod1b, type = 3)
Anova(ev_bio_mod1b, type = 2)

summary(ev_bio_mod2b)
Anova(ev_bio_mod2b, type = 3)
Anova(ev_bio_mod2b, type = 2)


#### percent changes ####

# Mv establishment high litter effect
MvEstDat %>%
  filter(Litter %in% c("None", "High")) %>%
  group_by(Competition, Litter) %>%
  summarise(est = mean(PropEstMvDenCor)) %>%
  ungroup() %>%
  pivot_wider(names_from = Litter, values_from = est) %>%
  mutate(diff = round((High - None)*100, digits = 1))

# Ev establishment average litter effect
EvEstDat %>%
  group_by(Competition, Litter) %>%
  summarise(est = mean(PropEstEv)) %>%
  ungroup() %>%
  pivot_wider(names_from = Litter, values_from = est) %>%
  mutate(Diff_Low = Low - None,
         Diff_Med = Med - None,
         Diff_High = High - None) %>%
  select(-c(None:High)) %>%
  pivot_longer(cols = starts_with("Diff_"), names_to = "Litter", values_to = "Diff") %>%
  summarise(diff = round(mean(Diff)*100, 1))

# Ev disease average litter effect
EvInfDat %>%
  group_by(Competition, Litter) %>%
  summarise(est = mean(PropInfEv)) %>%
  ungroup() %>%
  pivot_wider(names_from = Litter, values_from = est) %>%
  mutate(Diff_Low = Low - None,
         Diff_Med = Med - None,
         Diff_High = High - None) %>%
  select(-c(None:High)) %>%
  pivot_longer(cols = starts_with("Diff_"), names_to = "Litter", values_to = "Diff") %>%
  summarise(diff = round(mean(Diff)*100, 1))

# Ev disease average competition effect
EvInfDat %>%
  group_by(Competition, Litter) %>%
  summarise(est = mean(PropInfEv)) %>%
  ungroup() %>%
  pivot_wider(names_from = Competition, values_from = est) %>%
  rename("alone" = "0", "comp" = "1") %>%
  mutate(Diff = comp - alone) %>%
  summarise(diff = round(mean(Diff)*100, 1))

# Mv plant biomass high litter effect
MvBioDat %>%
  filter(Litter %in% c("None", "High")) %>%
  group_by(Competition, Litter) %>%
  summarise(est = mean(PerCapWeight.g)) %>%
  ungroup() %>%
  pivot_wider(names_from = Litter, values_from = est) %>%
  mutate(Diff = (High - None)/None) %>%
  summarise(diff = round(mean(Diff)*100, 1))

# Mv total biomass high litter effect
MvBioDat %>%
  filter(Litter %in% c("None", "High")) %>%
  group_by(Competition, Litter) %>%
  summarise(est = mean(Weight.g)) %>%
  ungroup() %>%
  pivot_wider(names_from = Litter, values_from = est) %>%
  mutate(Diff = (High - None)/None) %>%
  summarise(diff = round(mean(Diff)*100, 1))

# Mv plant biomass average competition effect
MvBioDat %>%
  group_by(Competition, Litter) %>%
  summarise(est = mean(PerCapWeight.g)) %>%
  ungroup() %>%
  pivot_wider(names_from = Competition, values_from = est) %>%
  rename("alone" = "0", "comp" = "1") %>%
  mutate(Diff = (comp - alone)/alone) %>%
  summarise(diff = round(mean(Diff)*100, 1))

# Mv total biomass average competition effect
MvBioDat %>%
  group_by(Competition, Litter) %>%
  summarise(est = mean(Weight.g)) %>%
  ungroup() %>%
  pivot_wider(names_from = Competition, values_from = est) %>%
  rename("alone" = "0", "comp" = "1") %>%
  mutate(Diff = (comp - alone)/alone) %>%
  summarise(diff = round(mean(Diff)*100, 1))

# Ev total biomass average litter effect
EvBioDat %>%
  group_by(Competition, Litter) %>%
  summarise(est = mean(Weight.g)) %>%
  ungroup() %>%
  pivot_wider(names_from = Litter, values_from = est) %>%
  mutate(Diff_Low = (Low - None)/None,
         Diff_Med = (Med - None)/None,
         Diff_High = (High - None)/None) %>%
  select(-c(None:High)) %>%
  pivot_longer(cols = starts_with("Diff_"), names_to = "Litter", values_to = "Diff") %>%
  summarise(diff = round(mean(Diff)*100, 1))

# Ev plant biomass average litter effect
EvBioDat %>%
  group_by(Competition, Litter) %>%
  summarise(est = mean(PerCapWeight.g)) %>%
  ungroup() %>%
  pivot_wider(names_from = Litter, values_from = est) %>%
  mutate(Diff_Low = (Low - None)/None,
         Diff_Med = (Med - None)/None,
         Diff_High = (High - None)/None) %>%
  select(-c(None:High)) %>%
  pivot_longer(cols = starts_with("Diff_"), names_to = "Litter", values_to = "Diff") %>%
  summarise(diff = round(mean(Diff)*100, 1))

# Ev plant biomass average competition effect
EvBioDat %>%
  group_by(Competition, Litter) %>%
  summarise(est = mean(PerCapWeight.g)) %>%
  ungroup() %>%
  pivot_wider(names_from = Competition, values_from = est) %>%
  rename("alone" = "0", "comp" = "1") %>%
  mutate(Diff = (comp - alone)/alone) %>%
  summarise(diff = round(mean(Diff)*100, 1))

# Ev total biomass average competition effect
EvBioDat %>%
  group_by(Competition, Litter) %>%
  summarise(est = mean(Weight.g)) %>%
  ungroup() %>%
  pivot_wider(names_from = Competition, values_from = est) %>%
  rename("alone" = "0", "comp" = "1") %>%
  mutate(Diff = (comp - alone)/alone) %>%
  summarise(diff = round(mean(Diff)*100, 1))


#### prediction datasets ####

# continuous
pred_dat_con <- tibble(Litter.g = rep(seq(0, 3.64, length.out = 100), 2),
                   Competition = rep(c(0, 1), each = 100)) %>%
  mutate(Planting = ifelse(Competition == 0, "alone", "in competition"))

# for percentage increase
pred_dat_inc <- tibble(Litter.g = rep(c(0, 1, 3.64), 2),
                   Competition = rep(c(0, 1), each = 3))

# categorical
pred_dat_cat <- tibble(Litter = rep(c("None", "Low", "Med", "High"), 2),
                       Competition = rep(c(0, 1), each = 4))


#### percentage change ####

# Mv establishment
(mv_est_inc <- pred_dat_inc %>%
  mutate(pred = predict(mv_est_mod1, newdata = ., type = "response")))
0.859-0.703 # all litter effect no Elymus
0.808-0.731 # all litter with Elymus

# Ev establishment
(ev_est_cat <- pred_dat_cat %>%
  mutate(pred = predict(ev_est_mod1b, newdata = ., type = "response")))
mean(c(0.843-0.743, 0.843-0.74, 0.843-0.763)) # average litter effect on Elymus without competition
mean(c(0.813-0.713, 0.813-0.773, 0.813-0.703)) # average litter effect on Elymus wit competition
mean(c(0.843-0.743, 0.843-0.74, 0.843-0.763,
       0.813-0.713, 0.813-0.773, 0.813-0.703)) # overall average litter effect

# Ev infection
(ev_inf_cat <- pred_dat_cat %>%
  mutate(pred = predict(ev_inf_mod1b, newdata = ., type = "response")))
mean(c(0.250-0.0330, 0.0868-0.0330, 0.153-0.0330)) # average litter effect on Elymus without competition
mean(c(0.250-0.0330, 0.0868-0.0330, 0.153-0.0330,
       0.562-0.225, 0.289-0.225, 0.498-0.225)) # overall average litter effect
0.225-0.0330 # competition effect without litter
mean(c(0.225-0.0330, 0.562-0.250, 0.289-0.0868, 0.498-0.153))

# Mv per capita biomass
(mv_percap_bio_inc <- pred_dat_inc %>%
    mutate(pred = predict(mv_bio_mod2, newdata = .)))
(exp(-0.0938)-exp(-0.261))/exp(-0.0938) # max litter effect with no competition
(exp(-0.198)-exp(-0.379))/exp(-0.198) # max litter effect with competition
(exp(-0.0938)-exp(-0.198))/exp(-0.0938) # competition no litter

# Mv total biomass
(mv_bio_inc <- pred_dat_inc %>%
    mutate(pred = predict(mv_bio_mod1, newdata = .)))
(exp(3.64)-exp(3.47))/exp(3.64) # max litter effect with no competition
(exp(3.64)-exp(3.47))/exp(3.64)  # competition no litter

# Ev per capita biomass
(ev_percap_bio_inc <- pred_dat_cat %>%
    mutate(pred = predict(ev_bio_mod2b, newdata = .)))
(exp(-1.88)-exp(-3.55))/exp(-1.88)  # competition no litter
mean(c((exp(-1.88)-exp(-3.55))/exp(-1.88), 
       (exp(-1.98)-exp(-4.16))/exp(-1.98),
       (exp(-1.88)-exp(-3.93))/exp(-1.88),
       (exp(-1.91)-exp(-3.69))/exp(-1.91))) # average competition

# Ev total biomass
(ev_bio_inc <- pred_dat_cat %>%
    mutate(pred = predict(ev_bio_mod1b, newdata = .)))
mean(c((exp(1.86)-exp(1.63))/exp(1.86), (exp(1.86)-exp(1.73))/exp(1.86), (exp(1.86)-exp(1.73))/exp(1.86))) # max litter effect with no competition
(exp(1.86)-exp(1.56))/exp(1.86) # competition no litter
mean(c((exp(1.86)-exp(1.56))/exp(1.86),
       (exp(1.63)-exp(-0.591))/exp(1.63),
       (exp(1.73)-exp(-0.284))/exp(1.73),
       (exp(1.73)-exp(-0.143))/exp(1.73))) # average competition


#### Mv establishment figure ####

# predicted values
mv_pred_est <- pred_dat_con %>%
  mutate(pred = predict(mv_est_mod1, newdata = ., type = "response"),
         pred.se = predict(mv_est_mod1, newdata = ., type = "response", se.fit = T)$se.fit)

# visualize
mv_est_fig <- ggplot(MvEstDat, aes(x = Litter.g, y = PropEstMvDenCor, fill = Planting, shape = Planting, linetype = Planting)) +
  geom_ribbon(data = mv_pred_est, aes(y = pred, ymin = pred - pred.se, ymax = pred + pred.se), alpha = 0.5, color = NA) +
  geom_line(data = mv_pred_est, aes(y = pred)) +
  stat_summary(fun.data = "mean_cl_boot", geom = "errorbar", width = 0, position = position_dodge(0.3), linetype = "solid", size = 0.4) +
  stat_summary(fun = "mean", geom = "point", position = position_dodge(0.3), size = 3) +
  scale_fill_manual(values = col_pal) +
  scale_linetype_manual(values = line_pal) +
  scale_shape_manual(values = shape_pal) +
  xlab("Litter (g)") +
  ylab(expression(paste(italic(Microstegium), " establishment", sep = ""))) +
  coord_cartesian(ylim = c(0.6, 0.94)) +
  fig_theme +
  theme(legend.position = c(0.25, 0.15),
        legend.margin = margin(0, 0, 0, 0))


#### Ev establishment figure ####

# significance dataset
summary(ev_est_mod1b)
ev_est_sig <- tibble(start = c(0, 0, 0),
                     end = c(0.91, 1.82, 3.64),
                     y = c(0.89, 0.91, 0.93),
                     label = c("0.003", "0.002", "0.01"),
                     Planting = "alone")

# visualize
ev_est_fig <- ggplot(EvEstDat, aes(x = Litter.g, y = PropEstEv, fill = Planting, shape = Planting, linetype = Planting)) +
  stat_summary(fun.data = "mean_cl_boot", geom = "errorbar", width = 0, position = position_dodge(0.3), linetype = "solid", size = 0.4) +
  stat_summary(fun = "mean", geom = "point", position = position_dodge(0.3), size = 3) +
  geom_signif(data = ev_est_sig,
              aes(xmin = start, xmax = end, annotations = label, y_position = y),
              textsize = 2.5, vjust = -0.2,
              manual = TRUE,
              size = 0.4) +
  scale_fill_manual(values = col_pal) +
  scale_linetype_manual(values = line_pal) +
  scale_shape_manual(values = shape_pal) +
  xlab("Litter (g)") +
  ylab(expression(paste(italic(Elymus), " establishment", sep = ""))) +
  coord_cartesian(ylim = c(0.6, 0.94)) +
  fig_theme +
  theme(legend.position = "none")


#### combined establishment figure ####

pdf("output/Figure2.pdf", width = 3, height = 6)
plot_grid(mv_est_fig, ev_est_fig,
          nrow = 2,
          labels = c("a", "b"),
          label_size = 11)
dev.off()


#### Ev infection figure ####

# significance dataset
summary(ev_inf_mod1b)
ev_inf_sig <- tibble(start = c(-0.1, 0, 0, 0),
                     end = c(0.1, 0.91, 1.82, 3.64),
                     y = c(0.4, 0.72, 0.76, 0.8),
                     label = "< 0.001",
                     Planting = "alone")

# visualize
pdf("output/Figure3.pdf", width = 3, height = 3)
ggplot(EvInfDat, aes(x = Litter.g, y = PropInfEv, fill = Planting, shape = Planting, linetype = Planting)) +
  stat_summary(fun.data = "mean_cl_boot", geom = "errorbar", width = 0, position = position_dodge(0.3), linetype = "solid", size = 0.4) +
  stat_summary(fun = "mean", geom = "point", position = position_dodge(0.3), size = 3) +
  geom_signif(data = ev_inf_sig,
              aes(xmin = start, xmax = end, annotations = label, y_position = y),
              textsize = 2.5, vjust = -0.2,
              manual = TRUE,
              size = 0.4) +
  scale_fill_manual(values = col_pal) +
  scale_linetype_manual(values = line_pal) +
  scale_shape_manual(values = shape_pal) +
  xlab("Litter (g)") +
  ylab(expression(paste(italic(Elymus), " disease incidence", sep = ""))) +
  coord_cartesian(ylim = c(0, 0.81)) +
  fig_theme +
  theme(legend.position = "none")
dev.off()


#### Mv per capita biomass figure ####

# predicted values
mv_pred_percap_bio <- pred_dat_con %>%
  mutate(pred = predict(mv_bio_mod2, newdata = .),
         pred.se = predict(mv_bio_mod2, newdata = ., se.fit = T)$se.fit)

# visualize
ggplot(MvBioDat, aes(x = Litter.g, y = LogPerCapWeight.g, fill = Planting, shape = Planting, linetype = Planting)) +
  geom_ribbon(data = mv_pred_percap_bio, aes(y = pred, ymin = pred - pred.se, ymax = pred + pred.se), alpha = 0.5, color = NA) +
  geom_line(data = mv_pred_percap_bio, aes(y = pred)) +
  stat_summary(fun.data = "mean_cl_boot", geom = "errorbar", width = 0, position = position_dodge(0.3), linetype = "solid", size = 0.4) +
  stat_summary(fun = "mean", geom = "point", position = position_dodge(0.3), size = 3) +
  scale_fill_manual(values = col_pal) +
  scale_linetype_manual(values = line_pal) +
  scale_shape_manual(values = shape_pal) +
  xlab("Litter (g)") +
  ylab(expression("Log(", paste(italic(Microstegium), " plant biomass)", sep = ""))) +
  # coord_cartesian(ylim = c(0.6, 0.94)) +
  fig_theme


#### Mv total biomass figure ####

# predicted values
mv_pred_bio <- pred_dat_con %>%
  mutate(pred = predict(mv_bio_mod1, newdata = .),
         pred.se = predict(mv_bio_mod1, newdata = ., se.fit = T)$se.fit)

# visualize
ggplot(MvBioDat, aes(x = Litter.g, y = LogWeight.g, fill = Planting, shape = Planting, linetype = Planting)) +
  geom_ribbon(data = mv_pred_bio, aes(y = pred, ymin = pred - pred.se, ymax = pred + pred.se), alpha = 0.5, color = NA) +
  geom_line(data = mv_pred_bio, aes(y = pred)) +
  stat_summary(fun.data = "mean_cl_boot", geom = "errorbar", width = 0, position = position_dodge(0.3), linetype = "solid", size = 0.4) +
  stat_summary(fun = "mean", geom = "point", position = position_dodge(0.3), size = 3) +
  scale_fill_manual(values = col_pal) +
  scale_linetype_manual(values = line_pal) +
  scale_shape_manual(values = shape_pal) +
  xlab("Litter (g)") +
  ylab(expression(paste("Log(total ", italic(Microstegium), " biomass)", sep = ""))) +
  # coord_cartesian(ylim = c(0.6, 0.94)) +
  fig_theme




#### supplementary figures ####


#### Mv total establishment ####

ggplot(MvEstDat, aes(x = Litter.g, y = GermMv, fill = Planting, shape = Planting, linetype = Planting)) +
  stat_summary(fun.data = "mean_cl_boot", geom = "errorbar", width = 0, position = position_dodge(0.3), linetype = "solid", size = 0.4) +
  stat_summary(fun = "mean", geom = "point", position = position_dodge(0.3), size = 3) +
  scale_fill_manual(values = col_pal) +
  scale_linetype_manual(values = line_pal) +
  scale_shape_manual(values = shape_pal) +
  xlab("Litter (g)") +
  ylab(expression(paste(italic(Microstegium), " germinants", sep = ""))) +
  fig_theme



#### old code starts here ####



#### biomass by treatment ####

# values for text
mbio_sum <-mbdat %>%
  group_by(Litter.g, SpPresent) %>%
  summarise(avg_weight = round(mean(Weight.g), 2),
            se_weight = round(sd(Weight.g)/sqrt(length(Weight.g)), 2)) 

ebio_sum <- ebdat %>%
  group_by(Litter.g, SpPresent) %>%
  summarise(avg_weight = round(mean(Weight.g), 2),
            se_weight = round(sd(Weight.g)/sqrt(length(Weight.g)), 2))

# save
write_csv(mbio_sum, "./output/mv_biomass_summary_table.csv")
write_csv(ebio_sum, "./output/ev_biomass_summary_table.csv")


#### edit data ####

# count data for temporal plot
countdat <- dat %>%
  filter(Shade == "no") %>%
  select(Date3, TrtID, Litter, SpPresent, Shade, NewGermEv, CorGermMv) %>%
  gather(key = "Focal", value = "Counts", -c(Date3, TrtID, Litter, SpPresent, Shade)) %>%
  mutate(Focal= dplyr::recode(Focal, NewGermEv = "Ev counts", CorGermMv = "Mv counts"),
         Litter = factor(Litter, levels = c("None", "Low", "Med", "High")),
         CountType = ifelse(Date3 == "2018-10-05" & Focal == "Ev counts", "stems", "individuals"),
         EstDate = ifelse(Focal == "Ev counts", "2018-07-24", "2018-06-27") %>% as.Date("%Y-%m-%d")) %>%
  filter(!is.na(Counts) & !(Focal == "Mv counts" & SpPresent == "Ev"))

# check that there are no unexpected counts
filter(countdat, Focal == "Ev counts" & SpPresent == "Mv")
filter(countdat, Focal == "Mv counts" & SpPresent == "Ev")

# establishment data
estdat <- select(eedat, Litter.g, SpPresent, Shade, NewGermEv) %>%
  dplyr::rename(Estab = NewGermEv) %>%
  mutate(Response = "Ev planted") %>%
  full_join(select(medat, Litter.g, SpPresent, Shade, CorGermMv) %>%
              dplyr::rename(Estab = CorGermMv) %>%
              mutate(Response = "Mv planted")) %>%
  full_join(select(medat, Litter.g, SpPresent, Shade, NewGermMv2) %>%
              dplyr::rename(Estab = NewGermMv2) %>%
              mutate(Response = "Mv planted + litter"))

# predicted establishment
estpred <- tibble(SpPresent = "Mv",
                  Litter.g = seq(0, max(dat$Litter.g), length.out = 100),
                  Response = "Mv planted") %>%
  mutate(Estab = predict(meMod2_2, newdata = ., type = "response"),
         Estab.se = predict(meMod2_2, newdata = ., type = "response", se.fit = T)$se.fit,
         Estab.CI = Estab.se * 1.96)
  
# biomass data
biodat <- select(ebdat, Litter.g, SpPresent, Shade, Weight.g) %>%
  mutate(Weight = Weight.g,
         Response = "Ev total") %>%
  select(-Weight.g) %>%
  full_join(select(ebdat, Litter.g, SpPresent, Shade, Weight.g, GermEv) %>%
              mutate(Weight = Weight.g/GermEv,
                     Response = "Ev individual") %>%
              select(-c(Weight.g, GermEv))) %>%
  full_join(select(mbdat, Litter.g, SpPresent, Shade, Weight.g) %>%
              mutate(Weight = Weight.g,
                     Response = "Mv total") %>%
              select(-Weight.g)) %>%
  full_join(select(mbdat, Litter.g, SpPresent, Shade, Weight.g, GermMv) %>%
              mutate(Weight = Weight.g/GermMv,
                     Response = "Mv individual") %>%
              select(-c(Weight.g, GermMv))) %>%
  mutate(Response = factor(Response, levels = c("Ev total", "Ev individual", "Mv total", "Mv individual")))

# predicted biomass
biopred <- tibble(Litter.g = rep(seq(0, max(dat$Litter.g), length.out = 100), 2),
                  SpPresent = rep(c("Mv", "Ev+Mv"), each = 100),
                  Response = "Mv total") %>%
  mutate(Weight = predict(mbMod2_2, newdata = ., type = "response"),
         Weight.se = predict(mbMod2_2, newdata = ., type = "response", se.fit = T)$se.fit,
         Weight.CI = Weight.se * 1.96,
         Response = factor(Response, levels = c("Ev total", "Ev individual", "Mv total", "Mv individual")))

# predicted infection
infpred <- tibble(SpPresent = rep(c("Ev", "Ev+Mv"), each = 100),
                  Litter.g = rep(seq(0, max(dat$Litter.g), length.out = 100), 2)) %>%
  mutate(PropInfEv = predict(eiMod2, newdata = ., type = "response"),
         PropInfEv.se = predict(eiMod2, newdata = ., type = "response", se.fit = T)$se.fit,
         PropInfEv.CI = PropInfEv.se * 1.96)


#### percent changes ####

# % change: y - py = x
# % change: (1 - p)y = x
# % change: 1 - p = x/y
# % change: p = 1 - x/y

# Mv total biomass litter effect (max amount vs. none)
min_mv_bio_litt = biopred %>%
  filter(Litter.g == max(dat$Litter.g) & SpPresent == "Mv") %>%
  select(Weight)

max_mv_bio = biopred %>%
  filter(Litter.g == min(dat$Litter.g) & SpPresent == "Mv") %>%
  select(Weight)

(prop_mv_bio_litt = 1 - min_mv_bio_litt/max_mv_bio)

# Mv total biomass live plant effect
min_mv_bio_live = biopred %>%
  filter(Litter.g == min(dat$Litter.g) & SpPresent == "Ev+Mv") %>%
  select(Weight)

(prop_mv_bio_live = 1 - min_mv_bio_live/max_mv_bio)
  
# Ev total biomass litter effect (present vs. none)

max_ev_bio = coef(ebMod4)[1]
min_ev_bio_litt = coef(ebMod4)[1] + coef(ebMod4)[3]

(prop_ev_bio_litt = 1 - min_ev_bio_litt/max_ev_bio)

# Ev total biomass live effect

min_ev_bio_live = coef(ebMod4)[1] + coef(ebMod4)[2]

(prop_ev_bio_live = 1 - min_ev_bio_live/max_ev_bio_live)

# Mv establishment litter effect (max amount vs. none)
min_mv_est_litt = estpred %>%
  filter(Litter.g == max(dat$Litter.g) & SpPresent == "Mv") %>%
  select(Estab)

max_mv_est_litt = estpred %>%
  filter(Litter.g == min(dat$Litter.g) & SpPresent == "Mv") %>%
  select(Estab)

(prop_mv_est_litt = 1 - min_mv_est_litt/max_mv_est_litt)

# Ev establishment litter effect

max_ev_est_litt = exp(coef(eeMod4)[1])
min_ev_est_litt = exp(coef(eeMod4)[1] + coef(eeMod4)[3])

(prop_ev_est_litt = 1 - min_ev_est_litt/max_ev_est_litt)

# Ev infection both effects

min_ev_inf = logit2prob(coef(eiMod3)[1])
max_ev_inf_litt = logit2prob(coef(eiMod3)[1] + coef(eiMod3)[3])

(prop_ev_inf_litt = max_ev_inf_litt/min_ev_inf - 1)

max_ev_inf_live = logit2prob(coef(eiMod3)[1] + coef(eiMod3)[2])

(prop_ev_inf_live = max_ev_inf_live/min_ev_inf - 1)

max_ev_inf = logit2prob(coef(eiMod3)[1] + coef(eiMod3)[2] + coef(eiMod3)[3]+ coef(eiMod3)[4])

(prop_ev_inf = max_ev_inf/min_ev_inf - 1)


#### create tables ####

Anova(eeMod4)
tab_model(eeMod4, pred.labels = c("(Intercept)","Mv present", "Litter present"))

Anova(meMod2_2) # p-values match tables
tab_model(meMod2_2, meMod2_6, dv.labels = c("planted", "planted + litter"), pred.labels = c("(Intercept)","Ev present", "Litter (g)"))
summary(meMod2_6)

tab_model(ebMod4, ebMod6, dv.labels = c("total", "individual"), pred.labels = c("(Intercept)","Mv present", "Litter present"))

tab_model(mbMod2_2, mbMod2_6, dv.labels = c("total", "individual"), pred.labels = c("(Intercept)","Ev present", "Litter (g)"))

tab_model(eiMod2, eiMod3, dv.labels = c("quantitative", "qualitative"), pred.labels = c("(Intercept)","Mv present", "Litter (g)", "Litter present", "Mv present:Litter present"))


#### figure theme ####

base_theme <- theme_bw() +
  theme(axis.text = element_text(size = axisText, color="black"),
        axis.title = element_text(size = axisTitle),
        plot.title = element_text(size = axisTitle, hjust = 0.5),
        panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.text = element_text(size=legendText),
        legend.title = element_text(size=legendTitle),
        legend.box.margin = margin(-10, 0, -10, -10),
        legend.margin = margin(c(0, 1, 1, 1)),
        strip.background = element_blank(),
        strip.text = element_text(size = axisTitle),
        strip.placement = "outside")

col_pal = c("#44A5B6","black", "#92D050")


#### temporal count figure ####

countplot <- countdat %>%
  ggplot(aes(x = Date3, y = Counts, fill = SpPresent, shape = CountType)) +
  geom_vline(aes(xintercept = EstDate), linetype = "dashed", size = 0.2) +
  stat_summary(fun.data = "mean_cl_boot", geom = "errorbar", size = 0.3) +
  stat_summary(fun.y = "mean", geom = "point") +
  stat_summary(fun.y = "mean", geom = "line", size = 0.2) +
  facet_grid(Focal~Litter, scales = "free_y", switch = "y") +
  scale_shape_manual(values = c(21, 22), name = "Count type") +
  scale_fill_manual(values = col_pal, name = "Species present") +  
  guides(fill = guide_legend(override.aes = list(shape = 21, size = 2.5)),
         shape = guide_legend(override.aes = list(size = 2.5))) +
  xlab("Date") +
  ylab("Ev counts") +
  base_theme +
  theme(axis.title.y = element_blank())

pdf("./output/temporal_count_figure.pdf", width = 6, height = 4)
countplot
dev.off()

jpeg("./output/temporal_count_figure.jpeg", width = 6, height = 4, units = "in", res = 600)
countplot
dev.off()


#### establishment figure ####

base_estplot <- estdat %>%
  ggplot(aes(x = Litter.g, y = Estab, fill = SpPresent)) +
  geom_line(data = estpred) +
  geom_ribbon(data = estpred, aes(ymin = Estab - Estab.CI, ymax = Estab + Estab.CI), fill = "gray", alpha = 0.5, color = NA) +
  stat_summary(fun.data = "mean_cl_boot", geom = "errorbar", size = 0.5, width = 0.3, position = position_dodge(0.4)) +
  stat_summary(fun.y = "mean", geom = "point", shape = 21, size = 2.5, position = position_dodge(0.4)) +
  facet_wrap(~Response) +
  scale_fill_manual(values = c("#44A5B6","black", "#92D050"), name = "Species present") +  
  xlab("Litter (g)") +
  ylab("Established individuals") +
  base_theme +
  theme(legend.position = "bottom",
        legend.direction = "horizontal",
        legend.box.margin = margin(0, 0, 0, 0)) + 
  guides(fill = guide_legend(override.aes = list(size = 4)))

estplot_ev <- filter(estdat, Response == "Ev planted") %>%
  ggplot(aes(x = Litter.g, y = Estab, fill = SpPresent)) +
  stat_summary(fun.data = "mean_cl_boot", geom = "errorbar", size = 0.5, width = 0.3, position = position_dodge(0.4)) +
  stat_summary(fun.y = "mean", geom = "point", shape = 21, size = 2.5, position = position_dodge(0.4)) +
  ylab("Established individuals") +
  xlab("") +
  ggtitle("Ev planted") +
  scale_fill_manual(values = col_pal[1:2]) +
  ylim(25, 49) +
  base_theme +
  theme(legend.position = "none")
  
estplot_mvp <- filter(estdat, Response == "Mv planted") %>%
  ggplot(aes(x = Litter.g, y = Estab, fill = SpPresent)) +
  geom_line(data = estpred) +
  geom_ribbon(data = estpred, aes(ymin = Estab - Estab.CI, ymax = Estab + Estab.CI), fill = "gray", alpha = 0.5, color = NA) +
  stat_summary(fun.data = "mean_cl_boot", geom = "errorbar", size = 0.5, width = 0.3, position = position_dodge(0.4)) +
  stat_summary(fun.y = "mean", geom = "point", shape = 21, size = 2.5, position = position_dodge(0.4)) +
  xlab("Litter (g)") +
  ggtitle("Mv planted") +
  scale_fill_manual(values = col_pal[2:3]) +
  ylim(25, 49) +
  base_theme +
  theme(axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        legend.position = "none")

estplot_mvl <- filter(estdat, Response == "Mv planted + litter") %>%
  ggplot(aes(x = Litter.g, y = Estab, fill = SpPresent)) +
  stat_summary(fun.data = "mean_cl_boot", geom = "errorbar", size = 0.5, width = 0.3, position = position_dodge(0.4)) +
  stat_summary(fun.y = "mean", geom = "point", shape = 21, size = 2.5, position = position_dodge(0.4)) +
  ggtitle("Mv planted + litter") +
  scale_fill_manual(values = col_pal[2:3]) +
  xlab("") +
  ylim(25, 49) +
  base_theme +
  theme(axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        legend.position = "none")

# combine plots
estleg <- get_legend(base_estplot)

estplot1 <- plot_grid(estplot_ev, estplot_mvp, estplot_mvl,
                     nrow = 1,
                     rel_widths = c(1, 0.85, 0.85),
                     labels = letters[1:3],
                     label_size = axisTitle)

estplot <- plot_grid(estplot1, estleg,
                     nrow = 2,
                     rel_heights = c(1, 0.1))

# save plots
pdf("./output/establishment_figure.pdf", width = 6, height = 3)
estplot
dev.off()

jpeg("./output/establishment_figure.jpeg", width = 6, height = 3, units = "in", res = 600)
estplot
dev.off()


#### biomass figure ####

base_bioplot <- biodat %>%
  ggplot(aes(x = Litter.g, y = Weight, fill = SpPresent)) +
  geom_line(data = biopred) +
  geom_ribbon(data = biopred, aes(ymin = Weight - Weight.CI, ymax = Weight + Weight.CI), alpha = 0.5, color = NA) +
  stat_summary(fun.data = "mean_cl_boot", geom = "errorbar", size = 0.5, width = 0.3, position = position_dodge(0.4)) +
  stat_summary(fun.y = "mean", geom = "point", shape = 21, size = 2.5, position = position_dodge(0.4)) +
  facet_wrap(~ Response, scales = "free") +
  scale_fill_manual(values = c("#44A5B6","black", "#92D050"), name = "Species present") +  
  xlab("Litter (g)") +
  ylab("Aboveground biomass (g)") +
  base_theme +
  theme(legend.position = "bottom",
                    legend.direction = "horizontal",
                    legend.box.margin = margin(0, 0, 0, 0))

bioplot_evt <- filter(biodat, Response == "Ev total") %>%
  ggplot(aes(x = Litter.g, y = Weight, fill = SpPresent)) +
  stat_summary(fun.data = "mean_cl_boot", geom = "errorbar", size = 0.5, width = 0.3, position = position_dodge(0.4)) +
  stat_summary(fun.y = "mean", geom = "point", shape = 21, size = 2.5, position = position_dodge(0.4)) +
  scale_fill_manual(values = col_pal[1:2]) +  
  ylab("Ev biomass (g)") +
  ggtitle("Total") +
  base_theme +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        legend.position = "none")

bioplot_evi <- filter(biodat, Response == "Ev individual") %>%
  ggplot(aes(x = Litter.g, y = Weight, fill = SpPresent)) +
  stat_summary(fun.data = "mean_cl_boot", geom = "errorbar", size = 0.5, width = 0.3, position = position_dodge(0.4)) +
  stat_summary(fun.y = "mean", geom = "point", shape = 21, size = 2.5, position = position_dodge(0.4)) +
  scale_fill_manual(values = col_pal[1:2]) +  
  ggtitle("Individual") +
  base_theme +
  theme(axis.title = element_blank(),
        axis.text.x = element_blank(),
        legend.position = "none")

bioplot_mvt <- filter(biodat, Response == "Mv total") %>%
  ggplot(aes(x = Litter.g, y = Weight, fill = SpPresent)) +
  geom_line(data = biopred) +
  geom_ribbon(data = biopred, aes(ymin = Weight - Weight.CI, ymax = Weight + Weight.CI), alpha = 0.5, color = NA) +
  stat_summary(fun.data = "mean_cl_boot", geom = "errorbar", size = 0.5, width = 0.3, position = position_dodge(0.4)) +
  stat_summary(fun.y = "mean", geom = "point", shape = 21, size = 2.5, position = position_dodge(0.4)) +
  scale_fill_manual(values = col_pal[2:3]) +  
  ylab("Mv biomass (g)") +
  xlab("Litter (g)") +
  base_theme +
  theme(legend.position = "none")

bioplot_mvi <- filter(biodat, Response == "Mv individual") %>%
  ggplot(aes(x = Litter.g, y = Weight, fill = SpPresent)) +
  stat_summary(fun.data = "mean_cl_boot", geom = "errorbar", size = 0.5, width = 0.3, position = position_dodge(0.4)) +
  stat_summary(fun.y = "mean", geom = "point", shape = 21, size = 2.5, position = position_dodge(0.4)) +
  scale_fill_manual(values = col_pal[2:3]) +  
  xlab("Litter (g)") +
  base_theme +
  theme(axis.title.y = element_blank(),
        legend.position = "none")

# combine plots
bioleg <- get_legend(base_bioplot)

bioplot1 <- plot_grid(bioplot_evt, bioplot_evi, bioplot_mvt, bioplot_mvi,
                      nrow = 2,
                      rel_widths = c(1, 0.95, 1, 0.95),
                      labels = letters[1:4],
                      label_size = axisTitle)

bioplot <- plot_grid(bioplot1, bioleg,
                     nrow = 2,
                     rel_heights = c(1, 0.1))

pdf("./output/biomass_figure.pdf", width = 4.5, height = 4.5)
bioplot
dev.off()

jpeg("./output/biomass_figure.jpeg", width = 4.5, height = 4.5, units = "in", res = 600)
bioplot
dev.off()


#### infection figure ####

# infplot1 <- eidat %>%
#   ggplot(aes(x = Litter.g, y = PropInfEv, fill = SpPresent)) +
#   geom_line(data = infpred) +
#   geom_ribbon(data = infpred, aes(ymin = PropInfEv - PropInfEv.CI, ymax = PropInfEv + PropInfEv.CI), alpha = 0.5, color = NA) +
#   stat_summary(fun.data = "mean_cl_boot", geom = "errorbar", size = 0.5, width = 0.3, position = position_dodge(0.4)) +
#   stat_summary(fun.y = "mean", geom = "point", shape = 21, size = 2.5, position = position_dodge(0.4), color = "gray") +
#   scale_fill_manual(values = c("#44A5B6","black"), name = "Species present") +  
#   xlab("Litter (g)") +
#   ylab("Proportion of Ev stems infected") +
#   base_theme +
#   theme(legend.position = "none")

infplot2 <- eidat %>%
  mutate(Litter.present = factor(Litter.present, levels = c("No litter", "Litter"))) %>%
  ggplot(aes(x = Litter.present, y = PropInfEv, fill = SpPresent)) +
  stat_summary(fun.data = "mean_cl_boot", geom = "errorbar", size = 0.5, width = 0.3, position = position_dodge(0.4)) +
  stat_summary(fun.y = "mean", geom = "point", shape = 21, size = 2.5, position = position_dodge(0.4)) +
  scale_fill_manual(values = col_pal[1:2], name = "Species present") +  
  ylab("Proportion of Ev stems infected") +
  base_theme +
  theme(axis.title.x = element_blank()) + 
  guides(fill = guide_legend(override.aes = list(size = 4)))

pdf("./output/infection_figure.pdf", width = 3, height = 2.5)
infplot2
dev.off()

jpeg("./output/infection_figure.jpeg", width = 3, height = 2.5, units = "in", res = 600)
infplot2
dev.off()
