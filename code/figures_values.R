#### goals #### 

# create tables and figures


#### set up ####

# clear all existing data
rm(list=ls())

# load libraries
library(tidyverse)
library(cowplot)

# import models
load("output/mv_establishment_model.rda")
load("output/mv_percap_bio_model.rda")

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
        axis.text = element_text(size = 7, color = "black"),
        axis.title = element_text(size = 9),
        legend.text = element_text(size = 7),
        legend.title = element_text(size = 7),
        strip.background = element_blank(),
        strip.text = element_text(size = 8))

col_pal = c("#92B851","#44A5B6")
line_pal = c("solid", "dashed")
shape_pal = c(21, 22)


#### edit data ####

# Mv establishment
MvEstDat <- MvEstDat %>%
  mutate(Planting = dplyr::recode(Competition, "0" = "alone", "1" = "in competition")) %>%
  ungroup()

# Ev establishment
EvEstDat <- EvEstDat %>%
  mutate(Litter = factor(Litter, levels = c("None", "Low", "Med", "High")),
         Planting = dplyr::recode(Competition, "0" = "alone", "1" = "in competition"))

# Ev infection
EvInfDat <- EvInfDat %>%
  mutate(Litter = factor(Litter, levels = c("None", "Low", "Med", "High")),
         Planting = dplyr::recode(Competition, "0" = "alone", "1" = "in competition"))

# Ev biomass
EvBioDat <- EvBioDat %>%
  mutate(Litter = factor(Litter, levels = c("None", "Low", "Med", "High")),
         PerCapWeight.g = Weight.g/GermEv,
         Planting = dplyr::recode(Competition, "0" = "alone", "1" = "in competition"))

# Mv biomass
MvBioDat <- MvBioDat %>%
  mutate(PerCapWeight.g = Weight.g/GermMv,
         Planting = dplyr::recode(Competition, "0" = "alone", "1" = "in competition"))

# establishment data
estDatL <- estDat %>%
  filter(Shade == "no") %>%
  select(Date2, Litter, SpPresent, Competition, PotID, NewGermEv, NewGermMv2) %>%
  pivot_longer(cols = starts_with("NewGerm"), names_to = "Species", values_to = "Establishment",
               names_prefix = "NewGerm") %>%
  mutate(Planting = dplyr::recode(Competition, "0" = "alone", "1" = "in competition"),
         EstDate = case_when(Species == "Mv2" & SpPresent == "Mv" & PotID == 1 ~ "2018-07-11", 
                             Species == "Ev" & SpPresent == "Ev" & PotID == 1 ~ "2018-07-24",
                             TRUE ~ NA_character_) %>%
           as.Date(),
         Litter = factor(Litter, levels = c("None", "Low", "Med", "High")) %>%
           dplyr::recode(Med = "Medium")) %>%
  filter(!(SpPresent == "Mv" & Species == "Ev") & !(SpPresent == "Ev" & Species == "Mv2")) %>%
  mutate(Species = dplyr::recode(Species, Ev = "E. virginicus", Mv2 = "M. vimineum") %>%
           factor(levels = c("M. vimineum", "E. virginicus")))


#### percent changes ####

# Mv establishment
MvEstDat %>%
  filter(Litter == "None" & Competition == 0) %>%
  summarise(est = mean(PropEstMvDenCor))

# Mv establishment high litter effect
MvEstDat %>%
  filter(Litter %in% c("None", "High")) %>%
  group_by(Competition, Litter) %>%
  summarise(est = mean(PropEstMvDenCor)) %>%
  ungroup() %>%
  pivot_wider(names_from = Litter, values_from = est) %>%
  mutate(diff = round((High - None)*100, digits = 1))

# Ev establishment
EvEstDat %>%
  filter(Litter == "None" & Competition == 0) %>%
  summarise(est = mean(PropEstEv))

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

# Ev disease
EvInfDat %>%
  filter(Litter == "None" & Competition == 0) %>%
  summarise(est = mean(PropInfEv))

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

# Mv plant biomass
MvBioDat %>%
  filter(Litter == "None" & Competition == 0) %>%
  summarise(est = mean(PerCapWeight.g)) 

# Mv plant biomass high litter effect
MvBioDat %>%
  filter(Litter %in% c("None", "High")) %>%
  group_by(Competition, Litter) %>%
  summarise(est = mean(PerCapWeight.g)) %>%
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

# Ev plant biomass
EvBioDat %>%
  filter(Litter == "None" & Competition == 0) %>%
  summarise(est = mean(PerCapWeight.g)) 

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

# relative abundance
MvBioDat %>%
  filter(Litter == "None" & SpPresent == "Ev+Mv") %>%
  summarise(est = mean(Weight.g/TotalWeight.g))

# relative abundance litter effect
MvBioDat %>%
  filter(SpPresent == "Ev+Mv") %>%
  group_by(Litter) %>%
  summarise(est = mean(Weight.g/TotalWeight.g)) %>%
  ungroup() %>%
  pivot_wider(names_from = Litter, values_from = est) %>%
  mutate(Diff_Low = (Low - None)/None * 100,
         Diff_Med = (Med - None)/None * 100,
         Diff_High = (High - None)/None * 100)


#### prediction dataset ####

# continuous
pred_dat_con <- tibble(Litter.g = rep(seq(0, 3.64, length.out = 100), 2),
                   Competition = rep(c(0, 1), each = 100)) %>%
  mutate(Planting = dplyr::recode(Competition, "0" = "alone", "1" = "in competition"))


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
  annotate(geom = "text",
           label = expression(paste("litter x competition: ", italic(P), " = 0.04", sep = "")),
           x = 0.9, y = 0.91, size = 2.5) +
  scale_fill_manual(values = col_pal, name = "Planting treatment") +
  scale_linetype_manual(values = line_pal, name = "Planting treatment") +
  scale_shape_manual(values = shape_pal, name = "Planting treatment") +
  xlab("Litter (g)") +
  ylab(expression(paste(italic(M.), " ", italic( vimineum), " establishment", sep = ""))) +
  coord_cartesian(ylim = c(0.6, 0.91)) +
  fig_theme +
  theme(legend.position = c(0.21, 0.17),
        legend.margin = margin(0, 0, 0, 0))


#### Ev establishment figure ####

ev_est_fig <- ggplot(EvEstDat, aes(x = Litter.g, y = PropEstEv, fill = Planting, shape = Planting)) +
  stat_summary(fun.data = "mean_cl_boot", geom = "errorbar", width = 0, position = position_dodge(0.3), linetype = "solid", size = 0.4) +
  stat_summary(fun = "mean", geom = "point", position = position_dodge(0.3), size = 3) +
  annotate(geom = "text",
           label = expression(paste("litter (categorical): ", italic(P), " < 0.001", sep = "")),
           x = 0.9, y = 0.91, size = 2.5) +
  scale_fill_manual(values = col_pal) +
  scale_shape_manual(values = shape_pal) +
  xlab("Litter (g)") +
  ylab(expression(paste(italic(E.), " ", italic(virginicus), " establishment", sep = ""))) +
  coord_cartesian(ylim = c(0.6, 0.91)) +
  fig_theme +
  theme(legend.position = "none")


#### Ev infection figure ####

ev_inf_fig <- ggplot(EvInfDat, aes(x = Litter.g, y = PropInfEv, fill = Planting, shape = Planting)) +
  stat_summary(fun.data = "mean_cl_boot", geom = "errorbar", width = 0, position = position_dodge(0.3), linetype = "solid", size = 0.4) +
  stat_summary(fun = "mean", geom = "point", position = position_dodge(0.3), size = 3) +
  annotate(geom = "text",
           label = expression(paste("litter (categorical): ", italic(P), " < 0.001", sep = "")),
           x = 0.9, y = 0.82, size = 2.5) +
  annotate(geom = "text",
           label = expression(paste("competition: ", italic(P), " < 0.001", sep = "")),
           x = 0.65, y = 0.78, size = 2.5) +
  scale_fill_manual(values = col_pal, name = "Species\nplanted") +
  scale_shape_manual(values = shape_pal, name = "Species\nplanted") +
  xlab("Litter (g)") +
  ylab(expression(paste(italic(E.), " ", italic(virginicus), " disease incidence", sep = ""))) +
  coord_cartesian(ylim = c(0, 0.82)) +
  fig_theme +
  theme(legend.position = "none")



#### combined establishment and infection figures ####

pdf("output/Fig2.pdf", width = 3, height = 8)
plot_grid(mv_est_fig, ev_est_fig, ev_inf_fig,
          nrow = 3,
          labels = c("a", "b", "c"),
          label_size = 10)
dev.off()


#### Mv plant biomass figure ####

# predicted values
mv_pred_percap_bio <- pred_dat_con %>%
  mutate(pred = predict(mv_bio_mod3, newdata = .),
         pred.se = predict(mv_bio_mod3, newdata = ., se.fit = T)$se.fit)

# visualize
mv_pcbio_fig <- ggplot(MvBioDat, aes(x = Litter.g, y = LogPerCapWeight.g, fill = Planting, shape = Planting, linetype = Planting)) +
  geom_ribbon(data = mv_pred_percap_bio, aes(y = pred, ymin = pred - pred.se, ymax = pred + pred.se), alpha = 0.5, color = NA) +
  geom_line(data = mv_pred_percap_bio, aes(y = pred)) +
  stat_summary(fun.data = "mean_cl_boot", geom = "errorbar", width = 0, position = position_dodge(0.3), linetype = "solid", size = 0.4) +
  annotate(geom = "text",
           label = expression(paste("litter: ", italic(P), " = 0.02", sep = "")),
           x = 0.4, y = 0.2, size = 2.5) +
  annotate(geom = "text",
           label = expression(paste("competition: ", italic(P), " = 0.04", sep = "")),
           x = 0.65, y = 0.17, size = 2.5) +
  stat_summary(fun = "mean", geom = "point", position = position_dodge(0.3), size = 3) +
  scale_fill_manual(values = col_pal, name = "Planting treatment") +
  scale_linetype_manual(values = line_pal, name = "Planting treatment") +
  scale_shape_manual(values = shape_pal, name = "Planting treatment") +
  xlab("Litter (g)") +
  ylab(expression(paste(italic(M.), " ", italic( vimineum), " plant biomass (ln g)", sep = ""))) +
  fig_theme +
  theme(legend.position = c(0.8, 0.83),
        legend.margin = margin(0, 0, 0, 0))


#### Ev plant biomass ####

ev_pcbio_fig <- ggplot(EvBioDat, aes(x = Litter.g, y = LogPerCapWeight.g, fill = Planting, shape = Planting, linetype = Planting)) +
  stat_summary(fun.data = "mean_cl_boot", geom = "errorbar", width = 0, position = position_dodge(0.3), linetype = "solid", size = 0.4) +
  stat_summary(fun = "mean", geom = "point", position = position_dodge(0.3), size = 3) +
  annotate(geom = "text",
  label = expression(paste("competition: ", italic(P), " < 0.001", sep = "")),
x = 0.65, y = -1.4, size = 2.5) +
  scale_fill_manual(values = col_pal) +
  scale_linetype_manual(values = line_pal) +
  scale_shape_manual(values = shape_pal) +
  xlab("Litter (g)") +
  ylab(expression(paste(italic(E.), " ", italic( virginicus), " plant biomass (ln g)", sep = ""))) +
  coord_cartesian(ylim = c(-4.5, -1.4)) +
  fig_theme +
  theme(legend.position = "none")


#### relative abundance ####

rel_fig <- ggplot(filter(MvBioDat, SpPresent == "Ev+Mv"), 
                  aes(x = Litter.g, y = Weight.g/TotalWeight.g)) +
  stat_summary(fun.data = "mean_cl_boot", geom = "errorbar", width = 0, linetype = "solid", size = 0.4) +
  stat_summary(fun = "mean", geom = "point", size = 3, shape = shape_pal[2], fill = col_pal[2]) +
  annotate(geom = "text",
           label = expression(paste("litter (categorical): ", italic(P), " = 0.02", sep = "")),
           x = 0.9, y = 0.99, size = 2.5) +
  xlab("Litter (g)") +
  ylab(expression(paste(italic(M.), " ", italic( vimineum), " relative abundance", sep = ""))) +
  coord_cartesian(ylim = c(0.958, 0.99)) +
  fig_theme +
  theme(legend.position = "none")


#### combined biomass figure ####

pdf("output/Fig3.pdf", width = 3, height = 8)
plot_grid(mv_pcbio_fig, ev_pcbio_fig, rel_fig,
          nrow = 3,
          labels = c("a", "b", "c"),
          label_size = 10)
dev.off()


#### Temporal establishment ####
pdf("output/FigS1.pdf", width = 6, height = 5)
ggplot(estDatL, aes(x = Date2, y = Establishment, fill = Planting, shape = Planting, linetype = Planting)) +
  geom_vline(aes(xintercept = EstDate), linetype = "dotted", size = 0.2) +
  stat_summary(fun.data = "mean_cl_boot", geom = "errorbar", size = 0.3, width = 0, linetype = "solid") +
  stat_summary(fun = "mean", geom = "line", size = 0.5) +
  stat_summary(fun = "mean", geom = "point") +
  facet_grid(Species ~ Litter, scales = "free_y", switch = "y", ) +
  scale_shape_manual(values = shape_pal, name = "Planting treatment") +
  scale_fill_manual(values = col_pal, name = "Planting treatment") +  
  scale_color_manual(values = col_pal, name = "Planting treatment") +  
  scale_linetype_manual(values = line_pal, name = "Planting treatment") +  
  xlab("Date") +
  ylab("Plants per pot") +
  ggtitle("Litter") +
  fig_theme +
  theme(strip.placement = "outside",
        strip.text.y = element_text(size = 9, face = "italic"),
        legend.position = "bottom",
        legend.direction = "horizontal",
        plot.title = element_text(size = 11, hjust = 0.5, vjust = -3),
        legend.box.margin = margin(0, 0, 0, 0),
        legend.margin = margin(0, 0, 0, 0))
dev.off()
