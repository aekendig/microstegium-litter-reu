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
load("output/ev_establishment_simplified_model.rda")
load("output/ev_infection_simplified_model.rda")
load("output/mv_tot_bio_simplified_model.rda")
load("output/ev_tot_bio_simplified_model.rda")
load("output/mv_percap_bio_simplified_model.rda")
load("output/ev_percap_bio_simplified_model.rda")
load("output/relative_biomass_model.rda")

# import data
MvEstDat <- read_csv("intermediate-data/mv_establishment_data.csv")
MvCountDat <- read_csv("intermediate-data/mv_final_count_data.csv")
EvEstDat <- read_csv("intermediate-data/ev_establishment_data.csv")
EvInfDat <- read_csv("intermediate-data/ev_infection_data.csv")
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

# Mv establishment
MvCountDat <- MvCountDat %>%
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
         PerCapWeight.g = Weight_g/GermEv,
         Planting = dplyr::recode(Competition, "0" = "alone", "1" = "in competition"))

# Mv biomass
MvBioDat <- MvBioDat %>%
  mutate(PerCapWeight.g = Weight_g/GermMv,
         Planting = dplyr::recode(Competition, "0" = "alone", "1" = "in competition"))


#### percent changes ####

# Mv establishment 
MvEstDat %>%
  filter(Litter %in% c("None", "High")) %>%
  dplyr::select(Competition, Litter, Litter.g) %>%
  unique() %>%
  mutate(est = predict(mv_est_mod1, newdata = ., type = "response"))

# Ev establishment
EvEstDat %>%
  filter(Competition == 0) %>%
  dplyr::select(Litter, Litter.g) %>%
  unique() %>%
  mutate(est = predict(ev_est_mod4, newdata = ., type = "response"))

# Ev infection
EvInfDat %>%
  dplyr::select(Litter, Litter.g, Competition) %>%
  unique() %>%
  mutate(est = predict(ev_inf_mod3, newdata = ., type = "response"))

# Mv pot biomass
MvBioDat %>%
  filter(Litter %in% c("None", "High") & Competition == 0) %>%
  dplyr::select(Litter, Litter.g, Competition) %>%
  unique() %>%
  mutate(est = exp(predict(mv_tot_bio_mod3, newdata = .))) %>%
  dplyr::select(-Litter.g) %>%
  pivot_wider(names_from = Litter, values_from = est) %>%
  mutate(Diff = (High - None)/None) %>%
  data.frame()

MvBioDat %>%
  filter(Litter == "None") %>%
  dplyr::select(Litter.g, Competition, SpPresent) %>%
  unique() %>%
  mutate(est = exp(predict(mv_tot_bio_mod3, newdata = .)),
         SpPresent = fct_recode(SpPresent, both = "Ev+Mv")) %>%
  dplyr::select(-Competition) %>%
  pivot_wider(names_from = SpPresent, values_from = est) %>%
  mutate(Diff = (both - Mv)/Mv)

# Mv plant biomass
MvBioDat %>%
  dplyr::select(Litter, Litter.g, Competition) %>%
  unique() %>%
  mutate(est = exp(predict(mv_bio_mod6, newdata = .)))

mv_bio_mod6b <- update(mv_bio_mod6, ~. + Competition)
summary(mv_bio_mod6b)

MvBioDat %>%
  dplyr::select(Competition, SpPresent) %>%
  unique() %>%
  mutate(est = exp(predict(mv_bio_mod6b, newdata = .)),
         SpPresent = fct_recode(SpPresent, both = "Ev+Mv")) %>%
  dplyr::select(-Competition) %>%
  pivot_wider(names_from = SpPresent, values_from = est) %>%
  mutate(Diff = (both - Mv)/Mv)

# Ev pot biomass
EvBioDat %>%
  filter(Litter %in% c("None", "Low") & Competition == 0) %>%
  dplyr::select(Litter, Competition) %>%
  unique() %>%
  mutate(est = exp(predict(ev_tot_bio_mod3, newdata = .))) %>%
  pivot_wider(names_from = Litter, values_from = est) %>%
  mutate(Diff = (Low - None)/None)

EvBioDat %>%
  filter(Litter == "None") %>%
  dplyr::select(Litter, Competition, SpPresent) %>%
  unique() %>%
  mutate(est = exp(predict(ev_tot_bio_mod3, newdata = .)),
         SpPresent = fct_recode(SpPresent, both = "Ev+Mv")) %>%
  dplyr::select(-Competition) %>%
  pivot_wider(names_from = SpPresent, values_from = est) %>%
  mutate(Diff = (both - Ev)/Ev)

# Ev plant biomass
EvBioDat %>%
  dplyr::select(Competition, SpPresent) %>%
  unique() %>%
  mutate(est = exp(predict(ev_bio_mod4, newdata = .)),
         SpPresent = fct_recode(SpPresent, both = "Ev+Mv")) %>%
  dplyr::select(-Competition) %>%
  pivot_wider(names_from = SpPresent, values_from = est) %>%
  mutate(Diff = (both - Ev)/Ev)


# relative abundance
MvBioDat %>%
  filter(Litter == "None" & SpPresent == "Ev+Mv") %>%
  summarise(est = mean(Weight_g/TotalWeight.g))

# relative abundance litter effect
MvBioDat %>%
  filter(SpPresent == "Ev+Mv") %>%
  group_by(Litter) %>%
  summarise(LogTotalWeight.g = mean(LogTotalWeight.g)) %>%
  ungroup() %>%
  mutate(est = exp(predict(rel_mod2, newdata = .) - LogTotalWeight.g)) %>%
  dplyr::select(-LogTotalWeight.g) %>%
  pivot_wider(names_from = Litter, values_from = est) %>%
  mutate(Diff_Low = (Low - None)/None * 100)


#### prediction dataset ####

# continuous
pred_dat_con <- tibble(Litter.g = rep(seq(0, 3.64, length.out = 100), 2),
                   Competition = rep(c(0, 1), each = 100)) %>%
  mutate(Planting = dplyr::recode(Competition, "0" = "alone", "1" = "in competition"))


#### Mv establishment figure ####

# predicted values
mv_pred_est <- pred_dat_con %>%
  mutate(pred = predict(mv_est_mod1, newdata = ., type = "response"),
         pred.se = predict(mv_est_mod1, newdata = ., type = "response", se.fit = T)$se.fit * 1.96)

# visualize
mv_est_fig <- ggplot(MvEstDat, aes(x = Litter.g, y = PropEstMvDenCor, fill = Planting, shape = Planting, linetype = Planting)) +
  geom_ribbon(data = mv_pred_est, aes(y = pred, ymin = pred - pred.se, ymax = pred + pred.se), alpha = 0.5, color = NA) +
  geom_line(data = mv_pred_est, aes(y = pred)) +
  stat_summary(fun.data = "mean_cl_boot", geom = "errorbar", width = 0, position = position_dodge(0.3), linetype = "solid", size = 0.4) +
  stat_summary(fun = "mean", geom = "point", position = position_dodge(0.3), size = 3) +
  annotate(geom = "text",
           label = expression(paste("litter x planting: ", italic(P), " = 0.04", sep = "")),
           x = 0, y = 0.91, size = 2.5, hjust = 0) +
  scale_fill_manual(values = col_pal, name = "Planting treatment") +
  scale_linetype_manual(values = line_pal, name = "Planting treatment") +
  scale_shape_manual(values = shape_pal, name = "Planting treatment") +
  xlab("Litter (g)") +
  ylab(expression(paste(italic(M.), " ", italic( vimineum), " establishment", sep = ""))) +
  coord_cartesian(ylim = c(0.6, 0.91)) +
  fig_theme +
  theme(legend.position = c(0.21, 0.17),
        legend.margin = margin(0, 0, 0, 0))

#### Mv count figure ####
mv_cnt_fig <- ggplot(MvCountDat, aes(x = Litter.g, y = GermMv, fill = Planting, shape = Planting, linetype = Planting)) +
  stat_summary(fun.data = "mean_cl_boot", geom = "errorbar", width = 0, position = position_dodge(0.3), linetype = "solid", size = 0.4) +
  stat_summary(fun = "mean", geom = "point", position = position_dodge(0.3), size = 3) +
  scale_fill_manual(values = col_pal, name = "Planting treatment") +
  scale_linetype_manual(values = line_pal, name = "Planting treatment") +
  scale_shape_manual(values = shape_pal, name = "Planting treatment") +
  xlab("Litter (g)") +
  ylab(expression(paste(italic(M.), " ", italic( vimineum), " plants", sep = ""))) +
  fig_theme +
  theme(legend.position = "none")


#### Ev establishment figure ####

EvEstSum <- EvEstDat %>%
  group_by(Litter.g, Planting) %>%
  summarise(y = mean(PropEstEv),
            ymin = mean_cl_boot(PropEstEv)$ymin,
            ymax = mean_cl_boot(PropEstEv)$ymax) %>%
  ungroup() %>%
  mutate(lets = c("a", "a", rep("b", 6)),
         lety = ymax + 0.01)

ev_est_fig <- ggplot(EvEstSum, aes(x = Litter.g, y = y, fill = Planting, shape = Planting)) +
  geom_errorbar(aes(ymin = ymin, ymax = ymax), width = 0, position = position_dodge(0.3), linetype = "solid", size = 0.4) +
  geom_point(position = position_dodge(0.3), size = 3) +
  geom_text(aes(y = lety, label = lets), position = position_dodge(0.3), size = 2.5) +
  annotate(geom = "text",
           label = expression(paste("litter (categorical): ", italic(P), " < 0.01", sep = "")),
           x = 0, y = 0.91, size = 2.5, hjust = 0) +
  scale_fill_manual(values = col_pal) +
  scale_shape_manual(values = shape_pal) +
  xlab("Litter (g)") +
  ylab(expression(paste(italic(E.), " ", italic(virginicus), " establishment", sep = ""))) +
  coord_cartesian(ylim = c(0.6, 0.91)) +
  fig_theme +
  theme(legend.position = "none")


#### Ev infection figure ####

EvInfSum <- EvInfDat %>%
  group_by(Litter.g, Planting) %>%
  summarise(y = mean(PropInfEv),
            ymin = mean_cl_boot(PropInfEv)$ymin,
            ymax = mean_cl_boot(PropInfEv)$ymax) %>%
  ungroup() %>%
  mutate(lets = c("a", "A", "b", "B", "c", "C", "d", "D"),
         lety = ymax + 0.02)

ev_inf_fig <- ggplot(EvInfSum, aes(x = Litter.g, y = y, fill = Planting, shape = Planting)) +
  geom_errorbar(aes(ymin = ymin, ymax = ymax), width = 0, position = position_dodge(0.3), linetype = "solid", size = 0.4) +
  geom_point(position = position_dodge(0.3), size = 3) +
  geom_text(aes(y = lety, label = lets), position = position_dodge(0.3), size = 2.5) +
  annotate(geom = "text",
           label = expression(paste("litter (categorical): ", italic(P), " < 0.01", sep = "")),
           x = 0, y = 0.82, size = 2.5, hjust = 0) +
  annotate(geom = "text",
           label = expression(paste("planting: ", italic(P), " < 0.01", sep = "")),
           x = 0, y = 0.78, size = 2.5, hjust = 0) +
  scale_fill_manual(values = col_pal, name = "Species\nplanted") +
  scale_shape_manual(values = shape_pal, name = "Species\nplanted") +
  xlab("Litter (g)") +
  ylab(expression(paste(italic(E.), " ", italic(virginicus), " disease incidence", sep = ""))) +
  coord_cartesian(ylim = c(0, 0.82)) +
  fig_theme +
  theme(legend.position = "none")


#### combined establishment and infection figures ####

tiff("output/Fig2.tiff", width = 6, height = 6, units = "in", res = 300)
plot_grid(mv_est_fig, mv_cnt_fig, ev_est_fig, ev_inf_fig,
          nrow = 2,
          labels = c("a", "b", "c", "d"),
          label_size = 10)
dev.off()


#### Mv pot biomass figure ####

# predicted values
mv_pred_bio <- pred_dat_con %>%
  mutate(pred = exp(predict(mv_tot_bio_mod3, newdata = .)),
         pred.se = exp(predict(mv_tot_bio_mod3, newdata = ., se.fit = T)$se.fit) * 1.96)

# visualize
mv_totbio_fig <- ggplot(MvBioDat, aes(x = Litter.g, y = Weight_g, fill = Planting, shape = Planting, linetype = Planting)) +
  geom_ribbon(data = mv_pred_bio, aes(y = pred, ymin = pred - pred.se, ymax = pred + pred.se), alpha = 0.5, color = NA) +
  geom_line(data = mv_pred_bio, aes(y = pred)) +
  stat_summary(fun.data = "mean_cl_boot", geom = "errorbar", width = 0, position = position_dodge(0.3), linetype = "solid", size = 0.4) +
  stat_summary(fun = "mean", geom = "point", position = position_dodge(0.3), size = 3) +
  annotate(geom = "text",
           label = expression(paste("litter: ", italic(P), " = 0.02", sep = "")),
           x = 0, y = 44.8, size = 2.5, hjust = 0) +
  annotate(geom = "text",
           label = expression(paste("planting: ", italic(P), " < 0.01", sep = "")),
           x = 0, y = 44, size = 2.5, hjust = 0) +
  scale_fill_manual(values = col_pal, name = "Planting treatment") +
  scale_linetype_manual(values = line_pal, name = "Planting treatment") +
  scale_shape_manual(values = shape_pal, name = "Planting treatment") +
  xlab("Litter (g)") +
  ylab(expression(paste(italic(M.), " ", italic( vimineum), " biomass per pot (g)", sep = ""))) +
  fig_theme +
  theme(legend.position = c(0.8, 0.83),
        legend.margin = margin(0, 0, 0, 0))


#### Mv plant biomass figure ####
# visualize
mv_pcbio_fig <- ggplot(MvBioDat, aes(x = Litter.g, y = PerCapWeight.g, fill = Planting, shape = Planting, linetype = Planting)) +
  stat_summary(fun.data = "mean_cl_boot", geom = "errorbar", width = 0, position = position_dodge(0.3), linetype = "solid", size = 0.4) +
  stat_summary(fun = "mean", geom = "point", position = position_dodge(0.3), size = 3) +
  scale_fill_manual(values = col_pal, name = "Planting treatment") +
  scale_linetype_manual(values = line_pal, name = "Planting treatment") +
  scale_shape_manual(values = shape_pal, name = "Planting treatment") +
  xlab("Litter (g)") +
  ylab(expression(paste(italic(M.), " ", italic( vimineum), " biomass per plant (g)", sep = ""))) +
  fig_theme +
  theme(legend.position = "none")


#### Ev pot biomass ####

EvBioSum <- EvBioDat %>%
  group_by(Litter.g, Planting) %>%
  summarise(y = mean(Weight_g),
            ymin = mean_cl_boot(Weight_g)$ymin,
            ymax = mean_cl_boot(Weight_g)$ymax) %>%
  ungroup() %>%
  mutate(lets = c("a", "A", "b", "B", "ab", "AB", "ab", "AB"),
         lety = ymax + 0.2)

ev_totbio_fig <- ggplot(EvBioSum, aes(x = Litter.g, y = y, fill = Planting, shape = Planting, linetype = Planting)) +
  geom_errorbar(aes(ymin = ymin, ymax = ymax), width = 0, position = position_dodge(0.3), linetype = "solid", size = 0.4) +
  geom_point(position = position_dodge(0.3), size = 3) +
  geom_text(aes(y = lety, label = lets), position = position_dodge(0.3), size = 2.5) +
  annotate(geom = "text",
           label = expression(paste("litter (categorical): ", italic(P), " = 0.01", sep = "")),
           x = 0, y = 8.9, size = 2.5, hjust = 0) +
  annotate(geom = "text",
           label = expression(paste("planting: ", italic(P), " < 0.01", sep = "")),
           x = 0, y = 8.5, size = 2.5, hjust = 0) +
  scale_fill_manual(values = col_pal) +
  scale_linetype_manual(values = line_pal) +
  scale_shape_manual(values = shape_pal) +
  xlab("Litter (g)") +
  ylab(expression(paste(italic(E.), " ", italic( virginicus), " biomass per pot (g)", sep = ""))) +
  fig_theme +
  theme(legend.position = "none")


#### Ev plant biomass ####

EvPCBioSum <- EvBioDat %>%
  filter(!is.na(PerCapWeight.g)) %>%
  group_by(Litter.g, Planting) %>%
  summarise(y = mean(PerCapWeight.g),
            ymin = mean_cl_boot(PerCapWeight.g)$ymin,
            ymax = mean_cl_boot(PerCapWeight.g)$ymax) %>%
  ungroup() %>%
  mutate(lets = rep(c("a", "A"), 4),
         lety = ymax + 0.002)

ev_pcbio_fig <- ggplot(EvPCBioSum, aes(x = Litter.g, y = y, fill = Planting, shape = Planting, linetype = Planting)) +
  geom_errorbar(aes(ymin = ymin, ymax = ymax), width = 0, position = position_dodge(0.3), linetype = "solid", size = 0.4) +
  geom_point(position = position_dodge(0.3), size = 3) +
  geom_text(aes(y = lety, label = lets), position = position_dodge(0.3), size = 2.5) +
  annotate(geom = "text",
           label = expression(paste("planting: ", italic(P), " < 0.01", sep = "")),
           x = 0, y = 0.09, size = 2.5, hjust = 0) +
  scale_fill_manual(values = col_pal) +
  scale_linetype_manual(values = line_pal) +
  scale_shape_manual(values = shape_pal) +
  xlab("Litter (g)") +
  ylab(expression(paste(italic(E.), " ", italic( virginicus), " biomass per tiller (g)", sep = ""))) +
  fig_theme +
  theme(legend.position = "none")



#### combined biomass figure ####

tiff("output/Fig3.tiff", width = 6, height = 6, units = "in", res = 300)
plot_grid(mv_totbio_fig, mv_pcbio_fig, ev_totbio_fig, ev_pcbio_fig,
          nrow = 2,
          labels = c("a", "b", "c", "d"),
          label_size = 10)
dev.off()



#### relative abundance ####

rel_fig <- ggplot(filter(MvBioDat, SpPresent == "Ev+Mv"), 
                  aes(x = Litter.g, y = Weight_g/TotalWeight.g)) +
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

