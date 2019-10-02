#### goals #### 

# create tables and figures


#### set up ####

# clear all existing data
rm(list=ls())

# load libraries
library(tidyverse)
library(sjPlot)
library(car)
library(Hmisc)
library(cowplot)

# import models
load("./output/mv_establishment_model.rda")
load("./output/mv_uncorrected_establishment_model.rda")
load("./output/ev_establishment_model.rda")
load("./output/ev_infection_model_grams.rda")
load("./output/ev_infection_model_present.rda")
load("./output/mv_biomass_model.rda")
load("./output/mv_pcbiomass_model.rda")
load("./output/ev_biomass_model.rda")
load("./output/ev_pcbiomass_model.rda")

# import data
dat <- read_csv("./output/establishment_infection_data.csv")
medat <- read_csv("./output/mv_establishment_data.csv")
eedat <- read_csv("./output/ev_establishment_data.csv")
mbdat <- read_csv("./output/mv_biomass_data.csv")
ebdat <- read_csv("./output/ev_biomass_data.csv")
eidat <- read_csv("./output/ev_infection_data.csv")

# plotting parameters
axisText = 9
axisTitle = 11
legendText = 8
legendTitle = 8


#### edit data ####

# count data for temporal plot
countdat <- dat %>%
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
                  Shade = "no",
                  Response = "Mv planted") %>%
  mutate(Estab = predict(meMod2, newdata = ., type = "response"),
         Estab.se = predict(meMod2, newdata = ., type = "response", se.fit = T)$se.fit)

# Ev establishment groups
estgrp <- tibble(Litter.g = sort(unique(dat$Litter.g)),
                 Estab = 44.5,
                 Letters = c("a", "b", "b", "b"),
                 Response = "Ev planted",
                 SpPresent = "Ev",
                 Shade = "no")
  
# biomass data
biodat <- select(ebdat, Litter.g, SpPresent, Shade, Weight.g) %>%
  mutate(Weight = Weight.g,
         Response = "Ev total") %>%
  select(-Weight.g) %>%
  full_join(select(ebdat, Litter.g, SpPresent, Shade, Weight.g, GermEv) %>%
              mutate(Weight = Weight.g/GermEv,
                     Response = "Ev per capita") %>%
              select(-c(Weight.g, GermEv))) %>%
  full_join(select(mbdat, Litter.g, SpPresent, Shade, Weight.g) %>%
              mutate(Weight = Weight.g,
                     Response = "Mv total") %>%
              select(-Weight.g)) %>%
  full_join(select(mbdat, Litter.g, SpPresent, Shade, Weight.g, GermMv) %>%
              mutate(Weight = Weight.g/GermMv,
                     Response = "Mv per capita") %>%
              select(-c(Weight.g, GermMv))) %>%
  mutate(Response = factor(Response, levels = c("Ev total", "Ev per capita", "Mv total", "Mv per capita")))

# predicted biomass
biopred <- tibble(Litter.g = rep(seq(0, max(dat$Litter.g), length.out = 100), 2),
                  SpPresent = rep(c("Mv", "Ev+Mv"), each = 100),
                  Shade = "no",
                  Response = "Mv total") %>%
  mutate(Weight = predict(mbMod2, newdata = ., type = "response"),
         Weight.se = predict(mbMod2, newdata = ., type = "response", se.fit = T)$se.fit) %>%
  mutate(Response = factor(Response, levels = c("Ev total", "Ev per capita", "Mv total", "Mv per capita")))

# biomass groups
biogrp <- tibble(Litter.g = c(rep(rep(sort(unique(dat$Litter.g)), each = 2), 2), rep(1.9, 4)),
                 Weight = c(rep(8.5, 8), rep(0.21, 8), rep(43, 2), rep(1.25, 2)),
                 Response = c(rep("Ev total", 8), rep("Ev per capita", 8), rep("Mv total", 2), rep("Mv per capita", 2)),
                 SpPresent = c(rep(c("Ev", "Ev+Mv"), 8), rep("Mv", 4)),
                 Letters = c("a", "b", "c", "d", "c", "d", "c", "d", rep(c("e", "f"), 4), "g", "h", "i", "j"),
                 Shade = c(rep("no", 17), "yes", "no", "yes")) %>%
  mutate(Litter.g = Litter.g + rep(c(-0.1, 0.1), 10),
         Response = factor(Response, levels = c("Ev total", "Ev per capita", "Mv total", "Mv per capita")))

# predicted infection
infpred <- tibble(SpPresent = rep(c("Ev", "Ev+Mv"), each = 100),
                  Litter.g = rep(seq(0, max(dat$Litter.g), length.out = 100), 2),
                  Shade = "no") %>%
  mutate(PropInfEv = predict(eiMod2, newdata = ., type = "response"),
         PropInfEv.se = predict(eiMod2, newdata = ., type = "response", se.fit = T)$se.fit)

# infection groups
infgrp <- tibble(Litter.present = rep(c("No litter", "Litter"), each = 2),
                 PropInfEv = rep(0.6, 4),
                 Letters = c("a", "b", "b", "c"),
                 SpPresent = rep(c("Ev", "Ev+Mv"), 2)) %>%
  mutate(Litter.present = factor(Litter.present, levels = c("No litter", "Litter")))
  
  
#### create tables ####

Anova(eeMod4)
tab_model(eeMod4, pred.labels = c("(Intercept)","Mv present", "Litter present"))

Anova(meMod2) # p-values match tables
tab_model(meMod2, meMod6, dv.labels = c("planted", "planted + litter"), pred.labels = c("(Intercept)","Ev present", "Litter (g)", "Shade"))

tab_model(ebMod4, ebMod6, dv.labels = c("total", "per capita"), pred.labels = c("(Intercept)","Mv present", "Litter present"))

tab_model(mbMod2, mbMod6, dv.labels = c("total", "per capita"), pred.labels = c("(Intercept)","Ev present", "Litter (g)", "Shade"))

tab_model(eiMod2, eiMod3, dv.labels = c("quantitative", "qualitative"), pred.labels = c("(Intercept)","Mv present", "Litter (g)", "Litter present", "Mv present:Litter present"))


#### temporal count figure ####

countplot <- countdat %>%
  ggplot(aes(x = Date3, y = Counts, color = Shade, fill = SpPresent, shape = CountType)) +
  geom_vline(aes(xintercept = EstDate), linetype = "dashed", size = 0.2) +
  stat_summary(fun.data = "mean_cl_boot", geom = "errorbar", size = 0.3) +
  stat_summary(fun.y = "mean", geom = "point") +
  stat_summary(fun.y = "mean", geom = "line", size = 0.2) +
  facet_grid(Focal~Litter, scales = "free_y", switch = "y") +
  scale_shape_manual(values = c(21, 22), name = "Count type") +
  scale_colour_manual(values = c("gray", "black"), name="Shade") +
  scale_fill_manual(values = c("#44A5B6","black", "#92D050"), name = "Species present") +  
  guides(fill = guide_legend(override.aes = list(shape = 21))) +
  xlab("Date") +
  ylab("Ev counts") +
  theme_bw() +
  theme(axis.text = element_text(size = axisText, color="black"),
        axis.title.x = element_text(size = axisTitle),
        axis.title.y = element_blank(),
        panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.text = element_text(size=legendText),
        legend.title = element_text(size=legendTitle),
        strip.background = element_blank(),
        strip.text = element_text(size = axisTitle),
        strip.placement = "outside")

pdf("./output/temporal_count_figure.pdf", width = 6, height = 4)
countplot
dev.off()

jpeg("./output/temporal_count_figure.jpeg", width = 6, height = 4, units = "in", res = 600)
countplot
dev.off()


#### establishment figure ####

estplot <- estdat %>%
  ggplot(aes(x = Litter.g, y = Estab, color = Shade, fill = SpPresent )) +
  geom_line(data = estpred) +
  geom_ribbon(data = estpred, aes(ymin = Estab - Estab.se, ymax = Estab + Estab.se), fill = "gray", alpha = 0.5, color = NA) +
  stat_summary(fun.data = "mean_cl_boot", geom = "errorbar", size = 0.5, width = 0.3, position = position_dodge(0.4)) +
  stat_summary(fun.y = "mean", geom = "point", shape = 21, size = 2, position = position_dodge(0.4)) +
  geom_text(data = estgrp, aes(label = Letters), color = "black", size = 3) +
  facet_wrap(~Response) +
  scale_colour_manual(values = c("gray", "black"), name="Shade") +
  scale_fill_manual(values = c("#44A5B6","black", "#92D050"), name = "Species present") +  
  xlab("Litter (g)") +
  ylab("Established individuals") +
  theme_bw() +
  theme(axis.text = element_text(size = axisText, color="black"),
        axis.title = element_text(size = axisTitle),
        panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.text = element_text(size=legendText),
        legend.title = element_text(size=legendTitle),
        strip.background = element_blank(),
        strip.text = element_text(size = axisTitle))

pdf("./output/establishment_figure.pdf", width = 6, height = 3)
estplot
dev.off()

jpeg("./output/establishment_figure.jpeg", width = 6, height = 3, units = "in", res = 600)
estplot
dev.off()


#### biomass figure ####

bioplot <- biodat %>%
  ggplot(aes(x = Litter.g, y = Weight, color = Shade, fill = SpPresent )) +
  geom_line(data = biopred) +
  geom_ribbon(data = biopred, aes(ymin = Weight - Weight.se, ymax = Weight + Weight.se), alpha = 0.5, color = NA) +
  stat_summary(fun.data = "mean_cl_boot", geom = "errorbar", size = 0.5, width = 0.3, position = position_dodge(0.4)) +
  stat_summary(fun.y = "mean", geom = "point", shape = 21, size = 2, position = position_dodge(0.4)) +
  geom_text(data = biogrp, aes(label = Letters), color = "black", size = 3) +
  facet_wrap(~ Response, scales = "free") +
  scale_colour_manual(values = c("gray", "black"), name="Shade") +
  scale_fill_manual(values = c("#44A5B6","black", "#92D050"), name = "Species present") +  
  xlab("Litter (g)") +
  ylab("Aboveground biomass (g)") +
  theme_bw() +
  theme(axis.text = element_text(size = axisText, color="black"),
        axis.title = element_text(size = axisTitle),
        panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.text = element_text(size=legendText),
        legend.title = element_text(size=legendTitle),
        strip.background = element_blank(),
        strip.text = element_text(size = axisTitle))

pdf("./output/biomass_figure.pdf", width = 6, height = 5)
bioplot
dev.off()

jpeg("./output/biomass_figure.jpeg", width = 6, height = 5, units = "in", res = 600)
bioplot
dev.off()


#### infection figure ####

infplot1 <- eidat %>%
  ggplot(aes(x = Litter.g, y = PropInfEv, fill = SpPresent)) +
  geom_line(data = infpred, color = "gray") +
  geom_ribbon(data = infpred, aes(ymin = PropInfEv - PropInfEv.se, ymax = PropInfEv + PropInfEv.se), alpha = 0.5, color = NA) +
  stat_summary(fun.data = "mean_cl_boot", geom = "errorbar", size = 0.5, width = 0.3, position = position_dodge(0.4), color = "gray") +
  stat_summary(fun.y = "mean", geom = "point", shape = 21, size = 2, position = position_dodge(0.4), color = "gray") +
  scale_fill_manual(values = c("#44A5B6","black"), name = "Species present") +  
  xlab("Litter (g)") +
  ylab("Proportion of Ev stems infected") +
  theme_bw() +
  theme(axis.text = element_text(size = axisText, color="black"),
        axis.title = element_text(size = axisTitle),
        panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.text = element_text(size=legendText),
        legend.title = element_text(size=legendTitle),
        strip.background = element_blank(),
        strip.text = element_text(size = axisTitle),
        legend.position = "none")

infplot2 <- eidat %>%
  mutate(Litter.present = factor(Litter.present, levels = c("No litter", "Litter"))) %>%
  ggplot(aes(x = Litter.present, y = PropInfEv, fill = SpPresent)) +
  stat_summary(fun.data = "mean_cl_boot", geom = "errorbar", size = 0.5, width = 0.3, position = position_dodge(0.4), color = "gray") +
  stat_summary(fun.y = "mean", geom = "point", shape = 21, size = 2, position = position_dodge(0.4), color = "gray") +
  geom_text(data = infgrp, aes(label = Letters), color = "black", size = 3, position = position_dodge(0.4)) +
  scale_fill_manual(values = c("#44A5B6","black"), name = "Species present") +  
  xlab("") +
  theme_bw() +
  theme(axis.text = element_text(size = axisText, color="black"),
        axis.title.x = element_text(size = axisTitle),
        axis.title.y = element_blank(),
        panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.text = element_text(size=legendText),
        legend.title = element_text(size=legendTitle),
        strip.background = element_blank(),
        strip.text = element_text(size = axisTitle))

pdf("./output/infection_figure.pdf", width = 5, height = 3)
plot_grid(infplot1, infplot2, nrow = 1, rel_widths = c(0.7, 1))
dev.off()

jpeg("./output/infection_figure.jpeg", width = 5, height = 3, units = "in", res = 600)
plot_grid(infplot1, infplot2, nrow = 1, rel_widths = c(0.7, 1))
dev.off()
