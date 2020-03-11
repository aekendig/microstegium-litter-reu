#### goals #### 

# create tables and figures


#### set up ####

# clear all existing data
rm(list=ls())

# load libraries
library(tidyverse)
library(sjPlot)
library(car)
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
legendText = 9
legendTitle = 9

# function to convert logit to probability
logit2prob <- function(logit){
  odds <- exp(logit)
  prob <- odds / (1 + odds)
  return(prob)
}


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
