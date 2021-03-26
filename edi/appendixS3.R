#### goals #### 

# Tables and figures of Appendix S3


#### set up ####

# clear all existing data
rm(list=ls())

# load libraries
library(tidyverse)
library(lme4)
library(cowplot)


# import data
heightDat <- read_csv("data/AppS3_heights_073018.csv")
leafDat <- read_csv("data/AppS3_leaf_moisture_080118.csv")
infDat <- read_csv("data/AppS3_Infection_091318.csv")

# figure theme
fig_theme <- theme_bw() +
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text = element_text(size = 9, color = "black"),
        axis.title = element_text(size = 11),
        legend.text = element_text(size = 9),
        legend.title = element_text(size = 9),
        strip.background = element_blank(),
        strip.text = element_text(size = 9))

col_pal = c("white","black")
line_pal = c("dashed", "solid")
shape_pal = c(17, 16)


#### edit data ####

# make height data long
heightDatL <- heightDat %>%
  pivot_longer(cols = starts_with("H"), names_to = "Replicate", values_to = "Height.cm") %>%
  mutate(Replicate = substr(Replicate, 2, 2),
         Pot = paste0(Species, Litter, Dry, PotID),
         LogHeight.cm = log(Height.cm),
         Species = recode(Species, Cc = "Calamagrostis", Et = "Eragrostis", Ev = "Elymus", Gs = "Glyceria", Mv = "Microstegium", Pc = "Dichanthelium"))

# look at notes
unique(leafDat$Notes)
unique(infDat$MvNotes)
unique(infDat$NativeNotes)

# format leaf moisture data
leafDat2 <- leafDat %>%
  mutate(DateTime = strptime(paste(Date, Time), format = "%m/%d/%y %H:%M"),
         Hours = as.numeric(difftime(DateTime, min(DateTime), units = "hours")),
         LeafMoisture = recode(LeafMoisture, Y = "1", N = "0") %>%
           as.numeric(),
         Species = recode(Species, Cc = "Calamagrostis", Et = "Eragrostis", Ev = "Elymus", Gs = "Glyceria", Mv = "Microstegium", Pc = "Dichanthelium"),
         Moisture = recode(Treatment, "Dry" = "low", "high moisture" = "high") %>%
           factor(levels = c("low", "high")))

# litter effect
litDat <- infDat %>%
  filter(Dry == "N") %>%
  mutate(Disease = ifelse(Species == "Mv", MvInf, NativeInf),
         Treatment = recode(Litter, Y = "litter", N = "no litter") %>%
           factor(levels = c("no litter", "litter")),
         Species = recode(Species, Cc = "Calamagrostis", Et = "Eragrostis", Ev = "Elymus", Gs = "Glyceria", Mv = "Microstegium", Pc = "Dichanthelium"))

# moisture effect
moistDat <- infDat %>%
  filter(Species == "Mv" & Litter == "Y") %>%
  mutate(Disease = MvInf,
         Moisture = recode(Dry, Y = "low", N = "high") %>%
           factor(levels = c("low", "high")))


#### height ####

# model
height_mod <- lmer(Height.cm ~ Species + (1 | Pot), data = heightDatL)
plot(height_mod)

# log-transformed
log_height_mod <- lmer(LogHeight.cm ~ Species + (1 | Pot), data = heightDatL)
plot(log_height_mod)

# averages
log_height_dat <- tibble(Species = unique(heightDatL$Species)) %>%
  mutate(LogHeight.cm = predict(log_height_mod, newdata = ., re.form = NA))

# figure
ggplot(heightDatL, aes(Species, LogHeight.cm)) +
  stat_summary(geom = "point", fun = "mean", size = 4) +
  geom_point(data = log_height_dat, color = "red") +
  fig_theme
# model estimates exactly match mean of raw data

# figure
pdf("output/AppS3_Figure1.pdf", width = 4, height = 3)
ggplot(heightDatL, aes(Species, Height.cm)) +
  stat_summary(geom = "errorbar", fun.data = "mean_cl_boot", width = 0) +
  stat_summary(geom = "point", fun = "mean", size = 2) +
  ylab("Height (cm)") +
  fig_theme +
  theme(axis.text.x = element_text(color = "black", size = 9, face = "italic",
                                   angle = 15, hjust = 0.75),)
dev.off()

# summary data
heightDatL %>%
  group_by(Species) %>%
  summarise(meanHeight = mean(Height.cm))


#### leaf moisture ####

leafDatSum <- leafDat2 %>%
  group_by(Hours, Species, Moisture) %>%
  summarise(prop = mean(LeafMoisture, na.rm = T)) %>%
  data.frame()

# figure 
pdf("output/AppS3_Figure2.pdf", width = 4.5, height = 3)
ggplot(leafDatSum, aes(x = Hours, y = prop, color = Species)) +
  geom_point(aes(shape = Moisture)) +
  geom_line(aes(linetype = Moisture)) +
  scale_linetype_manual(values = line_pal) +
  scale_shape_manual(values = shape_pal) +
  ylab("Proportion of pots with wet leaves") +
  xlab("Hours since litter addition") +
  fig_theme
dev.off()
  
# NA's
leafDat2 %>%
  filter(Notes %in% c("Covered by litter", "Quite covered with litter"))
# all except for Elymus couldn't be counted

leafDat2 %>%
  filter(Notes %in% c("Covered by litter", "Quite covered with litter")) %>%
  group_by(Species) %>%
  count()
  

#### litter ####

# figure
litFig <- ggplot(litDat, aes(Species, Disease, fill = Treatment)) +
  stat_summary(fun = "mean", geom = "bar", position = "dodge", color = "black") +
  scale_fill_manual(values = col_pal) +
  ylab("Proportion of pots with lesions") +
  fig_theme +
  theme(axis.text.x = element_text(color = "black", size = 9, face = "italic",
                                   angle = 15, hjust = 0.75),
        legend.position = c(0.15, 0.8))


# species
species = unique(litDat$Species)

# stats function
litStatsFun <- function(sp){
  litDat %>%
    filter(Species == sp) %>%
    group_by(Treatment) %>%
    summarise(Infected = sum(Disease),
              Healthy = length(Disease) - Infected) %>%
    select(-Treatment) %>%
    fisher.test(x = .)
}

# run function
for(i in 1:length(species)){
  print(litStatsFun(sort(species)[i]))
}


#### moisture ####

# figure 
moistFig <- ggplot(moistDat, aes(Moisture, Disease)) +
  stat_summary(fun = "mean", geom = "bar", color = "black", fill = "black") +
  ylab("Proportion of pots with lesions") +
  ylim(c(0, 1)) +
  fig_theme

# stats
moistDat %>%
  group_by(Moisture) %>%
  summarise(Infected = sum(Disease),
            Healthy = length(Disease) - Infected) %>%
  select(-Moisture) %>%
  fisher.test(x = .)


#### combine figures ####
pdf("output/AppS3_Figure3.pdf", width = 6, height = 3)
plot_grid(moistFig, litFig,
          nrow = 1,
          labels = c("a", "b"),
          label_size = 11,
          rel_widths = c(0.5, 1))
dev.off()