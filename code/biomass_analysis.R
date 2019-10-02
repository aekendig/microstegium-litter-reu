#### goals #### 

# analyze effects of litter and live plant competition on:
  # Mv and Ev biomass


#### set up ####

# clear all existing data
rm(list=ls())

# load libraries
library(tidyverse)
library(car)

# import data (all data combined up to post harvest)
dat1 <- read_csv("./data/MvEv_Biomass_110918.csv")
ecounts <- read_csv("./output/ev_establishment_data.csv")
mcounts <- read_csv("./output/mv_count_data.csv")


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
         WeighedSp = dplyr::recode(WeighedSp, MV = "Mv"), 
         TrtID = paste(SpPresent, Litter, Shade, PotID, sep="."),
         Litter.yes=as.factor(ifelse(Litter == "None", 0, 1)),
         Litter.present = dplyr::recode(Litter.yes, "0" = "No litter", "1" = "Litter"),
         LogWeight.g = log(Weight.g)) %>%
  filter(Notes != "intentionally not measured" | is.na(Notes))

# check for NA's
filter(dat2, is.na(Weight.g))

# split by species and merge with count data
mdat <- dat2 %>% 
  filter(WeighedSp == "Mv" & SpPresent != "Ev") %>%
  full_join(select(mcounts, TrtID, GermMv))

edat <- filter(dat2, WeighedSp == "Ev" & SpPresent != "Mv") %>%
  full_join(select(ecounts, TrtID, GermEv))


#### biomass statistics ####

# Mv models
mbMod1 <- lm(Weight.g ~ SpPresent * Litter.g + Shade, data = mdat)
summary(mbMod1)
Anova(mbMod1, type = 3)
# no sig interaction

mbMod2 <- lm(Weight.g ~ SpPresent + Litter.g + Shade, data = mdat)
summary(mbMod2)
Anova(mbMod2)
# all are sig
plot(mbMod2)

mbMod3 <- lm(Weight.g ~ SpPresent * Litter.yes + Shade, data = mdat)
summary(mbMod3)
Anova(mbMod3, type = 3)
# no sig interaction

mbMod4 <- lm(Weight.g ~ SpPresent + Litter.yes + Shade, data = mdat)
summary(mbMod4)
Anova(mbMod4)
# sp present and shade are sig

mbMod5 <- lm(LogWeight.g ~ offset(log(GermMv)) + SpPresent * Litter.g + Shade, data = mdat)
summary(mbMod5)
Anova(mbMod5, type = 3)
# no sig interaction

mbMod6 <- lm(LogWeight.g ~ offset(log(GermMv)) + SpPresent + Litter.g + Shade, data = mdat)
summary(mbMod6)
Anova(mbMod6)
# shade is sig

# Ev models
ebMod1 <- lm(Weight.g ~ SpPresent * Litter.g, data = edat)
summary(ebMod1)
Anova(ebMod1, type = 3)
# no sig interaction

ebMod2 <- lm(Weight.g ~ SpPresent + Litter.g, data = edat)
summary(ebMod2)
Anova(ebMod2)
# sp present sig

ebMod3 <- lm(Weight.g ~ SpPresent * Litter.yes , data = edat)
summary(ebMod3)
Anova(ebMod3, type = 3)
# no sig interaction

ebMod4 <- lm(Weight.g ~ SpPresent + Litter.yes , data = edat)
summary(ebMod4)
Anova(ebMod4)
# both are sig
plot(ebMod4)

ebMod5 <- lm(LogWeight.g ~ offset(log(GermEv)) + SpPresent * Litter.yes, data = edat)
summary(ebMod5)
Anova(ebMod5, type = 3)
# no sig interaction

ebMod6 <- lm(LogWeight.g ~ offset(log(GermEv)) + SpPresent + Litter.yes, data = edat)
summary(ebMod6)
Anova(ebMod6)
# litter no longer sig
# plot(ebMod6)


#### outputs ####

# save datasets
write_csv(mdat, "./output/mv_biomass_data.csv")
write_csv(edat, "./output/ev_biomass_data.csv")

# save models
save(mbMod2, file = "./output/mv_biomass_model.rda")
save(mbMod6, file = "./output/mv_pcbiomass_model.rda")
save(ebMod4, file = "./output/ev_biomass_model.rda")
save(ebMod6, file = "./output/ev_pcbiomass_model.rda")
