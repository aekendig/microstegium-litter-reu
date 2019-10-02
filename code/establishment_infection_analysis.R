#### goals #### 

# analyze effects of litter and live plant competition on:
  # Mv and Ev establishment
  # Ev infection


#### set up ####

# clear all existing data
rm(list=ls())

# load libraries
library(tidyverse)
library(Hmisc)
library(car)

# import data (all data combined up to post harvest)
dat1 <- read_csv("./data/MvEv_GerminationInfection_PostHarvest.csv")


#### edit data ####

# look at notes
unique(dat1$Notes)

# dates
dat1 %>%
  group_by(Date) %>%
  summarise(nSamps = n()) %>%
  data.frame()

# give records a pseudo date when they were counted very close to one another
# date formats: proper format, days post planting, dpp as numeric
# make a shade treatment
# reorder litter
# make litter quantitative
# re-order species present
# make a treatment ID
# make a binary litter variable
dat2 <- dat1 %>%
  mutate(
    Date2 = case_when(
      Date %in% c(20180721, 20180722, 20180723) ~ 20180724,
      Date %in% c(20180731, 20180801) ~ 20180802,
      Date >= 20180822 ~ 20181005,
      TRUE ~ Date),
    Date3 = as.Date(as.character(Date2),"%Y%m%d"),
    Days = round(difftime(Date3,"2018-06-15",units="days")),
    Time = as.numeric(Days),
    Shade = ifelse(Treatment == "Shade", "yes", "no"),
    Litter = dplyr::recode(Treatment, Shade = "Med") %>% factor(levels = c("None", "Low", "Med", "High")),
    Litter.g = case_when(Litter == "None" ~ 0,
                         Litter == "Low" ~ 0.91,
                         Litter == "Med" ~ 1.82,
                         Litter == "High" ~ 3.64),
    SpPresent = factor(SpPresent, c("Ev", "Mv", "Ev+Mv")),
    TrtID = paste(SpPresent, Litter, Shade, PotID, sep="."),
    Litter.yes=as.factor(ifelse(Litter == "None", 0, 1)),
    Litter.present = dplyr::recode(Litter.yes, "0" = "No litter", "1" = "Litter"))
    
# check dates
dat2 %>%
  ggplot(aes(Date, Date2)) +
  geom_point() +
  geom_line()

dat2 %>%  
  select(Date, Date2) %>%
  unique() %>%
  data.frame()


#### Mv germination from litter ####

# Lili removed litter-derived Microstegium on 7/4 and 7/11 from Ev-only pots. For 7/11, she included the number that were removed on 7/4. All counts after that were the observed numbers. Update new counts to include the plants that had been removed

# check counts
dat2 %>%
  filter(SpPresent == "Ev") %>%
  ggplot(aes(x = Date3, y = GermMv, color = TrtID)) +
  geom_point() +
  geom_line() # no counts after early August
  
# get the older data
mvInit <- dat2 %>%
  filter(Litter.yes == 1 & SpPresent == "Ev" & Date3 =="2018-07-11") %>%
  select(TrtID, GermMv) %>%
  rename(NewGermMv = GermMv)
mvInit

# update next dataset (late July)
mvJul <- dat2 %>%
  filter(Litter.yes == 1 & SpPresent == "Ev" & Date3 =="2018-07-24") %>%
  select(Date3, TrtID, GermMv) %>%
  full_join(mvInit) %>%
  mutate(NewGermMv = GermMv + NewGermMv)
mvJul

# update next dataset (early August)
mvAug <- dat2 %>%
  filter(Litter.yes == 1 & SpPresent == "Ev" & Date3 =="2018-08-02") %>%
  select(Date3, TrtID, GermMv) %>%
  full_join(select(mvJul, -c(GermMv, Date3))) %>%
  mutate(NewGermMv = GermMv + NewGermMv)
mvAug

# update next dataset (post harverst)
mvOct <- dat2 %>%
  filter(Litter.yes == 1 & SpPresent == "Ev" & Date3 =="2018-10-05") %>%
  select(Date3, TrtID, GermMv) %>%
  full_join(select(mvAug, -c(GermMv, Date3))) 
mvOct

# merge new data
datml <- full_join(mvJul, mvAug) %>%
  full_join(mvOct)

# merge with main data
# replace NA's
dat3 <- dat2 %>%
  full_join(datml) %>%
  mutate(NewGermMv = ifelse(is.na(NewGermMv), GermMv, NewGermMv))

# re-check counts
dat3 %>%
  filter(SpPresent == "Ev") %>%
  ggplot(aes(x = Date3, y = NewGermMv, color = TrtID)) +
  geom_point() +
  geom_line()

# mean of mv germination in all treatments
corSum <- dat3 %>%
  group_by(Date3, Litter, SpPresent) %>%
  summarise(corGerm = round(mean(NewGermMv, na.rm=T))) %>%
  ungroup() # rounded means so that they're integers
corSum

# mean of mv germination in ev treatments
corSub <- corSum %>%
  filter(SpPresent == "Ev") %>%
  select(-SpPresent) %>%
  mutate(corGerm = replace_na(corGerm, 0))
corSub

# germination corrections over time
corSub %>%
  ggplot(aes(x = Date3, y = corGerm, color = Litter)) +
  geom_point() +
  geom_line()
# drop in Low"
# Low 1 is at 3 on 6/25, then drops
# Low 2 is at 1 on 6/25, then drops
# These may be due to observational errors or plants dying. Either way, the same error or change could have occurred in other pots, so keep these numbers the same.

# add germination corrections to full data
dat4 <- full_join(dat3,corSub)
dat4


#### accidentally removed plants ####

# When Amy counted in July (7/24) and Lily counted in August (8/2) they accidentally removed some of the plants, which need to be added back

#subset notes
pulledNotes = unlist(lapply(dat4$Notes, grep, pattern="Pulled",
                            ignore.case=T, value=T))
pulledNotes
pulledDat <- dat4 %>%
  filter(Notes %in% pulledNotes) %>%
  select(Date3,TrtID,Notes)
pulledDat
pulledDat$Notes

# add in the pulled plants (manually check this - order may change)
pulledDat$PulledEv = c(1, 2, 1, 2, 1, 1, 0, 1, 1, 1, 1, 0, 1) # assuming that when Lily didn't indicate whether it was Ev or Mv, it was Ev (easier to pull by accident)
pulledDat$PulledMv = c(0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0)

# check values
select(pulledDat, Notes, PulledEv, PulledMv)

# update data with July pulls
julPulledDat <- dat4 %>%
  filter(Date3 > "2018-07-24") %>%
  full_join(filter(pulledDat, Date3 == "2018-07-24") %>% select(-c(Date3, Notes))) %>%
  mutate(PulledEv = replace_na(PulledEv, 0),
         PulledMv = replace_na(PulledMv, 0),
         NewGermMv2 = NewGermMv + PulledMv,
         NewGermEv = GermEv + PulledEv) %>%
  select(-c(PulledEv, PulledMv))

# update data with August pulls
augPulledDat <- julPulledDat %>%
  filter(Date3 > "2018-08-02") %>%
  full_join(filter(pulledDat, Date3 == "2018-08-02") %>% select(-c(Date3, Notes))) %>%
  mutate(PulledEv = replace_na(PulledEv, 0),
         PulledMv = replace_na(PulledMv, 0),
         NewGermMv2 = NewGermMv2 + PulledMv,
         NewGermEv = NewGermEv + PulledEv) %>%
  select(-c(PulledEv, PulledMv))

# merge full data
dat5 <- dat4 %>%
  filter(Date3 <= "2018-07-24") %>%
  full_join(filter(julPulledDat, Date3 <= "2018-08-02")) %>%
  full_join(augPulledDat) %>%
  mutate(NewGermMv2 = ifelse(is.na(NewGermMv2), NewGermMv, NewGermMv2),
         NewGermEv = ifelse(is.na(NewGermEv), GermEv, NewGermEv))
dat5

# missing row
anti_join(dat4, dat5) %>% data.frame() # Mv.Med.yes.3
# removed the pot without data


#### final data formatting ####

# remove Mv from litter (make negative values 0)
# remove pot that was dropped
# pot-scale infection prevalence (use observed total counts, note the Mv in Ev only pots on 7/11 include some non-observed values (pulled on 7/4))
dat6 <- dat5 %>%
  mutate(CorGermMv = NewGermMv2 - corGerm,
         CorGermMv = ifelse(CorGermMv < 0, 0, CorGermMv),
         TrtID != "Mv.Med.yes.3",
         PropInfMv = InfectedMv/GermMv,
         PropInfEv = InfectedEv/GermEv)

# check values greater than 50
filter(dat6, CorGermMv > 50) %>% select(GermMv, NewGermMv, NewGermMv2, CorGermMv) # all started as greater than 50

filter(dat6, NewGermEv > 50 & Date < "2018-10-05") %>% select(GermEv, NewGermEv) # none


#### initial visualizations ####

# unusual values
dat6 %>%
  ggplot(aes(x = CorGermMv)) + 
  geom_histogram()

dat6 %>%
  ggplot(aes(x = NewGermEv)) + 
  geom_histogram()

dat6 %>%
  ggplot(aes(x = PropInfEv)) + 
  geom_histogram()

# establishment: which date to use for each species?
dat6 %>%
  filter(SpPresent != "Ev") %>%
  ggplot(aes(x = Date3, y = CorGermMv, shape = Shade, linetype = Shade)) +
  stat_summary(fun.y = "mean", geom = "point") +
  stat_summary(fun.y = "mean", geom = "line") +
  stat_summary(fun.data = "mean_cl_boot", geom = "errorbar") +
  facet_grid(Litter ~ SpPresent)
# third date
sort(unique(dat6$Date3)) # "2018-06-27"

dat6 %>%
  filter(SpPresent != "Mv") %>%
  ggplot(aes(x = Date3, y = NewGermEv)) +
  stat_summary(fun.y = "mean", geom = "point") +
  stat_summary(fun.y = "mean", geom = "line") +
  stat_summary(fun.data = "mean_cl_boot", geom = "errorbar") +
  facet_grid(Litter ~ SpPresent)
# 6th or last date "2018-07-24" or "2018-10-05"

# infection: which date to use
dat6 %>%
  filter(SpPresent != "Mv") %>%
  ggplot(aes(x = Date3, y = PropInfEv)) +
  stat_summary(fun.y = "mean", geom = "point") +
  stat_summary(fun.y = "mean", geom = "line") +
  stat_summary(fun.data = "mean_cl_boot", geom = "errorbar") +
  facet_grid(Litter ~ SpPresent)
# last date

# final density (for biomass analysis): which date to use?
dat6 %>%
  filter(SpPresent != "Ev") %>%
  ggplot(aes(x = Date3, y = GermMv, shape = Shade, linetype = Shade)) +
  stat_summary(fun.y = "mean", geom = "point") +
  stat_summary(fun.y = "mean", geom = "line") +
  stat_summary(fun.data = "mean_cl_boot", geom = "errorbar") +
  facet_grid(Litter ~ SpPresent)
# last date "2018-10-05"

dat6 %>%
  filter(SpPresent != "Mv") %>%
  ggplot(aes(x = Date3, y = GermEv, shape = Shade, linetype = Shade)) +
  stat_summary(fun.y = "mean", geom = "point") +
  stat_summary(fun.y = "mean", geom = "line") +
  stat_summary(fun.data = "mean_cl_boot", geom = "errorbar") +
  facet_grid(Litter ~ SpPresent)
# 6th date


#### establishment statistics ####

# subset data by date
MvEstDat <- dat6 %>%
  filter(SpPresent != "Ev" & Date3 == "2018-06-27" & !is.na(CorGermMv))

EvEstDat1 <- dat6 %>%
  filter(SpPresent != "Mv" & Date3 == "2018-07-24" & !is.na(NewGermEv))

EvEstDat2 <- dat6 %>%
  filter(SpPresent != "Mv" & Date3 == "2018-10-05"& !is.na(NewGermEv))

# mean vs. variance
mean(MvEstDat$CorGermMv)
var(MvEstDat$CorGermMv) # lower

mean(EvEstDat1$NewGermEv)
var(EvEstDat1$NewGermEv) # lower

mean(EvEstDat2$NewGermEv)
var(EvEstDat2$NewGermEv) # much higher - need quasiPoisson

# Mv models
meMod1 <- glm(CorGermMv ~ SpPresent * Litter.g + Shade, data = MvEstDat, family = poisson)
summary(meMod1)
Anova(meMod1, type = 3)
# this is the same as doing drop1, test = "Chisq"
# no sig interaction

meMod2 <- glm(CorGermMv ~ SpPresent + Litter.g + Shade, data = MvEstDat, family = poisson)
summary(meMod2)
Anova(meMod2)
# Litter has a significant effect
# plot(meMod2)

meMod3 <- glm(CorGermMv ~ SpPresent * Litter.yes + Shade, data = MvEstDat, family = poisson)
summary(meMod3)
Anova(meMod3, type = 3)
# no sig interaction

meMod4 <- glm(CorGermMv ~ SpPresent + Litter.yes + Shade, data = MvEstDat, family = poisson)
summary(meMod4)
Anova(meMod4)
# no sig effects

# Mv models without litter correction
meMod5 <- glm(NewGermMv2 ~ SpPresent * Litter.g + Shade, data = MvEstDat, family = poisson)
summary(meMod5)
Anova(meMod5, type = 3)
# no sig interaction

meMod6 <- glm(NewGermMv2 ~ SpPresent + Litter.g + Shade, data = MvEstDat, family = poisson)
summary(meMod6)
Anova(meMod6)
# no sig effect

meMod7 <- glm(NewGermMv2 ~ SpPresent * Litter.yes + Shade, data = MvEstDat, family = poisson)
summary(meMod7)
Anova(meMod7, type = 3)
# no sig interaction

meMod8 <- glm(NewGermMv2 ~ SpPresent + Litter.yes + Shade, data = MvEstDat, family = poisson)
summary(meMod8)
Anova(meMod8)
# no sig effects

# Ev models
eeMod1 <- glm(NewGermEv ~ SpPresent * Litter.g, data = EvEstDat1, family = poisson)
summary(eeMod1)
Anova(eeMod1, type = 3)
# no sig interaction

eeMod2 <- glm(NewGermEv ~ SpPresent + Litter.g, data = EvEstDat1, family = poisson)
summary(eeMod2)
Anova(eeMod2)
# no sig interaction

eeMod3 <- glm(NewGermEv ~ SpPresent * Litter.yes, data = EvEstDat1, family = poisson)
summary(eeMod3)
Anova(eeMod3, type = 3)
# no sig interaction

eeMod4 <- glm(NewGermEv ~ SpPresent + Litter.yes, data = EvEstDat1, family = poisson)
summary(eeMod4)
Anova(eeMod4)
# litter has a significant effect
# plot(eeMod4)

# Ev models with post-harvest counts
eeMod5 <- glm(NewGermEv ~ SpPresent * Litter.g, data = EvEstDat2, family = quasipoisson)
summary(eeMod5)
Anova(eeMod5, type = 3)
# no sig interaction

eeMod6 <- glm(NewGermEv ~ SpPresent + Litter.g, data = EvEstDat2, family = quasipoisson)
summary(eeMod6)
Anova(eeMod6)
# species has a sig effect
# plot(eeMod6)

eeMod7 <- glm(NewGermEv ~ SpPresent * Litter.yes, data = EvEstDat2, family = quasipoisson)
summary(eeMod7)
Anova(eeMod7, type = 3)
# no sig interaction

eeMod8 <- glm(NewGermEv ~ SpPresent + Litter.yes, data = EvEstDat2, family = quasipoisson)
summary(eeMod8)
Anova(eeMod8)
# litter and species have a sig effect
# plot(eeMod8)
# The effect of Mv for post-harvest, but not day 6 may be because Mv made the Ev smaller with fewer tillers, but didn't affect the number of seeds that established. This will be captured with biomass.


#### infection statistics ####

# Ev models with post-harvest counts
eiMod1 <- glm(cbind(InfectedEv,GermEv) ~ SpPresent * Litter.g, data = EvEstDat2, family = binomial)
summary(eiMod1)
Anova(eiMod1, type = 3)
# no sig interaction

eiMod2 <- glm(cbind(InfectedEv,GermEv) ~ SpPresent + Litter.g, data = EvEstDat2, family = binomial)
summary(eiMod2)
Anova(eiMod2)
# sig effect of both
# plot(eiMod2)

eiMod3 <- glm(cbind(InfectedEv,GermEv) ~ SpPresent * Litter.yes, data = EvEstDat2, family = binomial)
summary(eiMod3)
Anova(eiMod3, type = 3)
# sig interaction
# plot(eiMod3)


#### outputs ####

# create Mv data for biomass analyses
MvBmDat <- dat6 %>%
  filter(SpPresent != "Ev" & Date3 == "2018-10-05")

# save datasets
write_csv(MvEstDat, "./output/mv_establishment_data.csv")
write_csv(EvEstDat1, "./output/ev_establishment_data.csv")
write_csv(EvEstDat2, "./output/ev_infection_data.csv")
write_csv(MvBmDat, "./output/mv_count_data.csv")
write_csv(dat6, "./output/establishment_infection_data.csv")

# save models
save(meMod2, file = "./output/mv_establishment_model.rda")
save(meMod6, file = "./output/mv_uncorrected_establishment_model.rda")
save(eeMod4, file = "./output/ev_establishment_model.rda")
save(eiMod2, file = "./output/ev_infection_model_grams.rda")
save(eiMod3, file = "./output/ev_infection_model_present.rda")