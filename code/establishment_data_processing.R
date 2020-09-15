#### goals #### 

# process establishment data


#### set up ####

# clear all existing data
rm(list=ls())

# load libraries
library(tidyverse)

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
# date formats: proper format, days post planting
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
      TRUE ~ Date) %>% 
      as.character() %>% 
      as.Date("%Y%m%d"),
    Days = round(difftime(Date2,"2018-06-15",units="days")),
    Shade = ifelse(Treatment == "Shade", "yes", "no"),
    Litter = recode(Treatment, Shade = "Med") %>% 
      factor(levels = c("None", "Low", "Med", "High")),
    Litter.g = case_when(Litter == "None" ~ 0,
                         Litter == "Low" ~ 0.91,
                         Litter == "Med" ~ 1.82,
                         Litter == "High" ~ 3.64),
    Litter.bin=as.factor(ifelse(Litter == "None", 0, 1)),
    SpPresent = factor(SpPresent, c("Ev", "Mv", "Ev+Mv")),
    Competition = recode(SpPresent, Ev = 0, Mv = 0, "Ev+Mv" = 1),
    TrtID = paste(SpPresent, Litter, Shade, PotID, sep="."))

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
  ggplot(aes(x = Date2, y = GermMv, color = TrtID)) +
  geom_point() +
  geom_line() # no counts after early August

# get the older data
mvInit <- dat2 %>%
  filter(Litter.bin == 1 & SpPresent == "Ev" & Date2 =="2018-07-11") %>%
  select(TrtID, GermMv) %>%
  rename(NewGermMv = GermMv)
mvInit

# update next dataset (late July)
mvJul <- dat2 %>%
  filter(Litter.bin == 1 & SpPresent == "Ev" & Date2 =="2018-07-24") %>%
  select(Date2, TrtID, GermMv) %>%
  full_join(mvInit) %>%
  mutate(NewGermMv = GermMv + NewGermMv)
mvJul

# update next dataset (early August)
mvAug <- dat2 %>%
  filter(Litter.bin == 1 & SpPresent == "Ev" & Date2 =="2018-08-02") %>%
  select(Date2, TrtID, GermMv) %>%
  full_join(select(mvJul, -c(GermMv, Date2))) %>%
  mutate(NewGermMv = GermMv + NewGermMv)
mvAug

# update next dataset (post harverst)
mvOct <- dat2 %>%
  filter(Litter.bin == 1 & SpPresent == "Ev" & Date2 =="2018-10-05") %>%
  select(Date2, TrtID, GermMv) %>%
  full_join(select(mvAug, -c(GermMv, Date2))) 
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
  ggplot(aes(x = Date2, y = NewGermMv, color = TrtID)) +
  geom_point() +
  geom_line()

# mean of mv germination in all treatments
corSum <- dat3 %>%
  group_by(Date2, Litter, SpPresent) %>%
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
  ggplot(aes(x = Date2, y = corGerm, color = Litter)) +
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
  select(Date2,TrtID,Notes)
pulledDat
pulledDat$Notes

# add in the pulled plants (manually check this - order may change)
pulledDat$PulledEv = c(1, 2, 1, 2, 1, 1, 0, 1, 1, 1, 1, 0, 1) # assuming that when Lily didn't indicate whether it was Ev or Mv, it was Ev (easier to pull by accident)
pulledDat$PulledMv = c(0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0)

# check values
select(pulledDat, Notes, PulledEv, PulledMv)

# update data with July pulls
julPulledDat <- dat4 %>%
  filter(Date2 > "2018-07-24") %>%
  full_join(filter(pulledDat, Date2 == "2018-07-24") %>% 
              select(-c(Date2, Notes))) %>%
  mutate(PulledEv = replace_na(PulledEv, 0),
         PulledMv = replace_na(PulledMv, 0),
         NewGermMv2 = NewGermMv + PulledMv,
         NewGermEv = GermEv + PulledEv) %>%
  select(-c(PulledEv, PulledMv))

# update data with August pulls
augPulledDat <- julPulledDat %>%
  filter(Date2 > "2018-08-02") %>%
  full_join(filter(pulledDat, Date2 == "2018-08-02") %>% 
              select(-c(Date2, Notes))) %>%
  mutate(PulledEv = replace_na(PulledEv, 0),
         PulledMv = replace_na(PulledMv, 0),
         NewGermMv2 = NewGermMv2 + PulledMv,
         NewGermEv = NewGermEv + PulledEv) %>%
  select(-c(PulledEv, PulledMv))

# merge full data
dat5 <- dat4 %>%
  filter(Date2 <= "2018-07-24") %>%
  full_join(filter(julPulledDat, Date2 <= "2018-08-02")) %>%
  full_join(augPulledDat) %>%
  mutate(NewGermMv2 = ifelse(is.na(NewGermMv2), NewGermMv, NewGermMv2),
         NewGermEv = ifelse(is.na(NewGermEv), GermEv, NewGermEv))
dat5

# missing row
anti_join(dat4, dat5) %>% data.frame() # Mv.Med.yes.3
# removed the pot without data


#### final data formatting ####

# remove Mv from litter (make negative values 0)
# columns with numbers planted and planted with litter additions
# pot-scale infection prevalence (use observed total counts, note the Mv in Ev only pots on 7/11 include some non-observed values (pulled on 7/4))
# remove pot that was dropped
dat6 <- dat5 %>%
  mutate(CorGermMv = NewGermMv2 - corGerm,
         CorGermMv = ifelse(CorGermMv < 0, 0, CorGermMv),
         PlantedEv = 50,
         PlantedMv = 50,
         PlantedLitterMv = 50 + corGerm,
         PropInfMv = InfectedMv/GermMv,
         PropInfEv = InfectedEv/GermEv) %>%
  filter(TrtID != "Mv.Med.yes.3")

# check values greater than 50
filter(dat6, CorGermMv > 50) %>% select(GermMv, NewGermMv, NewGermMv2, CorGermMv) # all started as greater than 50

filter(dat6, NewGermEv > 50 & Date < "2018-10-05") %>% select(GermEv, NewGermEv) # none

# if the estimated planted+litter for Mv is lower than observed, increase it
# proportion established
# don't do for last date for Ev (units = stems)
# do for corrected count (numerator corrected) and corrected planted (denominator corrected) for Mv
dat7 <- dat6 %>%
  mutate(PlantedMv2 = ifelse(PlantedMv < CorGermMv, CorGermMv, PlantedMv),
         PlantedLitterMv2 = ifelse(PlantedLitterMv < NewGermMv2, NewGermMv2, PlantedLitterMv),
         PropEstEv = ifelse(Date == "2018-10-05", NA_real_, NewGermEv/PlantedEv),
         PropEstMvNumCor = CorGermMv/PlantedMv2,
         PropEstMvDenCor = NewGermMv2/PlantedLitterMv2,
         HealthyEv = GermEv - InfectedEv,
         UngermEv = PlantedEv - NewGermEv,
         UngermMvNum = PlantedMv2 - CorGermMv,
         UngermMvDen = PlantedLitterMv2 - NewGermMv2)


#### initial visualizations ####

# unusual values
dat7 %>%
  ggplot(aes(x = CorGermMv)) + 
  geom_histogram()

dat7 %>%
  ggplot(aes(x = NewGermEv)) + 
  geom_histogram()

dat7 %>%
  ggplot(aes(x = PropInfEv)) + 
  geom_histogram()

# establishment: which date to use for each species?
dat7 %>%
  filter(SpPresent != "Ev") %>%
  ggplot(aes(x = Date2, y = NewGermMv2, shape = Shade, linetype = Shade)) +
  stat_summary(fun = "mean", geom = "point") +
  stat_summary(fun = "mean", geom = "line") +
  stat_summary(fun.data = "mean_cl_boot", geom = "errorbar") +
  facet_grid(Litter ~ SpPresent)
# fifth date
sort(unique(dat7$Date2)) # "2018-07-11"

dat7 %>%
  filter(SpPresent != "Mv") %>%
  ggplot(aes(x = Date2, y = NewGermEv)) +
  stat_summary(fun = "mean", geom = "point") +
  stat_summary(fun = "mean", geom = "line") +
  stat_summary(fun.data = "mean_cl_boot", geom = "errorbar") +
  facet_grid(Litter ~ SpPresent)
# 6th or last date "2018-07-24" or "2018-10-05"

# infection: which date to use
dat7 %>%
  filter(SpPresent != "Mv") %>%
  ggplot(aes(x = Date2, y = PropInfEv)) +
  stat_summary(fun = "mean", geom = "point") +
  stat_summary(fun = "mean", geom = "line") +
  stat_summary(fun.data = "mean_cl_boot", geom = "errorbar") +
  facet_grid(Litter ~ SpPresent)
# last date


#### subset data ####

# subset data by date
MvEstDat <- dat7 %>%
  filter(SpPresent != "Ev" & Date2 == "2018-07-11") %>%
  filter(Shade == "no")

EvEstDat <- dat7 %>%
  filter(SpPresent != "Mv" & Date2 == "2018-07-24")

EvInfDat <- dat7 %>%
  filter(SpPresent != "Mv" & Date2 == "2018-10-05" & !is.na(GermEv))
# one replicate from Ev low litter was lost

# save data
write_csv(MvEstDat, "intermediate-data/mv_establishment_data.csv")
write_csv(EvEstDat, "intermediate-data/ev_establishment_data.csv")
write_csv(EvInfDat, "intermediate-data/ev_infection_data.csv")
write_csv(dat7, "intermediate-data/establishment_infection_data.csv")