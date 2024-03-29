#### goals #### 

# process establishment data


#### set up ####

# clear all existing data
rm(list=ls())

# load libraries
library(tidyverse)

# import data (all data combined up to post harvest)
dat1 <- read_csv("data/MvEv_GerminationInfection_PostHarvest.csv")


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
  dplyr::select(Date, Date2) %>%
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
  dplyr::select(TrtID, GermMv) %>%
  rename(NewGermMv = GermMv)
mvInit

# update next dataset (late July)
mvJul <- dat2 %>%
  filter(Litter.bin == 1 & SpPresent == "Ev" & Date2 =="2018-07-24") %>%
  dplyr::select(Date2, TrtID, GermMv) %>%
  full_join(mvInit) %>%
  mutate(NewGermMv = GermMv + NewGermMv)
mvJul

# update next dataset (early August)
mvAug <- dat2 %>%
  filter(Litter.bin == 1 & SpPresent == "Ev" & Date2 =="2018-08-02") %>%
  dplyr::select(Date2, TrtID, GermMv) %>%
  full_join(dplyr::select(mvJul, -c(GermMv, Date2))) %>%
  mutate(NewGermMv = GermMv + NewGermMv)
mvAug

# update next dataset (post harverst)
mvOct <- dat2 %>%
  filter(Litter.bin == 1 & SpPresent == "Ev" & Date2 =="2018-10-05") %>%
  dplyr::select(Date2, TrtID, GermMv) %>%
  full_join(dplyr::select(mvAug, -c(GermMv, Date2))) 
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
  dplyr::select(-SpPresent) %>%
  mutate(corGerm = replace_na(corGerm, 0))
corSub

# germination corrections over time
corSub %>%
  ggplot(aes(x = Date2, y = corGerm, color = Litter)) +
  geom_point() +
  geom_line()

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
  dplyr::select(Date2,TrtID,Notes)
pulledDat
pulledDat$Notes

# add in the pulled plants (manually check this - order may change)
pulledDat$PulledEv = c(1, 2, 1, 2, 1, 1, 0, 1, 1, 1, 1, 0, 1) # assuming that when Lily didn't indicate whether it was Ev or Mv, it was Ev (easier to pull by accident)
pulledDat$PulledMv = c(0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0)

# check values
dplyr::select(pulledDat, Notes, PulledEv, PulledMv)

# update data with July pulls
julPulledDat <- dat4 %>%
  filter(Date2 > "2018-07-24") %>%
  full_join(filter(pulledDat, Date2 == "2018-07-24") %>% 
              dplyr::select(-c(Date2, Notes))) %>%
  mutate(PulledEv = replace_na(PulledEv, 0),
         PulledMv = replace_na(PulledMv, 0),
         NewGermMv2 = NewGermMv + PulledMv,
         NewGermEv = GermEv + PulledEv) %>%
  dplyr::select(-c(PulledEv, PulledMv))

# update data with August pulls
augPulledDat <- julPulledDat %>%
  filter(Date2 > "2018-08-02") %>%
  full_join(filter(pulledDat, Date2 == "2018-08-02") %>% 
              dplyr::select(-c(Date2, Notes))) %>%
  mutate(PulledEv = replace_na(PulledEv, 0),
         PulledMv = replace_na(PulledMv, 0),
         NewGermMv2 = NewGermMv2 + PulledMv,
         NewGermEv = NewGermEv + PulledEv) %>%
  dplyr::select(-c(PulledEv, PulledMv))

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
filter(dat6, CorGermMv > 50) %>% dplyr::select(GermMv, NewGermMv, NewGermMv2, CorGermMv) # all started as greater than 50

filter(dat6, NewGermEv > 50 & Date < "2018-10-05") %>% dplyr::select(GermEv, NewGermEv) # none

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

# counts for biomass: which date to use
dat7 %>%
  filter(SpPresent != "Ev" & Shade == "no") %>%
  ggplot(aes(x = Date2, y = GermMv)) +
  stat_summary(fun = "mean", geom = "point") +
  stat_summary(fun = "mean", geom = "line") +
  stat_summary(fun.data = "mean_cl_boot", geom = "errorbar") +
  facet_grid(Litter ~ SpPresent)
# last date

dat7 %>%
  filter(SpPresent != "Mv") %>%
  ggplot(aes(x = Date2, y = GermEv)) +
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

MvCountDat <- dat7 %>%
  filter(SpPresent != "Ev" & Date2 == "2018-10-05") %>%
  filter(Shade == "no")

# save data
write_csv(MvEstDat, "intermediate-data/mv_establishment_data.csv")
write_csv(EvEstDat, "intermediate-data/ev_establishment_data.csv")
write_csv(EvInfDat, "intermediate-data/ev_infection_data.csv")
write_csv(MvCountDat, "intermediate-data/mv_final_count_data.csv")
write_csv(dat7, "intermediate-data/establishment_infection_data.csv")


#### supplementary figure ####

# number Mv removed
# goal is to show actual numbers that were in the pots, not the cumulative numbers had we not removed the Mv (values in finalized dataset)
# need to subtract 7/4/ from 7/11 because 7/4 counts are in the 7/11 dataset despite removal
mvRem <- dat2 %>%
  filter(SpPresent == "Ev" & !is.na(GermMv)) %>%
  dplyr::select(TrtID, Litter, SpPresent, Date2, GermMv) %>%
  pivot_wider(names_from = Date2,
              values_from = GermMv) %>%
  rename(jun_21 = '2018-06-21',
         jul_04 = '2018-07-04',
         jul_11 = '2018-07-11',
         aug_02 = '2018-08-02') %>%
  mutate(jul_11 = jul_11 - jul_04) %>%
  pivot_longer(cols = jun_21:aug_02,
               names_to = "Date",
               values_to = "GermMv") %>%
  mutate(Date = fct_recode(Date, 
                           '2018-06-21' = 'jun_21',
                           '2018-07-04' = 'jul_04',
                           '2018-07-11' = 'jul_11',
                           '2018-08-02' = 'aug_02') %>%
           as.character() %>% as.Date())

# time intervals between Mv removals
mvRem %>%
  dplyr::select(Date) %>%
  unique() %>%
  arrange(Date) %>%
  mutate(days = lead(Date) - Date)

# combien removals with rest of data
dat8 <- dat7 %>%
  dplyr::select(TrtID, Litter, SpPresent, Date2, NewGermEv, NewGermMv2) %>%
  full_join(mvRem %>%
              rename(GermMvRem = GermMv, Date2 = Date)) %>%
  mutate(GermEv = NewGermEv,
         GermMv = case_when(SpPresent == "Ev" ~ GermMvRem,
                             TRUE ~ NewGermMv2),
         SpPresent = fct_recode(SpPresent, "Both" = "Ev+Mv")) %>%
  dplyr::select(-c(NewGermEv, NewGermMv2, GermMvRem)) %>%
  pivot_longer(cols = c(GermEv, GermMv),
               names_to = "Sp",
               values_to = "Germ",
               names_prefix = "Germ") %>%
  filter(!is.na(Germ))

dat8Text <- dat8 %>%
  filter(Sp == "Ev" & Date2 == max(dat8$Date2)) %>%
  group_by(Sp, SpPresent, Litter, Date2) %>%
  summarise(Germ = mean(Germ)) %>%
  ungroup() %>%
  mutate(til = "tillers",
         Germ = case_when(SpPresent == "Ev" & Litter == "None" ~ Germ - 18,
                          SpPresent == "Ev" & Litter != "None" ~ Germ + 14,
                          SpPresent == "Both" & Litter == "None" ~ Germ + 12,
                          SpPresent == "Both" & Litter != "None" ~ Germ - 8))

# figure for supplement
pdf("output/AppS2_FigS1.pdf", width = 5, height = 5)
ggplot(dat8, aes(x = Date2, y = Germ, color = Sp)) +
  stat_summary(geom = "errorbar", width = 0, fun.data = "mean_se") +
  stat_summary(geom = "line", fun = "mean") +
  stat_summary(geom = "point", size = 0.5, fun = "mean") +
  geom_text(data = dat8Text, aes(label = til), hjust = 1, size = 2, show.legend = F) +
  facet_grid(Litter ~ SpPresent) +
  labs(x = "Date", y = "Seedlings") +
  scale_color_manual(values = c("black", "deepskyblue2"), name = "Species") +
  theme_bw() +
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text = element_text(size = 7, color = "black"),
        axis.title = element_text(size = 9),
        legend.text = element_text(size = 7),
        legend.title = element_text(size = 7),
        strip.background = element_blank(),
        strip.text = element_text(size = 8),
        legend.position = c(0.01, 0.93),
        legend.margin = margin(-0.1, 0, 0.2, 2, unit = "cm"),
        legend.background = element_blank(),
        legend.key.height = unit(0.3, unit = "cm"))
dev.off()


#### t-test of unintended Mv ####

# select fourth harvesting date
# select Mv in treatments of interest
dat8sub <- dat8 %>%
  filter(Date2 == sort(unique(Date2))[4] & Sp == "Mv" & SpPresent != "Mv")

# visualize
ggplot(dat8sub, aes(x = SpPresent, y = Germ)) +
  stat_summary(geom = "errorbar", width = 0, fun.data = "mean_se") +
  stat_summary(geom = "point", fun = "mean", size = 2)

# t test
t.test(Germ ~ SpPresent, data = dat8sub)
