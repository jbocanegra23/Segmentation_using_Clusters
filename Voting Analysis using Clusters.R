
## The following process was done for a client of mine in order to calculate 
## and segment target groups for her marketing campaign. This uses clustering
## also to ID different groups within voting patterns. And then give her counts
## in order to calculate budget projections for fundraising.

library(dplyr)
library(readxl)
library(tidyr)
library(WriteXLS)
library(dplyr)
library(ISLR)
library(ggplot2)


## File intake
franklin <- read.delim("FRANKLIN.txt", sep = ",")
general_election <- franklin %>% select(1, 63, 71, 81, 86, 92, 99)
party_affinity <- franklin %>% select(1, 11, 62, 69, 78, 85, 91, 96, 103)

#WriteXLS(general_election, "general_election.xlsx")
#WriteXLS(party_affinity, "party_affinity.xlsx")

## Some work here that is easier to do with excel

general_election <- read_excel("general_election.xlsx")
party_affinity <- read_excel("party_affinity.xlsx")

## data wrangling and translation
franklin.small <- franklin %>% select(1, 4:6, 12:16, 39, 42, 43)
franklin.small <- franklin.small %>% left_join(party_affinity) %>% left_join(general_election)
franklin.small$AddressID <- paste(franklin.small$RESIDENTIAL_ADDRESS1, franklin.small$RESIDENTIAL_SECONDARY_ADDR, franklin.small$RESIDENTIAL_CITY, sep = "_")
franklin.small$Generals_Voted <- franklin.small$GENERAL.11.07.2006 + franklin.small$GENERAL.11.04.2008 +
                                franklin.small$GENERAL.11.02.2010 + franklin.small$GENERAL.11.06.2012 + 
                                franklin.small$GENERAL.11.04.2014 + franklin.small$GENERAL.11.08.2016

## cluster analysis
general_election.cluster <- kmeans(franklin.small[, c(18:23)], centers = 5, nstart = 20)
franklin.small$Cluster <- general_election.cluster$cluster

franklin.small.analysis <- franklin.small %>% group_by(Cluster) %>%
  summarise(Ave_R_Precent = mean(`Republicans%`, na.rm = TRUE),
            Voter_Count = n(),
            Lean_Republican = sum(`Republicans%` > 0.6, na.rm = TRUE),
            Lean_Democrat = sum(`Republicans%` < 0.4, na.rm = TRUE),
            Swing = sum(`Republicans%` < 0.6 & `Republicans%` > 0.4, na.rm = TRUE),
            Ave_Gens_Voted = mean(Generals_Voted),
            Ave_Pri_Voted = mean(Primaries_Voted))


#WriteXLS(franklin.small, "franklin.small.xlsx")

## excel work to calculate solid Rs, solid Ds, and then swing ##

franklin.fixed <- read_excel("franklin.small.fixed.xlsx", sheet = 1)

household_counts <- franklin.fixed %>% filter(Target_Universe == "Yes") %>%
  group_by(Cluster) %>%
  summarise(HH_Counts = n_distinct(AddressID))

household_counts <- franklin.fixed %>% filter(Target_Universe == "Yes") %>%
  summarise(HH_Counts = n_distinct(AddressID))

NumInHH <- franklin.fixed %>% group_by(AddressID) %>%
  summarise(NumInHH = n(),
            NumInHH.Targets = sum(Target_Universe == "Yes"),
            NumInHH.TargetsML = sum(Target_Universe == "Yes" & Cluster == "Most Likely to Vote"),
            NumInHH.TargetsG1 = sum(Target_Universe == "Yes" & Cluster == "GOTV - 1"))

Targeted_HH <- NumInHH %>% filter(NumInHH.Targets > 0) %>%
  summarise(Overall.Reach = sum(NumInHH))

Targeted_HH.ML <- NumInHH %>% filter(NumInHH.TargetsML > 0) %>%
  summarise(Overall.MLReach = sum(NumInHH))

Targeted_HH.G1 <- NumInHH %>% filter(NumInHH.TargetsG1 > 0) %>%
  summarise(Overall.G1Reach = sum(NumInHH))

ZIP.Counts <- franklin.fixed %>% filter(Target_Universe == "Yes") %>%
  group_by(RESIDENTIAL_ZIP) %>%
  summarise(Individuals = n_distinct(SOS_VOTERID),
            Households = n_distinct(AddressID))

City.Counts <- franklin.fixed %>% filter(Target_Universe == "Yes") %>%
  group_by(RESIDENTIAL_CITY) %>%
  summarise(Individuals = n_distinct(SOS_VOTERID),
            Households = n_distinct(AddressID))

HouseDistrict.Counts <- franklin.fixed %>% filter(Target_Universe == "Yes") %>%
  group_by(STATE_REPRESENTATIVE_DISTRICT) %>%
  summarise(Individuals = n_distinct(SOS_VOTERID),
            Households = n_distinct(AddressID))

#WriteXLS(ZIP.Counts, "ZIP_Counts.xlsx")
#WriteXLS(City.Counts, "City_Counts.xlsx")
#WriteXLS(HouseDistrict.Counts, "HouseDistrict_Counts.xlsx")



