####
#### What's a Sport?
####

library(tidyverse)
library(Rmisc)
library(effectsize)
library(jtools)
library(lme4)
library(esc)
library(lmerTest)
library(EMAtools)
library(ggplot2)
library(dplyr)

## data 

d.1 = read.csv("/Users/matiasfonolla/Desktop/RAD Lab/Sports_Study_1.csv", stringsAsFactors = FALSE, na.strings=c(""))

## set seed

set.seed(2022) 

## clean data

#Study 1

d.1 = d.1 %>%
  filter(Q28 == "Yes.") %>% #all participants reported paying attention, so no exclusions
  select(-(1:6), -(8:14)) %>%
  pivot_longer(cols = c(X1_Q317_1:X105_Q14_1), names_to = "activity", values_to = "rating", values_drop_na = TRUE)

## recode factor levels

d.1$activity = recode_factor(d.1$activity, 
                             
                             "X1_Q317_1" = "Soccer_Sport",
                             "X1_Q11_1" =  "Soccer_Normality",
                             "X1_Q12_1" =  "Soccer_Athletic",
                             "X1_Q13_1" =  "Soccer_Competition",
                             "X1_Q14_1" =  "Soccer_Rules",
                             "X2_Q317_1" = "Baseball_Sport",
                             "X2_Q11_1" =  "Baseball_Normality",
                             "X2_Q12_1" =  "Baseball_Athletic",
                             "X2_Q13_1" =  "Baseball_Competition",
                             "X2_Q14_1" =  "Baseball_Rules",
                             "X3_Q317_1" = "Basketball_Sport",
                             "X3_Q11_1" =  "Basketball_Normality",
                             "X3_Q12_1" =  "Basketball_Athletic",
                             "X3_Q13_1" =  "Basketball_Competition",
                             "X3_Q14_1" =  "Basketball_Rules",
                             "X4_Q317_1" = "Football_Sport",
                             "X4_Q11_1" =  "Football_Normality",
                             "X4_Q12_1" =  "Football_Athletic",
                             "X4_Q13_1" =  "Football_Competition",
                             "X4_Q14_1" =  "Football_Rules",
                             "X5_Q317_1" = "Cricket_Sport",
                             "X5_Q11_1" =  "Cricket_Normality",
                             "X5_Q12_1" =  "Cricket_Athletic",
                             "X5_Q13_1" =  "Cricket_Competition",
                             "X5_Q14_1" =  "Cricket_Rules",
                             "X6_Q317_1" = "Tennis_Sport",
                             "X6_Q11_1" =  "Tennis_Normality",
                             "X6_Q12_1" =  "Tennis_Athletic",
                             "X6_Q13_1" =  "Tennis_Competition",
                             "X6_Q14_1" =  "Tennis_Rules",
                             "X7_Q317_1" = "Rugby_Sport",
                             "X7_Q11_1" =  "Rugby_Normality",
                             "X7_Q12_1" =  "Rugby_Athletic",
                             "X7_Q13_1" =  "Rugby_Competition",
                             "X7_Q14_1" =  "Rugby_Rules",
                             "X8_Q317_1" = "Boxing_Sport",
                             "X8_Q11_1" =  "Boxing_Normality",
                             "X8_Q12_1" =  "Boxing_Athletic",
                             "X8_Q13_1" =  "Boxing_Competition",
                             "X8_Q14_1" =  "Boxing_Rules",
                             "X9_Q317_1" = "IceHockey_Sport",
                             "X9_Q11_1" =  "IceHockey_Normality",
                             "X9_Q12_1" =  "IceHockey_Athletic",
                             "X9_Q13_1" =  "IceHockey_Competition",
                             "X9_Q14_1" =  "IceHockey_Rules",
                             "X10_Q317_1"= "FieldHockey_Sport",
                             "X10_Q11_1" = "FieldHockey_Normality",
                             "X10_Q12_1" = "FieldHockey_Athletic",
                             "X10_Q13_1" = "FieldHockey_Competition",
                             "X10_Q14_1" = "FieldHockey_Rules",
                             "X11_Q317_1"= "Volleyball_Sport",
                             "X11_Q11_1" = "Volleyball_Normality",
                             "X11_Q12_1" = "Volleyball_Athletic",
                             "X11_Q13_1" = "Volleyball_Competition",
                             "X11_Q14_1" = "Volleyball_Rules",
                             "X12_Q317_1"= "Handball_Sport",
                             "X12_Q11_1" = "Handball_Normality",
                             "X12_Q12_1" = "Handball_Athletic",
                             "X12_Q13_1" = "Handball_Competition",
                             "X12_Q14_1" = "Handball_Rules",
                             "X13_Q317_1"= "Softball_Sport",
                             "X13_Q11_1" = "Softball_Normality",
                             "X13_Q12_1" = "Softball_Athletic",
                             "X13_Q13_1" = "Softball_Competition",
                             "X13_Q14_1" = "Softball_Rules",
                             "X14_Q317_1"= "Waterpolo_Sport",
                             "X14_Q11_1" = "Waterpolo_Normality",
                             "X14_Q12_1" = "Waterpolo_Athletic",
                             "X14_Q13_1" = "Waterpolo_Competition",
                             "X14_Q14_1" = "Waterpolo_Rules",
                             "X15_Q317_1"= "Lacrosse_Sport",
                             "X15_Q11_1" = "Lacrosse_Normality",
                             "X15_Q12_1" = "Lacrosse_Athletic",
                             "X15_Q13_1" = "Lacrosse_Competition",
                             "X15_Q14_1" = "Lacrosse_Rules",
                             "X16_Q317_1"= "Golf_Sport",
                             "X16_Q11_1" = "Golf_Normality",
                             "X16_Q12_1" = "Golf_Athletic",
                             "X16_Q13_1" = "Golf_Competition",
                             "X16_Q14_1" = "Golf_Rules",
                             "X17_Q317_1"= "Diving_Sport",
                             "X17_Q11_1" = "Diving_Normality",
                             "X17_Q12_1" = "Diving_Athletic",
                             "X17_Q13_1" = "Diving_Competition",
                             "X17_Q14_1" = "Diving_Rules",
                             "X18_Q317_1"= "Polevaulting_Sport",
                             "X18_Q11_1" = "Polevaulting_Normality",
                             "X18_Q12_1" = "Polevaulting_Athletic",
                             "X18_Q13_1" = "Polevaulting_Competition",
                             "X18_Q14_1" = "Polevaulting_Rules",
                             "X19_Q317_1"= "Wingsurfing_Sport",
                             "X19_Q11_1" = "Wingsurfing_Normality",
                             "X19_Q12_1" = "Wingsurfing_Athletic",
                             "X19_Q13_1" = "Wingsurfing_Competition",
                             "X19_Q14_1" = "Wingsurfing_Rules",
                             "X20_Q317_1"= "Judo_Sport",
                             "X20_Q11_1" = "Judo_Normality",
                             "X20_Q12_1" = "Judo_Athletic",
                             "X20_Q13_1" = "Judo_Competition",
                             "X20_Q14_1" = "Judo_Rules",
                             "X21_Q317_1"= "Sumowrestling_Sport",
                             "X21_Q11_1" = "Sumowrestling_Normality",
                             "X21_Q12_1" = "Sumowrestling_Athletic",
                             "X21_Q13_1" = "Sumowrestling_Competition",
                             "X21_Q14_1" = "Sumowrestling_Rules",
                             "X22_Q317_1"= "Bungeejumping_Sport",
                             "X22_Q11_1" = "Bungeejumping_Normality",
                             "X22_Q12_1" = "Bungeejumping_Athletic",
                             "X22_Q13_1" = "Bungeejumping_Competition",
                             "X22_Q14_1" = "Bungeejumping_Rules",
                             "X23_Q317_1"= "Fishing_Sport",
                             "X23_Q11_1" = "Fishing_Normality",
                             "X23_Q12_1" = "Fishing_Athletic",
                             "X23_Q13_1" = "Fishing_Competition",
                             "X23_Q14_1" = "Fishing_Rules",
                             "X24_Q317_1"= "taekwondo_Sport",
                             "X24_Q11_1" = "taekwondo_Normality",
                             "X24_Q12_1" = "taekwondo_Athletic",
                             "X24_Q13_1" = "taekwondo_Competition",
                             "X24_Q14_1" = "taekwondo_Rules",
                             "X25_Q317_1"= "Skateboarding_Sport",
                             "X25_Q11_1" = "Skateboarding_Normality",
                             "X25_Q12_1" = "Skateboarding_Athletic",
                             "X25_Q13_1" = "Skateboarding_Competition",
                             "X25_Q14_1" = "Skateboarding_Rules",
                             "X26_Q317_1"= "Hurdles_Sport",
                             "X26_Q11_1" = "Hurdles_Normality",
                             "X26_Q12_1" = "Hurdles_Athletic",
                             "X26_Q13_1" = "Hurdles_Competition",
                             "X26_Q14_1" = "Hurdles_Rules",
                             "X27_Q317_1"= "MoutainBiking_Sport",
                             "X27_Q11_1" = "MoutainBiking_Normality",
                             "X27_Q12_1" = "MoutainBiking_Athletic",
                             "X27_Q13_1" = "MoutainBiking_Competition",
                             "X27_Q14_1" = "MoutainBiking_Rules",
                             "X28_Q317_1"= "Curling_Sport",
                             "X28_Q11_1" = "Curling_Normality",
                             "X28_Q12_1" = "Curling_Athletic",
                             "X28_Q13_1" = "Curling_Competition",
                             "X28_Q14_1" = "Curling_Rules",
                             "X29_Q317_1"= "Badminton_Sport",
                             "X29_Q11_1" = "Badminton_Normality",
                             "X29_Q12_1" = "Badminton_Athletic",
                             "X29_Q13_1" = "Badminton_Competition",
                             "X29_Q14_1" = "Badminton_Rules",
                             "X30_Q317_1"= "Rowing_Sport",
                             "X30_Q11_1" = "Rowing_Normality",
                             "X30_Q12_1" = "Rowing_Athletic",
                             "X30_Q13_1" = "Rowing_Competition",
                             "X30_Q14_1" = "Rowing_Rules",
                             "X31_Q317_1"= "Fencing_Sport",
                             "X31_Q11_1" = "Fencing_Normality",
                             "X31_Q12_1" = "Fencing_Athletic",
                             "X31_Q13_1" = "Fencing_Competition",
                             "X31_Q14_1" = "Fencing_Rules",
                             "X32_Q317_1"= "Gymnastics_Sport",
                             "X32_Q11_1" = "Gymnastics_Normality",
                             "X32_Q12_1" = "Gymnastics_Athletic",
                             "X32_Q13_1" = "Gymnastics_Competition",
                             "X32_Q14_1" = "Gymnastics_Rules",
                             "X33_Q317_1"= "Rockclimbing_Sport",
                             "X33_Q11_1" = "Rockclimbing_Normality",
                             "X33_Q12_1" = "Rockclimbing_Athletic",
                             "X33_Q13_1" = "Rockclimbing_Competition",
                             "X33_Q14_1" = "Rockclimbing_Rules",
                             "X34_Q317_1"= "Karate_Sport",
                             "X34_Q11_1" = "Karate_Normality",
                             "X34_Q12_1" = "Karate_Athletic",
                             "X34_Q13_1" = "Karate_Competition",
                             "X34_Q14_1" = "Karate_Rules",
                             "X35_Q317_1"= "CCSkiing_Sport",
                             "X35_Q11_1" = "CCSkiing_Normality",
                             "X35_Q12_1" = "CCSkiing_Athletic",
                             "X35_Q13_1" = "CCSkiing_Competition",
                             "X35_Q14_1" = "CCSkiing_Rules",
                             "X36_Q317_1"= "Bowling_Sport",
                             "X36_Q11_1" = "Bowling_Normality",
                             "X36_Q12_1" = "Bowling_Athletic",
                             "X36_Q13_1" = "Bowling_Competition",
                             "X36_Q14_1" = "Bowling_Rules",
                             "X37_Q317_1"= "Figureskating_Sport",
                             "X37_Q11_1" = "Figureskating_Normality",
                             "X37_Q12_1" = "Figureskating_Athletic",
                             "X37_Q13_1" = "Figureskating_Competition",
                             "X37_Q14_1" = "Figureskating_Rules",
                             "X38_Q317_1"= "Javelinthrowing_Sport",
                             "X38_Q11_1" = "Javelinthrowing_Normality",
                             "X38_Q12_1" = "Javelinthrowing_Athletic",
                             "X38_Q13_1" = "Javelinthrowing_Competition",
                             "X38_Q14_1" = "Javelinthrowing_Rules",
                             "X39_Q317_1"= "Horseracing_Sport",
                             "X39_Q11_1" = "Horseracing_Normality",
                             "X39_Q12_1" = "Horseracing_Athletic",
                             "X39_Q13_1" = "Horseracing_Competition",
                             "X39_Q14_1" = "Horseracing_Rules",
                             "X40_Q317_1"= "Bobsleighing_Sport",
                             "X40_Q11_1" = "Bobsleighing_Normality",
                             "X40_Q12_1" = "Bobsleighing_Athletic",
                             "X40_Q13_1" = "Bobsleighing_Competition",
                             "X40_Q14_1" = "Bobsleighing_Rules",
                             "X41_Q317_1"= "BMXriding_Sport",
                             "X41_Q11_1" = "BMXriding_Normality",
                             "X41_Q12_1" = "BMXriding_Athletic",
                             "X41_Q13_1" = "BMXriding_Competition",
                             "X41_Q14_1" = "BMXriding_Rules",
                             "X42_Q317_1"= "Kickball_Sport",
                             "X42_Q11_1" = "Kickball_Normality",
                             "X42_Q12_1" = "Kickball_Athletic",
                             "X42_Q13_1" = "Kickball_Competition",
                             "X42_Q14_1" = "Kickball_Rules",
                             "X43_Q317_1"= "Racewalking_Sport",
                             "X43_Q11_1" = "Racewalking_Normality",
                             "X43_Q12_1" = "Racewalking_Athletic",
                             "X43_Q13_1" = "Racewalking_Competition",
                             "X43_Q14_1" = "Racewalking_Rules",
                             "X44_Q317_1"= "Archery_Sport",
                             "X44_Q11_1" = "Archery_Normality",
                             "X44_Q12_1" = "Archery_Athletic",
                             "X44_Q13_1" = "Archery_Competition",
                             "X44_Q14_1" = "Archery_Rules",
                             "X45_Q317_1"= "Wrestling_Sport",
                             "X45_Q11_1" = "Wrestling_Normality",
                             "X45_Q12_1" = "Wrestling_Athletic",
                             "X45_Q13_1" = "Wrestling_Competition",
                             "X45_Q14_1" = "Wrestling_Rules",
                             "X46_Q317_1"= "Surfing_Sport",
                             "X46_Q11_1" = "Surfing_Normality",
                             "X46_Q12_1" = "Surfing_Athletic",
                             "X46_Q13_1" = "Surfing_Competition",
                             "X46_Q14_1" = "Surfing_Rules",
                             "X47_Q317_1"= "Croquet_Sport",
                             "X47_Q11_1" = "Croquet_Normality",
                             "X47_Q12_1" = "Croquet_Athletic",
                             "X47_Q13_1" = "Croquet_Competition",
                             "X47_Q14_1" = "Croquet_Rules",
                             "X48_Q317_1"= "Ultimatefrisbee_Sport",
                             "X48_Q11_1" = "Ultimatefrisbee_Normality",
                             "X48_Q12_1" = "Ultimatefrisbee_Athletic",
                             "X48_Q13_1" = "Ultimatefrisbee_Competition",
                             "X48_Q14_1" = "Ultimatefrisbee_Rules",
                             "X49_Q317_1"= "Artisticswimming_Sport",
                             "X49_Q11_1" = "Artisticswimming_Normality",
                             "X49_Q12_1" = "Artisticswimming_Athletic",
                             "X49_Q13_1" = "Artisticswimming_Competition",
                             "X49_Q14_1" = "Artisticswimming_Rules",
                             "X50_Q317_1"= "Kayakracing_Sport",
                             "X50_Q11_1" = "Kayakracing_Normality",
                             "X50_Q12_1" = "Kayakracing_Athletic",
                             "X50_Q13_1" = "Kayakracing_Competition",
                             "X50_Q14_1" = "Kayakracing_Rules",
                             "X51_Q317_1"= "Nascarracing_Sport",
                             "X51_Q11_1" = "Nascarracing_Normality",
                             "X51_Q12_1" = "Nascarracing_Athletic",
                             "X51_Q13_1" = "Nascarracing_Competition",
                             "X51_Q14_1" = "Nascarracing_Rules",
                             "X52_Q317_1"= "Marathon_Sport",
                             "X52_Q11_1" = "Marathon_Normality",
                             "X52_Q12_1" = "Marathon_Athletic",
                             "X52_Q13_1" = "Marathon_Competition",
                             "X52_Q14_1" = "Marathon_Rules",
                             "X53_Q317_1"= "Weightlifting_Sport",
                             "X53_Q11_1" = "Weightlifting_Normality",
                             "X53_Q12_1" = "Weightlifting_Athletic",
                             "X53_Q13_1" = "Weightlifting_Competition",
                             "X53_Q14_1" = "Weightlifting_Rules",
                             "X54_Q317_1"= "Axethrowing_Sport",
                             "X54_Q11_1" = "Axethrowing_Normality",
                             "X54_Q12_1" = "Axethrowing_Athletic",
                             "X54_Q13_1" = "Axethrowing_Competition",
                             "X54_Q14_1" = "Axethrowing_Rules",
                             "X55_Q317_1"= "Dodgeball_Sport",
                             "X55_Q11_1" = "Dodgeball_Normality",
                             "X55_Q12_1" = "Dodgeball_Athletic",
                             "X55_Q13_1" = "Dodgeball_Competition",
                             "X55_Q14_1" = "Dodgeball_Rules",
                             "X56_Q317_1"= "Tugofwar_Sport",
                             "X56_Q11_1" = "Tugofwar_Normality",
                             "X56_Q12_1" = "Tugofwar_Athletic",
                             "X56_Q13_1" = "Tugofwar_Competition",
                             "X56_Q14_1" = "Tugofwar_Rules",
                             "X57_Q317_1"= "Cheerleading_Sport",
                             "X57_Q11_1" = "Cheerleading_Normality",
                             "X57_Q12_1" = "Cheerleading_Athletic",
                             "X57_Q13_1" = "Cheerleading_Competition",
                             "X57_Q14_1" = "Cheerleading_Rules",
                             "X58_Q317_1"= "Parkour_Sport",
                             "X58_Q11_1" = "Parkour_Normality",
                             "X58_Q12_1" = "Parkour_Athletic",
                             "X58_Q13_1" = "Parkour_Competition",
                             "X58_Q14_1" = "Parkour_Rules",
                             "X59_Q317_1"= "Ballet_Sport",
                             "X59_Q11_1" = "Ballet_Normality",
                             "X59_Q12_1" = "Ballet_Athletic",
                             "X59_Q13_1" = "Ballet_Competition",
                             "X59_Q14_1" = "Ballet_Rules",
                             "X60_Q317_1"= "Hiking_Sport",
                             "X60_Q11_1" = "Hiking_Normality",
                             "X60_Q12_1" = "Hiking_Athletic",
                             "X60_Q13_1" = "Hiking_Competition",
                             "X60_Q14_1" = "Hiking_Rules",
                             "X61_Q317_1"= "Chess_Sport",
                             "X61_Q11_1" = "Chess_Normality",
                             "X61_Q12_1" = "Chess_Athletic",
                             "X61_Q13_1" = "Chess_Competition",
                             "X61_Q14_1" = "Chess_Rules",
                             "X62_Q317_1"= "Scubadiving_Sport",
                             "X62_Q11_1" = "Scubadiving_Normality",
                             "X62_Q12_1" = "Scubadiving_Athletic",
                             "X62_Q13_1" = "Scubadiving_Competition",
                             "X62_Q14_1" = "Scubadiving_Rules",
                             "X63_Q317_1"= "Billiards_Sport",
                             "X63_Q11_1" = "Billiards_Normality",
                             "X63_Q12_1" = "Billiards_Athletic",
                             "X63_Q13_1" = "Billiards_Competition",
                             "X63_Q14_1" = "Billiards_Rules",
                             "X64_Q317_1"= "Stretching_Sport",
                             "X64_Q11_1" = "Stretching_Normality",
                             "X64_Q12_1" = "Stretching_Athletic",
                             "X64_Q13_1" = "Stretching_Competition",
                             "X64_Q14_1" = "Stretching_Rules",
                             "X65_Q317_1"= "Sailing_Sport",
                             "X65_Q11_1" = "Sailing_Normality",
                             "X65_Q12_1" = "Sailing_Athletic",
                             "X65_Q13_1" = "Sailing_Competition",
                             "X65_Q14_1" = "Sailing_Rules",
                             "X66_Q317_1"= "Yoga_Sport",
                             "X66_Q11_1" = "Yoga_Normality",
                             "X66_Q12_1" = "Yoga_Athletic",
                             "X66_Q13_1" = "Yoga_Competition",
                             "X66_Q14_1" = "Yoga_Rules",
                             "X67_Q317_1"= "Playingcatch_Sport",
                             "X67_Q11_1" = "Playingcatch_Normality",
                             "X67_Q12_1" = "Playingcatch_Athletic",
                             "X67_Q13_1" = "Playingcatch_Competition",
                             "X67_Q14_1" = "Playingcatch_Rules",
                             "X68_Q317_1"= "Poker_Sport",
                             "X68_Q11_1" = "Poker_Normality",
                             "X68_Q12_1" = "Poker_Athletic",
                             "X68_Q13_1" = "Poker_Competition",
                             "X68_Q14_1" = "Poker_Rules",
                             "X69_Q317_1"= "Marchingband_Sport",
                             "X69_Q11_1" = "Marchingband_Normality",
                             "X69_Q12_1" = "Marchingband_Athletic",
                             "X69_Q13_1" = "Marchingband_Competition",
                             "X69_Q14_1" = "Marchingband_Rules",
                             "X70_Q317_1"= "Beerpong_Sport",
                             "X70_Q11_1" = "Beerpong_Normality",
                             "X70_Q12_1" = "Beerpong_Athletic",
                             "X70_Q13_1" = "Beerpong_Competition",
                             "X70_Q14_1" = "Beerpong_Rules",
                             "X71_Q317_1"= "Videogame_Sport",
                             "X71_Q11_1" = "Videogame_Normality",
                             "X71_Q12_1" = "Videogame_Athletic",
                             "X71_Q13_1" = "Videogame_Competition",
                             "X71_Q14_1" = "Videogame_Rules",
                             "X72_Q317_1"= "Darts_Sport",
                             "X72_Q11_1" = "Darts_Normality",
                             "X72_Q12_1" = "Darts_Athletic",
                             "X72_Q13_1" = "Darts_Competition",
                             "X72_Q14_1" = "Darts_Rules",
                             "X73_Q317_1"= "Paddleboarding_Sport",
                             "X73_Q11_1" = "Paddleboarding_Normality",
                             "X73_Q12_1" = "Paddleboarding_Athletic",
                             "X73_Q13_1" = "Paddleboarding_Competition",
                             "X73_Q14_1" = "Paddleboarding_Rules",
                             "X74_Q317_1"= "Eating_Sport",
                             "X74_Q11_1" = "Eating_Normality",
                             "X74_Q12_1" = "Eating_Athletic",
                             "X74_Q13_1" = "Eating_Competition",
                             "X74_Q14_1" = "Eating_Rules",
                             "X75_Q317_1"= "Spelling_Sport",
                             "X75_Q11_1" = "Spelling_Normality",
                             "X75_Q12_1" = "Spelling_Athletic",
                             "X75_Q13_1" = "Spelling_Competition",
                             "X75_Q14_1" = "Spelling_Rules",
                             "X76_Q317_1"= "Tag_Sport",
                             "X76_Q11_1" = "Tag_Normality",
                             "X76_Q12_1" = "Tag_Athletic",
                             "X76_Q13_1" = "Tag_Competition",
                             "X76_Q14_1" = "Tag_Rules",
                             "X77_Q317_1"= "Nerfgunfight_Sport",
                             "X77_Q11_1" = "Nerfgunfight_Normality",
                             "X77_Q12_1" = "Nerfgunfight_Athletic",
                             "X77_Q13_1" = "Nerfgunfight_Competition",
                             "X77_Q14_1" = "Nerfgunfight_Rules",
                             "X78_Q317_1"= "Hideandseek_Sport",
                             "X78_Q11_1" = "Hideandseek_Normality",
                             "X78_Q12_1" = "Hideandseek_Athletic",
                             "X78_Q13_1" = "Hideandseek_Competition",
                             "X78_Q14_1" = "Hideandseek_Rules",
                             "X79_Q317_1"= "Walkingdog_Sport",
                             "X79_Q11_1" = "Walkingdog_Normality",
                             "X79_Q12_1" = "Walkingdog_Athletic",
                             "X79_Q13_1" = "Walkingdog_Competition",
                             "X79_Q14_1" = "Walkingdog_Rules",
                             "X80_Q317_1"= "Math_Sport",
                             "X80_Q11_1" = "Math_Normality",
                             "X80_Q12_1" = "Math_Athletic",
                             "X80_Q13_1" = "Math_Competition",
                             "X80_Q14_1" = "Math_Rules",
                             "X81_Q317_1"= "Sewing_Sport",
                             "X81_Q11_1" = "Sewing_Normality",
                             "X81_Q12_1" = "Sewing_Athletic",
                             "X81_Q13_1" = "Sewing_Competition",
                             "X81_Q14_1" = "Sewing_Rules",
                             "X82_Q317_1"= "Stampcollecting_Sport",
                             "X82_Q11_1" = "Stampcollecting_Normality",
                             "X82_Q12_1" = "Stampcollecting_Athletic",
                             "X82_Q13_1" = "Stampcollecting_Competition",
                             "X82_Q14_1" = "Stampcollecting_Rules",
                             "X83_Q317_1"= "Shoplifting_Sport",
                             "X83_Q11_1" = "Shoplifting_Normality",
                             "X83_Q12_1" = "Shoplifting_Athletic",
                             "X83_Q13_1" = "Shoplifting_Competition",
                             "X83_Q14_1" = "Shoplifting_Rules",
                             "X84_Q317_1"= "Birdwatching_Sport",
                             "X84_Q11_1" = "Birdwatching_Normality",
                             "X84_Q12_1" = "Birdwatching_Athletic",
                             "X84_Q13_1" = "Birdwatching_Competition",
                             "X84_Q14_1" = "Birdwatching_Rules",
                             "X85_Q317_1"= "Acting_Sport",
                             "X85_Q11_1" = "Acting_Normality",
                             "X85_Q12_1" = "Acting_Athletic",
                             "X85_Q13_1" = "Acting_Competition",
                             "X85_Q14_1" = "Acting_Rules",
                             "X86_Q317_1"= "Writingpaper_Sport",
                             "X86_Q11_1" = "Writingpaper_Normality",
                             "X86_Q12_1" = "Writingpaper_Athletic",
                             "X86_Q13_1" = "Writingpaper_Competition",
                             "X86_Q14_1" = "Writingpaper_Rules",
                             "X87_Q317_1"= "Cooking_Sport",
                             "X87_Q11_1" = "Cooking_Normality",
                             "X87_Q12_1" = "Cooking_Athletic",
                             "X87_Q13_1" = "Cooking_Competition",
                             "X87_Q14_1" = "Cooking_Rules",
                             "X88_Q317_1"= "Knifefighting_Sport",
                             "X88_Q11_1" = "Knifefighting_Normality",
                             "X88_Q12_1" = "Knifefighting_Athletic",
                             "X88_Q13_1" = "Knifefighting_Competition",
                             "X88_Q14_1" = "Knifefighting_Rules",
                             "X89_Q317_1"= "Hopscotch_Sport",
                             "X89_Q11_1" = "Hopscotch_Normality",
                             "X89_Q12_1" = "Hopscotch_Athletic",
                             "X89_Q13_1" = "Hopscotch_Competition",
                             "X89_Q14_1" = "Hopscotch_Rules",
                             "X90_Q317_1"= "Flyingkite_Sport",
                             "X90_Q11_1" = "Flyingkite_Normality",
                             "X90_Q12_1" = "Flyingkite_Athletic",
                             "X90_Q13_1" = "Flyingkite_Competition",
                             "X90_Q14_1" = "Flyingkite_Rules",
                             "X91_Q317_1"= "Woodworking_Sport",
                             "X91_Q11_1" = "Woodworking_Normality",
                             "X91_Q12_1" = "Woodworking_Athletic",
                             "X91_Q13_1" = "Woodworking_Competition",
                             "X91_Q14_1" = "Woodworking_Rules",
                             "X92_Q317_1"= "Beautypageant_Sport",
                             "X92_Q11_1" = "Beautypageant_Normality",
                             "X92_Q12_1" = "Beautypageant_Athletic",
                             "X92_Q13_1" = "Beautypageant_Competition",
                             "X92_Q14_1" = "Beautypageant_Rules",
                             "X93_Q317_1"= "Hackeysack_Sport",
                             "X93_Q11_1" = "Hackeysack_Normality",
                             "X93_Q12_1" = "Hackeysack_Athletic",
                             "X93_Q13_1" = "Hackeysack_Competition",
                             "X93_Q14_1" = "Hackeysack_Rules",
                             "X94_Q317_1"= "Treeclimbing_Sport",
                             "X94_Q11_1" = "Treeclimbing_Normality",
                             "X94_Q12_1" = "Treeclimbing_Athletic",
                             "X94_Q13_1" = "Treeclimbing_Competition",
                             "X94_Q14_1" = "Treeclimbing_Rules",
                             "X95_Q317_1"= "Battleofthebands_Sport",
                             "X95_Q11_1" = "Battleofthebands_Normality",
                             "X95_Q12_1" = "Battleofthebands_Athletic",
                             "X95_Q13_1" = "Battleofthebands_Competition",
                             "X95_Q14_1" = "Battleofthebands_Rules",
                             "X96_Q317_1"= "Roadtrip_Sport",
                             "X96_Q11_1" = "Roadtrip_Normality",
                             "X96_Q12_1" = "Roadtrip_Athletic",
                             "X96_Q13_1" = "Roadtrip_Competition",
                             "X96_Q14_1" = "Roadtrip_Rules",
                             "X97_Q317_1"= "Jeopardy_Sport",
                             "X97_Q11_1" = "Jeopardy_Normality",
                             "X97_Q12_1" = "Jeopardy_Athletic",
                             "X97_Q13_1" = "Jeopardy_Competition",
                             "X97_Q14_1" = "Jeopardy_Rules",
                             "X98_Q317_1"= "Lecturing_Sport",
                             "X98_Q11_1" = "Lecturing_Normality",
                             "X98_Q12_1" = "Lecturing_Athletic",
                             "X98_Q13_1" = "Lecturing_Competition",
                             "X98_Q14_1" = "Lecturing_Rules",
                             "X99_Q317_1"= "Makingsandcastle_Sport",
                             "X99_Q11_1" = "Makingsandcastle_Normality",
                             "X99_Q12_1" = "Makingsandcastle_Athletic",
                             "X99_Q13_1" = "Makingsandcastle_Competition",
                             "X99_Q14_1" = "Makingsandcastle_Rules",
                             "X100_Q317_1"="Swingingonswingset_Sport",
                             "X100_Q11_1"= "Swingingonswingset_Normality",
                             "X100_Q12_1"= "Swingingonswingset_Athletic",
                             "X100_Q13_1"= "Swingingonswingset_Competition",
                             "X100_Q14_1"= "Swingingonswingset_Rules",
                             "X101_Q317_1"="Sheepherding_Sport",
                             "X101_Q11_1"= "Sheepherding_Normality",
                             "X101_Q12_1"= "Sheepherding_Athletic",
                             "X101_Q13_1"= "Sheepherding_Competition",
                             "X101_Q14_1"= "Sheepherding_Rules",
                             "X102_Q317_1"="Civilwarreenact_Sport",
                             "X102_Q11_1"= "Civilwarreenact_Normality",
                             "X102_Q12_1"= "Civilwarreenact_Athletic",
                             "X102_Q13_1"= "Civilwarreenact_Competition",
                             "X102_Q14_1"= "Civilwarreenact_Rules",
                             "X103_Q317_1"="Fightingwar_Sport",
                             "X103_Q11_1"= "Fightingwar_Normality",
                             "X103_Q12_1"= "Fightingwar_Athletic",
                             "X103_Q13_1"= "Fightingwar_Competition",
                             "X103_Q14_1"= "Fightingwar_Rules",
                             "X104_Q317_1"="Takingouttrash_Sport",
                             "X104_Q11_1"= "Takingouttrash_Normality",
                             "X104_Q12_1"= "Takingouttrash_Athletic",
                             "X104_Q13_1"= "Takingouttrash_Competition",
                             "X104_Q14_1"= "Takingouttrash_Rules",
                             "X105_Q317_1"="Drivingcar_Sport",
                             "X105_Q11_1"= "Drivingcar_Normality",
                             "X105_Q12_1"= "Drivingcar_Athletic",
                             "X105_Q13_1"= "Drivingcar_Competition",
                             "X105_Q14_1" ="Drivingcar_Rules")          


## separate out activity types and measures

d.1 = d.1 %>% separate(activity, c("Activity", "Measure"))

# overall demographics

gender = prop.table(table(d.1$Q32))
d.1$Q56_1 = as.numeric(as.character(d.1$Q56_1))
age = summarySE(d.1, measurevar = "Q56_1", na.rm = TRUE)


#summary

d.1 = spread(d.1, Measure, rating)

d.1$Sport <- as.numeric(d.1$Sport, na.rm = TRUE)
d.1$Athletic <- as.numeric(d.1$Athletic, na.rm = TRUE)
d.1$Competition <- as.numeric(d.1$Competition, na.rm = TRUE)
d.1$Normality <- as.numeric(d.1$Normality, na.rm = TRUE)
d.1$Rules <- as.numeric(d.1$Rules, na.rm = TRUE)



# linear mixed effect model


model.normality = lmer(Sport ~ Normality + (1|Activity), data = d.1)
summary(model.normality)

model.rules = lmer(Sport ~ Rules + (1|Activity), data = d.1)
summary(model.rules)

model.full = lmer(Sport ~ Normality + Rules + Competition + Athletic + (1|Activity), data = d.1)
summary(model.full)


lme.dscore(model.full, d.1, type = "lme4")

is.factor(d.1$Q32)

d.1$Q32 <- as.factor(d.1$Q32)

gender.sport <- lmer(Sport ~ Q32 + (1|Activity), data = d.1)

summary(gender.sport)




# transforming d.1 for simple linear regressions


d.2 <- d.1 %>%
  select(Activity, Athletic, Competition, Normality, Rules, Sport) %>%
  group_by(Activity)%>%
  summarise_if(is.numeric, mean, na.rm = TRUE)



#linear regressions corrected

m1 = lm(Sport ~ Normality, d.2)
summary(aov(m1))

m2 = lm(Sport ~ Rules, d.2)
summary(aov(m2))

m3 = lm(Sport ~ Competition, d.2)
summary(aov(m3))

m4 = lm(Sport ~ Athletic, d.2)
summary(aov(m4))


plot(d.2$Sport, d.2$Normality)

plot(d.2$Sport, d.2$Rules)

plot(d.2$Sport, d.2$Competition)

plot(d.2$Sport, d.2$Athletic)

summary(m1)
summary(m2)
summary(m3)
summary(m4)


multi <- lm(Sport ~ Normality + Rules + Competition + Athletic, data = d.2) 
summary(aov(multi))


summary(multi)


#Boxplots

library(data.table)
d.2_long <- melt(d.2, measure.vars = c("Athletic", "Competition", "Normality","Rules"), variable.name = "Measures")


sum.meas = summarySE(d.2_long, measurevar = "value", groupvars = "Measures")

sum.meas.sport = summarySE(d.2_long, measurevar = "Sport", na.rm = T)

sum.meas <- sum.meas %>%
  append(sum.meas.sport)
rm(sum.meas.sport)


sum.meas <- as.data.frame(sum.meas)


d.2_long %>%
  ggplot(aes(Measures, value)) +
  geom_boxplot()



# 4 scatterplots 

d.2_long %>%
  ggplot(aes(value, Sport)) +
  geom_point()+
  facet_wrap(~ Measures)+
  geom_point(data = sum.meas, aes(value, Sport), color = "red") +
  geom_errorbar(data = sum.meas, aes(xmin = value - ci, xmax = value + ci)) 
