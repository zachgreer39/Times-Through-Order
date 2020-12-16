##install.packages("tidyverse");
library(tidyverse);
##install.packages("DBI"):
library(DBI);
##install.packages("purrr");
library(purrr);
##install.packages("cluster");
library(cluster);
##install.packages("ggpubr");
library(ggpubr);
##install.packages("data.table");
library(data.table)
##install.packages("scales");
library(scales)
##install.packages("corrplot")
library(corrplot)
##install.packages("reshape2");
library(reshape2)
##install.packages("ggforce");
library(ggforce)

##Connection to AWS Database
con=dbConnect(RMySQL::MySQL(), dbname="zgreer_baseball",
              host="zgreer-baseball.cqridrsjfpub.us-east-2.rds.amazonaws.com",
              port=3306, user="guest", password="Password1!");
##dbListTables(con);
##dbDisconnect(con);

##Reading in tables
tbl_gamelogs=tbl(con, "clustered_gamelogs");
gamelog_df=collect(tbl_gamelogs %>%
                     select(-c(join_team, wOBA1, wOBA2, wOBA3,
                               wOBA4, cluster))) %>%
  readr::type_convert();
tbl_pitchdf=tbl(con, "pitch_df");
pitch_df=collect(tbl_pitchdf) %>%
  readr::type_convert();
tbl_plate_appearances=tbl(con, "plate_appearances");
plate_appearances=collect(tbl_plate_appearances %>%
                            select(-c(start, batPos, count,
                                      pitches, play))) %>%
  readr::type_convert();
rm(tbl_gamelogs, tbl_pitchdf, tbl_plate_appearances);


##Adding additional details to pitch dataframe
balls=c("ball", "blocked_ball", "hit_by_pitch", "pitchout");
not_swings=c(balls, "called_strike");
swings=setdiff(unique(pitch_df$description), not_swings);
fastballs=c("4-Seam Fastball","2-Seam Fastball",
            "Cutter","Split-Finger","Sinker");
pitch_df=pitch_df %>%
  mutate(Ball=ifelse(description %in% balls, 1, 0),
         Strikes=ifelse(!(description %in% balls), 1, 0),
         Swings=ifelse(description %in% swings, 1, 0),
         Whiff=ifelse(description %in% c("swinging_strike",
                                         "swinging_strike_blocked",
                                         "missed_bunt"), 1, 0),
         PitchType=ifelse(pitch_name %in% fastballs, "Fastball",
                          "Offspeed")) %>%
  group_by(game_pk, pitcher) %>%
  mutate(pitch=order(at_bat_number, pitch_number)) %>%
  inner_join(plate_appearances %>%
               select(game_pk, at_bat_number, TTO, events) %>%
               distinct()) %>% drop_na() %>%
  filter(!(pitch_name %in% c("null", "Pitch Out")));
rm(balls, not_swings, swings, fastballs);


##Metrics by pitch
pitches=pitch_df %>% group_by(PitchType, pitch_name) %>%
  summarise(Count=n(),Balls=sum(Ball),Strikes=sum(Strikes),
            Swings=sum(Swings), Whiffs=sum(Whiff), 
            avg_velo=round(mean(velocity, na.rm=TRUE),2),max_velo=max(velocity), 
            avg_x_movement=round(mean(x_movement, na.rm=TRUE),2),
            avg_y_movement=round(mean(y_movement, na.rm=TRUE),2),
            avg_spinrate=round(mean(spinrate, na.rm=TRUE),2)) %>%
  left_join(pitch_df %>% filter(hit_rating!="null") %>%
              group_by(pitch_name) %>% summarise(Hits=n())) %>%
  left_join(pitch_df %>% filter(hit_rating %in% c(5,6)) %>%
              group_by(pitch_name) %>% summarise(HardHits=n())) %>%
  mutate(HardHits=ifelse(is.na(HardHits),0,HardHits), 
         Hits=ifelse(is.na(Hits),0.001,Hits),
         HardHitPercentage=round(HardHits/Hits,2)) %>%
  transmute(PitchType,pitch_name,Count,StrikePercentage=round(Strikes/Count,4), 
            WhiffsperSwing=round(Whiffs/Swings,4), avg_velo, max_velo, 
            avg_x_movement,avg_y_movement,avg_spinrate,HardHitPercentage) %>% 
  arrange(desc(Count));
##Attaching an ID column
pitchid=seq(1:nrow(pitches));
pitches=cbind(pitchid=pitchid, pitches);
rm(pitchid);


##Joining pitch_id to pitch_df
pitch_df=pitch_df %>%
  left_join(pitches %>% select(pitchid, pitch_name));


##Calculating Pitch Metrics for different sample sizes
##Metrics at the at bat level
at_bat_pitches=pitch_df %>%
  group_by(game_pk, pitcher, inning, TTO, at_bat_number,
           PitchType, pitchid, pitch_name, events)  %>%
  summarise(Count=n(), Balls=sum(Ball), Strikes=sum(Strikes),
            Swings=sum(Swings), Whiffs=sum(Whiff),
            avg_velo=round(mean(velocity, na.rm=TRUE),2),max_velo=max(velocity),
            avg_x_movement=round(mean(x_movement, na.rm=TRUE),2),
            avg_y_movement=round(mean(y_movement, na.rm=TRUE),2),
            avg_spinrate=round(mean(spinrate, na.rm=TRUE),2));


##Metrics at the TTO level
TTO_pitches=pitch_df %>%
  group_by(game_pk, pitcher, TTO,
           PitchType, pitchid, pitch_name) %>%
  summarise(Count=n(), Balls=sum(Ball), Strikes=sum(Strikes),
            Swings=sum(Swings), Whiffs=sum(Whiff),
            avg_velo=round(mean(velocity, na.rm=TRUE),2),
            max_velo=max(velocity),
            avg_x_movement=round(mean(x_movement, na.rm=TRUE),2),
            avg_y_movement=round(mean(y_movement, na.rm=TRUE),2),
            avg_spinrate=round(mean(spinrate, na.rm=TRUE),2)) %>%
  left_join(pitch_df %>% filter(hit_rating!="null") %>%
              group_by(game_pk, pitcher, TTO, pitch_name) %>%
              summarise(Hits=n())) %>%
  left_join(pitch_df %>% filter(hit_rating %in% c(5,6)) %>%
              group_by(game_pk, pitcher, TTO, pitch_name) %>%
              summarise(HardHits=n())) %>%
  mutate(HardHits=ifelse(is.na(HardHits),0,HardHits),
         Hits=ifelse(is.na(Hits),0.001,Hits),
         HardHitPercentage=round(HardHits/Hits,4)) %>%
  transmute(game_pk, pitcher, TTO, PitchType, pitchid, pitch_name, Count,
            StrikePercentage=round(Strikes/Count,4), 
            WhiffsperSwing=ifelse(Swings==0,0,round(Whiffs/Swings,4)), 
            avg_velo, max_velo,  avg_x_movement, avg_y_movement, 
            avg_spinrate, HardHitPercentage);


##Metrics at sample level
pitcher_pitches=pitch_df %>%
  group_by(pitcher, PitchType, pitchid, pitch_name) %>%
  summarise(Count=n(), Balls=sum(Ball), Strikes=sum(Strikes),
            Swings=sum(Swings), Whiffs=sum(Whiff),
            avg_velo=round(mean(velocity, na.rm=TRUE),2),
            max_velo=max(velocity),
            avg_x_movement=round(mean(x_movement, na.rm=TRUE),2),
            avg_y_movement=round(mean(y_movement, na.rm=TRUE),2),
            avg_spinrate=round(mean(spinrate, na.rm=TRUE),2)) %>%
  left_join(pitch_df %>% filter(hit_rating!="null") %>%
              group_by(pitcher, pitch_name) %>%
              summarise(Hits=n())) %>%
  left_join(pitch_df %>% filter(hit_rating %in% c(5,6)) %>%
              group_by(pitcher, pitch_name) %>%
              summarise(HardHits=n())) %>%
  mutate(HardHits=ifelse(is.na(HardHits),0,HardHits),
         Hits=ifelse(is.na(Hits),0.001,Hits),
         HardHitPercentage=round(HardHits/Hits,2)) %>%
  transmute(pitcher, PitchType, pitchid, pitch_name, Count,
            StrikePercentage=round(Strikes/Count,4), 
            WhiffsperSwing=ifelse(Swings==0,0,round(Whiffs/Swings,4)), 
            avg_velo, max_velo,  avg_x_movement, avg_y_movement, 
            avg_spinrate, HardHitPercentage);


##Pulling the top 2 pitches per pitcher 
primary_pitches=pitcher_pitches %>% group_by(pitcher) %>%
  top_n(n=2, wt=Count);
##Pulling two pitches from pitches that had 3
top_2=primary_pitches %>%
  semi_join(primary_pitches %>% group_by(pitcher) %>%
              count() %>% filter(n>2)) %>%
  group_by(pitcher) %>% top_n(n=2, wt=StrikePercentage) %>%
  top_n(n=2, wt=avg_velo);
##Removing that third pitch (via anti_join) and binding back together
primary_pitches=rbind(primary_pitches %>%
                        anti_join(top_2, by=c("pitcher")), top_2);
rm(top_2);


##Distinguising between primary and secondary pitch
primary_pitches=rbind(primary_pitches %>% top_n(n=1, wt=Count) %>%
                        top_n(n=1, wt=StrikePercentage) %>%
                        top_n(n=1, wt=WhiffsperSwing) %>%
                        top_n(n=1, wt=avg_velo) %>%
                        mutate(Order="Primary"),
                      primary_pitches %>%
                        anti_join(primary_pitches %>%
                                    top_n(n=1, wt=Count) %>%
                                    top_n(n=1, wt=StrikePercentage) %>%
                                    top_n(n=1, wt=WhiffsperSwing) %>%
                                    top_n(n=1, wt=avg_velo)) %>%
                        mutate(Order="Secondary"));


##Calculating strike percentage and primary pitch usage
##for the first pitch and the atbat
TTO_pitchusage=pitch_df %>%
  group_by(game_pk, pitcher, TTO) %>%
  filter(pitch_number==1) %>%
  left_join(primary_pitches %>%
              select(pitcher, pitch_name, Order)) %>%
  mutate(primary=ifelse(is.na(Order),0,1)) %>%
  summarise(at_bats=n(), FirstPitchPrimary=round(sum(primary)/n(),2),
            FirstPitchStrikePercentage=round(sum(Strikes)/n(),2)) %>%
  left_join(pitch_df %>% group_by(game_pk, pitcher, TTO) %>% 
  left_join(primary_pitches %>%
              select(pitcher, pitch_name, Order)) %>%
  mutate(primary=ifelse(is.na(Order),0,1)) %>% 
  summarise(pitches=n(), PrimaryPitchUsage=round(sum(primary)/n(),2),
            StrikePercentage=round(sum(Strikes)/n(),2))) %>%
  transmute(game_pk, pitcher, TTO, pitchesperab=round(pitches/at_bats,2), 
            PrimaryPitchUsage, StrikePercentage, FirstPitchPrimary, 
            FirstPitchStrikePercentage);


##First Strike Percentage for pitchers in sample
pitcher_pitchusage=pitch_df %>%
  group_by(pitcher) %>%
  filter(pitch_number==1) %>%
  left_join(primary_pitches %>%
              select(pitcher, pitch_name, Order)) %>%
  mutate(primary=ifelse(is.na(Order),0,1)) %>%
  summarise(at_bats=n(), FirstPitchPrimary=round(sum(primary)/n(),2),
            FirstPitchStrikePercentage=round(sum(Strikes)/n(),2)) %>%
  left_join(pitch_df %>% group_by(pitcher) %>% 
              left_join(primary_pitches %>%
                          select(pitcher, pitch_name, Order)) %>%
              mutate(primary=ifelse(is.na(Order),0,1)) %>% 
              summarise(pitches=n(), PrimaryPitchUsage=round(sum(primary)/n(),2),
                        StrikePercentage=round(sum(Strikes)/n(),2))) %>%
  transmute(pitcher, pitchesperab=round(pitches/at_bats,2), PrimaryPitchUsage, 
            StrikePercentage, FirstPitchPrimary, FirstPitchStrikePercentage);


##Adding TTO First Pitch Metrics to Gamelog DF
gamelog_staging=gamelog_df %>%
  mutate(wOBAg=round(wOBAg,3), StrikePercent=round(StrikePercent,2),
         WhiffsperSwing=round(WhiffsperSwing,2)) %>%
  inner_join(TTO_pitchusage %>% filter(TTO==1) %>% select(-TTO)) %>%
  inner_join(TTO_pitchusage %>% filter(TTO==2) %>% select(-TTO),
             by=c("game_pk", "pitcher"), suffix=c("_first", "_second")) %>% 
  inner_join(pitcher_pitchusage) %>%
  transmute(game_pk, pitcher, WHIP, HRper9, Kper9, BBper9, wOBA=wOBAg, BABIP,
            PitchesAB_TTO1_Diff=round((
              pitchesperab_first-pitchesperab)/pitchesperab,4), 
            PitchesAB_TTO2_Gap=round((
              pitchesperab_second-pitchesperab_first)/pitchesperab_first,4), 
            PrimaryUsage_TTO1_Diff=round((
              PrimaryPitchUsage_first-PrimaryPitchUsage)/PrimaryPitchUsage,4), 
            PrimaryUsage_TTO2_Gap=round((
              PrimaryPitchUsage_second-PrimaryPitchUsage_first)/
                PrimaryPitchUsage_first,4), 
            StrikePercentage_TTO1_Diff=round((
              StrikePercentage_first-StrikePercentage)/StrikePercentage,4), 
            StrikePercentage_TTO2_Gap=round((
              StrikePercentage_second-StrikePercentage_first)/
                StrikePercentage_first,4),
            FirstPitchPrimary_TTO1_Diff=round((
              FirstPitchPrimary_first-FirstPitchPrimary)/FirstPitchPrimary,4), 
            FirstPitchPrimary_TTO2_Gap=round((
              FirstPitchPrimary_second-FirstPitchPrimary_first)/
                FirstPitchPrimary_first,4),
            FirstPitchStrikePercentage_TTO1_Diff=round((
              FirstPitchStrikePercentage_first-FirstPitchStrikePercentage)/
                FirstPitchStrikePercentage,4), 
            FirstPitchStrikePercentage_TTO2_Gap=round((
              FirstPitchStrikePercentage_second-FirstPitchStrikePercentage_first)/
                FirstPitchStrikePercentage_first,4));


##Adding Pitch Metrics to staging dataframe
gamelog_stagingOG=gamelog_staging %>% 
  inner_join(primary_pitches %>% filter(Order=="Primary") %>%
               inner_join(pitcher_pitches) %>% select(1,3,8:12) %>% 
               inner_join(primary_pitches %>% filter(Order=="Secondary") %>%
                            inner_join(pitcher_pitches) %>% select(1,3,8:12), 
                          by=c("pitcher"), 
                          suffix=c("_primary", "_secondary")), by=c("pitcher")) %>% 
  inner_join(TTO_pitches %>% filter(TTO==1) %>% select(1:2,5,9:15), 
             by=c("game_pk", "pitcher", "pitchid_primary"="pitchid")) %>%
  left_join(TTO_pitches %>% filter(TTO==1) %>% select(1:2,5,9:15), 
            by=c("game_pk", "pitcher", "pitchid_secondary"="pitchid"), 
            suffix=c("_primaryTTO1", "_secondaryTTO1")) %>%
  inner_join(TTO_pitches %>% filter(TTO==2) %>% select(1:2,5,9:15), 
             by=c("game_pk", "pitcher", "pitchid_primary"="pitchid")) %>%
  left_join(TTO_pitches %>% filter(TTO==2) %>% select(1:2,5,9:15), 
            by=c("game_pk", "pitcher", "pitchid_secondary"="pitchid"), 
            suffix=c("_primaryTTO2", "_secondaryTTO2")) %>%
  mutate(avg_velo_primary_TTO1_Diff=(avg_velo_primaryTTO1-
                                       avg_velo_primary)/avg_velo_primary, 
         max_velo_primary_TTO1_Diff=(max_velo_primaryTTO1-
                                       max_velo_primary)/max_velo_primary,
         avg_x_movement_primary_TTO1_Diff=(avg_x_movement_primaryTTO1-
                                             avg_x_movement_primary)/
           avg_x_movement_primary, 
         avg_y_movement_primary_TTO1_Diff=(avg_y_movement_primaryTTO1-
                                             avg_y_movement_primary)/
           avg_y_movement_primary, 
         avg_spinrate_primary_TTO1_Diff=(avg_spinrate_primaryTTO1-
                                           avg_spinrate_primary)/
           avg_spinrate_primary,
         avg_velo_secondary_TTO1_Diff=(avg_velo_secondaryTTO1-
                                         avg_velo_secondary)/avg_velo_secondary, 
         max_velo_secondary_TTO1_Diff=(max_velo_secondaryTTO1-
                                         max_velo_secondary)/max_velo_secondary,
         avg_x_movement_secondary_TTO1_Diff=(avg_x_movement_secondaryTTO1-
                                               avg_x_movement_secondary)/
           avg_x_movement_secondary, 
         avg_y_movement_secondary_TTO1_Diff=(avg_y_movement_secondaryTTO1-
                                               avg_y_movement_secondary)/
           avg_y_movement_secondary, 
         avg_spinrate_secondary_TTO1_Diff=(avg_spinrate_secondaryTTO1-
                                             avg_spinrate_secondary)/
           avg_spinrate_secondary,
         avg_velo_primary_TTO2_Gap=(avg_velo_primaryTTO2-
                                      avg_velo_primaryTTO1)/
           avg_velo_primaryTTO1, 
         max_velo_primary_TTO2_Gap=(max_velo_primaryTTO2-
                                      max_velo_primaryTTO1)/
           max_velo_primaryTTO1,
         avg_x_movement_primary_TTO2_Gap=(avg_x_movement_primaryTTO2-
                                            avg_x_movement_primaryTTO1)/
           avg_x_movement_primaryTTO1, 
         avg_y_movement_primary_TTO2_Gap=(avg_y_movement_primaryTTO2-
                                            avg_y_movement_primaryTTO1)/
           avg_y_movement_primaryTTO1, 
         avg_spinrate_primary_TTO2_Gap=(avg_spinrate_primaryTTO2-
                                          avg_spinrate_primaryTTO1)/
           avg_spinrate_primaryTTO1,
         avg_velo_secondary_TTO2_Gap=(avg_velo_secondaryTTO2-
                                        avg_velo_secondaryTTO1)/
           avg_velo_secondaryTTO1, 
         max_velo_secondary_TTO2_Gap=(max_velo_secondaryTTO2-
                                        max_velo_secondaryTTO1)/
           max_velo_secondaryTTO1,
         avg_x_movement_secondary_TTO2_Gap=(avg_x_movement_secondaryTTO2-
                                              avg_x_movement_secondaryTTO1)/
           avg_x_movement_secondaryTTO1, 
         avg_y_movement_secondary_TTO2_Gap=(avg_y_movement_secondaryTTO2-
                                              avg_y_movement_secondaryTTO1)/
           avg_y_movement_secondaryTTO1, 
         avg_spinrate_secondary_TTO2_Gap=(avg_spinrate_secondaryTTO2-
                                            avg_spinrate_secondaryTTO1)/
           avg_spinrate_secondaryTTO1) %>% drop_na() %>% ungroup() %>%
  mutate(FirstPitchPrimary_TTO2_Gap=
           ifelse(FirstPitchPrimary_TTO2_Gap=="Inf",5,
                  FirstPitchPrimary_TTO2_Gap),
         FirstPitchStrikePercentage_TTO2_Gap=
           ifelse(FirstPitchStrikePercentage_TTO2_Gap=="Inf",7,
                  FirstPitchStrikePercentage_TTO2_Gap),
         avg_x_movement_primary_TTO1_Diff=
           ifelse(avg_x_movement_primary_TTO1_Diff=="-Inf",-15,
                  avg_x_movement_primary_TTO1_Diff),
         avg_x_movement_secondary_TTO1_Diff=
           ifelse(avg_x_movement_secondary_TTO1_Diff=="-Inf",-15,
                  avg_x_movement_secondary_TTO1_Diff),
         avg_x_movement_primary_TTO2_Gap=
           ifelse(avg_x_movement_primary_TTO2_Gap=="-Inf",-10,
                  ifelse(avg_x_movement_primary_TTO2_Gap=="Inf",10,
                         avg_x_movement_primary_TTO2_Gap)),
         avg_y_movement_primary_TTO2_Gap=
           ifelse(avg_y_movement_primary_TTO2_Gap=="-Inf",-5,
                  avg_y_movement_primary_TTO2_Gap),
         avg_x_movement_secondary_TTO2_Gap=
           ifelse(avg_x_movement_secondary_TTO2_Gap=="-Inf",-10,
                  ifelse(avg_x_movement_secondary_TTO2_Gap=="Inf",10,
                         avg_x_movement_secondary_TTO2_Gap)),
         avg_y_movement_secondary_TTO2_Gap=
           ifelse(avg_y_movement_secondary_TTO2_Gap=="-Inf",-5,
                  avg_y_movement_secondary_TTO2_Gap)) %>%
  select(1:30,33,42,51,60,67:86);


##Creating a scalable dataframe for clustering
reduced_df=scale(gamelog_staging %>% select(-c("game_pk", "pitcher")));

tot_withinss=map_dbl(1:20, function(k) {
  model=kmeans(x=reduced_df, centers=k)
  model$tot.withinss});
elbow_df=data.frame(k=1:20, tot_withinss=tot_withinss);
ggplot(elbow_df, aes(x=k, y=tot_withinss)) + geom_line() + 
  scale_x_continuous(breaks=1:20);
sil_width=map_dbl(2:20, function(k){
  model=pam(x=reduced_df, k=k)
  model$silinfo$avg.width});
sil_df=data.frame(k=2:20, sil_width=sil_width);
ggplot(sil_df, aes(x=k, y=sil_width)) + geom_line() +
  scale_x_continuous(breaks=2:20);
##13 clusters
model=kmeans(reduced_df, centers=13);
gamelog_staging=cbind(gamelog_staging, model$cluster) %>% 
  rename("cluster"="model$cluster");
rm(elbow_df, model, reduced_df, sil_df, sil_width, tot_withinss);

##Joining to main gamelog dataframe
gamelog_df=gamelog_df %>% 
  left_join(gamelog_staging %>% select(pitcher, game_pk, cluster)) %>% 
  mutate(cluster=ifelse(is.na(cluster),"NA", cluster));
##After examining count of good starts and bad starts per cluster
##Cluster 13 was composed of 97% bad starts


reduced_df=scale(gamelog_staging %>% filter(cluster!=13) %>% select(3:54))

tot_withinss=map_dbl(1:20, function(k) {
  model=kmeans(x=reduced_df, centers=k)
  model$tot.withinss});
elbow_df=data.frame(k=1:20, tot_withinss=tot_withinss);
ggplot(elbow_df, aes(x=k, y=tot_withinss)) + geom_line() + 
  scale_x_continuous(breaks=1:20);
sil_width=map_dbl(2:20, function(k){
  model=pam(x=reduced_df, k=k)
  model$silinfo$avg.width});
sil_df=data.frame(k=2:20, sil_width=sil_width);
ggplot(sil_df, aes(x=k, y=sil_width)) + geom_line() +
  scale_x_continuous(breaks=2:20);
##18 clusters
model=kmeans(reduced_df, centers=18);
gamelog_staging=cbind(gamelog_staging %>% filter(cluster!=13), 
                      model$cluster) %>% 
  rename("cluster2"="model$cluster");
rm(elbow_df, model, reduced_df, sil_df, sil_width, tot_withinss);


##Joining to main gamelog dataframe
gamelog_df=gamelog_df %>% 
  left_join(gamelog_staging %>% select(pitcher, game_pk, cluster2)) %>% 
  mutate(cluster2=ifelse(is.na(cluster2),"NA", cluster2));
##Remove Clusters 8 & 9


reduced_df=scale(gamelog_staging %>% 
                   filter(!(cluster2 %in% c(8,9))) %>% select(3:54));

tot_withinss=map_dbl(1:20, function(k) {
  model=kmeans(x=reduced_df, centers=k)
  model$tot.withinss});
elbow_df=data.frame(k=1:20, tot_withinss=tot_withinss);
ggplot(elbow_df, aes(x=k, y=tot_withinss)) + geom_line() + 
  scale_x_continuous(breaks=1:20);
sil_width=map_dbl(2:20, function(k){
  model=pam(x=reduced_df, k=k)
  model$silinfo$avg.width});
sil_df=data.frame(k=2:20, sil_width=sil_width);
ggplot(sil_df, aes(x=k, y=sil_width)) + geom_line() +
  scale_x_continuous(breaks=2:20);
##11 clusters
model=kmeans(reduced_df, centers=11);
gamelog_staging=cbind(gamelog_staging %>% filter(!(cluster2 %in% c(8,9))), 
                      model$cluster) %>% 
  rename("cluster3"="model$cluster");
rm(elbow_df, model, reduced_df, sil_df, sil_width, tot_withinss);


##Joining to main gamelog dataframe
gamelog_df=gamelog_df %>% 
  left_join(gamelog_staging %>% select(pitcher, game_pk, cluster3)) %>% 
  mutate(cluster3=ifelse(is.na(cluster3),"NA", cluster3));
##No overwhemingly bad clusters. Keep overwhemingly good




reduced_df=scale(gamelog_staging %>% 
                   filter(cluster3 %in% c(2,4,9,10,11)) %>% select(3:54));

tot_withinss=map_dbl(1:20, function(k) {
  model=kmeans(x=reduced_df, centers=k)
  model$tot.withinss});
elbow_df=data.frame(k=1:20, tot_withinss=tot_withinss);
ggplot(elbow_df, aes(x=k, y=tot_withinss)) + geom_line() + 
  scale_x_continuous(breaks=1:20);
sil_width=map_dbl(2:20, function(k){
  model=pam(x=reduced_df, k=k)
  model$silinfo$avg.width});
sil_df=data.frame(k=2:20, sil_width=sil_width);
ggplot(sil_df, aes(x=k, y=sil_width)) + geom_line() +
  scale_x_continuous(breaks=2:20);
##10 clusters
model=kmeans(reduced_df, centers=10);
gamelog_staging=cbind(gamelog_staging %>% filter(cluster3 %in% c(2,4,9,10,11)), 
                      model$cluster) %>% 
  rename("cluster4"="model$cluster");
rm(elbow_df, model, reduced_df, sil_df, sil_width, tot_withinss);


##Joining to main gamelog dataframe
gamelog_df=gamelog_df %>% 
  left_join(gamelog_staging %>% select(pitcher, game_pk, cluster4)) %>% 
  mutate(cluster4=ifelse(is.na(cluster4),"NA", cluster4));
##Remove Clusters 3

reduced_df=scale(gamelog_staging %>% 
                   filter(cluster4!=3) %>% select(9:54));
##Only keeping pitch variables after tossing majority of bad starts
##via clustering methods

tot_withinss=map_dbl(1:20, function(k) {
  model=kmeans(x=reduced_df, centers=k)
  model$tot.withinss});
elbow_df=data.frame(k=1:20, tot_withinss=tot_withinss);
ggplot(elbow_df, aes(x=k, y=tot_withinss)) + geom_line() + 
  scale_x_continuous(breaks=1:20);
sil_width=map_dbl(2:20, function(k){
  model=pam(x=reduced_df, k=k)
  model$silinfo$avg.width});
sil_df=data.frame(k=2:20, sil_width=sil_width);
ggplot(sil_df, aes(x=k, y=sil_width)) + geom_line() +
  scale_x_continuous(breaks=2:20);
##12 clusters
model=kmeans(reduced_df, centers=12);
gamelog_staging=cbind(gamelog_staging %>% filter(cluster4!=3), 
                      model$cluster) %>% 
  rename("cluster5"="model$cluster");
rm(elbow_df, model, reduced_df, sil_df, sil_width, tot_withinss);


##Joining to main gamelog dataframe
gamelog_df=gamelog_df %>% 
  left_join(gamelog_staging %>% select(pitcher, game_pk, cluster5)) %>% 
  mutate(cluster5=ifelse(is.na(cluster5),"NA", cluster5));
##Remove Clusters 2,7,12




reduced_df=scale(gamelog_staging %>% 
                   filter(!cluster5 %in% c(2,7,12)) %>% select(9:54));
##Only keeping pitch variables after tossing majority of bad starts
##via clustering methods

tot_withinss=map_dbl(1:20, function(k) {
  model=kmeans(x=reduced_df, centers=k)
  model$tot.withinss});
elbow_df=data.frame(k=1:20, tot_withinss=tot_withinss);
ggplot(elbow_df, aes(x=k, y=tot_withinss)) + geom_line() + 
  scale_x_continuous(breaks=1:20);
sil_width=map_dbl(2:20, function(k){
  model=pam(x=reduced_df, k=k)
  model$silinfo$avg.width});
sil_df=data.frame(k=2:20, sil_width=sil_width);
ggplot(sil_df, aes(x=k, y=sil_width)) + geom_line() +
  scale_x_continuous(breaks=2:20);
##9 clusters
model=kmeans(reduced_df, centers=9);
gamelog_staging=cbind(gamelog_staging %>% filter(!cluster5 %in% c(2,7,12)), 
                      model$cluster) %>% 
  rename("cluster6"="model$cluster");
rm(elbow_df, model, reduced_df, sil_df, sil_width, tot_withinss);


##Joining to main gamelog dataframe
gamelog_df=gamelog_df %>% 
  left_join(gamelog_staging %>% select(pitcher, game_pk, cluster6)) %>% 
  mutate(cluster6=ifelse(is.na(cluster6),"NA", cluster6));
##Keep 3, 5 and 7 because less than 10% bad
##Cluster with performance indicators back on




reduced_df=scale(gamelog_staging %>% 
                   filter(cluster6 %in% c(3,5,7)) %>% select(3:54));


tot_withinss=map_dbl(1:10, function(k) {
  model=kmeans(x=reduced_df, centers=k)
  model$tot.withinss});
elbow_df=data.frame(k=1:10, tot_withinss=tot_withinss);
ggplot(elbow_df, aes(x=k, y=tot_withinss)) + geom_line() + 
  scale_x_continuous(breaks=1:10);
sil_width=map_dbl(2:10, function(k){
  model=pam(x=reduced_df, k=k)
  model$silinfo$avg.width});
sil_df=data.frame(k=2:10, sil_width=sil_width);
ggplot(sil_df, aes(x=k, y=sil_width)) + geom_line() +
  scale_x_continuous(breaks=2:10);
##5 clusters
model=kmeans(reduced_df, centers=5);
gamelog_staging=cbind(gamelog_staging %>% filter(cluster6 %in% c(3,5,7)), 
                      model$cluster) %>% 
  rename("cluster7"="model$cluster");
rm(elbow_df, model, reduced_df, sil_df, sil_width, tot_withinss);


##Joining to main gamelog dataframe
gamelog_df=gamelog_df %>% 
  left_join(gamelog_staging %>% select(pitcher, game_pk, cluster7)) %>% 
  mutate(cluster7=ifelse(is.na(cluster7),"NA", cluster7));
##Keep 1 and 5 and remove performance variables




reduced_df=scale(gamelog_staging %>% 
                   filter(cluster7 %in% c(1,5)) %>% select(3:54));


tot_withinss=map_dbl(1:10, function(k) {
  model=kmeans(x=reduced_df, centers=k)
  model$tot.withinss});
elbow_df=data.frame(k=1:10, tot_withinss=tot_withinss);
ggplot(elbow_df, aes(x=k, y=tot_withinss)) + geom_line() + 
  scale_x_continuous(breaks=1:10);
sil_width=map_dbl(2:10, function(k){
  model=pam(x=reduced_df, k=k)
  model$silinfo$avg.width});
sil_df=data.frame(k=2:10, sil_width=sil_width);
ggplot(sil_df, aes(x=k, y=sil_width)) + geom_line() +
  scale_x_continuous(breaks=2:10);
##4 clusters
model=kmeans(reduced_df, centers=4);
gamelog_staging=cbind(gamelog_staging %>% filter(cluster7 %in% c(1,5)), 
                      model$cluster) %>% 
  rename("cluster8"="model$cluster");
rm(elbow_df, model, reduced_df, sil_df, sil_width, tot_withinss);


##Joining to main gamelog dataframe
gamelog_df=gamelog_df %>%
  left_join(gamelog_staging %>% select(pitcher, game_pk, cluster8)) %>% 
  mutate(cluster8=ifelse(is.na(cluster8),"NA", cluster8));





gamelog_df=gamelog_df %>%
  mutate(GoldStandard=ifelse(cluster8 %in% c(1,2),1,0), 
         LowKGoldStandard=ifelse(cluster8==1,1,0), 
         HighKGoldStandard=ifelse(cluster8==2,1,0)) %>%
  mutate(GoldType=ifelse(GoldStandard+LowKGoldStandard==2, "Low K Gold", 
                         ifelse(GoldStandard+HighKGoldStandard==2,"High K Gold", 
                                ifelse(cluster==13, "Bad Start", "Regular"))))





