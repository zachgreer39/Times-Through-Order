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

##Connection to AWS Database
con=dbConnect(RMySQL::MySQL(), dbname="zgreer_baseball",
              host="zgreer-baseball.cqridrsjfpub.us-east-2.rds.amazonaws.com",
              port=3306, user="guest", password="Password1!");
##dbListTables(con);
##dbDisconnect(con);

##Reading in tables
tbl_gamelogs=tbl(con, "clustered_gamelogs");
gamelog_df=collect(tbl_gamelogs) %>%
  readr::type_convert();
tbl_pitchdf=tbl(con, "pitch_df");
pitch_df=collect(tbl_pitchdf) %>%
  readr::type_convert();
tbl_plate_appearances=tbl(con, "plate_appearances");
plate_appearances=collect(tbl_plate_appearances) %>%
  readr::type_convert();
rm(tbl_gamelogs, tbl_pitchdf, tbl_plate_appearances);

##Adding pitch details to pitch_df
balls=c("ball", "blocked_ball", "hit_by_pitch", "pitchout");
not_swings=c(balls, "called_strike");
swings=setdiff(unique(pitch_df$description), not_swings);
fastballs=c("4-Seam Fastball","2-Seam Fastball","Cutter","Split-Finger","Sinker");
pitch_df=pitch_df %>%
  mutate(Ball=ifelse(description %in% balls, 1, 0),
         Strikes=ifelse(!(description %in% balls), 1, 0),
         Swings=ifelse(description %in% swings, 1, 0), 
         Whiff=ifelse(description %in% c("swinging_strike", 
                      "swinging_strike_blocked", "missed_bunt"), 1, 0), 
         PitchType=ifelse(pitch_name %in% fastballs, "Fastball", "Offspeed"));
rm(balls, not_swings, swings); 

##Pulling the main fastball and offspeed pitch by pitcher
main_pitch=pitch_df %>% filter(!(pitch_name %in% c("null", "Pitch Out"))) %>% 
  group_by(pitcher, PitchType, pitch_name) %>% count(pitch_name) %>% 
  group_by(pitcher, PitchType) %>% top_n(1) %>% arrange(pitcher, desc(n)) %>% 
  group_by(pitcher) %>% mutate(order=order(desc(n))) %>% 
  filter(order!=3) %>% select(-c(n, order));


##Adding TTO to pitch_df and dropping off plate appearances with no match
pitch_df=pitch_df %>% 
  inner_join(plate_appearances %>% 
               select(game_pk,at_bat_number,TTO) %>% drop_na()) %>%
  mutate(Split=ifelse(TTO %in% c(1,2),"First 18", "Post 18"))

##Ptches per at_bat
pitch_cnt=pitch_df %>% 
  group_by(game_pk,at_bat_number,Split,pitcher,batter) %>% 
  summarise(pitches=n());

##First Pitch Strike Percentage and Whiffs per Swing split by first 18 outs
TimeThroughOrder=pitch_cnt %>% group_by(game_pk, pitcher, Split) %>%
  summarise(pitches=sum(pitches)) %>% 
  left_join(pitch_df %>% filter(pitch_number==1) %>% 
              group_by(game_pk, pitcher, Split) %>%
  summarise(FirstPitchStrikePercent=sum(Strikes)/n(), 
            WhiffsperSwing=sum(Whiff)/sum(Swings)));





##Pitch Velocity/Movement on Main Fastball and Offspeed Pitch
pitch_metrics=pitch_df %>% group_by(game_pk, pitcher, Split, pitch_name) %>% 
  summarise(Count=n(), Velocity=mean(velocity), X_Movement=mean(x_movement), 
            Y_Movement=mean(y_movement), SpinRate=mean(spinrate)) %>%
  drop_na() %>% inner_join(main_pitch) %>% 
  left_join(pitch_cnt %>% group_by(game_pk, pitcher, Split) %>% 
              summarise(total_pitches=sum(pitches))) %>% 
  mutate(PitchPercentage=Count/total_pitches) %>% 
  select(-c("total_pitches"));


pitch_metrics=pitch_metrics %>% filter(Split=="First 18") %>% 
  inner_join(pitch_metrics %>% filter(Split=="Post 18"), 
             by=c("game_pk", "pitcher", "pitch_name"), 
             suffix=c("_first18", "_post18")) %>% 
  select(-c("Split_first18", "Split_post18"));


pitch_metrics=pitch_metrics %>% filter(PitchType_first18=="Fastball") %>% 
  left_join(pitch_metrics %>% filter(PitchType_first18!="Fastball"), 
            by=c("game_pk", "pitcher"), suffix=c("_fastball", "_offspeed"));



gamelog_df=gamelog_df %>% 
  inner_join(TimeThroughOrder %>% filter(Split=="First 18") %>%
               left_join(TimeThroughOrder %>% filter(Split=="Post 18"), 
                         by=c("game_pk", "pitcher"), 
                         suffix=c("_first18", "_post18")) %>% 
               filter(!is.na(Split_post18)) %>% 
               select(-c("Split_first18", "Split_post18"))) %>%
  inner_join(pitch_metrics);





gamelog_df=gamelog_df %>% select(-c(wOBA1, wOBA2, wOBA3, wOBA4)) %>% 
  left_join(plate_appearances %>% 
              mutate(Split=ifelse(TTO %in% c(1,2),"First 18", "Post 18")) %>% 
              group_by(game_pk, pitcher, Split) %>% 
              summarise(wOBA=sum(wOBA_points)/sum(wOBA_denom)) %>% 
              filter(Split=="First 18") %>% rename(wOBAFirst18=wOBA) %>% 
              select(-Split)) %>% 
  left_join(plate_appearances %>% 
              mutate(Split=ifelse(TTO %in% c(1,2),"First 18", "Post 18")) %>% 
              group_by(game_pk, pitcher, Split) %>% 
              summarise(wOBA=sum(wOBA_points)/sum(wOBA_denom)) %>% 
              filter(Split=="Post 18") %>% rename(wOBAPost18=wOBA) %>% 
              select(-Split));

##UPDATE THIS TO BACK UP THE GAME LOGS AND 
##HAVE A NEW DATAFRAME FOR CALCULATIONS




reduced_df=as.data.frame(gamelog_df %>%
  transmute(row_name=paste(game_pk, pitcher), Innings, TBF, BABIP, 
            WHIP, HRper9, Kper9, BBper9, StrikePercent, wOBAFirst18, wOBAPost18, 
            pitches_first18, FirstPitchStrikePercent_first18, 
            WhiffsperSwing_first18, PitchPercentage_first18_fastball, 
            Velocity_first18_fastball, X_Movement_first18_fastball, 
            Y_Movement_first18_fastball, SpinRate_first18_fastball, 
            PitchPercentage_first18_offspeed, Velocity_first18_offspeed, 
            X_Movement_first18_offspeed, Y_Movement_first18_offspeed, 
            SpinRate_first18_offspeed, pitches_post18, 
            FirstPitchStrikePercent_post18, WhiffsperSwing_post18, 
            PitchPercentage_post18_fastball, Velocity_post18_fastball, 
            X_Movement_post18_fastball, Y_Movement_post18_fastball, 
            SpinRate_post18_fastball, PitchPercentage_post18_offspeed, 
            Velocity_post18_offspeed, X_Movement_post18_offspeed, 
            Y_Movement_post18_offspeed, SpinRate_post18_offspeed));
row.names(reduced_df)=reduced_df$row_name;
reduced_df=reduced_df %>% select(-row_name);

reduced_df=reduced_df %>% 
  mutate(WhiffsperSwing_first18=ifelse(WhiffsperSwing_first18=="NaN",
                                       0,WhiffsperSwing_first18),
  WhiffsperSwing_post18=ifelse(WhiffsperSwing_post18=="NaN",
                               0,WhiffsperSwing_post18));

reduced_df[is.na(reduced_df)]=0


##Clustering Starts on performance
##Using ERA as dependent variable because of inclusion of BABIP and 
##looking at individual games is a smaller scope
reduced_df=scale(reduced_df);
tot_withinss=map_dbl(1:10, function(k) {
  model=kmeans(x=reduced_df, centers=k)
  model$tot.withinss});
elbow_df=data.frame(k=1:10, tot_withinss=tot_withinss);
ggplot(elbow_df, aes(x=k, y=tot_withinss)) + geom_line() + 
  scale_x_continuous(breaks=1:10);
##Stuff around 6-8
sil_width=map_dbl(2:10, function(k){
  model=pam(x=reduced_df, k=k)
  model$silinfo$avg.width});
sil_df=data.frame(k=2:10, sil_width=sil_width);
ggplot(sil_df, aes(x=k, y=sil_width)) + geom_line() +
  scale_x_continuous(breaks=2:10);
##Lowest dip at 6

##6 Clusters for Dataset
model=kmeans(reduced_df, centers=6);
gamelog_df=cbind(gamelog_df, model$cluster) %>% 
  rename("cluster_2"="model$cluster");
rm(elbow_df, model, reduced_df, sil_df, sil_width, tot_withinss);

gamelog_df %>%
ggplot(aes(x=wOBAFirst18, y=wOBAPost18, group=cluster_2)) + 
  geom_point() + facet_wrap(~cluster_2)

gamelog_df %>% group_by(cluster_2) %>% count(classified)


##3 overwhemingly bad
gamelog_df=gamelog_df %>% filter(cluster_2!=3);




reduced_df=as.data.frame(gamelog_df %>%
                           transmute(row_name=paste(game_pk, pitcher), Innings, TBF, BABIP, 
                                     WHIP, HRper9, Kper9, BBper9, StrikePercent, wOBAFirst18, wOBAPost18, 
                                     pitches_first18, FirstPitchStrikePercent_first18, 
                                     WhiffsperSwing_first18, PitchPercentage_first18_fastball, 
                                     Velocity_first18_fastball, X_Movement_first18_fastball, 
                                     Y_Movement_first18_fastball, SpinRate_first18_fastball, 
                                     PitchPercentage_first18_offspeed, Velocity_first18_offspeed, 
                                     X_Movement_first18_offspeed, Y_Movement_first18_offspeed, 
                                     SpinRate_first18_offspeed, pitches_post18, 
                                     FirstPitchStrikePercent_post18, WhiffsperSwing_post18, 
                                     PitchPercentage_post18_fastball, Velocity_post18_fastball, 
                                     X_Movement_post18_fastball, Y_Movement_post18_fastball, 
                                     SpinRate_post18_fastball, PitchPercentage_post18_offspeed, 
                                     Velocity_post18_offspeed, X_Movement_post18_offspeed, 
                                     Y_Movement_post18_offspeed, SpinRate_post18_offspeed));
row.names(reduced_df)=reduced_df$row_name;
reduced_df=reduced_df %>% select(-row_name);

reduced_df=reduced_df %>% 
  mutate(WhiffsperSwing_first18=ifelse(WhiffsperSwing_first18=="NaN",
                                       0,WhiffsperSwing_first18),
         WhiffsperSwing_post18=ifelse(WhiffsperSwing_post18=="NaN",
                                      0,WhiffsperSwing_post18));

reduced_df[is.na(reduced_df)]=0


##Clustering Starts on performance
##Using ERA as dependent variable because of inclusion of BABIP and 
##looking at individual games is a smaller scope
reduced_df=scale(reduced_df);
tot_withinss=map_dbl(1:10, function(k) {
  model=kmeans(x=reduced_df, centers=k)
  model$tot.withinss});
elbow_df=data.frame(k=1:10, tot_withinss=tot_withinss);
ggplot(elbow_df, aes(x=k, y=tot_withinss)) + geom_line() + 
  scale_x_continuous(breaks=1:10);
##Stuff going on around 5-7
sil_width=map_dbl(2:10, function(k){
  model=pam(x=reduced_df, k=k)
  model$silinfo$avg.width});
sil_df=data.frame(k=2:10, sil_width=sil_width);
ggplot(sil_df, aes(x=k, y=sil_width)) + geom_line() +
  scale_x_continuous(breaks=2:10);
##Lowest dip at 4 and 5

##5 Clusters for Dataset
model=kmeans(reduced_df, centers=5);
gamelog_df=cbind(gamelog_df, model$cluster) %>% 
  rename("cluster_3"="model$cluster");
rm(elbow_df, model, reduced_df, sil_df, sil_width, tot_withinss);



gamelog_df %>%
  ggplot(aes(x=wOBAFirst18, y=wOBAPost18, group=cluster_3)) + 
  geom_point() + facet_wrap(~cluster_3)


gamelog_df %>% group_by(cluster_3) %>% count(classified)


##Cluster 5 is shitty


gamelog_df=gamelog_df %>% filter(cluster_3!=5);




reduced_df=as.data.frame(gamelog_df %>%
                           transmute(row_name=paste(game_pk, pitcher), Innings, TBF, BABIP, 
                                     WHIP, HRper9, Kper9, BBper9, StrikePercent, wOBAFirst18, wOBAPost18, 
                                     pitches_first18, FirstPitchStrikePercent_first18, 
                                     WhiffsperSwing_first18, PitchPercentage_first18_fastball, 
                                     Velocity_first18_fastball, X_Movement_first18_fastball, 
                                     Y_Movement_first18_fastball, SpinRate_first18_fastball, 
                                     PitchPercentage_first18_offspeed, Velocity_first18_offspeed, 
                                     X_Movement_first18_offspeed, Y_Movement_first18_offspeed, 
                                     SpinRate_first18_offspeed, pitches_post18, 
                                     FirstPitchStrikePercent_post18, WhiffsperSwing_post18, 
                                     PitchPercentage_post18_fastball, Velocity_post18_fastball, 
                                     X_Movement_post18_fastball, Y_Movement_post18_fastball, 
                                     SpinRate_post18_fastball, PitchPercentage_post18_offspeed, 
                                     Velocity_post18_offspeed, X_Movement_post18_offspeed, 
                                     Y_Movement_post18_offspeed, SpinRate_post18_offspeed));
row.names(reduced_df)=reduced_df$row_name;
reduced_df=reduced_df %>% select(-row_name);

reduced_df=reduced_df %>% 
  mutate(WhiffsperSwing_first18=ifelse(WhiffsperSwing_first18=="NaN",
                                       0,WhiffsperSwing_first18),
         WhiffsperSwing_post18=ifelse(WhiffsperSwing_post18=="NaN",
                                      0,WhiffsperSwing_post18));

reduced_df[is.na(reduced_df)]=0


##Clustering Starts on performance
##Using ERA as dependent variable because of inclusion of BABIP and 
##looking at individual games is a smaller scope
reduced_df=scale(reduced_df);
tot_withinss=map_dbl(1:10, function(k) {
  model=kmeans(x=reduced_df, centers=k)
  model$tot.withinss});
elbow_df=data.frame(k=1:10, tot_withinss=tot_withinss);
ggplot(elbow_df, aes(x=k, y=tot_withinss)) + geom_line() + 
  scale_x_continuous(breaks=1:10);
##Stuff going on around 6-9
sil_width=map_dbl(2:10, function(k){
  model=pam(x=reduced_df, k=k)
  model$silinfo$avg.width});
sil_df=data.frame(k=2:10, sil_width=sil_width);
ggplot(sil_df, aes(x=k, y=sil_width)) + geom_line() +
  scale_x_continuous(breaks=2:10);
##Lowest dip 7

##9 Clusters for Dataset
model=kmeans(reduced_df, centers=7);
gamelog_df=cbind(gamelog_df, model$cluster) %>% 
  rename("cluster_4"="model$cluster");
rm(elbow_df, model, reduced_df, sil_df, sil_width, tot_withinss);



gamelog_df %>%
  ggplot(aes(x=wOBAFirst18, y=wOBAPost18, group=cluster_4)) + 
  geom_point() + facet_wrap(~cluster_4)


gamelog_df %>% group_by(cluster_4) %>% count(classified)


##Cluster 1 is ~40% bad so toss



gamelog_df=gamelog_df %>% filter(cluster_4!=1);




reduced_df=as.data.frame(gamelog_df %>%
                           transmute(row_name=paste(game_pk, pitcher), Innings, TBF, BABIP, 
                                     WHIP, HRper9, Kper9, BBper9, StrikePercent, wOBAFirst18, wOBAPost18, 
                                     pitches_first18, FirstPitchStrikePercent_first18, 
                                     WhiffsperSwing_first18, PitchPercentage_first18_fastball, 
                                     Velocity_first18_fastball, X_Movement_first18_fastball, 
                                     Y_Movement_first18_fastball, SpinRate_first18_fastball, 
                                     PitchPercentage_first18_offspeed, Velocity_first18_offspeed, 
                                     X_Movement_first18_offspeed, Y_Movement_first18_offspeed, 
                                     SpinRate_first18_offspeed, pitches_post18, 
                                     FirstPitchStrikePercent_post18, WhiffsperSwing_post18, 
                                     PitchPercentage_post18_fastball, Velocity_post18_fastball, 
                                     X_Movement_post18_fastball, Y_Movement_post18_fastball, 
                                     SpinRate_post18_fastball, PitchPercentage_post18_offspeed, 
                                     Velocity_post18_offspeed, X_Movement_post18_offspeed, 
                                     Y_Movement_post18_offspeed, SpinRate_post18_offspeed));
row.names(reduced_df)=reduced_df$row_name;
reduced_df=reduced_df %>% select(-row_name);

reduced_df=reduced_df %>% 
  mutate(WhiffsperSwing_first18=ifelse(WhiffsperSwing_first18=="NaN",
                                       0,WhiffsperSwing_first18),
         WhiffsperSwing_post18=ifelse(WhiffsperSwing_post18=="NaN",
                                      0,WhiffsperSwing_post18));

reduced_df[is.na(reduced_df)]=0


##Clustering Starts on performance
##Using ERA as dependent variable because of inclusion of BABIP and 
##looking at individual games is a smaller scope
reduced_df=scale(reduced_df);
tot_withinss=map_dbl(1:10, function(k) {
  model=kmeans(x=reduced_df, centers=k)
  model$tot.withinss});
elbow_df=data.frame(k=1:10, tot_withinss=tot_withinss);
ggplot(elbow_df, aes(x=k, y=tot_withinss)) + geom_line() + 
  scale_x_continuous(breaks=1:10);
##No real elbow
sil_width=map_dbl(2:10, function(k){
  model=pam(x=reduced_df, k=k)
  model$silinfo$avg.width});
sil_df=data.frame(k=2:10, sil_width=sil_width);
ggplot(sil_df, aes(x=k, y=sil_width)) + geom_line() +
  scale_x_continuous(breaks=2:10);
##Lowest dip 6

##6 Clusters for Dataset
model=kmeans(reduced_df, centers=6);
gamelog_df=cbind(gamelog_df, model$cluster) %>% 
  rename("cluster_5"="model$cluster");
rm(elbow_df, model, reduced_df, sil_df, sil_width, tot_withinss);



gamelog_df %>%
  ggplot(aes(x=wOBAFirst18, y=wOBAPost18, group=cluster_5)) + 
  geom_point() + facet_wrap(~cluster_5)


gamelog_df %>% group_by(cluster_5) %>% count(classified)


##Cluster 1 has no Bad Starts


Cluster_1=gamelog_df %>% filter(cluster_5==1) %>% 
  select(-c(cluster_2, cluster_3, cluster_4, cluster_5)) %>% 
  mutate(StartType="Gold Standard");

gamelog_df=gamelog_df %>% left_join(Cluster_1);

str(gamelog_df)

summary=gamelog_df %>% group_by(StartType) %>% 
  summarise(Starts=n(), WinPercent=round(sum(W)/(sum(W)+sum(L)),2), 
            Innings=round(mean(Innings),2), TBF=round(mean(TBF),2), 
            H=round(mean(H),2), HR=round(mean(HR),2), K=round(mean(K),2), 
            BB=round(mean(BB),2), WHIP=round(mean(WHIP),2), 
            BABIP=round(mean(BABIP),3), ERA=round(mean(ERA),2), 
            wOBA=round(mean(wOBAg),3), 
            wOBAGap=round(100*((mean(wOBAPost18)-mean(wOBAFirst18))/
                            mean(wOBAFirst18)),2),
            PitchCnt=round(mean(PitchCnt),2), StrikePercent=round(mean(StrikePercent),2), 
            WhiffsperSwing=round(mean(WhiffsperSwing, na.rm=TRUE),2), 
            FirstPitchStrikeGap=round(100*((mean(FirstPitchStrikePercent_post18, na.rm=TRUE)-
                                   mean(FirstPitchStrikePercent_first18, na.rm=TRUE))/
              mean(FirstPitchStrikePercent_first18, na.rm=TRUE)),2), 
            WhiffsperSwingGap=round(100*((mean(WhiffsperSwing_post18, na.rm=TRUE)-
                                 mean(WhiffsperSwing_first18, na.rm=TRUE))/
              mean(WhiffsperSwing_first18, na.rm=TRUE)),2), 
            FastballUsageGap=round(100*((mean(PitchPercentage_post18_fastball, na.rm=TRUE)-
                                 mean(PitchPercentage_first18_fastball, na.rm=TRUE))/
              mean(PitchPercentage_first18_fastball, na.rm=TRUE)),2),
            FastballVelocityGap=round(100*((mean(Velocity_post18_fastball, na.rm=TRUE)-
                                mean(Velocity_first18_fastball, na.rm=TRUE))/
              mean(Velocity_first18_fastball, na.rm=TRUE)),2),
            FastballX_MovementGap=round(100*((mean(X_Movement_post18_fastball, na.rm=TRUE)-
                                mean(X_Movement_first18_fastball, na.rm=TRUE))/
              mean(X_Movement_first18_fastball, na.rm=TRUE)),2),
            FastballY_MovementGap=round(100*((mean(Y_Movement_post18_fastball, na.rm=TRUE)-
                                mean(Y_Movement_first18_fastball, na.rm=TRUE))/
              mean(Y_Movement_first18_fastball, na.rm=TRUE)),2),
            FastballSpinRateGap=round(100*((mean(SpinRate_post18_fastball, na.rm=TRUE)-
                                     mean(SpinRate_first18_fastball, na.rm=TRUE))/
              mean(SpinRate_first18_fastball, na.rm=TRUE)),2),
            OffspeedUsageGap=round(100*((mean(PitchPercentage_post18_offspeed, na.rm=TRUE)-
                                    mean(PitchPercentage_first18_offspeed, na.rm=TRUE))/
              mean(PitchPercentage_first18_offspeed, na.rm=TRUE)),2),
            OffspeedVelocityGap=round(100*((mean(Velocity_post18_offspeed, na.rm=TRUE)-
                                       mean(Velocity_first18_offspeed, na.rm=TRUE))/
              mean(Velocity_first18_offspeed, na.rm=TRUE)),2),
            OffspeedX_MovementGap=round(100*((mean(X_Movement_post18_offspeed, na.rm=TRUE)-
                                         mean(X_Movement_first18_offspeed, na.rm=TRUE))/
              mean(X_Movement_first18_offspeed, na.rm=TRUE)),2),
            OffspeedY_MovementGap=round(100*((mean(Y_Movement_post18_offspeed, na.rm=TRUE)-
                                         mean(Y_Movement_first18_offspeed, na.rm=TRUE))/
              mean(Y_Movement_first18_offspeed, na.rm=TRUE)),2),
            OffspeedSpinRateGap=round(100*((mean(SpinRate_post18_offspeed, na.rm=TRUE)-
                                       mean(SpinRate_first18_offspeed, na.rm=TRUE))/
              mean(SpinRate_first18_offspeed, na.rm=TRUE)),2))


variables=setdiff(colnames(summary),c("StartType", "Starts", "WinPercent"));


density_plot=function(x) {
  ggplot(gamelog_df) +
  geom_density(aes(x=x)) +
  geom_rug(aes(x=x, y=0), position=position_jitter(height=0))}




density_plot(gamelog_df$BABIP)
ggqqplot(gamelog_df$BABIP)
density_plot(gamelog_df$wOBAg)
ggqqplot(gamelog_df$wOBAg)
density_plot(gamelog_df$wOBAFirst18)
ggqqplot(gamelog_df$wOBAFirst18)
density_plot(gamelog_df$wOBAPost18)
ggqqplot(gamelog_df$wOBAPost18)
density_plot(gamelog_df$WHIP)
ggqqplot(gamelog_df$WHIP)
density_plot(gamelog_df$Kper9)
ggqqplot(gamelog_df$Kper9)
##Not normal but will work for percentiles
density_plot(gamelog_df$BBper9)
ggqqplot(gamelog_df$BBper9)
density_plot(gamelog_df$PitchCnt)
ggqqplot(gamelog_df$PitchCnt)
density_plot(gamelog_df$StrikePercent)
ggqqplot(gamelog_df$StrikePercent)
density_plot(gamelog_df$WhiffsperSwing)
ggqqplot(gamelog_df$WhiffsperSwing)
density_plot(gamelog_df$pitches_first18)
ggqqplot(gamelog_df$pitches_first18)
density_plot(gamelog_df$pitches_post18)
ggqqplot(gamelog_df$pitches_post18)
##Jagged peaks but works for percentile
density_plot(gamelog_df$FirstPitchStrikePercent_first18)
ggqqplot(gamelog_df$FirstPitchStrikePercent_first18)
density_plot(gamelog_df$FirstPitchStrikePercent_post18)
ggqqplot(gamelog_df$FirstPitchStrikePercent_post18)
##Normal at game level, but not split between time frame
##Potential dependent variable to be predicted
##density_plot(gamelog_df$WhiffsperSwing_first18)
##ggqqplot(gamelog_df$WhiffsperSwing_first18)
##density_plot(gamelog_df$WhiffsperSwing_post18)
##ggqqplot(gamelog_df$WhiffsperSwing_post18)
density_plot(gamelog_df$Velocity_first18_fastball)
ggqqplot(gamelog_df$Velocity_first18_fastball)
density_plot(gamelog_df$Velocity_post18_fastball)
ggqqplot(gamelog_df$Velocity_post18_fastball)
##Bimodal X_Movement, possible split?
density_plot(gamelog_df$X_Movement_first18_fastball)
ggqqplot(gamelog_df$X_Movement_first18_fastball)
density_plot(gamelog_df$X_Movement_post18_fastball)
ggqqplot(gamelog_df$X_Movement_post18_fastball)
density_plot(gamelog_df$Y_Movement_first18_fastball)
ggqqplot(gamelog_df$Y_Movement_first18_fastball)
density_plot(gamelog_df$Y_Movement_post18_fastball)
ggqqplot(gamelog_df$Y_Movement_post18_fastball)
density_plot(gamelog_df$SpinRate_first18_fastball)
ggqqplot(gamelog_df$SpinRate_first18_fastball)
density_plot(gamelog_df$SpinRate_post18_fastball)
ggqqplot(gamelog_df$SpinRate_post18_fastball)
density_plot(gamelog_df$PitchPercentage_first18_fastball)
ggqqplot(gamelog_df$PitchPercentage_first18_fastball)
density_plot(gamelog_df$PitchPercentage_post18_fastball)
ggqqplot(gamelog_df$PitchPercentage_post18_fastball)
density_plot(gamelog_df$Velocity_first18_offspeed)
ggqqplot(gamelog_df$Velocity_first18_offspeed)
density_plot(gamelog_df$Velocity_post18_offspeed)
ggqqplot(gamelog_df$Velocity_post18_offspeed)
##X_Movement is trimodal
density_plot(gamelog_df$X_Movement_first18_offspeed)
ggqqplot(gamelog_df$X_Movement_first18_offspeed)
density_plot(gamelog_df$X_Movement_post18_offspeed)
ggqqplot(gamelog_df$X_Movement_post18_offspeed)
density_plot(gamelog_df$Y_Movement_first18_offspeed)
ggqqplot(gamelog_df$Y_Movement_first18_offspeed)
density_plot(gamelog_df$Y_Movement_post18_offspeed)
ggqqplot(gamelog_df$Y_Movement_post18_offspeed)
density_plot(gamelog_df$SpinRate_first18_offspeed)
ggqqplot(gamelog_df$SpinRate_first18_offspeed)
density_plot(gamelog_df$SpinRate_post18_offspeed)
ggqqplot(gamelog_df$SpinRate_post18_offspeed)
density_plot(gamelog_df$PitchPercentage_first18_offspeed)
ggqqplot(gamelog_df$PitchPercentage_first18_offspeed)
density_plot(gamelog_df$PitchPercentage_post18_offspeed)
ggqqplot(gamelog_df$PitchPercentage_post18_offspeed)







pitches_reduced=pitch_df %>% 
  inner_join(pitch_df %>% inner_join(gamelog_df %>% 
                                       select(game_pk, pitcher, StartType)) %>%
  semi_join(main_pitch) %>% group_by(game_pk, pitcher, at_bat_number) %>% 
  summarise() %>% summarise(at_bat=frank(at_bat_number), 
                            at_bat_number=at_bat_number)) %>% 
  semi_join(main_pitch);





MovingAverages=pitches_reduced %>% 
  group_by(game_pk, pitcher, PitchType, at_bat, at_bat_number) %>% 
  summarise(Velocity=mean(velocity, na.rm=TRUE), 
            X_Movement=mean(x_movement, na.rm=TRUE), 
            Y_Movement=mean(y_movement, na.rm=TRUE), 
            SpinRate=mean(spinrate, na.rm=TRUE)) %>% 
  group_by(game_pk, pitcher, PitchType) %>%
  mutate(VelocityMovingAverage=round((Velocity+lag(Velocity)+
                                        lag(Velocity,2))/3,2), 
         X_MovementMovingAverage=round((X_Movement+lag(X_Movement)+
                                    lag(X_Movement,2))/3,2),
         Y_MovementMovingAverage=round((Y_Movement+lag(Y_Movement)+
                                    lag(Y_Movement,2))/3,2),
         SpinRateMovingAverage=round((SpinRate+lag(SpinRate)+
                                        lag(SpinRate,2))/3,2)) %>%
  select(-c(Velocity, X_Movement, Y_Movement, SpinRate)) %>% drop_na() %>%
  left_join(gamelog_df %>% select(game_pk, pitcher, StartType)) %>% 
  mutate(StartType=ifelse(is.na(StartType), "All Others", StartType)) %>% 
  group_by(StartType, PitchType, at_bat) %>%
  summarise(Velocity=round(mean(VelocityMovingAverage),2), 
            X_Movement=round(mean(X_MovementMovingAverage),2), 
            Y_Movement=round(mean(Y_MovementMovingAverage),2),
            SpinRate=round(mean(SpinRateMovingAverage),2)) %>%
  group_by(StartType, PitchType) %>%
  summarise(at_bat=at_bat, Velocity=round((Velocity+lag(Velocity)+
                                             lag(Velocity,2))/3,2), 
            X_Movement=round((X_Movement+lag(X_Movement)+
                                lag(X_Movement,2))/3,2),
            Y_Movement=round((Y_Movement+lag(Y_Movement)+
                                lag(Y_Movement,2))/3,2),
            SpinRate=round((SpinRate+lag(SpinRate)+lag(SpinRate,2))/3,2)) %>%
  drop_na();









##Big Spike at end for Fastballs in Gold Standard
##Both spike at end
MovingAverages %>% filter(PitchType=="Fastball" & at_bat<=27) %>%
  ggplot(aes(x=at_bat, y=Velocity, group=StartType)) + 
  geom_line(aes(color=StartType))

MovingAverages %>% filter(PitchType=="Offspeed" & at_bat<=27) %>%
  ggplot(aes(x=at_bat, y=Velocity, group=StartType)) + 
  geom_line(aes(color=StartType))




##Fastball X-Movement drops for regular starts and spikes for gold standard
MovingAverages %>% filter(PitchType=="Fastball" & at_bat<=27) %>%
  ggplot(aes(x=at_bat, y=X_Movement, group=StartType)) + 
  geom_line(aes(color=StartType))

MovingAverages %>% filter(PitchType=="Offspeed" & at_bat<=27) %>%
  ggplot(aes(x=at_bat, y=X_Movement, group=StartType)) + 
  geom_line(aes(color=StartType))



##Fastball Y-Movement drops hard at end for fastballs in regular starts
MovingAverages %>% filter(PitchType=="Fastball" & at_bat<=27) %>%
  ggplot(aes(x=at_bat, y=Y_Movement, group=StartType)) + 
  geom_line(aes(color=StartType))

MovingAverages %>% filter(PitchType=="Offspeed" & at_bat<=27) %>%
  ggplot(aes(x=at_bat, y=Y_Movement, group=StartType)) + 
  geom_line(aes(color=StartType))




##Drops at end a lot more for regular starts before spiking for both
MovingAverages %>% filter(PitchType=="Fastball" & at_bat<=27) %>%
  ggplot(aes(x=at_bat, y=SpinRate, group=StartType)) + 
  geom_line(aes(color=StartType))

MovingAverages %>% filter(PitchType=="Offspeed" & at_bat<=27) %>%
  ggplot(aes(x=at_bat, y=SpinRate, group=StartType)) + 
  geom_line(aes(color=StartType))




pitch_averages=pitch_df %>% group_by(pitch_name) %>% 
  filter(!(pitch_name %in% c("null", "Pitch Out"))) %>% 
  summarise(velocityavg=mean(velocity, na.rm=TRUE), 
            x_movementavg=mean(x_movement, na.rm=TRUE), 
            y_movementavg=mean(y_movement, na.rm=TRUE), 
            spinrateavg=mean(spinrate, na.rm=TRUE));





gold_pitchers=gamelog_df %>% 
  semi_join(gamelog_df %>% filter(StartType=="Gold Standard") %>% 
              select(pitcher) %>% distinct()) %>%
  left_join(pitch_averages, by=c("pitch_name_fastball"="pitch_name")) %>% 
  left_join(pitch_averages, by=c("pitch_name_offspeed"="pitch_name"), 
            suffix=c("_fastball", "_offspeed"));




goldpitches_reduced=pitch_df %>% 
  inner_join(pitch_df %>% inner_join(gold_pitchers %>% 
                                       select(game_pk, pitcher, StartType)) %>%
               semi_join(main_pitch) %>% group_by(game_pk, pitcher, at_bat_number) %>% 
               summarise() %>% summarise(at_bat=frank(at_bat_number), 
                                         at_bat_number=at_bat_number)) %>% 
  semi_join(main_pitch) %>%
  left_join(pitch_averages) %>%
  mutate(velocity_gap=100*(velocity-velocityavg)/velocityavg,
         x_movement_gap=100*(x_movement-x_movementavg)/x_movementavg, 
         y_movement_gap=100*(y_movement-y_movementavg)/y_movementavg, 
         spinrate_gap=100*(spinrate-spinrateavg)/spinrateavg)





GoldMovingAverages=goldpitches_reduced %>% 
  group_by(game_pk, pitcher, PitchType, at_bat, at_bat_number) %>% 
  summarise(Velocity=mean(velocity, na.rm=TRUE), 
            X_Movement=mean(x_movement, na.rm=TRUE), 
            Y_Movement=mean(y_movement, na.rm=TRUE), 
            SpinRate=mean(spinrate, na.rm=TRUE)) %>% 
  group_by(game_pk, pitcher, PitchType) %>%
  mutate(VelocityMovingAverage=round((Velocity+lag(Velocity)+
                                        lag(Velocity,2))/3,2), 
         X_MovementMovingAverage=round((X_Movement+lag(X_Movement)+
                                          lag(X_Movement,2))/3,2),
         Y_MovementMovingAverage=round((Y_Movement+lag(Y_Movement)+
                                          lag(Y_Movement,2))/3,2),
         SpinRateMovingAverage=round((SpinRate+lag(SpinRate)+
                                        lag(SpinRate,2))/3,2)) %>%
  select(-c(Velocity, X_Movement, Y_Movement, SpinRate)) %>% drop_na() %>%
  left_join(gold_pitchers %>% select(game_pk, pitcher, StartType)) %>% 
  mutate(StartType=ifelse(is.na(StartType), "All Others", StartType)) %>% 
  group_by(StartType, PitchType, at_bat) %>%
  summarise(Velocity=round(mean(VelocityMovingAverage),2), 
            X_Movement=round(mean(X_MovementMovingAverage),2), 
            Y_Movement=round(mean(Y_MovementMovingAverage),2),
            SpinRate=round(mean(SpinRateMovingAverage),2)) %>%
  group_by(StartType, PitchType) %>%
  summarise(at_bat=at_bat, Velocity=round((Velocity+lag(Velocity)+
                                             lag(Velocity,2))/3,2), 
            X_Movement=round((X_Movement+lag(X_Movement)+
                                lag(X_Movement,2))/3,2),
            Y_Movement=round((Y_Movement+lag(Y_Movement)+
                                lag(Y_Movement,2))/3,2),
            SpinRate=round((SpinRate+lag(SpinRate)+lag(SpinRate,2))/3,2)) %>%
  drop_na();









##Big Spike at end for Fastballs in Gold Standard
##Both spike at end
GoldMovingAverages %>% filter(PitchType=="Fastball" & at_bat<=18) %>%
  ggplot(aes(x=at_bat, y=Velocity, group=StartType)) + 
  geom_line(aes(color=StartType))

GoldMovingAverages %>% filter(PitchType=="Offspeed" & at_bat<=18) %>%
  ggplot(aes(x=at_bat, y=Velocity, group=StartType)) + 
  geom_line(aes(color=StartType))




##Fastball X-Movement drops for regular starts and spikes for gold standard
GoldMovingAverages %>% filter(PitchType=="Fastball" & at_bat<=27) %>%
  ggplot(aes(x=at_bat, y=X_Movement, group=StartType)) + 
  geom_line(aes(color=StartType))

GoldMovingAverages %>% filter(PitchType=="Offspeed" & at_bat<=27) %>%
  ggplot(aes(x=at_bat, y=X_Movement, group=StartType)) + 
  geom_line(aes(color=StartType))



##Fastball Y-Movement drops hard at end for fastballs in regular starts
GoldMovingAverages %>% filter(PitchType=="Fastball" & at_bat<=27) %>%
  ggplot(aes(x=at_bat, y=Y_Movement, group=StartType)) + 
  geom_line(aes(color=StartType))

GoldMovingAverages %>% filter(PitchType=="Offspeed" & at_bat<=27) %>%
  ggplot(aes(x=at_bat, y=Y_Movement, group=StartType)) + 
  geom_line(aes(color=StartType))




##Drops at end a lot more for regular starts before spiking for both
GoldMovingAverages %>% filter(PitchType=="Fastball" & at_bat<=18) %>%
  ggplot(aes(x=at_bat, y=SpinRate, group=StartType)) + 
  geom_line(aes(color=StartType))

GoldMovingAverages %>% filter(PitchType=="Offspeed" & at_bat<=18) %>%
  ggplot(aes(x=at_bat, y=SpinRate, group=StartType)) + 
  geom_line(aes(color=StartType))












GoldMovingAverages=goldpitches_reduced %>% 
  group_by(game_pk, pitcher, PitchType, at_bat, at_bat_number) %>% 
  summarise(Velocity=mean(velocity_gap, na.rm=TRUE), 
            X_Movement=mean(x_movement_gap, na.rm=TRUE), 
            Y_Movement=mean(y_movement_gap, na.rm=TRUE), 
            SpinRate=mean(spinrate_gap, na.rm=TRUE)) %>% 
  group_by(game_pk, pitcher, PitchType) %>%
  mutate(VelocityMovingAverage=round((Velocity+lag(Velocity)+
                                        lag(Velocity,2))/3,2), 
         X_MovementMovingAverage=round((X_Movement+lag(X_Movement)+
                                          lag(X_Movement,2))/3,2),
         Y_MovementMovingAverage=round((Y_Movement+lag(Y_Movement)+
                                          lag(Y_Movement,2))/3,2),
         SpinRateMovingAverage=round((SpinRate+lag(SpinRate)+
                                        lag(SpinRate,2))/3,2)) %>%
  mutate(VelocityMovingAverage=ifelse(is.na(VelocityMovingAverage),Velocity,VelocityMovingAverage), 
         X_MovementMovingAverage=ifelse(is.na(X_MovementMovingAverage),X_Movement,X_MovementMovingAverage),
         Y_MovementMovingAverage=ifelse(is.na(Y_MovementMovingAverage),Y_Movement,Y_MovementMovingAverage),
         SpinRateMovingAverage=ifelse(is.na(SpinRateMovingAverage),SpinRate,SpinRateMovingAverage)) %>%
  select(-c(Velocity, X_Movement, Y_Movement, SpinRate)) %>% drop_na() %>%
  left_join(gold_pitchers %>% select(game_pk, pitcher, StartType)) %>% 
  mutate(StartType=ifelse(is.na(StartType), "All Others", StartType)) %>% 
  group_by(StartType, PitchType, at_bat) %>%
  summarise(Velocity=round(mean(VelocityMovingAverage),2), 
            X_Movement=round(mean(X_MovementMovingAverage),2), 
            Y_Movement=round(mean(Y_MovementMovingAverage),2),
            SpinRate=round(mean(SpinRateMovingAverage),2)) %>%
  group_by(StartType, PitchType) %>%
  summarise(at_bat=at_bat, Velocity2=Velocity, X_Movement2=X_Movement, 
            Y_Movement2=Y_Movement, SpinRate2=SpinRate, 
            VelocityMovingAverage=round((Velocity+lag(Velocity))/2,2), 
            X_MovementMovingAverage=round((X_Movement+lag(X_Movement))/2,2),
            Y_MovementMovingAverage=round((Y_Movement+lag(Y_Movement))/2,2),
            SpinRateMovingAverage=round((SpinRate+lag(SpinRate))/2,2))  %>%
  drop_na();





##Big Spike at end for Fastballs in Gold Standard
##Both spike at end
GoldMovingAverages %>% filter(PitchType=="Fastball" & at_bat<=18) %>%
  ggplot(aes(x=at_bat, y=VelocityMovingAverage, group=StartType)) + 
  geom_line(aes(color=StartType))

GoldMovingAverages %>% filter(PitchType=="Offspeed" & at_bat<=18) %>%
  ggplot(aes(x=at_bat, y=VelocityMovingAverage, group=StartType)) + 
  geom_line(aes(color=StartType))




##Fastball X-Movement drops for regular starts and spikes for gold standard
GoldMovingAverages %>% filter(PitchType=="Fastball" & at_bat<=18) %>%
  ggplot(aes(x=at_bat, y=X_MovementMovingAverage, group=StartType)) + 
  geom_line(aes(color=StartType))

GoldMovingAverages %>% filter(PitchType=="Offspeed" & at_bat<=18) %>%
  ggplot(aes(x=at_bat, y=X_MovementMovingAverage, group=StartType)) + 
  geom_line(aes(color=StartType))



##Fastball Y-Movement drops hard at end for fastballs in regular starts
GoldMovingAverages %>% filter(PitchType=="Fastball" & at_bat<=18) %>%
  ggplot(aes(x=at_bat, y=Y_MovementMovingAverage, group=StartType)) + 
  geom_line(aes(color=StartType))

GoldMovingAverages %>% filter(PitchType=="Offspeed" & at_bat<=18) %>%
  ggplot(aes(x=at_bat, y=Y_MovementMovingAverage, group=StartType)) + 
  geom_line(aes(color=StartType))




##Drops at end a lot more for regular starts before spiking for both
GoldMovingAverages %>% filter(PitchType=="Fastball" & at_bat<=18) %>%
  ggplot(aes(x=at_bat, y=SpinRateMovingAverage, group=StartType)) + 
  geom_line(aes(color=StartType))

GoldMovingAverages %>% filter(PitchType=="Offspeed" & at_bat<=18) %>%
  ggplot(aes(x=at_bat, y=SpinRateMovingAverage, group=StartType)) + 
  geom_line(aes(color=StartType))
