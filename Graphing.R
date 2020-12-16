##GRAPHING PORTION FROM TIME THROUGH ORDER ANALYSIS FILE
##CLEAN AND ORGANIZE

####
####
####
###Graphing portion starts here
###Clean later
###
###



##Summary tables broken up by Cluster

##Summary Table by Gold Standard
test=gamelog_df %>% group_by(GoldType) %>%
  summarise(Games=n(), WinPercent=round(sum(W)/(sum(W)+sum(L)),2), 
            PitchCnt=round(mean(PitchCnt),2), Innings=round(mean(Innings),2), 
            TBF=round(mean(TBF),2), H=round(mean(H),2), HR=round(mean(HR),2), 
            K=round(mean(K),2), BB=round(mean(BB),2), WHIP=round(mean(WHIP),2), 
            BABIP=round(mean(BABIP),3), ERA=round(mean(ERA),2), 
            FIP=round(mean(FIP),2), xFIP=round(mean(xFIP),2), 
            wOBA=round(mean(wOBAg),3), 
            Whiffsper100Swings=round(mean(WhiffsperSwing)*100,2));

##Summary Table by Original Performance Based Tables
test=gamelog_df %>% group_by(classified) %>%
  summarise(Games=n(), WinPercent=round(sum(W)/(sum(W)+sum(L)),2), 
            PitchCnt=round(mean(PitchCnt),2), Innings=round(mean(Innings),2), 
            TBF=round(mean(TBF),2), H=round(mean(H),2), HR=round(mean(HR),2), 
            K=round(mean(K),2), BB=round(mean(BB),2), WHIP=round(mean(WHIP),2), 
            BABIP=round(mean(BABIP),3), ERA=round(mean(ERA),2), 
            FIP=round(mean(FIP),2), xFIP=round(mean(xFIP),2), 
            wOBA=round(mean(wOBAg),3), 
            Whiffsper100Swings=round(mean(WhiffsperSwing)*100,2));

##Raw table on Pitch Metrics by TTO
TTO_pitchtable=TTO_pitches %>% semi_join(primary_pitches, 
                                         by=c("pitcher", "pitchid")) %>%
  inner_join(gamelog_df %>% select(game_pk, pitcher, GoldType)) %>%
  mutate(StrikeHelper=Count*StrikePercentage, WhiffHelper=WhiffsperSwing*Count, 
         VeloHelper=Count*avg_velo, xHelper=Count*avg_x_movement, 
         yHelper=Count*avg_y_movement, SpinHelper=Count*avg_spinrate, 
         HardHitHelper=Count*HardHitPercentage) %>% 
  group_by(game_pk, pitcher, TTO) %>% 
  mutate(TotalPitches=sum(Count), Usage=Count/TotalPitches) %>%
  group_by(GoldType, TTO, PitchType, pitchid, pitch_name) %>%
  summarise(Usage=mean(Usage), Pitches=sum(Count), 
            StrikePercentage=round(sum(StrikeHelper)/Pitches,4), 
            WhiffsperSwing=round(sum(WhiffHelper)/Pitches,4),
            avgVelo=round(sum(VeloHelper)/Pitches,4), 
            avgX_movement=round(sum(xHelper)/Pitches,4), 
            avgY_movement=round(sum(yHelper)/Pitches,4), 
            SpinRate=round(sum(SpinHelper)/Pitches,4), 
            HardHitPercentage=round(sum(HardHitHelper)/Pitches,4)) %>% 
  arrange(GoldType, pitchid, TTO)





##wOBAs by TTO
gamelog_df$GoldType=factor(gamelog_df$GoldType, 
                           levels=c("High K Gold Start", "Low K Gold Start", 
                                    "Regular Start", "Bad Start"))
plate_appearances %>% inner_join(gamelog_df %>% select(game_pk, pitcher, GoldType)) %>% 
  group_by(GoldType, TTO) %>% mutate(TTO=as.factor(TTO)) %>%
  summarise(wOBA=sum(wOBA_points)/sum(wOBA_denom)) %>%
  ggplot(aes(x=GoldType, y=wOBA, group=TTO)) +
  geom_bar(stat="identity", aes(fill=TTO), color="black", position="dodge") + 
  labs(title="Time Through Order (wOBA)", x="Pitch Name", y="wOBA") + 
  scale_y_continuous(breaks=seq(0,.6,by = .05), limits=c(0,.6), 
                     labels=number_format(accuracy=0.001)) + 
  theme(legend.position="bottom")






##Dataframe for Pitch Metrics by atbat for graphing
##Edit for specifics needed
at_bat_pitches %>% inner_join(plate_appearances %>% group_by(game_pk, pitcher) %>% 
                                transmute(game_pk, pitcher, at_bat_number, at_bat=frank(at_bat_number))) %>%
  inner_join(gamelog_df %>% select(game_pk, pitcher, GoldType)) %>%
  group_by(game_pk, pitcher, at_bat) %>% 
  mutate(total=sum(Count), usage=round(Count/total,4)) %>%
  semi_join(primary_pitches, by=c("pitcher", "pitchid")) %>%
  mutate(veloHelper=Count*avg_velo, xHelper=Count*avg_x_movement, 
         yHelper=Count*avg_y_movement, spinHelper=Count*avg_spinrate) %>%
  group_by(GoldType, PitchType, pitchid, pitch_name, at_bat) %>%
  summarise(Usage=mean(usage), Pitches=sum(Count),
            avgVelo=round(sum(veloHelper)/Pitches,4), 
            avgX_movement=round(sum(xHelper)/Pitches,4), 
            avgY_movement=round(sum(yHelper)/Pitches,4), 
            SpinRate=round(sum(spinHelper)/Pitches,4))




##Pitches per AB by Gold Standard Type
pitch_df %>% inner_join(gamelog_df %>% select(game_pk, pitcher, GoldType)) %>%
  inner_join(plate_appearances %>% group_by(game_pk, pitcher) %>% 
               transmute(game_pk, pitcher, at_bat_number, at_bat=frank(at_bat_number))) %>% 
  group_by(GoldType, game_pk, pitcher, at_bat) %>% summarise(Pitches=n()) %>% 
  group_by(GoldType) %>% summarise(pitchesperAB=sum(Pitches)/n())


##Pitches per AB by atbat, GoldType
pitch_df %>% inner_join(gamelog_df %>% select(game_pk, pitcher, GoldType)) %>%
  inner_join(plate_appearances %>% group_by(game_pk, pitcher) %>% 
               transmute(game_pk, pitcher, at_bat_number, at_bat=frank(at_bat_number))) %>% 
  group_by(GoldType, game_pk, pitcher, at_bat) %>% summarise(Pitches=n()) %>% 
  group_by(GoldType, at_bat) %>% summarise(pitchesperAB=sum(Pitches)/n())



##Pitches per AB by TTO, GoldType
pitch_df %>% inner_join(gamelog_df %>% select(game_pk, pitcher, GoldType)) %>%
  inner_join(plate_appearances %>% group_by(game_pk, pitcher) %>% 
               transmute(game_pk, pitcher, at_bat_number, at_bat=frank(at_bat_number))) %>% 
  group_by(GoldType, game_pk, pitcher, TTO, at_bat) %>% summarise(Pitches=n()) %>% 
  group_by(GoldType, TTO) %>% summarise(pitchesperAB=sum(Pitches)/n())

gamelog_df=gamelog_df %>%
  mutate(GoldType=ifelse(GoldStandard+LowKGoldStandard==2, "Low K Gold Start", 
                         ifelse(GoldStandard+HighKGoldStandard==2,"High K Gold Start", 
                                ifelse(cluster==13, "Bad Start", "Regular Start"))))

##write.csv(gamelog_df, "gamelog2_df.csv", row.names=FALSE, col.names=TRUE)






##Graphing 


##Pitches per AB by atbat, GoldType 
pitches_ab=pitch_df %>% inner_join(gamelog_df %>% select(game_pk, pitcher, GoldType)) %>%
  inner_join(plate_appearances %>% group_by(game_pk, pitcher) %>% 
               transmute(game_pk, pitcher, at_bat_number, at_bat=frank(at_bat_number))) %>% 
  group_by(GoldType, game_pk, pitcher, at_bat) %>% summarise(Pitches=n()) %>% 
  group_by(GoldType, at_bat) %>% summarise(pitchesperAB=sum(Pitches)/n()) %>%
  filter(at_bat<=27)




##Pitches per AtBat (Bad Starts, High K and Low K starts)
pitches_ab %>% group_by(GoldType) %>%
  mutate(pitchesMA=(pitchesperAB+lag(pitchesperAB)+lag(pitchesperAB,2)+
                      lag(pitchesperAB,3)+lag(pitchesperAB,4))/5) %>%
  mutate(pitchesMA=ifelse(is.na(pitchesMA),(pitchesperAB+lag(pitchesperAB)+lag(pitchesperAB,2)+lag(pitchesperAB,3))/4,pitchesMA)) %>% 
  mutate(pitchesMA=ifelse(is.na(pitchesMA),(pitchesperAB+lag(pitchesperAB)+lag(pitchesperAB,2))/3,pitchesMA)) %>% 
  mutate(pitchesMA=ifelse(is.na(pitchesMA),(pitchesperAB+lag(pitchesperAB))/2,pitchesMA)) %>%
  mutate(pitchesMA=ifelse(is.na(pitchesMA),pitchesperAB,pitchesMA)) %>%
  filter(GoldType!="Regular Start") %>% drop_na() %>%
  ggplot(aes(x=at_bat, y=pitchesMA, group=GoldType)) + 
  geom_line(aes(color=GoldType))
##
##
##Clean up graph
###
###




pitchmetrics_ab=at_bat_pitches %>% inner_join(plate_appearances %>% group_by(game_pk, pitcher) %>% 
                                                transmute(game_pk, pitcher, at_bat_number, at_bat=frank(at_bat_number))) %>%
  inner_join(gamelog_df %>% select(game_pk, pitcher, GoldType)) %>%
  group_by(game_pk, pitcher, at_bat) %>%
  mutate(TotalPitches=sum(Count), Usage=Count/TotalPitches) %>%
  semi_join(gamelog_df %>% filter(GoldType=="High K Gold Start") %>% select(pitcher) %>% distinct()) %>%
  filter(GoldType %in% c("High K Gold Start", "Regular Start")) %>%
  semi_join(primary_pitches, by=c("pitcher", "pitchid")) %>%
  mutate(veloHelper=Count*avg_velo, xHelper=Count*avg_x_movement, 
         yHelper=Count*avg_y_movement, spinHelper=Count*avg_spinrate) %>%
  group_by(GoldType, PitchType, pitchid, pitch_name, at_bat) %>%
  summarise(Pitches=sum(Count), Usage=round(mean(Usage),4), 
            avgVelo=round(sum(veloHelper)/Pitches,4), 
            avgX_movement=round(sum(xHelper)/Pitches,4), 
            avgY_movement=round(sum(yHelper)/Pitches,4), 
            SpinRate=round(sum(spinHelper)/Pitches,4))

pitchmetrics_ab %>% group_by(GoldType, PitchType, pitchid, pitch_name) %>% summarise(pitches=mean(Pitches))


graphs=function(x){
  x2=pitchmetrics_ab %>% filter(at_bat<=27 & pitchid==x) %>% group_by(GoldType) %>%
    mutate(VeloMA=(avgVelo+lag(avgVelo)+lag(avgVelo,2)+lag(avgVelo,3)+lag(avgVelo,4))/5) %>% 
    mutate(VeloMA=ifelse(is.na(VeloMA),(avgVelo+lag(avgVelo)+lag(avgVelo,2)+lag(avgVelo,3))/4,VeloMA)) %>% 
    mutate(VeloMA=ifelse(is.na(VeloMA),(avgVelo+lag(avgVelo)+lag(avgVelo,2)+lead(avgVelo))/4,VeloMA)) %>% 
    mutate(VeloMA=ifelse(is.na(VeloMA),(avgVelo+lag(avgVelo)+lead(avgVelo))/3,VeloMA)) %>%
    mutate(VeloMA=ifelse(is.na(VeloMA),(avgVelo+lead(avgVelo))/2,VeloMA)) %>%
    ggplot(aes(x=at_bat, y=VeloMA, group=GoldType)) + 
    geom_line(aes(color=GoldType))
  
  y=pitchmetrics_ab %>% filter(at_bat<=27 & pitchid==x) %>% group_by(GoldType) %>%
    mutate(X_MA=(avgX_movement+lag(avgX_movement)+lag(avgX_movement,2)+lag(avgX_movement,3)+lag(avgX_movement,4))/5) %>% 
    mutate(X_MA=ifelse(is.na(X_MA),(avgX_movement+lag(avgX_movement)+lag(avgX_movement,2)+lag(avgX_movement,3))/4,X_MA)) %>% 
    mutate(X_MA=ifelse(is.na(X_MA),(avgX_movement+lag(avgX_movement)+lag(avgX_movement,2)+lead(avgX_movement))/4,X_MA)) %>% 
    mutate(X_MA=ifelse(is.na(X_MA),(avgX_movement+lag(avgX_movement)+lead(avgX_movement))/3,X_MA)) %>%
    mutate(X_MA=ifelse(is.na(X_MA),(avgX_movement+lead(avgX_movement))/2,X_MA)) %>%
    ggplot(aes(x=at_bat, y=X_MA, group=GoldType)) + 
    geom_line(aes(color=GoldType))
  
  z=pitchmetrics_ab %>% filter(at_bat<=27 & pitchid==x) %>% group_by(GoldType) %>%
    mutate(Y_MA=(avgY_movement+lag(avgY_movement)+lag(avgY_movement,2)+lag(avgY_movement,3)+lag(avgY_movement,4))/5) %>% 
    mutate(Y_MA=ifelse(is.na(Y_MA),(avgY_movement+lag(avgY_movement)+lag(avgY_movement,2)+lag(avgY_movement,3))/4,Y_MA)) %>% 
    mutate(Y_MA=ifelse(is.na(Y_MA),(avgY_movement+lag(avgY_movement)+lag(avgY_movement,2)+lead(avgY_movement))/4,Y_MA)) %>% 
    mutate(Y_MA=ifelse(is.na(Y_MA),(avgY_movement+lag(avgY_movement)+lead(avgY_movement))/3,Y_MA)) %>%
    mutate(Y_MA=ifelse(is.na(Y_MA),(avgY_movement+lead(avgY_movement))/2,Y_MA)) %>%
    ggplot(aes(x=at_bat, y=Y_MA, group=GoldType)) + 
    geom_line(aes(color=GoldType))
  
  u=pitchmetrics_ab %>% filter(at_bat<=27 & pitchid==x) %>% group_by(GoldType) %>%
    mutate(SpinRateMA=(SpinRate+lag(SpinRate)+lag(SpinRate,2)+lag(SpinRate,3)+lag(SpinRate,4))/5) %>% 
    mutate(SpinRateMA=ifelse(is.na(SpinRateMA),(SpinRate+lag(SpinRate)+lag(SpinRate,2)+lag(SpinRate,3))/4,SpinRateMA)) %>% 
    mutate(SpinRateMA=ifelse(is.na(SpinRateMA),(SpinRate+lag(SpinRate)+lag(SpinRate,2)+lead(SpinRate))/4,SpinRateMA)) %>% 
    mutate(SpinRateMA=ifelse(is.na(SpinRateMA),(SpinRate+lag(SpinRate)+lead(SpinRate))/3,SpinRateMA)) %>%
    mutate(SpinRateMA=ifelse(is.na(SpinRateMA),(SpinRate+lead(SpinRate))/2,SpinRateMA)) %>%
    ggplot(aes(x=at_bat, y=SpinRateMA, group=GoldType)) + 
    geom_line(aes(color=GoldType))
  
  u2=pitchmetrics_ab %>% filter(at_bat<=27 & pitchid==x) %>% group_by(GoldType) %>%
    mutate(UsageMA=(Usage+lag(Usage)+lag(Usage,2)+lag(Usage,3)+lag(Usage,4))/5) %>% 
    mutate(UsageMA=ifelse(is.na(UsageMA),(Usage+lag(Usage)+lag(Usage,2)+lag(Usage,3))/4,UsageMA)) %>% 
    mutate(UsageMA=ifelse(is.na(UsageMA),(Usage+lag(Usage)+lag(Usage,2)+lead(Usage))/4,UsageMA)) %>% 
    mutate(UsageMA=ifelse(is.na(UsageMA),(Usage+lag(Usage)+lead(Usage))/3,UsageMA)) %>%
    mutate(UsageMA=ifelse(is.na(UsageMA),(Usage+lead(Usage))/2,UsageMA)) %>%
    ggplot(aes(x=at_bat, y=UsageMA, group=GoldType)) + 
    geom_line(aes(color=GoldType))
  
  ggarrange(x2,u,u2,y,z,ncol=3,nrow=2)
}


list=c(594798, 545333, 453286, 519242, 572020)

list=572020

pitchmetrics_ab=at_bat_pitches %>% inner_join(plate_appearances %>% group_by(game_pk, pitcher) %>% 
                                                transmute(game_pk, pitcher, at_bat_number, at_bat=frank(at_bat_number))) %>%
  inner_join(gamelog_df %>% filter(pitcher %in% list) %>% select(game_pk, pitcher, GoldType)) %>%
  group_by(game_pk, pitcher, at_bat) %>%
  mutate(TotalPitches=sum(Count), Usage=Count/TotalPitches) %>%
  semi_join(gamelog_df %>% filter(GoldType=="High K Gold Start") %>% select(pitcher) %>% distinct()) %>%
  filter(GoldType %in% c("High K Gold Start", "Regular Start")) %>%
  semi_join(primary_pitches, by=c("pitcher", "pitchid")) %>%
  mutate(veloHelper=Count*avg_velo, xHelper=Count*avg_x_movement, 
         yHelper=Count*avg_y_movement, spinHelper=Count*avg_spinrate) %>%
  group_by(GoldType, PitchType, pitchid, pitch_name, at_bat) %>%
  summarise(Pitches=sum(Count), Usage=round(mean(Usage),4), 
            avgVelo=round(sum(veloHelper)/Pitches,4), 
            avgX_movement=round(sum(xHelper)/Pitches,4), 
            avgY_movement=round(sum(yHelper)/Pitches,4), 
            SpinRate=round(sum(spinHelper)/Pitches,4)) 



pitchmetrics_ab %>% filter(at_bat<=27) %>% group_by(GoldType) %>%
  mutate(avgX_movementMA=(avgX_movement+lag(avgX_movement)+lag(avgX_movement,2)+lag(avgX_movement,3)+lag(avgX_movement,4))/5) %>% 
  mutate(avgX_movementMA=ifelse(is.na(avgX_movementMA),(avgX_movement+lag(avgX_movement)+lag(avgX_movement,2)+lag(avgX_movement,3))/4,avgX_movementMA)) %>% 
  mutate(avgX_movementMA=ifelse(is.na(avgX_movementMA),(avgX_movement+lag(avgX_movement)+lag(avgX_movement,2)+lead(avgX_movement))/4,avgX_movementMA)) %>% 
  mutate(avgX_movementMA=ifelse(is.na(avgX_movementMA),(avgX_movement+lag(avgX_movement)+lead(avgX_movement))/3,avgX_movementMA)) %>%
  mutate(avgX_movementMA=ifelse(is.na(avgX_movementMA),(avgX_movement+lead(avgX_movement))/2,avgX_movementMA)) %>%
  ggplot(aes(x=at_bat, y=avgX_movementMA, group=GoldType)) + 
  geom_line(aes(color=GoldType)) +
  scale_x_continuous(breaks=seq(0,27,by=3)) +
  facet_wrap(~pitch_name)


pitchmetrics_ab %>% filter(at_bat<=27) %>% group_by(GoldType) %>%
  mutate(SpinRateMA=(SpinRate+lag(SpinRate)+lag(SpinRate,2)+lag(SpinRate,3)+lag(SpinRate,4))/5) %>% 
  mutate(SpinRateMA=ifelse(is.na(SpinRateMA),(SpinRate+lag(SpinRate)+lag(SpinRate,2)+lag(SpinRate,3))/4,SpinRateMA)) %>% 
  mutate(SpinRateMA=ifelse(is.na(SpinRateMA),(SpinRate+lag(SpinRate)+lag(SpinRate,2)+lead(SpinRate))/4,SpinRateMA)) %>% 
  mutate(SpinRateMA=ifelse(is.na(SpinRateMA),(SpinRate+lag(SpinRate)+lead(SpinRate))/3,SpinRateMA)) %>%
  mutate(SpinRateMA=ifelse(is.na(SpinRateMA),(SpinRate+lead(SpinRate))/2,SpinRateMA)) %>%
  ggplot(aes(x=at_bat, y=SpinRateMA, group=GoldType)) + 
  geom_line(aes(color=GoldType)) +
  scale_x_continuous(breaks=seq(0,27,by=3)) +
  facet_wrap(~pitch_name)



str(pitchmetrics_ab)




highKgold=gamelog_stagingOG %>% left_join(gamelog_df %>% select(game_pk, pitcher, GoldType)) %>%
  filter(GoldType=="High K Gold Start") %>% select(7,9:54)

others=gamelog_stagingOG %>% left_join(gamelog_df %>% select(game_pk, pitcher, GoldType)) %>%
  filter(GoldType!="High K Gold Start") %>% select(7,9:54)

lowKgold=gamelog_stagingOG %>% left_join(gamelog_df %>% select(game_pk, pitcher, GoldType)) %>%
  filter(GoldType=="Low K Gold Start") %>% select(7,9:54)

regular=gamelog_stagingOG %>% left_join(gamelog_df %>% select(game_pk, pitcher, GoldType)) %>%
  filter(GoldType=="Regular Start") %>% select(7,9:54)

bad=gamelog_stagingOG %>% left_join(gamelog_df %>% select(game_pk, pitcher, GoldType)) %>%
  filter(GoldType=="Bad Start") %>% select(7,9:54)








corr_simple <- function(data, sig=0.5){
  #convert data to numeric in order to run correlations
  #convert to factor first to keep the integrity of the data - each value will become a number rather than turn into NA
  df_cor <- data %>% mutate_if(is.character, as.factor)
  df_cor <- df_cor %>% mutate_if(is.factor, as.numeric)
  #run a correlation and drop the insignificant ones
  corr <- cor(df_cor)
  #prepare to drop duplicates and correlations of 1     
  corr[lower.tri(corr,diag=TRUE)] <- NA 
  #drop perfect correlations
  corr[corr == 1] <- NA 
  #turn into a 3-column table
  corr <- as.data.frame(as.table(corr))
  #remove the NA values from above 
  corr <- na.omit(corr) 
  #select significant values  
  corr <- subset(corr, abs(Freq) > sig) 
  #sort by highest correlation
  corr <- corr[order(-abs(corr$Freq)),] 
  #print table
  print(corr)
  #turn corr back into matrix in order to plot with corrplot
  mtx_corr <- reshape2::acast(corr, Var1~Var2, value.var="Freq")
  
  #plot correlations visually
  corrplot(mtx_corr, is.corr=FALSE, tl.col="black", na.label=" ")
}
corr_simple(highKgold)
corr_simple(lowKgold)
corr_simple(regular)
corr_simple(bad)




cor(highKgold$wOBA, highKgold$avg_velo_primary_TTO1_Diff)
cor(lowKgold$wOBA, lowKgold$avg_velo_primary_TTO1_Diff)
cor(bad$wOBA, bad$avg_velo_primary_TTO1_Diff)
gamelog_stagingOG %>% left_join(gamelog_df %>% select(game_pk, pitcher, GoldType)) %>%
  filter(GoldType!="Regular Start") %>%
  ggplot(aes(x=avg_velo_primary_TTO1_Diff, y=wOBA, group=GoldType)) + 
  geom_point(aes(color=GoldType, shape=GoldType, size=GoldType)) +
  ##coord_cartesian(xlim=c(-1, 1), ylim=c(0, .75)) + 
  theme(legend.position="bottom") + 
  scale_size_manual(values=c(4,2.5,1)) + 
  scale_shape_manual(values=c(16,15,6)) + 
  labs(x="Percentage Difference in Velocity on Primary Pitch", 
       title="Primary Pitch Velocity vs wOBA", 
       caption="% Difference calculated by comparing average velocity during the first 9 batters
       of the game with that pitcher's average velocity across the entire sample")



cor(highKgold$avg_velo_primary_TTO2_Gap, highKgold$PrimaryUsage_TTO2_Gap)
cor(lowKgold$PrimaryUsage_TTO2_Gap, lowKgold$avg_velo_primary_TTO2_Gap)
cor(bad$PrimaryUsage_TTO2_Gap, bad$avg_velo_primary_TTO2_Gap)
gamelog_stagingOG %>% left_join(gamelog_df %>% select(game_pk, pitcher, GoldType)) %>%
  filter(GoldType!="Regular Start") %>%
  ggplot(aes(x=avg_velo_primary_TTO2_Gap, y=PrimaryUsage_TTO2_Gap, group=GoldType)) + 
  geom_point(aes(color=GoldType, shape=GoldType, size=GoldType)) +
  ##coord_cartesian(xlim=c(-1, 1), ylim=c(0, .75)) + 
  theme(legend.position="bottom") + 
  scale_size_manual(values=c(4,2.5,1)) + 
  scale_shape_manual(values=c(16,15,6))  + 
  labs(x="Percentage Difference in Velocity on Primary Pitch", y="Percentage Difference in Usage on Primary Pitch",
       title="Average Velocity vs Usage on Primary Pitch", 
       caption="% Difference calculated by comparing metrics during the first 9 batters
       of the game with that pitcher's metrics during the second 9 batters")


cor(highKgold$WhiffsperSwing_primaryTTO2, highKgold$avg_x_movement_primary_TTO1_Diff)
cor(lowKgold$WhiffsperSwing_primaryTTO2, lowKgold$avg_x_movement_primary_TTO1_Diff)
cor(bad$WhiffsperSwing_primaryTTO2, bad$avg_x_movement_primary_TTO1_Diff)
gamelog_stagingOG %>% left_join(gamelog_df %>% select(game_pk, pitcher, GoldType)) %>%
  filter(GoldType!="Regular Start") %>%
  ggplot(aes(x=avg_x_movement_primary_TTO1_Diff, y=WhiffsperSwing_primaryTTO2, group=GoldType)) + 
  geom_point(aes(color=GoldType, shape=GoldType, size=GoldType)) +
  coord_cartesian(xlim=c(-1, 1), ylim=c(0, .75)) + 
  theme(legend.position="bottom") + 
  scale_size_manual(values=c(4,2.5,1)) + 
  scale_shape_manual(values=c(16,15,6)) + 
  labs(x="Percentage Difference in X Movement on Primary Pitch", y="Percentage Difference in Whiffs per Swing on Primary Pitch",
       title="Whiffs per Swing during the Second Time Through the Order 
       vs X_Movement during the First Time Through the Order", 
       caption="% Difference calculated for Whiffs per Swing by comparing the first 9 batters with the 
       second 9 batters. % Difference calculated forX_Movement by comparing average X_Movement 
       during the first9 batters with that pitcher's average X_Movement across the entire sample")





##Correlation for both good and bad starts
##one negative, one positive
cor(highKgold$PitchesAB_TTO1_Diff, highKgold$PrimaryUsage_TTO1_Diff)
cor(lowKgold$PitchesAB_TTO1_Diff, lowKgold$PrimaryUsage_TTO1_Diff)
cor(regular$PitchesAB_TTO1_Diff, regular$PrimaryUsage_TTO1_Diff)
cor(bad$PitchesAB_TTO1_Diff, bad$PrimaryUsage_TTO1_Diff)





##Correlation for both good and bad starts
##one negative, one positive
cor(highKgold$PitchesAB_TTO1_Diff, highKgold$PrimaryUsage_TTO1_Diff)
cor(lowKgold$PitchesAB_TTO1_Diff, lowKgold$PrimaryUsage_TTO1_Diff)
cor(regular$PitchesAB_TTO1_Diff, regular$PrimaryUsage_TTO1_Diff)
cor(bad$PitchesAB_TTO1_Diff, bad$PrimaryUsage_TTO1_Diff)















##Comparing High K starts to Regular Starts
pitchmetrics_ab=at_bat_pitches %>% inner_join(plate_appearances %>% group_by(game_pk, pitcher) %>% 
                                                transmute(game_pk, pitcher, at_bat_number, at_bat=frank(at_bat_number))) %>%
  inner_join(gamelog_df %>% select(game_pk, pitcher, GoldType)) %>%
  group_by(game_pk, pitcher, at_bat) %>%
  mutate(TotalPitches=sum(Count), Usage=Count/TotalPitches) %>%
  semi_join(gamelog_df %>% filter(GoldType=="High K Gold Start") %>% select(pitcher) %>% distinct()) %>%
  filter(GoldType %in% c("High K Gold Start", "Regular Start")) %>%
  semi_join(primary_pitches, by=c("pitcher", "pitchid")) %>%
  mutate(veloHelper=Count*avg_velo, xHelper=Count*avg_x_movement, 
         yHelper=Count*avg_y_movement, spinHelper=Count*avg_spinrate) %>%
  group_by(GoldType, PitchType, pitchid, pitch_name, at_bat) %>%
  summarise(Pitches=sum(Count), Usage=round(mean(Usage),4), 
            avgVelo=round(sum(veloHelper)/Pitches,4), 
            avgX_movement=round(sum(xHelper)/Pitches,4), 
            avgY_movement=round(sum(yHelper)/Pitches,4), 
            SpinRate=round(sum(spinHelper)/Pitches,4))
pitchmetrics_ab %>% group_by(GoldType, pitchid, pitch_name) %>% summarise(pitches=mean(Pitches))

graphs(1)
##Four Seam Fastball
##Velocity trends up for good starts
##SpinRate trends up for good starts
graphs(2)
##Big Negative Break on slider at first before both converge around 0
graphs(8)
##Knuckle Curve Velocity trends up






##Comparing Low K Good Starts to Regular Starts
pitchmetrics_ab=at_bat_pitches %>% inner_join(plate_appearances %>% group_by(game_pk, pitcher) %>% 
                                                transmute(game_pk, pitcher, at_bat_number, at_bat=frank(at_bat_number))) %>%
  inner_join(gamelog_df %>% select(game_pk, pitcher, GoldType)) %>%
  group_by(game_pk, pitcher, at_bat) %>%
  mutate(TotalPitches=sum(Count), Usage=Count/TotalPitches) %>%
  semi_join(gamelog_df %>% filter(GoldType=="Low K Gold Start") %>% select(pitcher) %>% distinct()) %>%
  filter(GoldType %in% c("Low K Gold Start", "Regular Start")) %>%
  semi_join(primary_pitches, by=c("pitcher", "pitchid")) %>%
  mutate(veloHelper=Count*avg_velo, xHelper=Count*avg_x_movement, 
         yHelper=Count*avg_y_movement, spinHelper=Count*avg_spinrate) %>%
  group_by(GoldType, PitchType, pitchid, pitch_name, at_bat) %>%
  summarise(Pitches=sum(Count), Usage=round(mean(Usage),4), 
            avgVelo=round(sum(veloHelper)/Pitches,4), 
            avgX_movement=round(sum(xHelper)/Pitches,4), 
            avgY_movement=round(sum(yHelper)/Pitches,4), 
            SpinRate=round(sum(spinHelper)/Pitches,4))
pitchmetrics_ab %>% group_by(GoldType, pitchid, pitch_name) %>% summarise(pitches=mean(Pitches))

graphs(1)
##Not much separates fastball between starts
graphs(2)
##X Movement for Sliders start in opposite directions and converge to 0
graphs(3)
##Big Increase in Low K Gold Starts for 2 Seamer Velocity and spin rate
graphs(4)
##Trend up for Velocity, Spinrate, and usage for changeup
##Low K Gold Starts start with good x break off the bat while it takes
##bad starts awhile to build up
graphs(7)
##Movement oscilates between negative and positve for good starts while
##remaining constant for regular starts for cutter


##Comparing Bad Starts to Regular Starts
pitchmetrics_ab=at_bat_pitches %>% inner_join(plate_appearances %>% group_by(game_pk, pitcher) %>% 
                                                transmute(game_pk, pitcher, at_bat_number, at_bat=frank(at_bat_number))) %>%
  inner_join(gamelog_df %>% select(game_pk, pitcher, GoldType)) %>%
  group_by(game_pk, pitcher, at_bat) %>%
  mutate(TotalPitches=sum(Count), Usage=Count/TotalPitches) %>%
  semi_join(gamelog_df %>% filter(GoldType=="Bad Start") %>% select(pitcher) %>% distinct()) %>%
  filter(GoldType %in% c("Bad Start", "Regular Start")) %>%
  semi_join(primary_pitches, by=c("pitcher", "pitchid")) %>%
  mutate(veloHelper=Count*avg_velo, xHelper=Count*avg_x_movement, 
         yHelper=Count*avg_y_movement, spinHelper=Count*avg_spinrate) %>%
  group_by(GoldType, PitchType, pitchid, pitch_name, at_bat) %>%
  summarise(Pitches=sum(Count), Usage=round(mean(Usage),4), 
            avgVelo=round(sum(veloHelper)/Pitches,4), 
            avgX_movement=round(sum(xHelper)/Pitches,4), 
            avgY_movement=round(sum(yHelper)/Pitches,4), 
            SpinRate=round(sum(spinHelper)/Pitches,4))
pitchmetrics_ab %>% group_by(GoldType, pitchid, pitch_name) %>% summarise(pitches=mean(Pitches))

graphs(1)
##Fastball 
##Velocity dips more for bad starts in velocity and spin rate
graphs(2)
##Slider
##Big Trend down at the beginning for velocity for bad starts
##Lots of movement changes for bad starts
graphs(5)
##Curveball
##Bad Starts start with little x axis break
graphs(7) 
##Cutter has starts in opposite sides for x axis break before converging to 0
graphs(8)
##Consistent trend down in velocity for bad starts in knuckle curve



###Include slider velocity

TTO_pitchtable$TTO=as.factor(TTO_pitchtable$TTO)

##Whiffs per Swing
TTO_pitchtable %>% filter(pitchid %in% c(1,2,3,4,5) & GoldType=="Bad Start") %>% 
  mutate(Metric="WhiffsperSwing") %>% select(1:5,15,WhiffsperSwing) %>% filter(TTO!=4) %>%
  ggplot(aes(fill=TTO, x=pitch_name, y=WhiffsperSwing)) + 
  geom_bar(stat="identity", aes(fill=TTO), position="dodge")


TTO_pitchtable %>% filter(pitchid %in% c(1,2,3,4,5) & GoldType=="Bad Start") %>% 
  mutate(Metric="Usage") %>% select(1:5,15,Usage) %>% filter(TTO!=4) %>%
  ggplot(aes(fill=TTO, x=pitch_name, y=Usage)) + 
  geom_bar(stat="identity", aes(fill=TTO), position="dodge") + 
  coord_cartesian(ylim=(c(0.2,0.7)))


##Don't use for bad starts
TTO_pitchtable %>% filter(pitchid %in% c(1,2,3,4,5) & GoldType=="Bad Start") %>% 
  mutate(Metric="StrikePercentage") %>% select(1:5,15,StrikePercentage) %>% filter(TTO!=4) %>%
  ggplot(aes(fill=TTO, x=pitch_name, y=StrikePercentage)) + 
  geom_bar(stat="identity", aes(fill=TTO), position="dodge") + 
  coord_cartesian(ylim=(c(0.55,0.7)))



TTO_pitchtable %>% filter(pitchid %in% c(1,2,3,4,5) & GoldType=="Bad Start") %>% 
  mutate(Metric="avgVelo") %>% select(1:5,15,avgVelo) %>% filter(TTO!=4) %>%
  ggplot(aes(fill=TTO, x=pitch_name, y=avgVelo)) + 
  geom_bar(stat="identity", aes(fill=TTO), position="dodge") + 
  coord_cartesian(ylim=(c(70,95)))


TTO_pitchtable$pitch_name=factor(TTO_pitchtable$pitch_name,
                                 levels=c("Curveball", "Slider", 
                                          "4-Seam Fastball", "2-Seam Fastball", 
                                          "Changeup"))
TTO_pitchtable$GoldType=factor(TTO_pitchtable$GoldType,
                               levels=c("High K Gold Start", "Low K Gold Start", 
                                        "Regular Start", "Bad Start"))

x=TTO_pitchtable %>% filter(pitchid %in% c(1,2,3,4,5) & GoldType=="High K Gold Start") %>% 
  mutate(Metric="avgSpinRate") %>% select(1:5,15,SpinRate) %>% filter(TTO!=4) %>%
  ggplot(aes(fill=TTO, x=pitch_name, y=SpinRate)) + 
  geom_bar(stat="identity", aes(fill=TTO), color="black", position="dodge") + 
  coord_cartesian(ylim=(c(1500,2750))) + 
  labs(title="High Strikeout Gold Standard Starts", x="Pitch Name", y="Spin Rate")

y=TTO_pitchtable %>% filter(pitchid %in% c(1,2,3,4,5) & GoldType=="Low K Gold Start") %>% 
  mutate(Metric="avgSpinRate") %>% select(1:5,15,SpinRate) %>% filter(TTO!=4) %>%
  ggplot(aes(fill=TTO, x=pitch_name, y=SpinRate)) + 
  geom_bar(stat="identity", aes(fill=TTO), color="black", position="dodge") + 
  coord_cartesian(ylim=(c(1500,2750))) + 
  labs(title="Low Strikeout Gold Standard Starts", x="Pitch Name", y="Spin Rate")

z=TTO_pitchtable %>% filter(pitchid %in% c(1,2,3,4,5) & GoldType=="Regular Start") %>% 
  mutate(Metric="avgSpinRate") %>% select(1:5,15,SpinRate) %>% filter(TTO!=4) %>%
  ggplot(aes(fill=TTO, x=pitch_name, y=SpinRate)) + 
  geom_bar(stat="identity", aes(fill=TTO), color="black", position="dodge") + 
  coord_cartesian(ylim=(c(1500,2750))) + 
  labs(title="Regular Starts", x="Pitch Name", y="Spin Rate")

u=TTO_pitchtable %>% filter(pitchid %in% c(1,2,3,4,5) & GoldType=="Bad Start") %>% 
  mutate(Metric="avgSpinRate") %>% select(1:5,15,SpinRate) %>% filter(TTO!=4) %>%
  ggplot(aes(fill=TTO, x=pitch_name, y=SpinRate)) + 
  geom_bar(stat="identity", aes(fill=TTO), color="black", position="dodge") + 
  coord_cartesian(ylim=(c(1500,2750))) + 
  labs(title="Bad Starts", x="Pitch Name", y="Spin Rate")

ggarrange(x,y,z,u, ncol=2, nrow=2)


##Spin Rate
TTO_pitchtable$pitch_name=factor(TTO_pitchtable$pitch_name,
                                 levels=c("Curveball", "Slider", 
                                          "4-Seam Fastball", "2-Seam Fastball", 
                                          "Changeup"))
TTO_pitchtable %>% filter(pitchid %in% c(1,2,3,4,5)) %>% 
  mutate(Metric="avgSpinRate") %>% select(1:5,15,SpinRate) %>% filter(TTO!=4) %>%
  ggplot(aes(fill=TTO, x=pitch_name, y=SpinRate)) + 
  geom_bar(stat="identity", aes(fill=TTO), color="black", position="dodge") + 
  coord_cartesian(ylim=(c(1500,2750))) + 
  labs(title="Spin Rate by Pitch", x="Pitch Name", y="Spin Rate (rpm)") +
  facet_wrap(~GoldType) + 
  theme(legend.position="bottom")

##Average Velocity
TTO_pitchtable$pitch_name=factor(TTO_pitchtable$pitch_name,
                                 levels=c("4-Seam Fastball", "2-Seam Fastball", 
                                          "Slider", "Changeup", "Curveball"))
TTO_pitchtable %>% filter(pitchid %in% c(1,2,3,4,5)) %>% 
  mutate(Metric="avgVelo") %>% select(1:5,15,avgVelo) %>% filter(TTO!=4) %>%
  ggplot(aes(fill=TTO, x=pitch_name, y=avgVelo)) + 
  geom_bar(stat="identity", aes(fill=TTO), color="black", position="dodge") + 
  coord_cartesian(ylim=(c(75,95))) + 
  labs(title="Average Velocity by Pitch", x="Pitch Name", y="Velocity (mph)") +
  facet_wrap(~GoldType) + 
  theme(legend.position="bottom")



TTO_pitchtable$pitch_name=factor(TTO_pitchtable$pitch_name,
                                 levels=c("4-Seam Fastball", "2-Seam Fastball", 
                                          "Slider", "Changeup", "Curveball"))
TTO_pitchtable %>% filter(pitchid %in% c(1,2,3,4,5)) %>% 
  mutate(Metric="WhiffsperSwing") %>% select(1:5,15,WhiffsperSwing) %>% filter(TTO!=4) %>%
  ggplot(aes(fill=TTO, x=pitch_name, y=WhiffsperSwing)) + 
  geom_bar(stat="identity", aes(fill=TTO), color="black", position="dodge") + 
  labs(title="Whiffs per Swing by Pitch", x="Pitch Name", y="Whiffs per Swing") +
  facet_wrap(~GoldType) + 
  theme(legend.position="bottom") + 
  scale_y_continuous(breaks=seq(0,1,by = .1), limits=c(0,1), 
                     labels=number_format(accuracy=0.01)) 













plate_appearances %>% inner_join(gamelog_df %>% select(game_pk, pitcher, GoldType)) %>% 
  group_by(GoldType, TTO) %>% mutate(TTO=as.factor(TTO)) %>%
  summarise(wOBA=sum(wOBA_points)/sum(wOBA_denom)) %>%
  ggplot(aes(x=GoldType, y=wOBA, group=TTO)) +
  geom_bar(stat="identity", aes(fill=TTO), color="black", position="dodge") + 
  labs(title="Time Through Order (wOBA)", x="Pitch Name", y="wOBA") + 
  scale_y_continuous(breaks=seq(0,.6,by = .05), limits=c(0,.6), 
                     labels=number_format(accuracy=0.001)) + 
  theme(legend.position="bottom")

