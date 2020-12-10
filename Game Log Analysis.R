##install.packages("tidyverse");
library(tidyverse);
##install.packages("DBI"):
library(DBI);
##install.packages("lubridate"):
library(lubridate);
##install.packages("tidyquant");
library(tidyquant);
##install.packages("purrr"); 
library(purrr);
##install.packages("cluster");
library(cluster);

setwd("~/Github/Times-Through-Order");

##Connection to AWS Database
con=dbConnect(RMySQL::MySQL(), dbname="zgreer_baseball",
              host="zgreer-baseball.cqridrsjfpub.us-east-2.rds.amazonaws.com",
              port=3306, user="guest", password="Password1!");
##dbListTables(con);
##dbDisconnect(con);

##Reading in pitching game log table
tbl_gamelog_df=tbl(con, "gamelog_df");
gamelog_df=collect(tbl_gamelog_df) %>% readr::type_convert();
rm(tbl_gamelog_df);

##Dividing to count of games by Date
game_cnt=gamelog_df %>% group_by(Date) %>% count(Date) %>% 
  transmute(game_year=substr(Date, 1,4), Date, games=n);


##Creating a dataframe of dates for graphing ease
##Function to filter and find min and max dates
filter_gamelog=function(x){game_cnt %>% 
    filter(game_year==x) %>% group_by(game_year) %>% 
    mutate(min_date=min(Date), max_date=max(Date))};
##Binded the output together to determine starting point
##and stopping point so all years are the same
##rbind(filter_gamelog(2019), filter_gamelog(2018), filter_gamelog(2017)) %>%  
##group_by(game_year) %>% summarise(min_date, max_date) %>% distinct();
##Binding the sequence for each year together
Date=seq(ymd("2017-03-28"),ymd("2017-10-01"), by="day");
date_df=as.data.frame(Date);
Date=seq(ymd("2018-03-28"),ymd("2018-10-01"), by="day");
date_df=rbind(date_df, as.data.frame(Date));
Date=seq(ymd("2019-03-28"),ymd("2019-10-01"), by="day");
date_df=rbind(date_df, as.data.frame(Date)) %>% 
  transmute(game_year=substr(Date, 1,4), Date);

##Joining with the count of games dataframe
daily_game_cnt=date_df %>% left_join(game_cnt) %>% 
  mutate(games=ifelse(is.na(games),0,games), 
         Date_no_year=substr(Date,6,10));
rm(Date, date_df, filter_gamelog, game_cnt);

##Reducing to weekly data
weekly_game_cnt=daily_game_cnt %>% 
  tq_transmute(select=games, mutate_fun=apply.weekly, FUN=sum) %>% 
  mutate(game_year=substr(Date, 1,4), Date_no_year=substr(Date,6,10)) %>%
  group_by(game_year) %>% 
  mutate(games3wkavg=(games+lag(games)+lag(games, n=2))/3) %>%
  drop_na();

##Creating a label for x axis
x_label=format(seq(ymd("2019-04-15"), ymd("2019-10-01"), 
                   by="weeks"), format="%m-%d");

##3 Week Moving Average Graph grouped by year
ggplot(weekly_game_cnt, aes(x=Date_no_year, y=games3wkavg, group=game_year)) + 
  geom_line(aes(color=game_year)) +
  scale_x_discrete(breaks=x_label) + 
  scale_y_continuous(breaks=seq(15,60,5)) + 
  theme_minimal() + 
  theme(axis.text.x=element_text(angle=45, hjust=1), legend.position="bottom", 
        plot.title=element_text(hjust=0.5, size=15), 
        plot.subtitle=element_text(hjust=0.5, face="italic", size=10)) + 
  labs(title="Starts With 27 or More Batters Faced", x="Week Start", 
       y="Starts per Week", subtitle="Three Week Moving Averages", 
       color="Game Year");
rm(daily_game_cnt, weekly_game_cnt, x_label);


##Calculating Rate Metrics
gamelog_df=gamelog_df %>% 
  mutate(WHIP=round((BB+H)/Innings,2), HRper9=round(9*HR/Innings,2), 
         Kper9=round(9*K/Innings,2), BBper9=round(9*BB/Innings, 2), 
         Year=year(Date));

##Reading in statcast pitches table
pitch_df_tbl=tbl(con, "pitch_df");
pitch_df=collect(pitch_df_tbl) %>% readr::type_convert();
rm(pitch_df_tbl);


##Calculating Pitch Count and Strike % and Joining to 
balls=c("ball", "blocked_ball", "hit_by_pitch", "pitchout");
not_swings=c(balls, "called_strike");
swings=setdiff(unique(pitch_df$description), not_swings);
pitch_df=pitch_df %>%
  mutate(Ball=ifelse(description %in% balls, 1, 0),
         Strikes=ifelse(!(description %in% balls), 1, 0),
         Swings=ifelse(description %in% swings, 1, 0), 
         Whiff=ifelse(description %in% c("swinging_strike", 
                        "swinging_strike_blocked", "missed_bunt"), 1, 0));
rm(balls, not_swings, swings); 
Strike_Details=pitch_df %>% group_by(game_pk, pitcher) %>% 
  summarise(PitchCnt=n(), StrikePercent=sum(Strikes)/PitchCnt, 
            Swings=sum(Swings), WhiffsperSwing=sum(Whiff)/sum(Swings));
gamelog_df=gamelog_df %>% left_join(Strike_Details);


##Reducing dataframe to variables I want to include for analysis
reduced_df=as.data.frame(gamelog_df %>% 
                mutate(row_name=paste(game_pk, pitcher, pitcher_team)) %>% 
  select(row_name, ERA, TBF, Innings, PitchCnt, StrikePercent, WhiffsperSwing, 
         HRper9, Kper9, BBper9, WHIP, BABIP, wOBAg));
row.names(reduced_df)=reduced_df$row_name;
reduced_df=reduced_df %>% select(-row_name);


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
##Elbow at 2
sil_width=map_dbl(2:10, function(k){
  model=pam(x=reduced_df, k=k)
  model$silinfo$avg.width});
sil_df=data.frame(k=2:10, sil_width=sil_width);
ggplot(sil_df, aes(x=k, y=sil_width)) + geom_line() +
  scale_x_continuous(breaks=2:10);
##Highest silhouette width is at 2
##cluster starts into 2 groups
model=kmeans(reduced_df, centers=2);
gamelog_df=cbind(gamelog_df, model$cluster) %>% 
  rename("cluster"="model$cluster", "mlbTeam_pitcher"="pitcher_team") %>% 
  mutate(classified=ifelse(cluster==1, "Good Start", "Bad Start"), GS=1);
rm(elbow_df, model, sil_df, sil_width, tot_withinss, reduced_df);
##Summarizing the clusters
cluster_summary=gamelog_df %>% group_by(classified) %>% 
  summarise(Count=n(), Win_Rate=sum(W)/(sum(W)+sum(L)), ERA=mean(ERA), 
        TBF=mean(TBF), Innings=mean(Innings), Pitches=mean(PitchCnt), 
        StrikePercent=weighted.mean(StrikePercent, PitchCnt), 
        WhiffsperSwing=weighted.mean(WhiffsperSwing, Swings), WHIP=mean(WHIP), 
        HRper9=mean(HRper9), Kper9=mean(Kper9), BBper9=mean(BBper9), 
        BABIP=mean(BABIP),FIP=mean(FIP));

##write.csv(gamelog_df, "gamelog_df.csv", col.names=TRUE, row.names=FALSE);

##dbRemoveTable(con, "clustered_gamelogs");
##dbCreateTable(con, "clustered_gamelogs", gamelog_df, row.names=NULL);
##dbWriteTable(con, "clustered_gamelogs", gamelog_df, row.names=FALSE, append=TRUE);

dbDisconnect(con);


