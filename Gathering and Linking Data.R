##install.packages("tidyverse");
library(tidyverse);
##install.packages("devtools")
##devtools::install_github("BillPetti/baseballr")
library(baseballr);
##install.packages("DBI"):
library(DBI);
##install.packages("lubridate"):
library(lubridate);
##install.packages("data.table"):
library(data.table);
##install.packages("reclin");
library(reclin);

##Connection to AWS Database
con=dbConnect(RMySQL::MySQL(), dbname="zgreer_baseball",
              host="zgreer-baseball.cqridrsjfpub.us-east-2.rds.amazonaws.com",
              port=3306, user="guest", password="Password1!");
##dbListTables(con);
##dbDisconnect(con);


##This section looks at a table of scraped pitching game logs and pulls starts
##where pitchers pitched to 27 batters or more
variables=c("fangraphsID","Date","Team","Opp","GS","W","L","SV","HLD","IP",
            "TBF","H","R","ER","HR","BB","SO","BABIP","ERA","FIP","xFIP","GSv2");
##Reading in pitching game log table
tbl_games_pitched=tbl(con, "tbl_games_pitched");
twentyseven_df=collect(tbl_games_pitched %>% select(all_of(variables)) %>%  
                         filter(TBF>=27 & Date>"2017-01-01" & GS==1)) %>%
  readr::type_convert();
rm(variables, tbl_games_pitched);
##str(twentyseven_df);


##This section looks at a table made of appearances built from Retrosheet data,
##limiting to pitchers 2017-2019 and creating a Date variable
##Reading in players table
tbl_players=tbl(con, "tbl_players");
players_df=collect(tbl_players %>% filter(fieldPos==1)) %>%
  readr::type_convert() %>% filter(substr(game_id, 4, 7)>=2017) %>%
  mutate(Date=ymd(paste(substr(game_id, 4, 7), "-", substr(game_id, 8, 9),
                        "-", substr(game_id, 10, 11), sep="")));


##This section pulls player_ids from the fangraphs game log data to match with
##other MLB data sources and then joins the dataframe containing starts with
##27 or more batters faced to the players dataframe from retrosheet
##

##This function uses the playername_lookup function and creates a dataframe
##from a vector of players
pull_ids=function(x) {player_ids=lapply(unique(x), playername_lookup);
data.frame(cbind(sapply(player_ids, '[[', 1),
                 sapply(player_ids, '[[', 2),
                 sapply(player_ids, '[[', 8),
                 sapply(player_ids, '[[', 9),
                 sapply(player_ids, '[[', 10),
                 sapply(player_ids, '[[', 11))) %>%
  rename(first_name=X1, last_name=X2, mlbID=X3, retroID=X4, brefID=X5,
         fangraphsID=X6) %>% transmute(name=paste(first_name,last_name), mlbID,
                                       retroID, brefID,
                                       fangraphsID=as.numeric(fangraphsID))};


##Assigning the output and removing the table that the playername_lookup
##function leaves
player_ids=pull_ids(twentyseven_df$fangraphsID);
rm(chadwick_player_lu_table);
##Joining Fangraphs gamelog to retrosheet appearances
twentyseven_df=twentyseven_df %>% inner_join(player_ids,by=c("fangraphsID")) %>%
  inner_join(players_df %>% select(game_id, start, retroID, team, Date),
             by=c("retroID", "Date"));
rm(player_ids, players_df);

##Pulling in play by play from games
game_ids=unique(twentyseven_df$game_id);
tbl_rawpbp=tbl(con, "tbl_rawpbp");
games_df=collect(tbl_rawpbp %>% filter(game_id %in% game_ids)) %>%
  readr::type_convert() %>% distinct();
rm(tbl_rawpbp);

##Reading in player appearances from retrosheet to label players in play by play
players_df=collect(tbl_players %>% filter(game_id %in% game_ids)) %>% 
  readr::type_convert() %>% mutate(Date=ymd(paste(substr(game_id,4,7), "-",
                  substr(game_id,8,9), "-", substr(game_id,10,11),sep="")));
players_df$team=as.numeric(players_df$team);
rm(tbl_players, game_ids);

##Dividing players_df for joins
starters=players_df %>% filter (start=="start");
subs=players_df %>% filter (start=="sub");
rm(players_df);

##Pulling plate appearances by starters
starts_marked=games_df %>% inner_join(starters) %>% 
                           distinct(game_id, inning, team, retroID, count, 
                                    pitches, play, start, name, batPos);
subs_df=games_df %>% anti_join(starts_marked);
rm(starters);

##Pulling plate appearances by subs
subs_marked=subs_df %>% inner_join(subs);
duplicated=subs_marked %>% group_by(game_id, inning, team, retroID, count,
                                    pitches,play)%>%count(retroID)%>%filter(n!=1);
distinct_subs=subs_marked %>% semi_join(duplicated) %>% 
  distinct(game_id, inning, team, retroID, count, pitches, play, start, name, batPos);
subs_marked=rbind(subs_marked %>% anti_join(duplicated) %>% 
                    select(-c(fieldPos, Date)), distinct_subs);
rm(duplicated, distinct_subs, subs, subs_df, games_df);

##Concatenating starts 
games_df=rbind(starts_marked, subs_marked);
rm(starts_marked, subs_marked);

##Creating a new variable to join pitchers with batters
twentyseven_df=twentyseven_df %>%
  rename(pitcher_name=name, pitcher_retro=retroID) %>%
  mutate(join_team=ifelse(team==0, 1,0)) %>% select(-c(start, brefID));

##Cleaning Data
plate_appearances=games_df %>% 
  inner_join(twentyseven_df %>% 
               transmute(Date, game_id, pitcher=mlbID, pitcher_retro, 
                         pitcher_team=Team,batter_team=str_remove_all(Opp, "@"), 
                         team=join_team,pitcher_name),by=c("game_id","team")) %>%
  filter(play!="NP") %>% 
  transmute(game_id, Date, pitcher_team, pitcher, pitcher_retro, pitcher_name, 
            pitcher_team, batter_retro=retroID, batter_name=name, 
            batter_team, start, batPos, inning, count, pitches, play);
gamelog_df=twentyseven_df  %>%
  transmute(game_id, Date, pitcher=mlbID,pitcher_retro,pitcher_fgID=fangraphsID, 
            pitcher_name, pitcher_team=Team,batter_team=str_remove_all(Opp,"@"),
            W, L, Innings=round(ifelse(is.na(as.numeric(substr(IP,3,3))),
                                       (as.numeric(substr(IP,1,1))),
                                       ((as.numeric(substr(IP,1,1))) + 
                                          (as.numeric(substr(IP,3,3))/3))),2), 
            TBF, H, R, ER, HR, BB, K=SO, BABIP, ERA, FIP, xFIP);
rm(twentyseven_df, games_df);


##Reading in statcast game IDs
tbl_game_pk=tbl(con, "tbl_game_pk");
game_pk_df=collect(tbl_game_pk %>% filter(game_year>=2017) %>%
                     transmute(game_pk,Date=game_date,home_team,away_team)) %>%
  readr::type_convert();
rm(tbl_game_pk);

##Creating a dataframe to join because abbreviations are different for some
team_join=as.data.frame(
  cbind(retro_team=c("SEA", "CLE", "TEX", "CHC", "MIN", "KCR", "TBR", "HOU",
                        "LAA", "OAK", "CHW", "ARI", "SFG", "NYM", "COL", "MIL",
                        "SDP", "CIN", "MIA", "ATL", "WSN", "PIT", "PHI", "BAL",
                        "BOS", "DET", "TOR", "LAD", "NYY", "STL"),
        statcast_team=c("SEA", "CLE", "TEX", "CHC", "MIN", "KC", "TB", "HOU", 
                          "LAA", "OAK", "CWS", "ARI", "SF", "NYM", "COL", "MIL", 
                          "SD", "CIN", "MIA", "ATL", "WSH", "PIT", "PHI", "BAL",
                          "BOS", "DET", "TOR", "LAD", "NYY", "STL")));

##Adding the statcast team abbreviation to games_df
gamelog_df=gamelog_df %>%
  inner_join(team_join, by=c("pitcher_team"="retro_team")) %>%
  rename(join_team=statcast_team);

game_pk_group=function() {
  rbind(game_pk_df %>% select(Date, game_pk, join_team=home_team), 
        game_pk_df %>% select(Date, game_pk, join_team=away_team))};

##Pulling double_headers
double_headers=gamelog_df %>% 
  inner_join(gamelog_df %>% select(Date,join_team) %>% distinct() %>% 
  inner_join(game_pk_group()) %>% count(Date, join_team) %>% filter(n!=1) %>% 
    select(-n)) %>% inner_join(game_pk_group());
##Removing double header games and joining to get game_pk
gamelog_df=rbind(gamelog_df %>% anti_join(double_headers) %>%
  inner_join(game_pk_group()), double_headers);
rm(double_headers, game_pk_df, team_join);


##This section pulls player_ids from the retrosheet pbp data to match with
##other MLB data sources
##Matching dataframes
player_ids=pull_ids(plate_appearances$batter_retro);
rm(chadwick_player_lu_table);

##Labeling batters in plate_appearances dataframe
plate_appearances=plate_appearances %>% 
  left_join(player_ids %>% transmute(batter_retro=retroID, batter=mlbID)) %>% 
  select(1:6,16,7:15);

##Finding appearances grouped by inning to identify plays during atbats
multi_rows=plate_appearances %>% group_by(game_id, pitcher, batter, inning) %>%
  count(inning) %>% filter(n!=1) %>% select(-n);
##Removing rows where plays occurred mid atbat
mid_ab=plate_appearances %>% semi_join(multi_rows) %>%
  filter(str_sub(play,1,2) %in% c("SB","BK","WP","PB","DI","PO","FL","CS","OA"))
plate_appearances=plate_appearances %>% anti_join(mid_ab)
rm(multi_rows, player_ids);

##Marking Events
plate_appearances=plate_appearances %>% 
  mutate(events=ifelse(str_detect(play, "HR"), "home_run", 
                ifelse(str_detect(play, "HP"), "hit_by_pitch", 
                ifelse(str_detect(play, "TP"), "triple_play",
                ifelse(str_detect(play, "IW"), "walk",
                ifelse(str_detect(play, "DP"), "double_play",
                ifelse(str_detect(play, "SH"), "sac_bunt",
                ifelse(str_detect(play, "SF"), "sac_fly",
                ifelse(str_sub(play,1,1)=="W", "walk",
                ifelse(str_sub(play,1,1)=="K", "strikeout",
                ifelse(str_sub(play,1,1)=="S", "single",
                ifelse(str_sub(play,1,1)=="D", "double",
                ifelse(str_sub(play,1,1)=="T", "triple",
                ifelse(str_detect(play, "FC"), "fielders_choice",
                ifelse(str_detect(play, "C/E"), "interf_def",
                ifelse(str_detect(play, "FO"), "force_out",
                ifelse(str_detect(play, "FINT"), "interf_def",
                ifelse(str_detect(play, "BINT"), "interf_def",
                ifelse(str_detect(play, "POCS"), "pickoff_caught_stealing",
                ifelse(str_detect(play, "PO"), "pickoff_",
                ifelse(str_detect(play, "CS"), "caught_stealing_",
                ifelse(str_detect(play, "E"), "field_error",
                "_out"))))))))))))))))))))));

##Pulling in game_ids and SPs for game_pks to cut down doubleheaders
tbl_fxdata=tbl(con, "tbl_fxdata");
game_pk_list=unique(gamelog_df$game_pk);
pitchers=collect(tbl_fxdata %>% filter(game_pk %in% game_pk_list & inning==1) %>% 
                   select(game_pk,pitcher) %>% distinct()) %>% 
  readr::type_convert();
pitchers$pitcher=as.character(pitchers$pitcher);
gamelog_df=gamelog_df %>% semi_join(pitchers);
rm(pitchers,game_pk_list);

##Reading in pitches from games
game_pk_list=gamelog_df$game_pk;
pitch_df=collect(tbl_fxdata %>% filter(game_pk %in% game_pk_list)) %>% 
  readr::type_convert();
rm(tbl_fxdata, game_pk_list);
pitch_df$batter=as.character(pitch_df$batter);
pitch_df$pitcher=as.character(pitch_df$pitcher);


##Joining the two tables together
##Creating a dataframe of plate appearances from statcast 
##to match with the dataframe from retrosheet
statcast_abs=pitch_df %>% 
  select(game_pk,pitcher,pitcher_team,inning,batter,at_bat_number,events) %>%
  filter(events!="null") %>% distinct() %>% 
  semi_join(gamelog_df, by=c("game_pk", "pitcher_team"="join_team"));

##Limiting to pitch_df to pitches thrown by teams in question
pitch_df=pitch_df %>% semi_join(statcast_abs, by=c("game_pk", "pitcher_team"));

##Linking the two dataframes
plate_appearances=plate_appearances %>% 
  left_join(gamelog_df %>% select(game_id, game_pk, pitcher_team, join_team) %>% 
              distinct()) %>% left_join(statcast_abs, 
            by=c("game_pk", "join_team"="pitcher_team", "batter", "inning")) %>% 
  transmute(game_id, game_pk, Date, pitcher_team, starter=pitcher.x, 
            starter_retro=pitcher_retro, starter_name=pitcher_name, 
            pitcher=pitcher.y, at_bat_number, batter, batter_retro, batter_name, 
            batter_team,start,batPos,inning,count,pitches,play,events.x,events.y);

##Linking the events to choose the right at_bat ny events
unmatched=plate_appearances %>% filter(events.x!=events.y);
plate_appearances=plate_appearances %>% anti_join(unmatched) %>% 
  select(-events.x) %>% rename(events=events.y);
close_match=unmatched %>% filter(str_detect(events.y, events.x)) %>% 
  select(-events.x) %>% rename(events=events.y);
unmatched=unmatched %>% anti_join(close_match);
plate_appearances=rbind(plate_appearances, close_match);
unmatched=unmatched %>% anti_join(plate_appearances, 
                                  by=c("game_pk", "at_bat_number")) %>% 
  select(-events.x) %>% rename(events=events.y);
plate_appearances=rbind(plate_appearances, unmatched);
rm(unmatched, close_match);


##Finding duplicated plate appearances and assigning
duplicated=plate_appearances %>% 
  semi_join(plate_appearances %>% count(game_pk,at_bat_number,pitcher,batter) %>% 
              filter(n!=1)) %>% arrange(game_pk, inning, at_bat_number, batPos);
plate_appearances=plate_appearances %>% anti_join(duplicated);
##Researched these games and pulled the correct at_bat
duplicated=duplicated[c(1,4,5,7,9:15,18,19,22:27,30,31,34,35,38:41,44:47,50:59,
                        62:65,68:71,74:81,84:90,91,93,96:101,104,105,108,109,
                        112:117,120:125,128:133,136:140,143:146,148,151,152,155),]
plate_appearances=rbind(plate_appearances, duplicated);
rm(duplicated);

##Plate Appearances missing an event
plate_appearances=plate_appearances %>% 
  mutate(events=ifelse(!is.na(events),events,ifelse(str_detect(play,"W"),"walk",
                ifelse(str_detect(play, "K"),"strikeout",
                ifelse(str_detect(play,"POCS2"),"pickoff_caught_stealing_2b",
                ifelse(str_detect(play, "PO1"),"pickoff_1b","field_out"))))));

##Filtering out plate appearances that continued to the next inning
event_list=c("caught_stealing_2b", "pickoff_caught_stealing_2b", "pickoff_1b",
"caught_stealing_3b", "pickoff_2b", "caught_stealing_home", 
"pickoff_caught_stealing_3b", "pickoff_caught_stealing_home", "other_out");
continued=plate_appearances %>% filter(events %in% event_list) %>%
  mutate(inning_join=inning+1);
plate_appearances=plate_appearances %>% anti_join(continued)

##Adding the next inning/at_bat for matching purposes
continued=continued %>% left_join(plate_appearances %>% 
                               group_by(game_pk, pitcher_team, inning) %>% 
                               summarise(next_at_bat=min(at_bat_number)) %>%
                               left_join(plate_appearances %>% 
                                           transmute(game_pk, pitcher_team, 
                                                     batter, inning, 
                                                     next_at_bat=at_bat_number)), 
              by=c("game_pk", "pitcher_team", "inning_join"="inning", "batter"));

##Ranking plate appearances to identify TTO
plate_appearances=plate_appearances %>% 
  group_by(game_pk, pitcher, batter, inning) %>%
  mutate(inning_ab=frank(at_bat_number)) %>% arrange(inning_ab) %>%
  group_by(game_pk, pitcher, batter) %>% 
  mutate(TTO=frank(inning, ties.method="first")) %>% select(-inning_ab);

##Pulling plate appearances with no pitcher (intentional walks, etc)
blank_pitchers=plate_appearances %>% filter(is.na(pitcher));
blank_pitchers=blank_pitchers %>% left_join(blank_pitchers %>% 
                     group_by(game_pk, pitcher_team, inning) %>% summarise() %>%
  left_join(plate_appearances %>% 
              group_by(game_pk, pitcher_team, pitcher, inning) %>%
  filter(!is.na(pitcher)) %>% 
    summarise()), by=c("game_pk","pitcher_team","inning"));
plate_appearances=plate_appearances %>% 
  anti_join(blank_pitchers, by=c("game_pk", "at_bat_number"));

##Only one pitcher in inning matched with blank DF
one_pitcher=blank_pitchers %>% 
  semi_join(blank_pitchers %>% 
              count(game_pk, pitcher.x, batter, at_bat_number) %>% 
              filter(n==1)) %>% mutate(pitcher=pitcher.y) %>% 
  select(-c(pitcher.x, pitcher.y));
blank_pitchers=blank_pitchers %>% anti_join(one_pitcher);

##Statcast pitcher matched starter pitcher
starter_matched=blank_pitchers %>% filter(starter==pitcher.y) %>% 
  mutate(pitcher=pitcher.y) %>% select(-c(pitcher.x, pitcher.y));
blank_pitchers=blank_pitchers %>% anti_join(starter_matched);

##Labeling Relievers to select right plate appearance
blank_pitchers=blank_pitchers %>% mutate(pitcher=pitcher.y) %>%
  select(-c(pitcher.x, pitcher.y));
blank_pitchers=blank_pitchers %>% 
  left_join(pull_ids(blank_pitchers$pitcher) %>% 
              transmute(pitcher=mlbID, name)) %>% arrange(game_pk, inning);
rm(chadwick_player_lu_table);
##Manually verified with box scores
blank_pitchers=blank_pitchers[c(1,3,6,7,9,10,12,15,17,20,22,23,25,27,29,30,32,33,
                                36,37,41,43,44,46,48,49,52,55,56,58,59,62,64,66,
                                68,73,74,76,80,83,84,87,90,92,94,97,101,103,106,
                                107,109,111,114,116,118,120,123,125,127,131,133,
                                136,137,139,141,144,146,149,151,153,155,157,160,
                                161,164,166,167,169,172),];
##Concatenating plate appearances back into one dataframe
plate_appearances=rbind(plate_appearances, starter_matched, one_pitcher, 
                        blank_pitchers %>% select(-name));
rm(statcast_abs, starter_matched, one_pitcher, blank_pitchers, event_list);

##Ranking plate appearances to identify TTO
plate_appearances=plate_appearances %>% 
  group_by(game_pk, pitcher, batter, inning) %>%
  mutate(inning_ab=frank(at_bat_number)) %>% arrange(inning_ab) %>%
  group_by(game_pk, pitcher, batter) %>% 
  mutate(TTO=frank(inning, ties.method="first")) %>% 
  select(-inning_ab) %>% ungroup();

##Calculating wOBA values
plate_appearances=plate_appearances %>% 
  mutate(wOBA_points=ifelse(str_sub(pitches, -1, -1)!="V" & events=="walk", .69,
                     ifelse(events=="hit_by_pitch", .72,
                     ifelse(events=="single", .89,
                     ifelse(events=="double", 1.27,
                     ifelse(events=="triple", 1.62,
                     ifelse(events=="home_run", 2.10,0)))))),
         wOBA_denom=ifelse(str_sub(pitches, -1, -1)=="V" & events=="walk",0,
                    ifelse(events=="sac_bunt",0,
                    ifelse(events=="interf_def",0,
                    ifelse(events=="batter_interference",0,
                    ifelse(events=="sac_bunt_double_play",0,1))))));

plate_appearances$TTO=as.character(plate_appearances$TTO);
##dataframe of wOBAs grouped by time through order
TTO_wOBAs=rbind(plate_appearances %>% group_by(game_pk, pitcher, TTO) %>% 
  summarise(wOBA=sum(wOBA_points)/sum(wOBA_denom)), plate_appearances %>%
  group_by(game_pk, pitcher) %>% 
    summarise(wOBA=sum(wOBA_points)/sum(wOBA_denom)) %>% 
  transmute(game_pk, pitcher, TTO="g", wOBA));

##Function to easily join wOBA TTO with 
wOBA_group=function(x) {
TTO_wOBAs %>% filter(TTO==x) %>% select(game_pk, pitcher, wOBA)};

##Joining to gamelog_df
gamelog_df=gamelog_df %>% left_join(
wOBA_group("g") %>% transmute(game_pk, pitcher, wOBAg=wOBA)) %>% left_join(
  wOBA_group(1) %>% transmute(game_pk, pitcher, wOBA1=wOBA)) %>% left_join(
    wOBA_group(2) %>% transmute(game_pk, pitcher, wOBA2=wOBA)) %>% left_join(
      wOBA_group(3) %>% transmute(game_pk, pitcher, wOBA3=wOBA)) %>% left_join(
        wOBA_group(4) %>% transmute(game_pk, pitcher, wOBA4=wOBA));

##Selecting Variables needed from pitch_df
pitch_df=pitch_df %>% 
  transmute(game_pk, game_date, pitcher_team, pitcher, batter_team, batter, 
            inning, at_bat_number, pitch_name, description, pitch_number, 
            hit_rating=launch_speed_angle, velocity=release_speed, 
            x_movement=pfx_x, y_movement=pfx_z, spinrate=release_spin_rate);



##Writing tables to AWS Database
##dbRemoveTable(con, "gamelog_df");
##dbCreateTable(con, "gamelog_df", gamelog_df, row.names=NULL);
##dbWriteTable(con, "gamelog_df", gamelog_df, row.names=FALSE, append=TRUE);
##dbRemoveTable(con, "plate_appearances")
##dbCreateTable(con, "plate_appearances", plate_appearances, row.names=NULL);
##dbWriteTable(con, "plate_appearances", plate_appearances, row.names=FALSE, append=TRUE);
##dbRemoveTable(con, "pitch_df")
##dbCreateTable(con, "pitch_df", pitch_df, row.names=NULL);
##dbWriteTable(con, "pitch_df", pitch_df, row.names=FALSE, append=TRUE);
dbDisconnect(con);

