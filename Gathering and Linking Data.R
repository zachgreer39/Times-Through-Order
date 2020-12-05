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
##install.packages("RMySQL");

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
rm(tbl_players);


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
player_ids=pull_ids(twentyseven_df$fangraphsID)
rm(chadwick_player_lu_table);
##Joining Fangraphs gamelog to retrosheet appearances
twentyseven_df=twentyseven_df %>% inner_join(player_ids,by=c("fangraphsID")) %>%
  inner_join(players_df %>% select(game_id, start, retroID, team, Date),
             by=c("retroID", "Date"));
rm(player_ids, players_df);


##Creating a table to join dataframes and pull in only games I need from pbp
##three_times_games=data.frame(game_id=unique(twentyseven_df$game_id));
##dbCreateTable(con, "three_times_games", three_times_games, row.names=NULL);
##dbWriteTable(con, "three_times_games", three_times_games, row.names=FALSE, append=TRUE);
##dbReadTable(con, "three_times_games");


##Joining the temp games table with the pbp table and writing it to a dataframe
##Reading in players table
tbl_rawpbp=tbl(con, "tbl_rawpbp");
three_times_games=tbl(con, "three_times_games");
games_df=collect(tbl_rawpbp %>% inner_join(three_times_games)) %>%
  readr::type_convert() %>% distinct();
rm(tbl_rawpbp, three_times_games);


##Reading in player appearances from retrosheet to label players in play by play
tbl_players=tbl(con, "tbl_players");
players_df=collect(tbl_players) %>% readr::type_convert() %>%
  filter(substr(game_id,4,7)>=2017) %>% mutate(
    Date=ymd(paste(substr(game_id,4,7), "-",substr(game_id,8,9), "-",
                   substr(game_id, 10, 11), sep=""))) %>% filter(retroID!="na");
players_df$team=as.numeric(players_df$team);
rm(tbl_players);
##Dividing players_df for joins
starters=players_df %>% filter (start=="start");
subs=players_df %>% filter (start=="sub");
rm(players_df);

##Pulling plate appearances by starters
starts_marked=games_df %>% inner_join(starters);
games_df=games_df %>% anti_join(starts_marked);
rm(starters);

##Pulling plate appearances by subs
subs_marked=games_df %>% inner_join(subs);
duplicated=subs_marked %>% group_by(game_id, inning, team, retroID, count,
                                    pitches, play) %>% count(retroID) %>% filter(n!=1);
distinct_subs=subs_marked %>% semi_join(duplicated) %>%
  group_by(game_id, inning, team, retroID, count, pitches, play) %>%
  mutate(fieldPos=as.character(min(as.numeric(fieldPos))),
         batPos=as.character(min(as.numeric(batPos)))) %>% distinct();
subs_marked=rbind(subs_marked %>% anti_join(duplicated),
                  subs_marked %>% semi_join(distinct_subs));
##Concatenating
games_df=rbind(starts_marked, subs_marked) %>%
  anti_join(rbind(starts_marked, subs_marked) %>%
              group_by(game_id, inning, team, retroID, count, pitches, play) %>%
              count(retroID) %>% filter(n!=1));
rm(distinct_subs, duplicated, starts_marked, subs, subs_marked);


##Creating a new variable to join pitchers with batters
twentyseven_df=twentyseven_df %>%
  rename(pitcher_name=name, pitcher_retro=retroID) %>%
  mutate(join_team=ifelse(team==0, 1,0)) %>% select(-start);


##Joining dataframes
games_df=games_df %>% inner_join(twentyseven_df,
                                 by=c("game_id", "Date","team"="join_team")) %>% filter(play!="NP") %>%
  rename("pitcher_team"="team.y");
rm(twentyseven_df);

##Reading in statcast game IDs
tbl_game_pk=tbl(con, "tbl_game_pk");
game_pk_df=collect(tbl_game_pk) %>%
  readr::type_convert();
rm(tbl_game_pk);


##Creating a dataframe to join because abbreviations are different for some
team_join=as.data.frame(
  cbind(games_df_Team=c("SEA", "CLE", "TEX", "CHC", "MIN", "KCR", "TBR", "HOU",
                        "LAA", "OAK", "CHW", "ARI", "SFG", "NYM", "COL", "MIL",
                        "SDP", "CIN", "MIA", "ATL", "WSN", "PIT", "PHI", "BAL",
                        "BOS", "DET", "TOR", "LAD", "NYY", "STL"),
        game_pk_df_Team=c("SEA", "CLE", "TEX", "CHC", "MIN", "KC", "TB", "HOU", "LAA",
                          "OAK", "CWS", "ARI", "SF", "NYM", "COL", "MIL", "SD", "CIN",
                          "MIA", "ATL", "WSH", "PIT", "PHI", "BAL", "BOS", "DET", "TOR",
                          "LAD", "NYY", "STL")));
games_df=games_df %>% inner_join(team_join, by=c("Team"="games_df_Team")) %>%
  rename(join_team=game_pk_df_Team);
##Pulling double_headers and removing them from games_df
double_headers=games_df %>% transmute(game_date=Date, join_team) %>%
  distinct() %>% inner_join(
    rbind(game_pk_df %>% transmute(game_date, game_pk, join_team=home_team),
          game_pk_df %>% transmute(game_date, game_pk, join_team=away_team))) %>%
  count(game_date, join_team) %>% filter(n!=1) %>%
  rename(Team=join_team, Date=game_date) %>% select(-n) %>%
  inner_join(rbind(
    game_pk_df %>% transmute(Date=game_date, Team=home_team, game_pk),
    game_pk_df %>% transmute(Date=game_date, Team=away_team, game_pk)));
double_headers_df=games_df %>% inner_join(double_headers);
##Removing double header games and joining to get game_pk
games_df=games_df %>% anti_join(double_headers_df) %>%
  inner_join(rbind(
    game_pk_df %>% transmute(Date=game_date, Team=home_team, game_pk),
    game_pk_df %>% transmute(Date=game_date, Team=away_team, game_pk)),
    by=c("join_team"="Team", "Date"));
rm(double_headers, game_pk_df);
games_df=games_df %>% anti_join(double_headers_df %>%
                                  select(game_pk) %>% distinct());
##Concatenating both back together
games_df=rbind(games_df, double_headers_df);
rm(double_headers_df);

##Creating a table to join to pitch level data
##temp_pitches=rbind(games_df %>% select(mlbID, game_pk) %>% distinct(),
##                 double_headers_df %>% select(mlbID, game_pk) %>% distinct());
##dbCreateTable(con, "temp_pitches", temp_pitches, row.names=NULL);
##dbWriteTable(con, "temp_pitches", temp_pitches, row.names=FALSE, append=TRUE);
##dbReadTable(con, "temp_pitches");
##rm(temp_pitches);


##This section pulls player_ids from the retrosheet pbp data to match with
##other MLB data sources
##Matching dataframes
player_ids=pull_ids(games_df$retroID);
rm(chadwick_player_lu_table);
##Joining to dataframes and splitting pbp and gamelog data
games_df=games_df %>% inner_join(player_ids, by=c("retroID"),
                                 suffix=c("_pitcher","_batter")) %>%
  mutate(Opp=str_remove_all(Opp, "@"));
games_df=games_df %>% inner_join(team_join, by=c("Opp"="games_df_Team")) %>%
  transmute(Date, game_id, game_pk, pitcher_name,
            retroID_pitcher=pitcher_retro, fangraphsID_pitcher, mlbID_pitcher,
            brefID_pitcher, fgTeam_pitcher=Team, mlbTeam_pitcher=join_team,
            retroID_batter=retroID, fangraphsID_batter, mlbID_batter,
            brefID_batter, fgTeam_batter=Opp, mlbTeam_batter=game_pk_df_Team,
            batPos, inning, count, pitches, play, GS, W, L, SV, HLD, IP, TBF, H,
            R, ER, HR, BB, SO, BABIP, ERA, FIP, xFIP)
pbp_df=games_df %>% select(Date, game_id, game_pk, pitcher_name,
                           retroID_pitcher, fangraphsID_pitcher, mlbID_pitcher,
                           brefID_pitcher, fgTeam_pitcher, mlbTeam_pitcher,
                           retroID_batter, fangraphsID_batter, mlbID_batter,
                           brefID_batter, fgTeam_batter, mlbTeam_batter,
                           batPos, inning, count, pitches, play);
gamelog_df=games_df %>% select(Date, game_id, game_pk, pitcher_name,
                               retroID_pitcher, fangraphsID_pitcher, mlbID_pitcher,
                               brefID_pitcher, fgTeam_pitcher, mlbTeam_pitcher,
                               fgTeam_batter, mlbTeam_batter, GS, W, L, SV, HLD,
                               IP, TBF, H, R, ER, HR, BB, SO, BABIP, ERA, FIP, xFIP) %>%
  distinct();
rm(player_ids, team_join, games_df);

##pitch_join_tbl=pbp_df %>% select(game_pk) %>% distinct();
##dbCreateTable(con, "pitch_join_tbl", pitch_join_tbl, row.names=NULL);
##dbWriteTable(con, "pitch_join_tbl", pitch_join_tbl, row.names=FALSE, append=TRUE);
##dbReadTable(con, "pitch_join_tbl");
##rm(pitch_join_tbl);


##Finding appearances grouped by inning to identify plays during atbats
multi_rows=pbp_df %>%
  group_by(game_pk, mlbID_pitcher, mlbTeam_pitcher, mlbID_batter, mlbTeam_batter, inning) %>%
  count(inning) %>% filter(n!=1) %>% select(-n);
##Removing rows where plays occurred mid atbat
ab_conclusion=pbp_df %>% semi_join(multi_rows) %>%
  filter(!((str_sub(play, 1, 2)=="SB") | (str_sub(play, 1, 2)=="BK") | (str_sub(play, 1, 2)=="WP")
           | (str_sub(play, 1, 2)=="PB") | (str_sub(play, 1, 2)=="DI") | (str_sub(play, 1, 2)=="PO")
           | (str_sub(play, 1, 3)=="FLE") | (str_sub(play, 1, 2)=="CS") | (str_sub(play, 1, 2)=="OA"))) %>%
  arrange(game_pk, mlbID_batter, inning, batPos);
##Finding plate appearances with multiple rows but no conclusion yet
##and assigning the max count to pull AB conclusion
loose_abs=pbp_df %>% semi_join(multi_rows %>%
                                 anti_join(ab_conclusion %>%
                                             group_by(game_pk, mlbID_pitcher, mlbTeam_pitcher, mlbID_batter, mlbTeam_batter, inning) %>%
                                             count(inning) %>% select(-n))) %>%
  group_by(game_pk, mlbID_pitcher, mlbTeam_pitcher, mlbID_batter, mlbTeam_batter, inning) %>%
  mutate(max_count=max(as.numeric(count))) %>%
  filter(as.numeric(count)==max_count) %>% select(-max_count);
##Binding the two types together
ab_conclusion=rbind(ab_conclusion, loose_abs);
rm(loose_abs)
##Should be more than multi_rows because of plate appearances where batters batted twice in the same inning
##Marking events that happened mid atbat and atbats
mid_ab=pbp_df %>% semi_join(multi_rows) %>% anti_join(ab_conclusion);
at_bats=rbind(pbp_df %>% anti_join(multi_rows), ab_conclusion);
rm(ab_conclusion, multi_rows, pbp_df);

##Marking Events
at_bats=at_bats %>% mutate(events =ifelse(str_detect(play, "HR"), "home_run",
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


##Joining pitch data with game_pks and cutting down to pitcher/batter pitches
tbl_fxdata=tbl(con, "tbl_fxdata");
pitch_join_tbl=tbl(con, "pitch_join_tbl");
pitch_df=collect(tbl_fxdata %>% semi_join(pitch_join_tbl)) %>%
  readr::type_convert() %>% distinct();
rm(pitch_join_tbl, tbl_fxdata);
pitch_df$batter=as.character(pitch_df$batter);
pitch_df$pitcher=as.character(pitch_df$pitcher);


##Joining the two tables together
##Creating a dataframe of at bats from pitch level data
statcast_abs=pitch_df %>% select(game_pk, pitcher, pitcher_team, inning, batter, at_bat_number, events) %>%
  filter(events!="null") %>% distinct();
##Limiting to pitches thrown by pitchers team
pitch_df=pitch_df %>%
  semi_join(statcast_abs %>% semi_join(gamelog_df, by=c("game_pk", "pitcher"="mlbID_pitcher")),
            by=c("game_pk", "pitcher_team"));
##Reducing at bats to only at bats thrown by pitchers team
statcast_abs=pitch_df %>% select(game_pk, pitcher, pitcher_team, inning, batter, at_bat_number, events) %>%
  filter(events!="null") %>% distinct();
##Retrosheet dataframe
ab_match=at_bats %>% transmute(game_pk, pitcher=mlbID_pitcher, batter=mlbID_batter, inning,
                               pitches, play, events) %>% distinct();
##Taking even matches
even_matches=ab_match %>% inner_join(statcast_abs);
ab_match=ab_match %>% anti_join(even_matches);
statcast_abs=statcast_abs %>% anti_join(even_matches);
##Joining the two dataframes
joined_df=ab_match %>% left_join(statcast_abs, by=c("game_pk", "pitcher", "batter", "inning")) %>%
  drop_na();
##Eliminating duplicated statcast at bats because of joins
##from joined_df
no_match_joined=joined_df %>% semi_join(joined_df %>% group_by(game_pk, at_bat_number, pitcher, batter)
                                        %>% count(at_bat_number) %>% filter(n!=1)) %>%
  left_join(at_bats %>% select(mlbID_batter, brefID_batter) %>% distinct(), by=c("batter"="mlbID_batter"));
joined_df=joined_df %>% anti_join(no_match_joined)
no_match_joined=no_match_joined[c(2,3,6,7,9,12,13,15,18,19,21,23,25,27,30,31),];
no_match_joined=no_match_joined %>%
  transmute(game_pk, pitcher, batter, inning, pitches, play, events=events.y, at_bat_number);
joined_df=joined_df %>%
  transmute(game_pk, pitcher, batter, inning, pitches, play, events=events.y, at_bat_number);
##from even matches
no_match_even=even_matches %>% semi_join(even_matches %>% group_by(game_pk, pitcher, batter, at_bat_number) %>%
                                           count(at_bat_number) %>% filter(n!=1)) %>%
  left_join(at_bats %>%
              select(mlbID_pitcher, pitcher_name, mlbID_batter, brefID_batter, game_pk, Date) %>% distinct(),
            by=c("game_pk", "batter"="mlbID_batter", "pitcher"="mlbID_pitcher"));
even_matches=even_matches %>% anti_join(no_match_even);
no_match_even=no_match_even[c(1,3,6,7,9),c(1:7,9)];
even_matches=even_matches %>% select(-pitcher_team);
atbat_match=rbind(even_matches, joined_df, no_match_even, no_match_joined);
rm(even_matches, joined_df, no_match_even, no_match_joined);
##Statcast AtBats with no match in retrosheet
blank_abs=statcast_abs %>% anti_join(atbat_match) %>%
  semi_join(gamelog_df, by=c("game_pk", "pitcher"="mlbID_pitcher")) %>%
  left_join(ab_match, by=c("game_pk", "pitcher", "batter", "inning")) %>%
  transmute(game_pk, pitcher, batter, inning, pitches, play, events=events.x, at_bat_number);
atbat_match=rbind(atbat_match, blank_abs);
statcast_abs=statcast_abs %>% anti_join(blank_abs);
rm(blank_abs);
##Removing matched SP at basts from statcast
statcast_abs=statcast_abs %>% anti_join(atbat_match);
ab_match=ab_match %>%
  anti_join(atbat_match, by=c("game_pk", "pitcher", "batter", "inning", "pitches", "play"));
##pulling off relievers
##even matches
reliever_even_matches=ab_match %>%
  inner_join(statcast_abs, by=c("game_pk", "batter", "inning", "events")) %>%
  transmute(game_pk, pitcher=pitcher.y, batter, inning, pitches, play, events, at_bat_number);
ab_match=ab_match %>%
  anti_join(reliever_even_matches, by=c("game_pk", "batter", "inning", "pitches", "play", "events"));
statcast_abs=statcast_abs %>% anti_join(reliever_even_matches);
##
reliever_no_match=ab_match %>%
  inner_join(statcast_abs, by=c("game_pk", "batter", "inning")) %>%
  transmute(game_pk, pitcher=pitcher.y, batter, inning, pitches, play, events=events.y, at_bat_number);
ab_match=ab_match %>%
  anti_join(reliever_no_match, by=c("game_pk", "batter", "inning", "pitches", "play"));
reliever_no_match=rbind(statcast_abs %>% anti_join(reliever_no_match) %>%
                          left_join(ab_match, by=c("game_pk", "batter", "inning")) %>%
                          transmute(game_pk, pitcher=pitcher.x, batter, inning, pitches, play, events=events.x, at_bat_number),
                        reliever_no_match)
reliever_at_bats=rbind(reliever_even_matches, reliever_no_match);
rm(reliever_even_matches, reliever_no_match);
rm(statcast_abs);
##Removing at bats with no match
##Most if not all from pulling in both games for doubleheaders
##because retrosheet only had date data, not individual game
ab_match=ab_match %>% anti_join(gamelog_df %>%
                                  transmute(game_pk, pitcher=mlbID_pitcher) %>% anti_join(atbat_match)) %>%
  mutate(at_bat_number=NA);
gamelog_df=gamelog_df %>%
  semi_join(atbat_match, by=c("game_pk", "mlbID_pitcher"="pitcher"));
at_bats=at_bats %>% mutate(pitcher=mlbID_pitcher) %>%
  semi_join(gamelog_df) %>% select(-pitcher);
##Binding at_bats together to link dataframes
atbat_match=rbind(ab_match, atbat_match, reliever_at_bats);
rm(ab_match, reliever_at_bats);
atbat_match=atbat_match %>%
  left_join(at_bats %>% transmute(game_pk, batter=mlbID_batter, batPos) %>% distinct());
##Pulling game_pk/abs that are duplicated
duplicated_abs=atbat_match %>% semi_join(atbat_match %>%
                                           group_by(game_pk, pitcher, batter, at_bat_number) %>%
                                           filter(!is.na(at_bat_number)) %>% count(at_bat_number) %>% filter(n!=1))
atbat_match=atbat_match %>% anti_join(duplicated_abs);
atbat_match=rbind(atbat_match,
                  duplicated_abs[c(1,4,5,8,9,12,13,16,17,20,21,24,25,28,29,32,33,36,37,40,41,44,45,46,49,50,53,56,51,92,60,
                                   66,67,69,108,93,71,72,75,76,78,99,80,81,96,87,84,85,91,89,101,102,105,106,110,111,114,115),]);
##108852 rows from retrosheet plus 3 added rows not in retrosheet equals 108855
##sanity check on rows is good
rm(duplicated_abs);
atbat_match=atbat_match %>% group_by(game_pk, pitcher, batter, inning) %>%
  mutate(inning_ab=frank(at_bat_number)) %>% arrange(inning_ab) %>%
  group_by(game_pk, pitcher, batter) %>% mutate(TTO=frank(inning, ties.method="first"));
rm(at_bats);

##Writing tables to AWS Database
##dbCreateTable(con, "gamelog_df", gamelog_df, row.names=NULL);
##dbWriteTable(con, "gamelog_df", gamelog_df, row.names=FALSE, append=TRUE);
##dbReadTable(con, "gamelog_df")
##dbCreateTable(con, "at_bats", atbat_match, row.names=NULL);
##dbWriteTable(con, "at_bats", atbat_match, row.names=FALSE, append=TRUE);
##dbCreateTable(con, "pitch_df", pitch_df, row.names=NULL);
##dbWriteTable(con, "pitch_df", pitch_df, row.names=FALSE, append=TRUE);
dbDisconnect(con);

