##install.packages("tidyverse");
library(tidyverse);
##install.packages("DBI"):
library(DBI);
##install.packages("stringr"); 
library(stringr);

setwd("~/Github/Times-Through-Order");

##Connection to AWS Database
con=dbConnect(RMySQL::MySQL(), dbname="zgreer_baseball",
              host="zgreer-baseball.cqridrsjfpub.us-east-2.rds.amazonaws.com",
              port=3306, user="guest", password="Password1!");
##dbListTables(con);
##dbDisconnect(con);

##Reading in pitching game log table
tbl_atbats=tbl(con, "at_bats");
atbats_df=collect(tbl_atbats) %>% readr::type_convert();
rm(tbl_atbats);


atbats_df=atbats_df %>% mutate(wOBA_points=
                       ifelse(str_sub(pitches, -1, -1)!="V" & events=="walk", .69, 
                              ifelse(events=="hit_by_pitch", .72, 
                                     ifelse(events=="single", .89, 
                                            ifelse(events=="doule", 1.27, 
                                                   ifelse(events=="triple", 1.62, 
                                                          ifelse(events=="home_run", 2.10,0)))))));


atbats_df %>% group_by(batter) %>% summarise(wOBA=sum(wOBA_points)/(n()+sum()))







test=atbats_df %>% count(events) %>% arrange(desc(n))
