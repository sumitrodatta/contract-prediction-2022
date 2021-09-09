library(tidyverse)
library(rvest)
library(janitor)
library(polite)

bbref_bow=bow("https://www.basketball-reference.com/",user_agent = "Sumitro Datta",force=TRUE)
print(bbref_bow)

get_free_agents<-function(year=2016)
{
  session=nod(bbref_bow,paste0("friv/free_agents.cgi?year=",year))
  a<-scrape(session) %>% html_nodes("table") %>% .[[1]] %>% html_table() %>% 
    mutate(WS=as.numeric(WS))
  #get rid of separator rows and players who didn't play in NBA previous season (signed from overseas, so WS=NA)
  #change Nene's name to match stats
  a<-a %>% filter(Player != "Player",!is.na(WS)) %>% 
    mutate(Rk=year,Player=ifelse(Player=="Nenê Hilário","Nenê",Player)) %>% rename(Season=Rk) %>% 
    select(Season,Player,Type,WS,Terms) %>% clean_names()
  return (a)
}

#load previous project free agent csv to append to
prev_free_agents=read_csv("https://raw.githubusercontent.com/sumitrodatta/contract-prediction-2021/main/Data/2016-2020%20Free%20Agents.csv")

free_agents<-get_free_agents(2021)

# remove retired players
free_agents<-free_agents %>% filter(terms != "Retired") %>% mutate(contract_yrs=NA,yr_1_salary=NA)
# remove players going to play in different countries and players w/explicitly non-guaranteed first year
no_contract<-free_agents %>% filter(
  str_detect(terms,"China|Greece|Israel|Russia|Spain|Turkey|Italy|Moscow|Germany|France|Australia|Croatia")|
    str_detect(terms,"camp|two-way|Exhibit|2-way"))
free_agents<-anti_join(free_agents,no_contract)
no_contract<-no_contract %>% mutate(contract_yrs=0,yr_1_salary=0)
free_agents<-full_join(free_agents,no_contract)

write_csv(prev_free_agents %>% bind_rows(free_agents) %>% arrange(season,player),
          "Data/2016-2021 Free Agents.csv")

#include option years, partial guaranteed years in counting contract years

salary_cap_hist_url<-"https://basketball.realgm.com/nba/info/salary_cap"
salary_cap_hist<-salary_cap_hist_url %>% read_html() %>% 
  html_nodes(xpath='//*[contains(concat( " ", @class, " " ), concat( " ", "compact", " " ))]') %>% 
  .[[1]] %>% html_table(fill=TRUE)
#add correct column names (ended up as first row)
colnames(salary_cap_hist)<-salary_cap_hist[1,]
salary_cap_hist<-salary_cap_hist[-1,]
#only take year and cap number, parse cap into a number (has dollar sign and commas originally)
salary_cap_hist<-salary_cap_hist %>% select(3:4) %>%
  rename(season=`Luxury Tax`,cap=BAE) %>%
  mutate(season=as.numeric(str_sub(season,end=-4))) %>%
  mutate(cap=parse_number(cap))
write_csv(salary_cap_hist,"Data/Salary Cap History.csv")
