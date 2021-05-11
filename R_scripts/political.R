# Title: Political
# Description: 

# Packages
library(dplyr)
library(stringr)


### House Congressional Elections Data ###

he_92_files <- list.files("../data/cqpress/house/elec92-00/")
he_92_files <- paste0("../data/cqpress/house/elec92-00/", he_92_files)
he_92_dfs <- lapply(he_92_files, read.csv, stringsAsFactors = F)
he_92_df <- he_92_dfs[[1]]

n <- length(he_92_dfs)

for (i in 2:n){
  he_92_df <- rbind(he_92_df, he_92_dfs[[i]])
}

he_00_files <- list.files("../data/cqpress/house/elec00-10/")
he_00_files <- paste0("../data/cqpress/house/elec00-10/", he_00_files)
he_00_dfs <- lapply(he_00_files, read.csv, stringsAsFactors = F)
he_00_df <- he_00_dfs[[1]]

n <- length(he_00_dfs)

for (i in 2:n){
  he_00_df <- rbind(he_00_df, he_00_dfs[[i]])
}


he_12_files <- list.files("../data/cqpress/house/elec12-16/")
he_12_files <- paste0("../data/cqpress/house/elec12-16/", he_12_files)
he_12_dfs <- lapply(he_12_files, read.csv, stringsAsFactors = F)
he_12_df <- he_12_dfs[[1]]

n <- length(he_12_dfs)

for (i in 2:n){
  he_12_df <- rbind(he_12_df, he_12_dfs[[i]])
}




house_elections <- rbind(he_92_df, he_00_df)
house_elections <- rbind(house_elections, he_12_df)
he_columns <- colnames(house_elections)[c(2,4,5,7,10,13,22,23)]
house_elections <- house_elections %>% select(he_columns)

# Accounting for "At-Large" Districts
house_elections[!str_detect(house_elections$Area, "[0-9]+"),]$Area = "1"
house_elections$Area <- unlist(str_extract_all(house_elections$Area, "[0-9]+"))



states_dict <- read.csv('../data/cw/icpsrcnt.csv') %>% select(State, STATEFIPS)
states_dict <- unique(states_dict)

house_elections <- merge(house_elections,states_dict, by = "State") %>%
  mutate(fipstate = STATEFIPS, district_code = Area,
         gop_prop = RepVotesMajorPercent, dem_prop = DemVotesMajorPercent) %>%
  select(raceYear, fipstate, district_code, RepStatus, DemStatus,
         gop_prop, dem_prop)

unops = house_elections$RepStatus == "N/A" | house_elections$DemStatus == "N/A"
house_elections$unopposed = 0
house_elections$unopposed[unops] = 1



gopwin_bool = house_elections$gop_prop > house_elections$dem_prop
house_elections$win_prop = 0
house_elections$win_prop[gopwin_bool]=house_elections$gop_prop[gopwin_bool]
house_elections$win_prop[!gopwin_bool]=house_elections$dem_prop[!gopwin_bool]
house_elections$gop_win = 0
house_elections$gop_win[gopwin_bool] = 1
house_elections$win_margin = 0
house_elections$win_margin[gopwin_bool] = house_elections$gop_prop[gopwin_bool] - house_elections$dem_prop[gopwin_bool]
house_elections$win_margin[!gopwin_bool] = house_elections$dem_prop[!gopwin_bool] - house_elections$gop_prop[!gopwin_bool]


house_elect <- house_elections %>% 
  select(raceYear, fipstate, district_code, gop_prop,
         dem_prop, win_prop, win_margin, unopposed, gop_win)

write.csv(house_elect,
          "../data/cqpress/house/he_full.csv", row.names = F)






he_1992 <- house_elections[house_elections$raceYear == 1992,]
he_2000 <- house_elections[house_elections$raceYear == 2000,]

he_92_00 <- merge(he_1992, he_2000, by = c("fipstate", "district_code")) %>%
  mutate(gop_delta_sh = gop_prop.y - gop_prop.x,
         dem_delta_sh = dem_prop.y - dem_prop.x,
         gop_prop_beg = gop_prop.x,
         dem_prop_beg = dem_prop.x,
         gop_prop_end = gop_prop.y,
         dem_prop_end = dem_prop.y,
         gop_win_beg = gop_win.x,
         gop_win_end = gop_win.y,
         unopposed_beg = unopposed.x,
         unopposed_end = unopposed.y,
         win_prop_beg = win_prop.x)

he_92_00$win_prty_delta_sh <- 0
gop_win_bool <- he_92_00$gop_win_beg == 1
he_92_00$win_prty_delta_sh[gop_win_bool] <- he_92_00$gop_prop.y[gop_win_bool] - he_92_00$win_prop.x[gop_win_bool]
he_92_00$win_prty_delta_sh[!gop_win_bool] <- he_92_00$dem_prop.y[!gop_win_bool] - he_92_00$win_prop.x[!gop_win_bool]
he_92_00$delta_win_margin <- he_92_00$win_margin.y - he_92_00$win_margin.x

he_92_00 <- he_92_00 %>% 
  select(fipstate, district_code, gop_win_beg, gop_win_end, unopposed_beg,
         unopposed_end, gop_prop_beg, gop_delta_sh, dem_prop_beg,
         dem_delta_sh, win_prop_beg, win_prty_delta_sh, delta_win_margin)


write.csv(he_92_00, "../data/cqpress/house/he_92_00.csv", row.names = F)


he_2002 <- house_elections[house_elections$raceYear == 2002,]
he_2010 <- house_elections[house_elections$raceYear == 2010,]

he_02_10 <- merge(he_2002, he_2010, by = c("fipstate", "district_code")) %>%
  mutate(gop_delta_sh = gop_prop.y - gop_prop.x,
         dem_delta_sh = dem_prop.y - dem_prop.x,
         gop_prop_beg = gop_prop.x,
         dem_prop_beg = dem_prop.x,
         gop_prop_end = gop_prop.y,
         dem_prop_end = dem_prop.y,
         gop_win_beg = gop_win.x,
         gop_win_end = gop_win.y,
         unopposed_beg = unopposed.x,
         unopposed_end = unopposed.y,
         win_prop_beg = win_prop.x)

he_02_10$win_prty_delta_sh <- 0
gop_win_bool <- he_02_10$gop_win_beg == 1
he_02_10$win_prty_delta_sh[gop_win_bool] <- he_02_10$gop_prop.y[gop_win_bool] - he_02_10$win_prop.x[gop_win_bool]
he_02_10$win_prty_delta_sh[!gop_win_bool] <- he_02_10$dem_prop.y[!gop_win_bool] - he_02_10$win_prop.x[!gop_win_bool]
he_02_10$delta_win_margin <- he_02_10$win_margin.y - he_02_10$win_margin.x

he_02_10 <- he_02_10 %>% 
  select(fipstate, district_code, gop_win_beg, gop_win_end, unopposed_beg,
         unopposed_end, gop_prop_beg, gop_delta_sh, dem_prop_beg,
         dem_delta_sh, win_prop_beg, win_prty_delta_sh, delta_win_margin)

write.csv(he_02_10, "../data/cqpress/house/he_02_10.csv", row.names = F)

he_2012 <- house_elections[house_elections$raceYear == 2012,]
he_2016 <- house_elections[house_elections$raceYear == 2016,]

he_12_16 <- merge(he_2012, he_2016, by = c("fipstate", "district_code")) %>%
  mutate(gop_delta_sh = gop_prop.y - gop_prop.x,
         dem_delta_sh = dem_prop.y - dem_prop.x,
         gop_prop_beg = gop_prop.x,
         dem_prop_beg = dem_prop.x,
         gop_prop_end = gop_prop.y,
         dem_prop_end = dem_prop.y,
         gop_win_beg = gop_win.x,
         gop_win_end = gop_win.y,
         unopposed_beg = unopposed.x,
         unopposed_end = unopposed.y,
         win_prop_beg = win_prop.x)

he_12_16$win_prty_delta_sh <- 0
gop_win_bool <- he_12_16$gop_win_beg == 1
he_12_16$win_prty_delta_sh[gop_win_bool] <- he_12_16$gop_prop.y[gop_win_bool] - he_12_16$win_prop.x[gop_win_bool]
he_12_16$win_prty_delta_sh[!gop_win_bool] <- he_12_16$dem_prop.y[!gop_win_bool] - he_12_16$win_prop.x[!gop_win_bool]
he_12_16$delta_win_margin <- he_12_16$win_margin.y - he_12_16$win_margin.x

he_12_16 <- he_12_16 %>% 
  select(fipstate, district_code, gop_win_beg, gop_win_end, unopposed_beg,
         unopposed_end, gop_prop_beg, gop_delta_sh, dem_prop_beg,
         dem_delta_sh, win_prop_beg, win_prty_delta_sh, delta_win_margin)


write.csv(he_12_16, "../data/cqpress/house/he_12_16.csv", row.names = F)




### Presidential Elections Data ###

# 1992 Election

p_92_files <- list.files("../data/cqpress/presidential/1992/")
p_92_files <- paste0("../data/cqpress/presidential/1992/", p_92_files)
p_92_dfs <- lapply(p_92_files, read.csv, stringsAsFactors = F)
p_92_df <- p_92_dfs[[1]]

n <- length(p_00_dfs)

for (i in 2:n){
  p_92_df <- rbind(p_92_df, p_92_dfs[[i]])
}

p_92_df$elec_year <- 1992
p_92_df$Area <- tolower(p_92_df$Area)
p_92_df$TotalVotes <- as.numeric(str_replace(p_92_df$TotalVotes, "[^0-9]+",""))
p_92_df$nat_votes <- sum(p_92_df$TotalVotes, na.rm = T)
p_92_df <- p_92_df %>%
  mutate(cnty_vs = TotalVotes/nat_votes)

county_cw90_path <-"../data/ipums/nhgis003_tot_pop/nhgis0003_ds122_1990_county.csv"
county_cw90 <- read.csv(county_cw90_path) %>% select(COUNTY, COUNTYA, STATE, STATEA)
county_cw90$Area <- tolower(county_cw90$COUNTY)
county_cw90 <- county_cw90 %>% mutate(State = STATE,
                                      fipstate = STATEA,
                                      fipscty = COUNTYA)

p_92_df <- merge(p_92_df, county_cw90, by = c("State", "Area")) %>%
  mutate(gop_vs = RepVotesTotalPercent,
         dem_vs = DemVotesTotalPercent,
         thirdp_vs = ThirdVotesTotalPercent,
         vote_total = TotalVotes)

gop_win_bool92 <- p_92_df$gop_vs > p_92_df$dem_vs & p_92_df$gop_vs > p_92_df$thirdp_vs
p_92_df$gop_win <- 0 
p_92_df$gop_win[gop_win_bool92] <- 1

dem_win_bool92 <- p_92_df$dem_vs > p_92_df$gop_vs & p_92_df$dem_vs > p_92_df$thirdp_vs
p_92_df$dem_win <- 0 
p_92_df$dem_win[dem_win_bool92] <- 1

p_92_df <- cnty_fips(p_92_df)  %>%
  select(fipstate, fipscty, gop_vs, dem_vs, cnty_vs,
         gop_win, dem_win, elec_year, vote_total)


# 2000 Election

p_00_files <- list.files("../data/cqpress/presidential/2000/")
p_00_files <- paste0("../data/cqpress/presidential/2000/", p_00_files)
p_00_dfs <- lapply(p_00_files, read.csv, stringsAsFactors = F)
p_00_df <- p_00_dfs[[1]]

n <- length(p_00_dfs)

for (i in 2:n){
  p_00_df <- rbind(p_00_df, p_00_dfs[[i]])
}

p_00_df$elec_year <- 2000
p_00_df$Area <- tolower(p_00_df$Area)
p_00_df$TotalVotes <- as.numeric(str_replace(p_00_df$TotalVotes, "[^0-9]+",""))
p_00_df$nat_votes <- sum(p_00_df$TotalVotes, na.rm = T)
p_00_df <- p_00_df %>%
  mutate(cnty_vs = TotalVotes/nat_votes)

county_cw00_path <-"../data/ipums/nhgis003_tot_pop/nhgis0003_ds149_2000_county.csv"
county_cw00 <- read.csv(county_cw00_path) %>% select(COUNTY, COUNTYA, STATE, STATEA)
county_cw00$Area <- tolower(county_cw00$COUNTY)
county_cw00 <- county_cw00 %>% mutate(State = STATE,
                                      fipstate = STATEA,
                                      fipscty = COUNTYA)

p_00_df <- merge(p_00_df, county_cw00, by = c("State", "Area")) %>%
  mutate(gop_vs = RepVotesTotalPercent,
         dem_vs = DemVotesTotalPercent,
         thirdp_vs = ThirdVotesTotalPercent,
         vote_total = TotalVotes)

gop_win_bool00 <- p_00_df$gop_vs > p_00_df$dem_vs & p_00_df$gop_vs > p_00_df$thirdp_vs
p_00_df$gop_win <- 0 
p_00_df$gop_win[gop_win_bool00] <- 1

dem_win_bool00 <- p_00_df$dem_vs > p_00_df$gop_vs & p_00_df$dem_vs > p_00_df$thirdp_vs
p_00_df$dem_win <- 0 
p_00_df$dem_win[dem_win_bool00] <- 1

p_00_df <- cnty_fips(p_00_df)  %>%
  select(fipstate, fipscty, gop_vs, dem_vs, cnty_vs,
         gop_win, dem_win, elec_year, vote_total)


# 2008 Election

p_08_files <- list.files("../data/cqpress/presidential/2008/")
p_08_files <- paste0("../data/cqpress/presidential/2008/", p_08_files)
p_08_dfs <- lapply(p_08_files, read.csv, stringsAsFactors = F)
p_08_df <- p_08_dfs[[1]]

n <- length(p_00_dfs)

for (i in 2:n){
  p_08_df <- rbind(p_08_df, p_08_dfs[[i]])
}

p_08_df$elec_year <- 2008
p_08_df$Area <- tolower(p_08_df$Area)
p_08_df$TotalVotes <- as.numeric(str_replace(p_08_df$TotalVotes, "[^0-9]+",""))
p_08_df$nat_votes <- sum(p_08_df$TotalVotes, na.rm = T)
p_08_df <- p_08_df %>%
  mutate(cnty_vs = TotalVotes/nat_votes)

p_08_df <- merge(p_08_df, county_cw00, by = c("State", "Area")) %>%
  mutate(gop_vs = RepVotesTotalPercent,
         dem_vs = DemVotesTotalPercent,
         thirdp_vs = ThirdVotesTotalPercent,
         vote_total = TotalVotes)

p_08_df$gop_vs <- as.numeric(p_08_df$gop_vs)
p_08_df$dem_vs <- as.numeric(p_08_df$dem_vs)
p_08_df$thirdp_vs <- as.numeric(p_08_df$thirdp_vs)

gop_win_bool08 <- p_08_df$gop_vs > p_08_df$dem_vs & p_08_df$gop_vs > p_08_df$thirdp_vs
p_08_df$gop_win <- 0 
p_08_df$gop_win[gop_win_bool08] <- 1

dem_win_bool08 <- p_08_df$dem_vs > p_08_df$gop_vs & p_08_df$dem_vs > p_08_df$thirdp_vs
p_08_df$dem_win <- 0 
p_08_df$dem_win[dem_win_bool08] <- 1

p_08_df <- cnty_fips(p_08_df)  %>%
  select(fipstate, fipscty, gop_vs, dem_vs, cnty_vs,
         gop_win, dem_win, elec_year, vote_total)



# 2012 Election

p_12_files <- list.files("../data/cqpress/presidential/2012/")
p_12_files <- paste0("../data/cqpress/presidential/2012/", p_12_files)
p_12_dfs <- lapply(p_12_files, read.csv, stringsAsFactors = F)
p_12_df <- p_12_dfs[[1]]

n <- length(p_12_dfs)

for (i in 2:n){
  p_12_df <- rbind(p_12_df, p_12_dfs[[i]])
}

p_12_df$elec_year <- 2012
p_12_df$Area <- tolower(p_12_df$Area)
p_12_df$TotalVotes <- as.numeric(str_replace(p_12_df$TotalVotes, "[^0-9]+",""))
p_12_df$nat_votes <- sum(p_12_df$TotalVotes, na.rm = T)
p_12_df <- p_12_df %>%
  mutate(cnty_vs = TotalVotes/nat_votes)

county_cw10_path<-"../data/ipums/nhgis003_tot_pop/nhgis0003_ds181_2010_county.csv"
county_cw10 <- read.csv(county_cw10_path, stringsAsFactors = F) %>%
  select(COUNTY, COUNTYA, STATE, STATEA)
county_cw10$Area <- str_replace(county_cw10$COUNTY, " County| Parish", "")
county_cw10$Area <- tolower(county_cw10$Area)
county_cw10 <- county_cw10 %>% mutate(State = STATE,
                                      fipstate = STATEA,
                                      fipscty = COUNTYA)

p_12_df <- merge(p_12_df, county_cw10, by = c("State", "Area")) %>%
  mutate(gop_vs = RepVotesTotalPercent,
         dem_vs = DemVotesTotalPercent,
         thirdp_vs = ThirdVotesTotalPercent,
         vote_total = TotalVotes)

gop_win_bool12 <- p_12_df$gop_vs > p_12_df$dem_vs & p_12_df$gop_vs > p_12_df$thirdp_vs
p_12_df$gop_win <- 0 
p_12_df$gop_win[gop_win_bool12] <- 1

dem_win_bool12 <- p_12_df$dem_vs > p_12_df$gop_vs & p_12_df$dem_vs > p_12_df$thirdp_vs
p_12_df$dem_win <- 0 
p_12_df$dem_win[dem_win_bool12] <- 1

p_12_df <- cnty_fips(p_12_df)  %>%
  select(fipstate, fipscty, gop_vs, dem_vs, cnty_vs,
         gop_win, dem_win, elec_year, vote_total)


# 2016 Election

p_16_files <- list.files("../data/cqpress/presidential/2016/")
p_16_files <- paste0("../data/cqpress/presidential/2016/", p_16_files)
p_16_dfs <- lapply(p_16_files, read.csv, stringsAsFactors = F)
p_16_df <- p_16_dfs[[1]]

n <- length(p_16_dfs)

for (i in 2:n){
  p_16_df <- rbind(p_16_df, p_16_dfs[[i]])
}

p_16_df$elec_year <- 2016
p_16_df$Area <- tolower(p_16_df$Area)
p_16_df$TotalVotes <- as.numeric(str_replace(p_16_df$TotalVotes, "[^0-9]+",""))
p_16_df$nat_votes <- sum(p_16_df$TotalVotes, na.rm = T)
p_16_df <- p_16_df %>%
  mutate(cnty_vs = TotalVotes/nat_votes)


p_16_df <- merge(p_16_df, county_cw10, by = c("State", "Area")) %>%
  mutate(gop_vs = RepVotesTotalPercent,
         dem_vs = DemVotesTotalPercent,
         thirdp_vs = ThirdVotesTotalPercent,
         vote_total = TotalVotes)

gop_win_bool16 <- p_16_df$gop_vs > p_16_df$dem_vs & p_16_df$gop_vs > p_16_df$thirdp_vs
p_16_df$gop_win <- 0 
p_16_df$gop_win[gop_win_bool16] <- 1

dem_win_bool16 <- p_16_df$dem_vs > p_16_df$gop_vs & p_16_df$dem_vs > p_16_df$thirdp_vs
p_16_df$dem_win <- 0 
p_16_df$dem_win[dem_win_bool16] <- 1

p_16_df <- cnty_fips(p_16_df)  %>%
  select(fipstate, fipscty, gop_vs, dem_vs, cnty_vs,
         gop_win, dem_win, elec_year, vote_total)





p_92_00 <- merge(p_92_df, p_00_df, by = c("fipstate", "fipscty")) %>%
  mutate(delta_gop_vs = gop_vs.y - gop_vs.x,
          gop_vs = gop_vs.x,
          gop_win = gop_win.x,
          dem_vs = dem_vs.x,
          dem_win = dem_win.x,
          cnty_vs = cnty_vs.x,
         vote_total = vote_total.x,
          p_elections = "elec_92-00") %>%
  select(fipstate, fipscty, delta_gop_vs, gop_vs, dem_vs,
         gop_win, dem_win, cnty_vs, p_elections, vote_total)

p_92_00$win_vs <- 0
gop_win_boolean1 <- p_92_00$gop_win == 1
p_92_00$win_vs[gop_win_boolean1] <- p_92_00$gop_vs[gop_win_boolean1]
p_92_00$win_vs[!gop_win_boolean1] <- p_92_00$dem_vs[!gop_win_boolean1]

p_92_08 <- merge(p_92_df, p_08_df, by = c("fipstate", "fipscty")) %>%
  mutate(delta_gop_vs = gop_vs.y - gop_vs.x,
         gop_vs = gop_vs.x,
         gop_win = gop_win.x,
         dem_vs = dem_vs.x,
         dem_win = dem_win.x,
         cnty_vs = cnty_vs.x,
         vote_total = vote_total.x,
         p_elections = "elec_92-08") %>%
  select(fipstate, fipscty, delta_gop_vs, gop_vs, dem_vs,
         gop_win, dem_win, cnty_vs, p_elections, vote_total)

p_92_08$win_vs <- 0
gop_win_boolean1 <- p_92_08$gop_win == 1
p_92_08$win_vs[gop_win_boolean1] <- p_92_08$gop_vs[gop_win_boolean1]
p_92_08$win_vs[!gop_win_boolean1] <- p_92_08$dem_vs[!gop_win_boolean1]

p_92_16 <- merge(p_92_df, p_16_df, by = c("fipstate", "fipscty")) %>%
  mutate(delta_gop_vs = gop_vs.y - gop_vs.x,
         gop_vs = gop_vs.x,
         gop_win = gop_win.x,
         dem_vs = dem_vs.x,
         dem_win = dem_win.x,
         cnty_vs = cnty_vs.x,
         vote_total = vote_total.x,
         p_elections = "elec_92-16") %>%
  select(fipstate, fipscty, delta_gop_vs, gop_vs, dem_vs,
         gop_win, dem_win, cnty_vs, p_elections, vote_total)

p_92_16$win_vs <- 0
gop_win_boolean1 <- p_92_16$gop_win == 1
p_92_16$win_vs[gop_win_boolean1] <- p_92_16$gop_vs[gop_win_boolean1]
p_92_16$win_vs[!gop_win_boolean1] <- p_92_16$dem_vs[!gop_win_boolean1]

p_00_08 <- merge(p_00_df, p_08_df, by = c("fipstate", "fipscty")) %>%
  mutate(delta_gop_vs = gop_vs.y - gop_vs.x,
         gop_vs = gop_vs.x,
         gop_win = gop_win.x,
         dem_vs = dem_vs.x,
         dem_win = dem_win.x,
         cnty_vs = cnty_vs.x,
         vote_total = vote_total.x,
         p_elections = "elec_00-08") %>%
  select(fipstate, fipscty, delta_gop_vs, gop_vs, dem_vs,
         gop_win, dem_win, cnty_vs, p_elections, vote_total)

p_00_08$win_vs <- 0
gop_win_boolean1 <- p_00_08$gop_win == 1
p_00_08$win_vs[gop_win_boolean1] <- p_00_08$gop_vs[gop_win_boolean1]
p_00_08$win_vs[!gop_win_boolean1] <- p_00_08$dem_vs[!gop_win_boolean1]

p_00_16 <- merge(p_00_df, p_16_df, by = c("fipstate", "fipscty")) %>%
  mutate(delta_gop_vs = gop_vs.y - gop_vs.x,
         gop_vs = gop_vs.x,
         gop_win = gop_win.x,
         dem_vs = dem_vs.x,
         dem_win = dem_win.x,
         cnty_vs = cnty_vs.x,
         vote_total = vote_total.x,
         p_elections = "elec_00-16") %>%
  select(fipstate, fipscty, delta_gop_vs, gop_vs, dem_vs,
         gop_win, dem_win, cnty_vs, p_elections, vote_total)

p_00_16$win_vs <- 0
gop_win_boolean1 <- p_00_16$gop_win == 1
p_00_16$win_vs[gop_win_boolean1] <- p_00_16$gop_vs[gop_win_boolean1]
p_00_16$win_vs[!gop_win_boolean1] <- p_00_16$dem_vs[!gop_win_boolean1]

p_08_16 <- merge(p_08_df, p_16_df, by = c("fipstate", "fipscty")) %>%
  mutate(delta_gop_vs = gop_vs.y - gop_vs.x,
         gop_vs = gop_vs.x,
         gop_win = gop_win.x,
         dem_vs = dem_vs.x,
         dem_win = dem_win.x,
         cnty_vs = cnty_vs.x,
         vote_total = vote_total.x,
         p_elections = "elec_08-16") %>%
  select(fipstate, fipscty, delta_gop_vs, gop_vs, dem_vs,
         gop_win, dem_win, cnty_vs, p_elections, vote_total)

p_08_16$win_vs <- 0
gop_win_boolean1 <- p_08_16$gop_win == 1
p_08_16$win_vs[gop_win_boolean1] <- p_08_16$gop_vs[gop_win_boolean1]
p_08_16$win_vs[!gop_win_boolean1] <- p_08_16$dem_vs[!gop_win_boolean1]







write.csv(p_92_00, "../data/cqpress/presidential/p_92_00.csv", row.names = F)
write.csv(p_92_08, "../data/cqpress/presidential/p_92_08.csv", row.names = F)
write.csv(p_92_16, "../data/cqpress/presidential/p_92_16.csv", row.names = F)
write.csv(p_00_08, "../data/cqpress/presidential/p_00_08.csv", row.names = F)
write.csv(p_00_16, "../data/cqpress/presidential/p_00_16.csv", row.names = F)
write.csv(p_08_16, "../data/cqpress/presidential/p_08_16.csv", row.names = F)





