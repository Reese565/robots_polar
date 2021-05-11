# Title: 
# Description: 
# Inputs: 
# Outputs:

# Source Files:
source("functions.R")

# Packages:
library(dplyr)

################################################################################
################################################################################
################################################################################


# US Employment Data
cbp90 <- cbpdd_build()
cbp90n <- national_isic(cbp90)
cbp90c <- county_emp(cbp90)
cbp90czind <- ind_cz_emp(cbp90)
cbp90czindp <- cz_ind_prop(cbp90)
cbp90czindp_cty <- cz.fips_data(cbp90czindp)


# EUKLEMS Data
euklems <- list.files('../data/eu_klems', pattern="*.csv", full.names=TRUE)
euklems_dfl <- lapply(euklems, read.csv, stringsAsFactors = F)
names(euklems_dfl) <- gsub('[^A-Z]','', euklems)
eu_bool <- names(euklems_dfl) != 'US'
euklems_dfl <- euklems_dfl[eu_bool]
euklems_dfl <- lapply(euklems_dfl, euklems_clean, base = '1995')



################################################################################


# United States IFR Data

us_ifr <- read.csv("../data/ifr/NA/NA.csv",
                  stringsAsFactors = F,
                  na.strings = '')
us_ifr <- build_ifr(us_ifr)
us_rpw90 <- rob_per_worker(cbp90n, us_ifr)


# Europe IFR Data

eu_ifr <- list.files('../data/ifr/Europe', pattern='*.csv', full.names=T)
eu_ifr_dfl <- lapply(eu_ifr, read.csv, stringsAsFactors = F, na.string = '')
eu_ifr <- gsub('[^A-Z]','', eu_ifr)
names(eu_ifr_dfl) <- gsub('^.','', eu_ifr)
eu_ifr_dfl <- lapply(eu_ifr_dfl, build_ifr)

eu_rpw95 <- list()
eu_rpw.c <- data.frame(matrix(rep(0, 25), nrow = 1))

countries <- names(eu_ifr_dfl)

for (country in countries){
  euklems <- euklems_dfl[[country]]
  ifr <- eu_ifr_dfl[[country]]
  rpw <- rob_per_worker(euklems, ifr)
  eu_rpw95[[country]] <- rpw
  
  rpw$country <- country
  rpw <- rpw %>% select(country, everything())
  rpw <- rpw[,-c(2,3)]
  colnames(eu_rpw.c) <- colnames(rpw)
  eu_rpw.c <- rbind(eu_rpw.c, rpw[1,])
  
  if (country == countries[length(countries)]){
    eu_rpw.c <- eu_rpw.c[-1,]
  }
}

rownames(eu_rpw.c) <- countries
eu_rpw.c <- eu_rpw.c[,-1]



### EU Robots Per Worker Quantile Selection ###

isic_group <- eu_rpw95[[1]][,1]
df_l <- list()

for (grp in isic_group){
  df_l[[grp]] <- data.frame(matrix(rep(0, 24), nrow = 1))
  colnames(df_l[[grp]]) <- colnames(eu_rpw95[[1]][,-c(1,2)])
  df_l[[grp]][["isic"]] <- "Empty"
  for (df in eu_rpw95){
    df_l[[grp]] <-  rbind(df_l[[grp]] ,df[df$isic == grp,-2])
  }
  df_l[[grp]] <- df_l[[grp]][-1,]
}



# 1990's Census

eu_rpw93_00 <- data.frame(matrix(rep(0, 24), nrow = 1))
colnames(eu_rpw93_00) <- colnames(eu_rpw95[[1]][,-c(1,2)])
eu_rpw93_00[["isic"]] = "Empty"


for (df in df_l){
  df_quantile <- apply(df[,-25], MARGIN = 2, quantile, 0.75)
  df_quantile$isic <- df$isic[1]
  eu_rpw93_00 <- rbind(eu_rpw93_00, df_quantile)
}

eu_rpw93_00 <- eu_rpw93_00[-1,c(25, 1:8)]

eu_delta_rpw93_00 <- eu_rpw93_00[-1,"yr.2000"] - eu_rpw93_00[-1,"yr.1993"]
eu_delta_rpw93_00 <- data.frame(isic = isic_group[-1],
                               eu_delta_rpw = eu_delta_rpw93_00)
eu_delta_rpw93_00 <- eu_delta_rpw93_00[-1,]

exp_robs93_00 <- merge(cbp90czindp, eu_delta_rpw93_00, by = "isic") %>%
  group_by(czone) %>%
  summarise(eu_delta_exp_robs = ind_prop %*% eu_delta_rpw)



# 2000's Census

eu_rpw01_10 <- data.frame(matrix(rep(0, 24), nrow = 1))
colnames(eu_rpw01_10) <- colnames(eu_rpw95[[1]][,-c(1,2)])
eu_rpw01_10[["isic"]] = "Empty"

for (df in df_l){
  df_quantile <- apply(df[,-25], MARGIN = 2, quantile, 0.75)
  df_quantile$isic <- df$isic[1]
  eu_rpw01_10 <- rbind(eu_rpw01_10, df_quantile)
}

eu_rpw01_10 <- eu_rpw01_10[-1,c(25, 9:18)]


eu_delta_rpw01_10 <- eu_rpw01_10[-1,"yr.2010"] - eu_rpw01_10[-1,"yr.2001"]
eu_delta_rpw01_10 <- data.frame(isic = isic_group[-1],
                               eu_delta_rpw = eu_delta_rpw01_10)
eu_delta_rpw01_10 <- eu_delta_rpw01_10[-1,]

exp_robs01_10 <- merge(cbp90czindp, eu_delta_rpw01_10, by = "isic") %>%
  group_by(czone) %>%
  summarise(eu_delta_exp_robs = ind_prop %*% eu_delta_rpw)



# 2010's Census

eu_rpw11_16 <- data.frame(matrix(rep(0, 24), nrow = 1))
colnames(eu_rpw11_16) <- colnames(eu_rpw95[[1]][,-c(1,2)])
eu_rpw11_16[["isic"]] = "Empty"

for (df in df_l){
  df_quantile <- apply(df[,-25], MARGIN = 2, quantile, 0.75)
  df_quantile$isic <- df$isic[1]
  eu_rpw11_16 <- rbind(eu_rpw11_16, df_quantile)
}

eu_rpw11_16 <- eu_rpw11_16[-1,c(25, 19:24)]


eu_delta_rpw11_16 <- eu_rpw11_16[-1,"yr.2016"] - eu_rpw11_16[-1,"yr.2011"]
eu_delta_rpw11_16 <- data.frame(isic = isic_group[-1],
                               eu_delta_rpw = eu_delta_rpw11_16)
eu_delta_rpw11_16 <- eu_delta_rpw11_16[-1,]

exp_robs11_16 <- merge(cbp90czindp, eu_delta_rpw11_16, by = "isic") %>%
  group_by(czone) %>%
  summarise(eu_delta_exp_robs = ind_prop %*% eu_delta_rpw)




### US CZ Delta Exposure to Robots ###

us_exprobs04_10.cz <- cz_exp_robs(begin = '2004',
                                 end = '2010',
                                 base = '1990',
                                 us = T)

us_exprobs11_16.cz <- cz_exp_robs(begin = '2011',
                                 end = '2016',
                                 base = '1990',
                                 us = T)



################################################################################



### County Demographics ###

# Commuting Zone County CW
cw_cz_cty <- read.dta("../data/cw/ddorn/cw_cty_czone.dta")
colnames(cw_cz_cty) <- c("czone", "fipscty")



# 1990's Census

demo_90 <- read.csv("../data/ipums/demog/demo_90.csv")
demo_90p <- demo_90

demo_90p[,c(4:9)] <- demo_90[,c(-1:-3)][,c(1:6)]/demo_90[,2]
demo_90p[,c(10:11)] <- demo_90[,c(-1:-3)][,c(7,8)]/demo_90[,2]
demo_90p[,c(12:18)] <- demo_90[,c(-1:-3)][,c(9:15)]/demo_90[,2]
demo_90p[,c(19:24)] <- demo_90[,c(-1:-3)][,c(16:21)]/demo_90[,3]

demo_90c <- merge(cw_cz_cty, demo_90p, by = "fipscty")


# 2000's Census

demo_00 <- read.csv("../data/ipums/demog/demo_00.csv")
demo_00p <- demo_00

demo_00p[,c(4:9)] <- demo_00[,c(-1:-3)][,c(1:6)]/demo_00[,2]
demo_00p[,c(10:11)] <- demo_00[,c(-1:-3)][,c(7,8)]/demo_00[,2]
demo_00p[,c(12:18)] <- demo_00[,c(-1:-3)][,c(9:15)]/demo_00[,2]
demo_00p[,c(19:24)] <- demo_00[,c(-1:-3)][,c(16:21)]/demo_00[,3]

demo_00c <- merge(cw_cz_cty, demo_00p, by = "fipscty")


# 2010's Census

demo_10 <- read.csv("../data/ipums/demog/demo_10.csv")
demo_10p <- demo_10

demo_10p[,c(4:9)] <- demo_10[,c(-1:-3)][,c(1:6)]/demo_10[,2]
demo_10p[,c(10:11)] <- demo_10[,c(-1:-3)][,c(7,8)]/demo_10[,2]
demo_10p[,c(12:18)] <- demo_10[,c(-1:-3)][,c(9:15)]/demo_10[,2]
demo_10p[,c(19:24)] <- demo_10[,c(-1:-3)][,c(16:21)]/demo_10[,3]

demo_10c <- merge(cw_cz_cty, demo_10p, by = "fipscty")



################################################################################


# China and Mexico Trade Data

china_trade <- industry_trade('china')
china_trade <- build_trade_df(china_trade)

mex_trade <- industry_trade('mexico')
mex_trade <- build_trade_df(mex_trade)


### Commuting Zone Import Penetration ###

# 1990's Census

china_imp90 <- delta_impPen(china_trade,
                         begin = '1993',
                         end = '2000')
china_imp90 <- cz_impPen(china_imp90, cbp90)
colnames(china_imp90) <- c("czone", "china_deltaip")

mex_imp90 <- delta_impPen(mex_trade,
                       begin = '1993',
                       end = '2000')
mex_imp90 <- cz_impPen(mex_imp90, cbp90)
colnames(mex_imp90) <- c("czone", "mex_deltaip")

imp_pen90 <- merge(china_imp90, mex_imp90, by = "czone")

# 2000's Census

china_imp00 <- delta_impPen(china_trade,
                         begin = '2001',
                         end = '2010')
china_imp00 <- cz_impPen(china_imp00, cbp90)
colnames(china_imp00) <- c("czone", "china_deltaip")

mex_imp00 <- delta_impPen(mex_trade,
                         begin = '2001',
                         end = '2010')
mex_imp00 <- cz_impPen(mex_imp00, cbp90)
colnames(mex_imp00) <- c("czone", "mex_deltaip")

imp_pen00 <- merge(china_imp00, mex_imp00, by = "czone")

# 2010's Census

china_imp10 <- delta_impPen(china_trade,
                         begin = '2011',
                         end = '2016')
china_imp10 <- cz_impPen(china_imp10, cbp90)
colnames(china_imp10) <- c("czone", "china_deltaip")

mex_imp10 <- delta_impPen(mex_trade,
                         begin = '1993',
                         end = '2000')
mex_imp10 <- cz_impPen(mex_imp10, cbp90)
colnames(mex_imp10) <- c("czone", "mex_deltaip")

imp_pen10 <- merge(china_imp10, mex_imp10, by = "czone")



################################################################################


### Political Data ###

# Nominate Scores

nom_1990.cc <- read.csv("../data/dw_nominate/nomc3/nom_1990cc.csv")
nom_2000.cc <- read.csv("../data/dw_nominate/nomc3/nom_2000cc.csv")
nom_2010.cc <- read.csv("../data/dw_nominate/nomc3/nom_2010cc.csv")

# Political Controls

he_90 <- read.csv("../data/cqpress/house/he_90.csv")
he_00 <- read.csv("../data/cqpress/house/he_00.csv")
he_10 <- read.csv("../data/cqpress/house/he_10.csv")


"Note: At-Large States are currently excluded from the analysis"

# Creating National Congressional District ID

dist_id90 <- str_pad(he_90$district_code, width = 3, pad = "0", side = "left")
he_90$dist_id <- paste0(he_90$fipstate, dist_id90)

dist_id00 <- str_pad(he_00$district_code, width = 3, pad = "0", side = "left")
he_00$dist_id <- paste0(he_00$fipstate, dist_id00)

dist_id10 <- str_pad(he_10$district_code, width = 3, pad = "0", side = "left")
he_10$dist_id <- paste0(he_10$fipstate, dist_id10)


pol_90 <- merge(nom_1990.cc, he_90, by = c("fipstate", "district_code"), all= F)
pol_00 <- merge(nom_2000.cc, he_00, by = c("fipstate", "district_code"), all= F)
pol_10 <- merge(nom_2010.cc, he_10, by = c("fipstate", "district_code"), all= F)



################################################################################


### Economic Controls ###

# CZ Share Manufacturing Employment

manuf_bool <- cbp90czindp_cty$isic %in% c("A", "B", "D-E", "F", "P", "Z")
cz_sh_manuf <- cbp90czindp_cty[!manuf_bool,] %>%
  group_by(czone, fipscty) %>%
  summarise(man_sh = sum(ind_prop))


# CZ Share Routine and Offshorability

ro_df <- read.dta("../data/ddorn/workfile2012.dta")
ro_df <- ro_df[,c("czone","yr","l_sh_routine33a","l_task_std_offshore")]


# 1990's Census

ro90 <- ro_df[ro_df$yr == 1990,] %>%
  mutate(sh_routine = l_sh_routine33a, sh_off = l_task_std_offshore) %>%
  select(czone, sh_routine, sh_off)

ecc90 <- merge(cz_sh_manuf, ro90, by = "czone")


# 2000's Census

ro00 <- ro_df[ro_df$yr == 2000,] %>%
  mutate(sh_routine = l_sh_routine33a, sh_off = l_task_std_offshore) %>%
  select(czone, sh_routine, sh_off)

ecc00 <- merge(cz_sh_manuf, ro00, by = "czone")


# 2010's Census

ro10 <- ro_df[ro_df$yr >= 2000,] %>%
  mutate(sh_routine = l_sh_routine33a, sh_off = l_task_std_offshore) %>%
  select(czone, sh_routine, sh_off)

ecc10 <- merge(cz_sh_manuf, ro10, by = "czone")


################################################################################


### First Stage DF's ###

# 1990's Census

cz_df93_00 <- merge(us_exprobs04_10.cz, exp_robs93_00, by = "czone")
cz_df93_00 <- merge(cz_df93_00, imp_pen90, by = "czone")
cz_df93_00 <- merge(cz_df93_00, demo_90c, by = "czone")
cz_df93_00 <- merge(cz_df93_00, ecc90, by = c("czone","fipscty"))
cz_df93_00 <- merge(cz_df93_00, pol_90, by = "fipscty")

write.dta(cz_df93_00, "../outputs/data/first_stage/cz_df93_00.dta")

# 2000's Census

cz_df01_10 <- merge(us_exprobs04_10.cz, exp_robs01_10, by = "czone")
cz_df01_10 <- merge(cz_df01_10, imp_pen00, by = "czone")
cz_df01_10 <- merge(cz_df01_10, demo_00c, by = "czone")
cz_df01_10 <- merge(cz_df01_10, ecc00, by = c("czone","fipscty"))
cz_df01_10 <- merge(cz_df01_10, pol_00, by = "fipscty")

write.dta(cz_df01_10, "../outputs/data/first_stage/cz_df01_10.dta")

# 2010's Census

cz_df11_16 <- merge(us_exprobs11_16.cz, exp_robs11_16, by = "czone")
cz_df11_16 <- merge(cz_df11_16, imp_pen10, by = "czone")
cz_df11_16 <- merge(cz_df11_16, demo_10c, by = "czone")
cz_df11_16 <- merge(cz_df11_16, ecc10, by = c("czone","fipscty"))
cz_df11_16 <- merge(cz_df11_16, pol_10, by = "fipscty")

write.dta(cz_df11_16, "../outputs/data/first_stage/cz_df11_16.dta")


cz_df93_00$t1 = 1
cz_df93_00$t2 = 0
cz_df93_00$t3 = 0
cz_df93_00$census = "1990"

cz_df01_10$t1 = 0
cz_df01_10$t2 = 1
cz_df01_10$t3 = 0
cz_df01_10$census = "2000"

cz_df11_16$t1 = 0
cz_df11_16$t2 = 0
cz_df11_16$t3 = 1
cz_df11_16$census = "2010"

full <- rbind(cz_df93_00, cz_df01_10)
full <- rbind(full, cz_df11_16) %>%
  arrange(czone, fipstate, district_code) %>%
  select(czone, fipstate, district_code, fipscty,
         census, t1, t2, t3, everything())

write.dta(full, "../outputs/data/first_stage/full_first.dta")


################################################################################

# Weighting For County-Congressional District Cells

weighted_columns <- c("delta_nom","nominate_beg","nominate_end",
                      "delta_exp_robs","eu_delta_exp_robs",
                      "china_deltaip", "mex_deltaip",
                      "man_sh", "sh_routine", "sh_off",
                      "tot_pop", "wa_pop",
                      "juv", "yngad", "adlt", "miad1","miad2", "sen",
                      "males", "females",
                      "white_nh", "black_nh", "nw_hisp",
                      "wht_hisp","asia_pi", "am_in", "other",
                      "no_dip","hs_dip","some_col", "assoc", "bach","grad_plus",
                      "win_prop")

full90cc <- cz_df93_00

wghts90 <- diag(full90cc$cd_prop_pop)
full90mat <- as.matrix(full90cc[,weighted_columns])
full90wghted <- t(full90mat) %*% wghts90
full90cc[,weighted_columns] <- t(full90wghted)

write.dta(full90cc, "../outputs/data/full90cc.dta")


full00cc <- cz_df01_10

wghts00 <- diag(full00cc$cd_prop_pop)
full00mat <- as.matrix(full00cc[,weighted_columns])
full00wghted <- t(full00mat) %*% wghts00
full00cc[,weighted_columns] <- t(full00wghted)

write.dta(full00cc, "../outputs/data/full00cc.dta")


full10cc <- cz_df11_16

wghts10 <- diag(full10cc$cd_prop_pop)
full10mat <- as.matrix(full10cc[,weighted_columns])
full10wghted <- t(full10mat) %*% wghts10
full10cc[,weighted_columns] <- t(full10wghted)

write.dta(full10cc, "../outputs/data/full10cc.dta")

################################################################################



