# Title: 
# Description: 
# Inputs: 
# Outputs:

# Source Files:
source("functions.R")

# Packages:
library(dplyr)
library(data.table)
library(purrr)

################################################################################
################################################################################
################################################################################


## US Employment Data

#1980's
cbp80 <- cbpdd_build('1980')
cbp80n <- national_isic(cbp80)
cbp80c <- county_emp(cbp80)
cbp80czind <- ind_cz_emp(cbp80)
cbp80czindp <- cz_ind_prop(cbp80)
cbp80czindp_cty <- cz.fips_data(cbp80czindp)

#1990's
cbp90 <- cbpdd_build()
cbp90n <- national_isic(cbp90)
cbp90c <- county_emp(cbp90)
cbp90czind <- ind_cz_emp(cbp90)
cbp90czindp <- cz_ind_prop(cbp90)
cbp90czindp_cty <- cz.fips_data(cbp90czindp)

#2000's
cbp00 <- cbpdd_build('2000')
cbp00n <- national_isic(cbp00)
cbp00c <- county_emp(cbp00)
cbp00czind <- ind_cz_emp(cbp00)
cbp00czindp <- cz_ind_prop(cbp00)
cbp00czindp_cty <- cz.fips_data(cbp00czindp)

#2010's
cbp10 <- read.csv("../data/cbp/cbp10co.csv")
cbp10 <- clean_cbp(cbp10)


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

exp_robs93_00 <- merge(cbp80czindp, eu_delta_rpw93_00, by = "isic") %>%
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

exp_robs01_10 <- merge(cbp80czindp, eu_delta_rpw01_10, by = "isic") %>%
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

exp_robs11_16 <- merge(cbp80czindp, eu_delta_rpw11_16, by = "isic") %>%
  group_by(czone) %>%
  summarise(eu_delta_exp_robs = ind_prop %*% eu_delta_rpw)


### Presidential ###

### 1992-2008

eu_rpw93_08 <- data.frame(matrix(rep(0, 24), nrow = 1))
colnames(eu_rpw93_08) <- colnames(eu_rpw95[[1]][,-c(1,2)])
eu_rpw93_08[["isic"]] = "Empty"


for (df in df_l){
  df_quantile <- apply(df[,-25], MARGIN = 2, quantile, 0.75)
  df_quantile$isic <- df$isic[1]
  eu_rpw93_08 <- rbind(eu_rpw93_08, df_quantile)
}

eu_rpw93_08 <- eu_rpw93_08[-1,c(25, 1:16)]

eu_delta_rpw93_08 <- eu_rpw93_08[-1,"yr.2008"] - eu_rpw93_08[-1,"yr.1993"]
eu_delta_rpw93_08 <- data.frame(isic = isic_group[-1],
                                eu_delta_rpw = eu_delta_rpw93_08)
eu_delta_rpw93_08 <- eu_delta_rpw93_08[-1,]

exp_robs93_08 <- merge(cbp80czindp, eu_delta_rpw93_08, by = "isic") %>%
  group_by(czone) %>%
  summarise(eu_delta_exp_robs = ind_prop %*% eu_delta_rpw)

### 1992-2016

eu_rpw93_16 <- data.frame(matrix(rep(0, 24), nrow = 1))
colnames(eu_rpw93_16) <- colnames(eu_rpw95[[1]][,-c(1,2)])
eu_rpw93_16[["isic"]] = "Empty"


for (df in df_l){
  df_quantile <- apply(df[,-25], MARGIN = 2, quantile, 0.75)
  df_quantile$isic <- df$isic[1]
  eu_rpw93_16 <- rbind(eu_rpw93_16, df_quantile)
}

eu_rpw93_16 <- eu_rpw93_16[-1,c(25, 1:24)]

eu_delta_rpw93_16 <- eu_rpw93_16[-1,"yr.2016"] - eu_rpw93_16[-1,"yr.1993"]
eu_delta_rpw93_16 <- data.frame(isic = isic_group[-1],
                                eu_delta_rpw = eu_delta_rpw93_16)
eu_delta_rpw93_16 <- eu_delta_rpw93_16[-1,]

exp_robs93_16 <- merge(cbp80czindp, eu_delta_rpw93_16, by = "isic") %>%
  group_by(czone) %>%
  summarise(eu_delta_exp_robs = ind_prop %*% eu_delta_rpw)


### 2000-2008

eu_rpw00_08 <- data.frame(matrix(rep(0, 24), nrow = 1))
colnames(eu_rpw00_08) <- colnames(eu_rpw95[[1]][,-c(1,2)])
eu_rpw00_08[["isic"]] = "Empty"

for (df in df_l){
  df_quantile <- apply(df[,-25], MARGIN = 2, quantile, 0.75)
  df_quantile$isic <- df$isic[1]
  eu_rpw00_08 <- rbind(eu_rpw00_08, df_quantile)
}

eu_rpw00_08 <- eu_rpw00_08[-1,c(25, 8:16)]

eu_delta_rpw00_08 <- eu_rpw00_08[-1,"yr.2008"] - eu_rpw00_08[-1,"yr.2000"]
eu_delta_rpw00_08 <- data.frame(isic = isic_group[-1],
                                eu_delta_rpw = eu_delta_rpw00_08)
eu_delta_rpw00_08 <- eu_delta_rpw00_08[-1,]

exp_robs00_08 <- merge(cbp80czindp, eu_delta_rpw00_08, by = "isic") %>%
  group_by(czone) %>%
  summarise(eu_delta_exp_robs = ind_prop %*% eu_delta_rpw)

### 2000-2016

eu_rpw00_16 <- data.frame(matrix(rep(0, 24), nrow = 1))
colnames(eu_rpw00_16) <- colnames(eu_rpw95[[1]][,-c(1,2)])
eu_rpw00_16[["isic"]] = "Empty"

for (df in df_l){
  df_quantile <- apply(df[,-25], MARGIN = 2, quantile, 0.75)
  df_quantile$isic <- df$isic[1]
  eu_rpw00_16 <- rbind(eu_rpw00_16, df_quantile)
}

eu_rpw00_16 <- eu_rpw00_16[-1,c(25, 8:24)]

eu_delta_rpw00_16 <- eu_rpw00_16[-1,"yr.2016"] - eu_rpw00_16[-1,"yr.2000"]
eu_delta_rpw00_16 <- data.frame(isic = isic_group[-1],
                                eu_delta_rpw = eu_delta_rpw00_16)
eu_delta_rpw00_16 <- eu_delta_rpw00_16[-1,]

exp_robs00_16 <- merge(cbp80czindp, eu_delta_rpw00_16, by = "isic") %>%
  group_by(czone) %>%
  summarise(eu_delta_exp_robs = ind_prop %*% eu_delta_rpw)

### 2008-2016

eu_rpw08_16 <- data.frame(matrix(rep(0, 24), nrow = 1))
colnames(eu_rpw08_16) <- colnames(eu_rpw95[[1]][,-c(1,2)])
eu_rpw08_16[["isic"]] = "Empty"

for (df in df_l){
  df_quantile <- apply(df[,-25], MARGIN = 2, quantile, 0.75)
  df_quantile$isic <- df$isic[1]
  eu_rpw08_16 <- rbind(eu_rpw08_16, df_quantile)
}

eu_rpw08_16 <- eu_rpw08_16[-1,c(25, 16:24)]

eu_delta_rpw08_16 <- eu_rpw08_16[-1,"yr.2016"] - eu_rpw08_16[-1,"yr.2008"]
eu_delta_rpw08_16 <- data.frame(isic = isic_group[-1],
                                eu_delta_rpw = eu_delta_rpw08_16)
eu_delta_rpw08_16 <- eu_delta_rpw08_16[-1,]

exp_robs08_16 <- merge(cbp80czindp, eu_delta_rpw08_16, by = "isic") %>%
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

### Presidential

us_exprobs04_08.cz <- cz_exp_robs(begin = '2004',
                                  end = '2008',
                                  base = '1990',
                                  us = T)

us_exprobs04_16.cz <- cz_exp_robs(begin = '2004',
                                  end = '2016',
                                  base = '1990',
                                  us = T)

us_exprobs08_16.cz <- cz_exp_robs(begin = '2008',
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

demo_90cz1 <- demo_90c %>%
  group_by(czone) %>%
  summarise_all(sum) %>%
  select(czone, tot_pop, wa_pop)

demo_90cz2 <- demo_90c %>%
  group_by(czone) %>%
  summarise_all(mean) %>%
  select(-fipscty, -tot_pop, -wa_pop)

demo_90cz <- merge(demo_90cz1, demo_90cz2, by = "czone")

# 2000's Census

demo_00 <- read.csv("../data/ipums/demog/demo_00.csv")
demo_00p <- demo_00

demo_00p[,c(4:9)] <- demo_00[,c(-1:-3)][,c(1:6)]/demo_00[,2]
demo_00p[,c(10:11)] <- demo_00[,c(-1:-3)][,c(7,8)]/demo_00[,2]
demo_00p[,c(12:18)] <- demo_00[,c(-1:-3)][,c(9:15)]/demo_00[,2]
demo_00p[,c(19:24)] <- demo_00[,c(-1:-3)][,c(16:21)]/demo_00[,3]

demo_00c <- merge(cw_cz_cty, demo_00p, by = "fipscty")

demo_00cz1 <- demo_00c %>%
  group_by(czone) %>%
  summarise_all(sum) %>%
  select(czone, tot_pop, wa_pop)

demo_00cz2 <- demo_00c %>%
  group_by(czone) %>%
  summarise_all(mean) %>%
  select(-fipscty, -tot_pop, -wa_pop)

demo_00cz <- merge(demo_00cz1, demo_00cz2, by = "czone")

# 2010's Census

demo_10 <- read.csv("../data/ipums/demog/demo_10.csv")
demo_10p <- demo_10

demo_10p[,c(4:9)] <- demo_10[,c(-1:-3)][,c(1:6)]/demo_10[,2]
demo_10p[,c(10:11)] <- demo_10[,c(-1:-3)][,c(7,8)]/demo_10[,2]
demo_10p[,c(12:18)] <- demo_10[,c(-1:-3)][,c(9:15)]/demo_10[,2]
demo_10p[,c(19:24)] <- demo_10[,c(-1:-3)][,c(16:21)]/demo_10[,3]

demo_10c <- merge(cw_cz_cty, demo_10p, by = "fipscty") 

demo_10cz1 <- demo_10c %>%
  group_by(czone) %>%
  summarise_all(sum) %>%
  select(czone, tot_pop, wa_pop)

demo_10cz2 <- demo_10c %>%
  group_by(czone) %>%
  summarise_all(mean) %>%
  select(-fipscty, -tot_pop, -wa_pop)

demo_10cz <- merge(demo_10cz1, demo_10cz2, by = "czone")

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
                         begin = '2000',
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
                         begin = '2011',
                         end = '2016')
mex_imp10 <- cz_impPen(mex_imp10, cbp90)
colnames(mex_imp10) <- c("czone", "mex_deltaip")

imp_pen10 <- merge(china_imp10, mex_imp10, by = "czone")


### Presidential ###

# 1992-2008

china_imp92_08 <- delta_impPen(china_trade,
                               begin = '1992',
                               end = '2008')
china_imp92_08 <- cz_impPen(china_imp92_08, cbp90)
colnames(china_imp92_08) <- c("czone", "china_deltaip")

mex_imp92_08 <- delta_impPen(mex_trade,
                             begin = '1992',
                             end = '2008')
mex_imp92_08 <- cz_impPen(mex_imp92_08, cbp90)
colnames(mex_imp92_08) <- c("czone", "mex_deltaip")

imp_pen92_08 <- merge(china_imp92_08, mex_imp92_08, by = "czone")

# 1992-2016

china_imp92_16 <- delta_impPen(china_trade,
                               begin = '1992',
                               end = '2016')
china_imp92_16 <- cz_impPen(china_imp92_16, cbp90)
colnames(china_imp92_16) <- c("czone", "china_deltaip")

mex_imp92_16 <- delta_impPen(mex_trade,
                             begin = '1992',
                             end = '2016')
mex_imp92_16 <- cz_impPen(mex_imp92_16, cbp90)
colnames(mex_imp92_16) <- c("czone", "mex_deltaip")

imp_pen92_16 <- merge(china_imp92_16, mex_imp92_16, by = "czone")


# 2000-2008

china_imp00_08 <- delta_impPen(china_trade,
                            begin = '2000',
                            end = '2008')
china_imp00_08 <- cz_impPen(china_imp00_08, cbp90)
colnames(china_imp00_08) <- c("czone", "china_deltaip")

mex_imp00_08 <- delta_impPen(mex_trade,
                          begin = '2000',
                          end = '2008')
mex_imp00_08 <- cz_impPen(mex_imp00_08, cbp90)
colnames(mex_imp00_08) <- c("czone", "mex_deltaip")

imp_pen00_08 <- merge(china_imp00_08, mex_imp00_08, by = "czone")

# 2000-2016

china_imp00_16 <- delta_impPen(china_trade,
                               begin = '2000',
                               end = '2016')
china_imp00_16 <- cz_impPen(china_imp00_16, cbp90)
colnames(china_imp00_16) <- c("czone", "china_deltaip")

mex_imp00_16 <- delta_impPen(mex_trade,
                             begin = '2000',
                             end = '2016')
mex_imp00_16 <- cz_impPen(mex_imp00_16, cbp90)
colnames(mex_imp00_16) <- c("czone", "mex_deltaip")

imp_pen00_16 <- merge(china_imp00_16, mex_imp00_16, by = "czone")

# 2008-2016

china_imp08_16 <- delta_impPen(china_trade,
                               begin = '2008',
                               end = '2016')
china_imp08_16 <- cz_impPen(china_imp08_16, cbp90)
colnames(china_imp08_16) <- c("czone", "china_deltaip")

mex_imp08_16 <- delta_impPen(mex_trade,
                             begin = '2008',
                             end = '2016')
mex_imp08_16 <- cz_impPen(mex_imp08_16, cbp90)
colnames(mex_imp08_16) <- c("czone", "mex_deltaip")

imp_pen08_16 <- merge(china_imp08_16, mex_imp08_16, by = "czone")


### Trade Instruments ###

t_iv_dirs <- list.files('../data/un_comtrade/')
t_iv_bool <- str_detect(t_iv_dirs, "^us-")
t_iv_subdirs <- t_iv_dirs[!t_iv_bool]
t_iv_nat <- toupper(str_extract(t_iv_subdirs, "[a-z]{2,3}"))

t_iv_dfs <- list()

for (i in 1:length(t_iv_nat)){
  t_iv_path <- paste0('../data/un_comtrade/', t_iv_subdirs[i], "/")
  t_files <- list.files(t_iv_path)
  t_file_paths <- paste0(t_iv_path, t_files)
  t_dfs <- lapply(t_file_paths, read.csv, stringsAsFactors = F)
  t_dfs <- lapply(t_dfs, clean_trade)
  t_df <- rbindlist(t_dfs)
  t_iv_dfs[[t_iv_nat[i]]] <- as.data.frame(t_df)
}

t_iv_dfs <- lapply(t_iv_dfs, hs6_to_sicdd)
t_iv_dfs <- lapply(t_iv_dfs, iapp_sf_sic_isic)
t_iv_dfs <- lapply(t_iv_dfs, assign_isic, ind_code = "sic")
t_iv_dfs <- lapply(t_iv_dfs, iapp_sf_isic_agg)
t_iv_dfs <- lapply(t_iv_dfs, exp_im_df_build)
t_iv_dfs <- lapply(t_iv_dfs, add_domestic_industry_output)
t_iv_dfs <- lapply(t_iv_dfs, add_initial_absorbtion)

drop_nonman <- function(dataframe){
  non_man_bool <- str_detect(dataframe$isic, "[A-Z]+")
  dataframe <- dataframe[!non_man_bool,]
  return(dataframe)
}

t_iv_dfs <- lapply(t_iv_dfs, drop_nonman)

use_cbp80_impPen <- function(trade_df){
  cz_impPen(trade_df, cbp80)
}

use_cbp90_impPen <- function(trade_df){
  cz_impPen(trade_df, cbp90)
}

use_cbp00_impPen <- function(trade_df){
  cz_impPen(trade_df, cbp00)
}

add_country_names <- function(dataframe_list){
  country_names <- names(dataframe_list)
  
  for (i in 1: length(dataframe_list)){
    county_label <- paste(country_names[i], "delta_ip", sep = "_")
    colnames(dataframe_list[[i]]) <- c("isic", county_label)
  }
  return(dataframe_list)
}

# 1990's Census

t_iv90_dfs <- lapply(t_iv_dfs, delta_impPen_w.lag,
                   begin = '2001',
                   end = '2010',
                   lag = 3)

t_iv90_dfs <- add_country_names(t_iv90_dfs)
t_iv90_df <- t_iv90_dfs %>% reduce(inner_join, by = "isic")
t_iv90_vec <- apply(t_iv90_df[,-1], MARGIN = 1, FUN = mean, na.rm = T)
t_iv90_df <- data.frame(isic = t_iv90_dfs[[1]]$isic,
                        delta_ip = t_iv90_vec)
t_iv90_df <- cz_impPen(t_iv90_df, cbp80)
colnames(t_iv90_df) <- c("czone", "eu_mu_deltaip")


# 2000's Census

t_iv00_dfs <- lapply(t_iv_dfs, delta_impPen_w.lag,
                     begin = '1993',
                     end = '2000',
                     lag = 3)
t_iv00_dfs <- add_country_names(t_iv00_dfs)
t_iv00_df <- t_iv00_dfs %>% reduce(left_join, by = "isic")
t_iv00_vec <- apply(t_iv00_df[,-1], MARGIN = 1, FUN = mean, na.rm = T)
t_iv00_df <- data.frame(isic = t_iv00_dfs[[1]]$isic,
                        delta_ip = t_iv00_vec)
t_iv00_df <- cz_impPen(t_iv00_df, cbp80)
colnames(t_iv00_df) <- c("czone", "eu_mu_deltaip")


# 2010's Census

t_iv10_dfs <- lapply(t_iv_dfs, delta_impPen_w.lag,
                     begin = '2011',
                     end = '2016',
                     lag = 3)
t_iv10_dfs <- add_country_names(t_iv10_dfs)
t_iv10_df <- t_iv10_dfs %>% reduce(left_join, by = "isic")
t_iv10_vec <- apply(t_iv10_df[,-1], MARGIN = 1, FUN = mean, na.rm = T)
t_iv10_df <- data.frame(isic = t_iv10_dfs[[1]]$isic,
                        delta_ip = t_iv10_vec)
t_iv10_df <- cz_impPen(t_iv10_df, cbp80)
colnames(t_iv10_df) <- c("czone", "eu_mu_deltaip")


# 1992-2000

t_iv92_00_dfs <- lapply(t_iv_dfs, delta_impPen_w.lag,
                     begin = '1992',
                     end = '2000',
                     lag = 3)
t_iv92_00_dfs <- add_country_names(t_iv92_00_dfs)
t_iv92_00_df <- t_iv92_00_dfs %>% reduce(left_join, by = "isic")
t_iv92_00_vec <- apply(t_iv92_00_df[,-1], MARGIN = 1, FUN = mean, na.rm = T)
t_iv92_00_df <- data.frame(isic = t_iv92_00_dfs[[1]]$isic,
                        delta_ip = t_iv92_00_vec)
t_iv92_00_df <- cz_impPen(t_iv92_00_df, cbp80)
colnames(t_iv92_00_df) <- c("czone", "eu_mu_deltaip")

# 1992-2008

t_iv92_08_dfs <- lapply(t_iv_dfs, delta_impPen_w.lag,
                     begin = '1992',
                     end = '2008',
                     lag = 3)
t_iv92_08_dfs <- add_country_names(t_iv92_08_dfs)
t_iv92_08_df <- t_iv92_08_dfs %>% reduce(left_join, by = "isic")
t_iv92_08_vec <- apply(t_iv92_08_df[,-1], MARGIN = 1, FUN = mean, na.rm = T)
t_iv92_08_df <- data.frame(isic = t_iv92_08_dfs[[1]]$isic,
                           delta_ip = t_iv92_08_vec)
t_iv92_08_df <- cz_impPen(t_iv92_08_df, cbp80)
colnames(t_iv92_08_df) <- c("czone", "eu_mu_deltaip")

# 1992-2016

t_iv92_16_dfs <- lapply(t_iv_dfs, delta_impPen_w.lag,
                        begin = '1992',
                        end = '2016',
                        lag = 3)
t_iv92_16_dfs <- add_country_names(t_iv92_16_dfs)
t_iv92_16_df <- t_iv92_16_dfs %>% reduce(left_join, by = "isic")
t_iv92_16_vec <- apply(t_iv92_16_df[,-1], MARGIN = 1, FUN = mean, na.rm = T)
t_iv92_16_df <- data.frame(isic = t_iv92_16_dfs[[1]]$isic,
                           delta_ip = t_iv92_16_vec)
t_iv92_16_df <- cz_impPen(t_iv92_16_df, cbp80)
colnames(t_iv92_16_df) <- c("czone", "eu_mu_deltaip")

# 2000-2008

t_iv00_08_dfs <- lapply(t_iv_dfs, delta_impPen_w.lag,
                        begin = '2000',
                        end = '2008',
                        lag = 3)
t_iv00_08_dfs <- add_country_names(t_iv00_08_dfs)
t_iv00_08_df <- t_iv00_08_dfs %>% reduce(left_join, by = "isic")
t_iv00_08_vec <- apply(t_iv00_08_df[,-1], MARGIN = 1, FUN = mean, na.rm = T)
t_iv00_08_df <- data.frame(isic = t_iv00_08_dfs[[1]]$isic,
                           delta_ip = t_iv00_08_vec)
t_iv00_08_df <- cz_impPen(t_iv00_08_df, cbp80)
colnames(t_iv00_08_df) <- c("czone", "eu_mu_deltaip")

# 2000-2016

t_iv00_16_dfs <- lapply(t_iv_dfs, delta_impPen_w.lag,
                        begin = '2000',
                        end = '2016',
                        lag = 3)
t_iv00_16_dfs <- add_country_names(t_iv00_16_dfs)
t_iv00_16_df <- t_iv00_16_dfs %>% reduce(left_join, by = "isic")
t_iv00_16_vec <- apply(t_iv00_16_df[,-1], MARGIN = 1, FUN = mean, na.rm = T)
t_iv00_16_df <- data.frame(isic = t_iv00_16_dfs[[1]]$isic,
                           delta_ip = t_iv00_16_vec)
t_iv00_16_df <- cz_impPen(t_iv00_16_df, cbp80)
colnames(t_iv00_16_df) <- c("czone", "eu_mu_deltaip")

# 2008-2016

t_iv08_16_dfs <- lapply(t_iv_dfs, delta_impPen_w.lag,
                        begin = '2008',
                        end = '2016',
                        lag = 3)
t_iv08_16_dfs <- add_country_names(t_iv08_16_dfs)
t_iv08_16_df <- t_iv08_16_dfs %>% reduce(left_join, by = "isic")
t_iv08_16_vec <- apply(t_iv08_16_df[,-1], MARGIN = 1, FUN = mean, na.rm = T)
t_iv08_16_df <- data.frame(isic = t_iv08_16_dfs[[1]]$isic,
                           delta_ip = t_iv08_16_vec)
t_iv08_16_df <- cz_impPen(t_iv08_16_df, cbp80)
colnames(t_iv08_16_df) <- c("czone", "eu_mu_deltaip")

################################################################################


### Political Data ###

# Nominate Scores

nom_1990.cc <- read.csv("../data/dw_nominate/nomc3/nom_1990cc.csv")
nom_2000.cc <- read.csv("../data/dw_nominate/nomc3/nom_2000cc.csv")
nom_2010.cc <- read.csv("../data/dw_nominate/nomc3/nom_2010cc.csv")

# Political Controls

he_90 <- read.csv("../data/cqpress/house/he_92_00.csv")
he_00 <- read.csv("../data/cqpress/house/he_02_10.csv")
he_10 <- read.csv("../data/cqpress/house/he_12_16.csv")


"Note: At-Large States are currently excluded from the analysis"

# Creating National Congressional District ID

dist_id90 <- str_pad(he_90$district_code, width = 3, pad = "0", side = "left")
he_90$dist_id <- as.numeric(paste0(he_90$fipstate, dist_id90))

dist_id00 <- str_pad(he_00$district_code, width = 3, pad = "0", side = "left")
he_00$dist_id <- as.numeric(paste0(he_00$fipstate, dist_id00))

dist_id10 <- str_pad(he_10$district_code, width = 3, pad = "0", side = "left")
he_10$dist_id <- as.numeric(paste0(he_10$fipstate, dist_id10))


p_92_00 <- read.csv("../data/cqpress/presidential/p_92_00.csv", stringsAsFactors = F)
p_92_08 <- read.csv("../data/cqpress/presidential/p_92_08.csv", stringsAsFactors = F)
p_92_16 <- read.csv("../data/cqpress/presidential/p_92_16.csv", stringsAsFactors = F)
p_00_08 <- read.csv("../data/cqpress/presidential/p_00_08.csv", stringsAsFactors = F)
p_00_16 <- read.csv("../data/cqpress/presidential/p_00_16.csv", stringsAsFactors = F)
p_08_16 <- read.csv("../data/cqpress/presidential/p_08_16.csv", stringsAsFactors = F)


pol_90 <- merge(nom_1990.cc, he_90, by = c("fipstate", "district_code"), all= F)
pol_00 <- merge(nom_2000.cc, he_00, by = c("fipstate", "district_code"), all= F)
pol_10 <- merge(nom_2010.cc, he_10, by = c("fipstate", "district_code"), all= F)



################################################################################


### Economic Controls ###

## CZ Share Manufacturing Employment

#1990's
manuf_bool_90 <- cbp90czindp$isic %in% c("A", "B", "D-E", "F", "P", "Z")
cz_sh_manuf_90 <- cbp90czindp[!manuf_bool_90,] %>%
  group_by(czone) %>%
  summarise(man_sh = sum(ind_prop))

#1990's
manuf_bool_00 <- cbp00czindp$isic %in% c("A", "B", "D-E", "F", "P", "Z")
cz_sh_manuf_00 <- cbp00czindp[!manuf_bool_00,] %>%
  group_by(czone) %>%
  summarise(man_sh = sum(ind_prop))

#2010's
cbp10t <- cbp10[cbp10$naics == "------",]
cbp10t <- replace_with_lb(cbp10t)
cbp10t<- cnty_fips(cbp10t)
cbp10t <- merge(cbp10t, cw_cz_cty, by = 'fipscty') %>%
  group_by(czone) %>%
  summarise(tot_emp = sum(emp))

cbp10m <- cbp10[cbp10$naics == "31----",]
cbp10m <- replace_with_lb(cbp10m)
cbp10m <- cnty_fips(cbp10m)
cbp10m <- merge(cbp10m, cw_cz_cty, by = 'fipscty') %>%
  group_by(czone) %>%
  summarise(man_emp = sum(emp))

cz_sh_manuf_10 <- merge(cbp10t, cbp10m, by = 'czone') %>%
  mutate(man_sh = man_emp/tot_emp) %>%
  select(czone, man_sh)
#cz_sh_manuf_10 <- merge(cz_sh_manuf_10cz, cw_cz_cty, by = "czone")


# CZ Share Routine and Offshorability

ro_df <- read.dta("../data/ddorn/workfile2012.dta")
ro_df <- ro_df[,c("czone","yr","l_sh_routine33a","l_task_std_offshore")]


# 1990's Census

ro90 <- ro_df[ro_df$yr == 1990,] %>%
  mutate(sh_routine = l_sh_routine33a, sh_off = l_task_std_offshore) %>%
  select(czone, sh_routine, sh_off)

ecc90 <- merge(cz_sh_manuf_90, ro90, by = "czone")


# 2000's Census

ro00 <- ro_df[ro_df$yr == 2000,] %>%
  mutate(sh_routine = l_sh_routine33a, sh_off = l_task_std_offshore) %>%
  select(czone, sh_routine, sh_off)

ecc00 <- merge(cz_sh_manuf_00, ro00, by = "czone")


# 2010's Census

ro10 <- ro_df[ro_df$yr >= 2000,] %>%
  mutate(sh_routine = l_sh_routine33a, sh_off = l_task_std_offshore) %>%
  select(czone, sh_routine, sh_off)

ecc10 <- merge(cz_sh_manuf_10, ro10, by = "czone")


################################################################################

states_dict <- read.csv('../data/cw/icpsrcnt.csv') %>%
  mutate(fipstate = STATEFIPS) %>%
  group_by(State, fipstate) %>%
  summarise(count = length(STATEFIPS)) %>%
  select(State, fipstate)

### Census Divisions ###

div_1 <- c("Connecticut", "Maine", "Massachusetts",
           "New Hampshire", "Rhode Island", "Vermont")
div_2 <- c("New Jersey", "New York", "Pennsylvania")
div_3 <- c("Illinois", "Indiana", "Michigan", "Ohio", "Wisconsin")
div_4 <- c("Iowa", "Kansas", "Minnesota", "Missouri",
            "Nebraska", "North Dakota", "South Dakota")
div_5 <- c("Delaware", "Florida", "Georgia", "Maryland", "North Carolina",
           "South Carolina", "Virginia", "District of Columbia", "West Virginia")
div_6 <- c("Alabama", "Kentucky", "Mississippi", "Tennessee")
div_7 <- c("Arkansas", "Louisiana", "Oklahoma", "Texas")
div_8 <- c("Arizona", "Colorado", "Idaho", "Montana",
           "Nevada", "New Mexico", "Utah",  "Wyoming")
div_9 <-c("Alaska", "California", "Hawaii", "Oregon", "Washington")

division_list <- list(div_1, div_2, div_3,
                    div_4, div_5, div_6,
                    div_7, div_8, div_9)

census_div_df <- data.frame(State = division_list[[1]],
                            census_division = 1)

for (i in 2:9){ 
  new_div_df <- data.frame(State = division_list[[i]],
                           census_division = i)
  census_div_df <- rbind(census_div_df, new_div_df)
}
 
census_divs <- merge(census_div_df, states_dict, by = "State", all.y = F)

cw_state_cty <- pol_10 %>%
  group_by(fipstate, fipscty) %>%
  select(fipstate, fipscty)

cw_state_czone <- merge(cw_state_cty, cw_cz_cty, by = "fipscty") %>%
  group_by(fipstate, czone) %>%
  summarise(temp = sum(fipscty)) %>%
  mutate(temp2 = 1) %>%
  group_by(fipstate, czone) %>%
  summarise(temp2 = sum(temp2)) %>%
  select(fipstate, czone)

cw_cend_cz <- merge(census_divs, cw_state_czone, by = "fipstate") %>%
  group_by(census_division, czone) %>%
  select(census_division, czone)


################################################################################


### Second Stage DF's ###


### 1990's Census
ssdf90cz <- merge(us_exprobs04_10.cz, exp_robs93_00, by = "czone")
ssdf90cz <- merge(ssdf90cz, imp_pen90, by = "czone")
ssdf90cz <- merge(ssdf90cz, ecc90, by = "czone")
ssdf90cz <- merge(ssdf90cz, t_iv90_df, by = "czone")
ssdf90 <- merge(ssdf90cz, demo_90c, by = "czone")
ssdf90 <- merge(ssdf90, pol_90, by = "fipscty")
ssdf90 <- merge(ssdf90, census_divs, by = "fipstate")



# write.dta(ssdf90, "../outputs/data/ssdf90.dta")

### 2000's Census

ssdf00cz <- merge(us_exprobs04_10.cz, exp_robs01_10, by = "czone")
ssdf00cz <- merge(ssdf00cz, imp_pen00, by = "czone")
ssdf00cz <- merge(ssdf00cz, ecc00, by = "czone")
ssdf00cz <- merge(ssdf00cz, t_iv00_df, by = "czone")
ssdf00 <- merge(ssdf00cz, demo_00c, by = "czone")
ssdf00 <- merge(ssdf00, pol_00, by = "fipscty")
ssdf00 <- merge(ssdf00, census_divs, by = "fipstate")


# write.dta(ssdf00, "../outputs/data/ssdf00.dta")

### 2010's Census

ssdf10cz <- merge(us_exprobs11_16.cz, exp_robs11_16, by = "czone")
ssdf10cz <- merge(ssdf10cz, imp_pen10, by = "czone")
ssdf10cz <- merge(ssdf10cz, ecc10, by = "czone")
ssdf10cz <- merge(ssdf10cz, t_iv10_df, by = "czone")
ssdf10 <- merge(ssdf10cz, demo_10c, by = "czone")
ssdf10 <- merge(ssdf10, pol_10, by = "fipscty")
ssdf10 <- merge(ssdf10, census_divs, by = "fipstate")


# write.dta(ssdf10, "../outputs/data/ssdf10.dta")

# CZ DF's

ssdf90CZ <- merge(ssdf90cz, demo_90cz, by = "czone")
ssdf00CZ <- merge(ssdf00cz, demo_00cz, by = "czone")
ssdf10CZ <- merge(ssdf10cz, demo_10cz, by = "czone")

ssdf90CZ$t1 = 1
ssdf90CZ$t2 = 0
ssdf90CZ$t3 = 0
ssdf90CZ$census = "1990"

ssdf00CZ$t1 = 0
ssdf00CZ$t2 = 1
ssdf00CZ$t3 = 0
ssdf00CZ$census = "2000"

ssdf10CZ$t1 = 0
ssdf10CZ$t2 = 0
ssdf10CZ$t3 = 1
ssdf10CZ$census = "2010"


fsscz <- rbind(ssdf90CZ, ssdf00CZ)
fsscz <- rbind(fsscz, ssdf10CZ)
fsscz <- merge(fsscz, cw_cend_cz, by = "czone", all.x=T)
fsscz <- fsscz %>%
  arrange(czone, census_division) %>%
  select(czone, census, census_division, t1, t2, t3, everything())

write.dta(fsscz, "../outputs/data/fsscz.dta")



# CTY-CD DF's

ssdf90$t1 = 1
ssdf90$t2 = 0
ssdf90$t3 = 0
ssdf90$census = "1990"

ssdf00$t1 = 0
ssdf00$t2 = 1
ssdf00$t3 = 0
ssdf00$census = "2000"

ssdf10$t1 = 0
ssdf10$t2 = 0
ssdf10$t3 = 1
ssdf10$census = "2010"

fss <- rbind(ssdf90, ssdf00)
fss <- rbind(fss, ssdf10) %>%
  arrange(czone, fipstate, district_code) %>%
  select(czone, fipstate, district_code, fipscty,
         census, t1, t2, t3, everything())

fss$beg_party[fss$beg_party == 100] <- "Democrat"
fss$beg_party[fss$beg_party == 200] <- "GOP"
fss$beg_party[fss$beg_party == 328] <- "Independent"
fss$end_party[fss$end_party == 100] <- "Democrat"
fss$end_party[fss$end_party == 200] <- "GOP"
fss$end_party[fss$end_party == 328] <- "Independent"

fss$party_held = 0
fss$party_held[fss$beg_party == fss$end_party] = 1


write.dta(fss, "../outputs/data/fss.dta")

################################################################################

### Presidential DF's ###

### 1992-2000

ssdf92_00.p <- merge(us_exprobs04_08.cz, exp_robs93_00, by = "czone")
ssdf92_00.p <- merge(ssdf92_00.p, imp_pen90, by = "czone")
ssdf92_00.p <- merge(ssdf92_00.p, ecc90, by = "czone")
ssdf92_00.p <- merge(ssdf92_00.p, demo_90c, by = "czone")
ssdf92_00.p <- merge(ssdf92_00.p, p_92_00, by = "fipscty")
ssdf92_00.p <- merge(ssdf92_00.p, t_iv92_00_df, by = "czone")
ssdf92_00.p <- merge(ssdf92_00.p, census_divs, by = "fipstate")


### 1992-2008

ssdf92_08.p <- merge(us_exprobs04_08.cz, exp_robs93_08, by = "czone")
ssdf92_08.p <- merge(ssdf92_08.p, imp_pen92_08, by = "czone")
ssdf92_08.p <- merge(ssdf92_08.p, ecc90, by = "czone")
ssdf92_08.p <- merge(ssdf92_08.p, demo_90c, by = "czone")
ssdf92_08.p <- merge(ssdf92_08.p, p_92_08, by = "fipscty")
ssdf92_08.p <- merge(ssdf92_08.p, t_iv92_08_df, by = "czone")
ssdf92_08.p <- merge(ssdf92_08.p, census_divs, by = "fipstate")

### 1992-2016

ssdf92_16.p <- merge(us_exprobs04_16.cz, exp_robs93_16, by = "czone")
ssdf92_16.p <- merge(ssdf92_16.p, imp_pen92_16, by = "czone")
ssdf92_16.p <- merge(ssdf92_16.p, ecc90, by = "czone")
ssdf92_16.p <- merge(ssdf92_16.p, demo_90c, by = "czone")
ssdf92_16.p <- merge(ssdf92_16.p, p_92_16, by = "fipscty")
ssdf92_16.p <- merge(ssdf92_16.p, t_iv92_16_df, by = "czone")
ssdf92_16.p <- merge(ssdf92_16.p, census_divs, by = "fipstate")

### 2000-2008

ssdf00_08.p <- merge(us_exprobs04_08.cz, exp_robs00_08, by = "czone")
ssdf00_08.p <- merge(ssdf00_08.p, imp_pen00_08, by = "czone")
ssdf00_08.p <- merge(ssdf00_08.p, ecc00, by = "czone")
ssdf00_08.p <- merge(ssdf00_08.p, demo_00c, by = "czone")
ssdf00_08.p <- merge(ssdf00_08.p, p_00_08, by = "fipscty")
ssdf00_08.p <- merge(ssdf00_08.p, t_iv00_08_df, by = "czone")
ssdf00_08.p <- merge(ssdf00_08.p, census_divs, by = "fipstate")

### 2000-2016

ssdf00_16.p <- merge(us_exprobs04_16.cz, exp_robs00_16, by = "czone")
ssdf00_16.p <- merge(ssdf00_16.p, imp_pen00_16, by = "czone")
ssdf00_16.p <- merge(ssdf00_16.p, ecc00, by = "czone")
ssdf00_16.p <- merge(ssdf00_16.p, demo_00c, by = "czone")
ssdf00_16.p <- merge(ssdf00_16.p, p_00_16, by = "fipscty")
ssdf00_16.p <- merge(ssdf00_16.p, t_iv00_16_df, by = "czone")
ssdf00_16.p <- merge(ssdf00_16.p, census_divs, by = "fipstate")

### 2008-2016

ssdf08_16.p <- merge(us_exprobs08_16.cz, exp_robs08_16, by = "czone")
ssdf08_16.p <- merge(ssdf08_16.p, imp_pen08_16, by = "czone")
ssdf08_16.p <- merge(ssdf08_16.p, ecc10, by = "czone")
ssdf08_16.p <- merge(ssdf08_16.p, demo_10c, by = "czone")
ssdf08_16.p <- merge(ssdf08_16.p, p_08_16, by = "fipscty")
ssdf08_16.p <- merge(ssdf08_16.p, t_iv08_16_df, by = "czone")
ssdf08_16.p <- merge(ssdf08_16.p, census_divs, by = "fipstate")



ssdf92_00.p$t1 = 1
ssdf92_00.p$t2 = 0
ssdf92_00.p$t3 = 0
ssdf92_00.p$t4 = 0
ssdf92_00.p$t5 = 0
ssdf92_00.p$t6 = 0

ssdf92_08.p$t1 = 0
ssdf92_08.p$t2 = 1
ssdf92_08.p$t3 = 0
ssdf92_08.p$t4 = 0
ssdf92_08.p$t5 = 0
ssdf92_08.p$t6 = 0

ssdf92_16.p$t1 = 0
ssdf92_16.p$t2 = 0
ssdf92_16.p$t3 = 1
ssdf92_16.p$t4 = 0
ssdf92_16.p$t5 = 0
ssdf92_16.p$t6 = 0

ssdf00_08.p$t1 = 0
ssdf00_08.p$t2 = 0
ssdf00_08.p$t3 = 0
ssdf00_08.p$t4 = 1
ssdf00_08.p$t5 = 0
ssdf00_08.p$t6 = 0

ssdf00_16.p$t1 = 0
ssdf00_16.p$t2 = 0
ssdf00_16.p$t3 = 0
ssdf00_16.p$t4 = 0
ssdf00_16.p$t5 = 1
ssdf00_16.p$t6 = 0


ssdf08_16.p$t1 = 0
ssdf08_16.p$t2 = 0
ssdf08_16.p$t3 = 0
ssdf08_16.p$t4 = 0
ssdf08_16.p$t5 = 0
ssdf08_16.p$t6 = 1


fssp <- rbind(ssdf92_00.p, ssdf92_08.p)
fssp <- rbind(fssp, ssdf92_16.p)
fssp <- rbind(fssp, ssdf00_08.p)
fssp <- rbind(fssp, ssdf00_16.p)
fssp <- rbind(fssp, ssdf08_16.p) %>%
  arrange(czone, fipstate, fipscty) %>%
  select(czone, fipstate, fipscty,
         t1, t2, t3, t4, t5, t6, everything())

write.dta(fssp, "../outputs/data/fssp_test.dta")
write.dta(fssp, "../outputs/data/fssp.dta")

################################################################################




# Weighting For County-Congressional District Cells

weighted_columns <- c("delta_nom","delta_abs_nom","nominate_beg",
                      "nominate_end", "abs_nominate_beg",
                      "delta_exp_robs","eu_delta_exp_robs",
                      "china_deltaip", "mex_deltaip",
                      "man_sh", "sh_routine", "sh_off",
                      "juv","yngad", "adlt", "miad1","miad2", "sen",
                      "males","females",
                      "white_nh","black_nh", "nw_hisp","wht_hisp",
                      "asia_pi", "am_in", "other",
                      "no_dip","hs_dip","some_col", "assoc", "bach","grad_plus",
                      "win_prop")



full90cc <- ssdf90

wghts90 <- diag(full90cc$cd_prop_pop)
full90mat <- as.matrix(full90cc[,weighted_columns])
full90wghted <- t(full90mat) %*% wghts90
full90cc[,weighted_columns] <- t(full90wghted)


full00cc <- ssdf00

wghts00 <- diag(full00cc$cd_prop_pop)
full00mat <- as.matrix(full00cc[,weighted_columns])
full00wghted <- t(full00mat) %*% wghts00
full00cc[,weighted_columns] <- t(full00wghted)


full10cc <- ssdf10

wghts10 <- diag(full10cc$cd_prop_pop)
full10mat <- as.matrix(full10cc[,weighted_columns])
full10wghted <- t(full10mat) %*% wghts10
full10cc[,weighted_columns] <- t(full10wghted)


fssc3 <- rbind(full90cc, full00cc)
fssc3 <- rbind(fssc3, full10cc) %>%
  arrange(czone, fipstate, district_code) %>%
  select(czone, fipstate, district_code, fipscty,
         census, t1, t2, t3, everything())
fssc3$beg_party[fssc3$beg_party == 100] <- "Democrat"
fssc3$beg_party[fssc3$beg_party == 200] <- "GOP"
fssc3$beg_party[fssc3$beg_party == 328] <- "Independent"
fssc3$end_party[fssc3$end_party == 100] <- "Democrat"
fssc3$end_party[fssc3$end_party == 200] <- "GOP"
fssc3$end_party[fssc3$end_party == 328] <- "Independent"

fssc3$party_held = 0
fssc3$party_held[fssc3$beg_party == fssc3$end_party] = 1

write.dta(fssc3, "../outputs/data/fssc3.dta")

