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

us_drpw <- us_delta_rpw('2004','2010','1990')

un_agged_ind <- merge(cbp90czindp_cty, us_drpw, by = "isic")






