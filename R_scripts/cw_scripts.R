# Title: Corsswalks
# Description:
# Outputs:
# Inputs:

# Packages
library(dplyr)
library(readr)
library(foreign)
library(stringr)
library(concordance)

# Options


## Industry



# cw_sic87_hs6 = read.dta('../data/crosswalks/industry/cw_hs6_sic87dd.dta') %>%
#   select(hs6, sic87dd, share)
# colnames(cw_sic87_hs6) = c('hs6', 'sic87', 'wght_sic87.hs6')
# 
# cw_n97_s87 = read.dta('../data/crosswalks/industry/cw_n97_s87.dta')
# colnames(cw_n97_s87) = c('sic87', 'naics97', 'wght_sic87.n97')


cw_n97_n02 = read_csv('../data/crosswalks/industry/2002_NAICS_to_1997_NAICS.csv',
                      col_types = cols(
                        .default = col_guess(),
                        NAICS97 = col_character(),
                        NAICS02 = col_character()
                      )) %>% select(NAICS97, NAICS02)
colnames(cw_n97_n02) = c('naics97', 'naics02')


### NAICS02 to NAICS07 Crosswalk w/Employment ratios

cw_n02_n07 = read_csv('../data/crosswalks/industry/2007_to_2002_NAICS.csv',
                      col_types = cols(
                        `2007 NAICS Code` = col_character(),
                        `2002 NAICS Code` = col_character()))
colnames(cw_n02_n07)= c('naics07', 'title_07', 'naics02', 'title_02')
cw_n02_n07 = cw_n02_n07[2:dim(cw_n02_n07)[1],] %>% select('naics02', 'naics07')

path_n02_n07_er = '../data/crosswalks/industry/naics_02.07_emp_ratios.csv'
n02_n07_er = read_csv(path_n02_n07_er)

n02_n07_er$naics02 =  gsub('[0-9]{2}-', '',n02_n07_er$`CES NAICS 2002 Tabcode`)
n02_n07_er$naics07 = gsub('[0-9]{2}-', '',n02_n07_er$`CES NAICS 2007 Tabcode`)

n02_n07_er = n02_n07_er %>% select(naics02, naics07, ratio)

cw_n02_n07_er = merge(cw_n02_n07, n02_n07_er,
                   by = c('naics07','naics02'), all=T)

cw_n02_n07_er$ratio[is.na(cw_n02_n07_er$ratio)] = 1

write.csv(cw_n02_n07_er, '../outputs/crosswalks/cw_n02_n07.csv')



### SIC87 to NAICS02 w/Employment Ratios

sic87_n02_path = '../data/crosswalks/industry/1987_SIC_to_2002_NAICS.csv'
cw_sic87_n02 = read_csv(sic87_n02_path,
                        col_types = cols(
                          `2002 NAICS` = col_character())) %>% 
  select(SIC, `2002 NAICS`)
colnames(cw_sic87_n02) = c('sic87', 'naics02')




path_sic87_n02_er = '../data/crosswalks/industry/sic.n02_emp_ratios.csv'
sic87_n02_er = read_csv(path_sic87_n02_er)

sic87_n02_er$sic87 =  gsub('[0-9]{2}-', '',
                           sic87_n02_er$`CES SIC Tabulating Code`)

sic87_n02_er$naics02 = gsub('[0-9]{2}-', '',
                            sic87_n02_er$`CES NAICS Tabulating Code`)

sic87_n02_er$ratio = sic87_n02_er$`SIC to NAICS Employment Ratio`

sic87_n02_er = sic87_n02_er %>% select(sic87, naics02, ratio)

cw_sic87_n02_er = merge(cw_sic87_n02, sic87_n02_er,
                      by = c('sic87','naics02'), all=T)

cw_sic87_n02_er$ratio[is.na(cw_sic87_n02_er$ratio)] = 1

dim(sic87_n02_er)

write.csv(cw_sic87_n02_er ,'../outputs/crosswalks/cw_s87_n02.csv')

test_merge = merge(cbp_sic_test, cw_sic87_n02, by.x = 'sic', by.y = 'sic87') %>%
  arrange(fipstate, fipscty)



cw_n07_isic4 = read_csv('../data/crosswalks/industry/2007_NAICS_to_ISIC_4-2.csv',
                        col_types = cols(
                          .default = col_guess(),
                          `2007 NAICS US` = col_character(),
                          `ISIC 4.0` = col_character()
                        )) %>% select("2007 NAICS US", "ISIC 4.0")
colnames(cw_n07_isic4) = c('naics07','isic.r4')

write.csv(cw_n07_isic4, '../outputs/crosswalks/cw_n07_isic4', row.names = F)

cw_sic87_n02_er[cw_sic87_n02_er$naics02 == '551114',]
cw_sic87_n02_er[cw_sic87_n02_er$sic87 == '0752',]
cw_sic87_n02_er[cw_sic87_n02_er$sic87 == 'Aux',]

cw_sic87_n02_er[is.na(cw_sic87_n02_er$sic87),]
tail(cw_sic87_n02_er)
## Merges



"SIC87 to NAICS02"

sic87_n02_er =  
  



## Geographic
  
cw_cty_czone = read.dta('../crosswalks/geographic/cw_cty_czone.dta')
  
  
  
