# Title: Demographics
# Description: Script that cleans and build demographic data from raw form
# Inputs: data/ipums/nhgis00...
# Outputs: data/ipums/demog

source("functions.R")
dir.create("../data/ipums/population")
pop_90 = county_pop('1990')
pop_00 = county_pop('2000')
pop_10 = county_pop('2010')


# Sex and Age

age_groups = c(juv = "0-17yrs", ynad = "18-25yrs", adlt = "26-35yrs",
               miad1 = "36-45yrs", miad2 = "46-64yrs", sen = "65yrs+")

sa_path_1990 = '../data/ipums/nhgis005_sex_age/nhgis0005_ds122_1990_county.csv'
sa_1990 = read_csv(sa_path_1990)


sa_1990 = sa_1990 %>%
  mutate(males = rowSums(.[21:123]),
         females = rowSums(.[124:226]),
         juv_m = rowSums(.[21:38]),
         yngad_m = rowSums(.[39:46]),
         adlt_m = rowSums(.[47:56]),
         miad1_m = rowSums(.[57:66]),
         miad2_m = rowSums(.[67:85]),
         sen_m = rowSums(.[86:123]),
         juv_f = rowSums(.[124:141]),
         yngad_f = rowSums(.[142:148]),
         adlt_f = rowSums(.[149:158]),
         miad1_f = rowSums(.[159:168]),
         miad2_f = rowSums(.[169:188]),
         sen_f = rowSums(.[189:226])) %>%
  mutate(juv = juv_m + juv_f,
         yngad = yngad_m + yngad_f,
         adlt = adlt_m + adlt_m,
         miad1 = miad1_m + miad1_f,
         miad2 = miad2_m + miad2_f,
         sen = sen_m + sen_f) %>%
  select(GISJOIN, YEAR, COUNTY, COUNTYA, STATE, STATEA,
         juv, yngad, adlt, miad1, miad2, sen, males, females)

sa_1990 = sa_1990 %>% mutate(wa_pop = rowSums(.[8:11]))

sa_1990 = sa_1990 %>%
  mutate(fipstate = STATEA,fipscty = COUNTYA)

sa_1990 = cnty_fips(sa_1990) %>%
  select(fipscty, wa_pop, juv, yngad, adlt, miad1, miad2, sen, males, females)


sa_path_2000 = '../data/ipums/nhgis005_sex_age/nhgis0005_ds146_2000_county.csv'
sa_2000 = read_csv(sa_path_2000)

sa_2000 = sa_2000 %>%
  mutate(males = rowSums(.[31:53]),
         females = rowSums(.[54:76]),
         juv = rowSums(.[31:34])+rowSums(.[54:57]),
         yngad = rowSums(.[35:38])+rowSums(.[58:61]),
         adlt = rowSums(.[39:40])+rowSums(.[62:63]),
         miad1 = rowSums(.[41:42])+rowSums(.[64:65]),
         miad2 = rowSums(.[43:47])+rowSums(.[66:70]),
         sen = rowSums(.[48:53])+rowSums(.[71:76])) %>%
  select(GISJOIN, YEAR, COUNTY, COUNTYA, STATE, STATEA,
         juv, yngad, adlt, miad1, miad2, sen, males, females)

sa_2000 = sa_2000 %>% mutate(wa_pop = rowSums(.[8:11]))

sa_2000 = sa_2000 %>%
  mutate(fipstate = STATEA,fipscty = COUNTYA)

sa_2000 = cnty_fips(sa_2000) %>%
  select(fipscty, wa_pop, juv, yngad, adlt, miad1, miad2, sen, males, females)

sa_path_2010 = '../data/ipums/nhgis005_sex_age/nhgis0005_ds172_2010_county.csv'
sa_2010 = read_csv(sa_path_2010)

sa_2010 = sa_2010 %>%
  mutate(males = H76002,
         females = H76026,
         juv = rowSums(.[41:45])+rowSums(.[66:69]),
         yngad = rowSums(.[46:49])+rowSums(.[70:73]),
         adlt = rowSums(.[50:51])+rowSums(.[74:75]),
         miad1 = rowSums(.[52:53])+rowSums(.[76:77]),
         miad2 = rowSums(.[54:58])+rowSums(.[78:82]),
         sen = rowSums(.[59:64])+rowSums(.[83:88])) %>%
  select(GISJOIN, YEAR, COUNTY, COUNTYA, STATE, STATEA,
         juv, yngad, adlt, miad1, miad2, sen, males, females)

sa_2010 = sa_2010 %>% mutate(wa_pop = rowSums(.[8:11]))

sa_2010 = sa_2010 %>%
  mutate(fipstate = STATEA,fipscty = COUNTYA)

sa_2010 = cnty_fips(sa_2010) %>%
  select(fipscty, wa_pop, juv, yngad, adlt, miad1, miad2, sen, males, females)


# Education

edu_path_1990 = '../data/ipums/nhgis004_educ/nhgis0004_ds123_1990_county.csv'
edu_1990 = read_csv(edu_path_1990)

edu_1990 = edu_1990 %>%
  mutate(no_dip = E33001+E33002,
         hs_dip = E33003,
         some_col = E33004,
         assoc = E33005,
         bach = E33006,
         grad_plus = E33007) %>%
  select(GISJOIN, YEAR, COUNTY, COUNTYA, STATE, STATEA,
         no_dip, hs_dip, some_col, assoc, bach, grad_plus)

edu_1990 = edu_1990 %>%
  mutate( fipstate = STATEA,fipscty = COUNTYA)

edu_1990 = cnty_fips(edu_1990) %>%
  select(fipscty, no_dip, hs_dip, some_col, assoc, bach, grad_plus)


edu_path_2000 = '../data/ipums/nhgis004_educ/nhgis0004_ds151_2000_county.csv'
edu_2000 = read_csv(edu_path_2000)

edu_2000 = edu_2000 %>%
  mutate(no_dip = rowSums(.[32:39]) + rowSums(.[48,55]),
         hs_dip = GKT009 + GKT025,
         some_col = rowSums(.[41:42])+rowSums(.[57:58]),
         assoc = GKT012 + GKT028,
         bach = GKT013 + GKT029,
         grad_plus = rowSums(.[45:47])+rowSums(.[61:63])) %>%
  select(GISJOIN, YEAR, COUNTY, COUNTYA, STATE, STATEA,
         no_dip, hs_dip, some_col, assoc, bach, grad_plus)

edu_2000 = edu_2000 %>%
  mutate( fipstate = STATEA,fipscty = COUNTYA)

edu_2000 = cnty_fips(edu_2000) %>%
  select(fipscty, no_dip, hs_dip, some_col, assoc, bach, grad_plus)


edu_path_2010 = '../data/ipums/nhgis004_educ/nhgis0004_ds176_20105_2010_county_E.csv'
edu_2010 = read_csv(edu_path_2010)

edu_2010 = edu_2010 %>%
  mutate(no_dip = rowSums(.[39:46]) + rowSums(.[56,63]),
         hs_dip = JN9E011 + JN9E028,
         some_col = rowSums(.[48:49])+rowSums(.[65:66]),
         assoc = JN9E014 + JN9E031,
         bach = JN9E015 + JN9E032,
         grad_plus = rowSums(.[52:54])+rowSums(.[69:71])) %>%
  select(GISJOIN, YEAR, COUNTY, COUNTYA, STATE, STATEA,
         no_dip, hs_dip, some_col, assoc, bach, grad_plus)

edu_2010 = edu_2010 %>%
  mutate( fipstate = STATEA,fipscty = COUNTYA)

edu_2010 = cnty_fips(edu_2010) %>%
  select(fipscty, no_dip, hs_dip, some_col, assoc, bach, grad_plus)


# Race and Ethnicity

r_path_1990 = '../data/ipums/nhgis006_race/nhgis0006_ds120_1990_county.csv'
r_1990 = read_csv(r_path_1990)

r_1990 = r_1990 %>%
  mutate(white_nh = ET2001,
         black_nh = ET2002,
         nw_hisp = ET2007 + ET2008 + ET2009 + ET2010,
         wht_hisp = ET2006,
         am_in = ET2003,
         asia_pi = ET2004,
         other = ET2005) %>%
  select(GISJOIN, YEAR, COUNTY, COUNTYA, STATE, STATEA,
         white_nh, black_nh, nw_hisp,wht_hisp, asia_pi, am_in, other)

r_1990 = r_1990 %>%
  mutate( fipstate = STATEA,fipscty = COUNTYA)

r_1990 = cnty_fips(r_1990) %>%
  select(fipscty, white_nh, black_nh, nw_hisp,wht_hisp, asia_pi, am_in, other)


r_path_2000 = '../data/ipums/nhgis006_race/nhgis0006_ds146_2000_county.csv'
r_2000 = read_csv(r_path_2000)

r_2000 = r_2000 %>%
  mutate(white_nh = FMS001,
         black_nh = FMS002,
         nw_hisp = FMS009 + FMS010 + FMS011 + FMS012 + FMS013 + FMS014,
         wht_hisp = FMS008,
         am_in = FMS003,
         asia_pi = FMS004 + FMS005,
         other = FMS006 + FMS007) %>%
  select(GISJOIN, YEAR, COUNTY, COUNTYA, STATE, STATEA,
         white_nh, black_nh, nw_hisp, wht_hisp, asia_pi, am_in, other)

r_2000 = r_2000 %>%
  mutate( fipstate = STATEA,fipscty = COUNTYA)

r_2000 = cnty_fips(r_2000) %>%
  select(fipscty, white_nh, black_nh, nw_hisp,wht_hisp, asia_pi, am_in, other)


r_path_2010 = '../data/ipums/nhgis006_race/nhgis0006_ds172_2010_county.csv'
r_2010 = read_csv(r_path_2010)

r_2010 = r_2010 %>%
  mutate(white_nh = H7Z003,
         black_nh = H7Z004,
         nw_hisp = H7Z012 + H7Z013 + H7Z014 + H7Z015 + H7Z016 + H7Z017,
         wht_hisp = H7Z011,
         am_in = H7Z005,
         asia_pi = H7Z006 + H7Z007,
         other = H7Z008 + H7Z009) %>%
  select(GISJOIN, YEAR, COUNTY, COUNTYA, STATE, STATEA,
         white_nh, black_nh, nw_hisp, wht_hisp, asia_pi, am_in, other)

r_2010 = r_2010 %>%
  mutate( fipstate = STATEA,fipscty = COUNTYA)

r_2010 = cnty_fips(r_2010) %>%
  select(fipscty, white_nh, black_nh, nw_hisp,wht_hisp, asia_pi, am_in, other)


######################### MERGING DEMOGRAPHICS TABLES ##########################

demo_90 = merge(pop_90, sa_1990, by = "fipscty")
demo_90 = merge(demo_90, r_1990, by = "fipscty")
demo_90 = merge(demo_90, edu_1990, by = "fipscty")

demo_00 = merge(pop_00, sa_2000, by = "fipscty")
demo_00 = merge(demo_00, r_2000, by = "fipscty")
demo_00 = merge(demo_00, edu_2000, by = "fipscty")

demo_10 = merge(pop_10, sa_2010, by = "fipscty")
demo_10 = merge(demo_10, r_2010, by = "fipscty")
demo_10 = merge(demo_10, edu_2010, by = "fipscty")

dir.create("../data/ipums/demog")
write.csv(demo_90, "../data/ipums/demog/demo_90.csv", row.names = F)
write.csv(demo_00, "../data/ipums/demog/demo_00.csv", row.names = F)
write.csv(demo_10, "../data/ipums/demog/demo_10.csv", row.names = F)


