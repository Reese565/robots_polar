# Title:
# Description:
# Inputs:
# Outputs:

source("functions.R")

################################################################################
################################### NOMINATE ###################################
################################################################################

dw_nom = read.csv("../data/dw_nominate/Hall_members.csv")
nom_bool = dw_nom$chamber == 'House' & dw_nom$congress >= 96
dw_nom = dw_nom[nom_bool,] %>%
  select(congress, state_icpsr, district_code, party_code,
         nominate_dim1, last_means)

cw_icpsr.fips = read.csv('../data/cw/icpsrcnt.csv')

cw_icpsr.fips = cw_icpsr.fips %>%
  mutate(state_icpsr = STATEICP, fipstate = STATEFIPS) %>%
  group_by(state_icpsr, fipstate) %>%
  summarise(temp = sum(County.cod)) %>%
  select(state_icpsr, fipstate)

dw_nom = merge(dw_nom, cw_icpsr.fips,
               by = "state_icpsr",
               all.x = T,
               all.y = F)

dw_nom = dw_nom %>%
  select(congress,
         fipstate,
         district_code,
         party_code,
         nominate_dim1,
         last_means) %>%
  arrange(congress,
          fipstate,
          district_code)

write.csv(dw_nom, "../data/dw_nominate/dw_nom.csv",
          row.names = F)

################################################################################
########################### CONGRESSIONAL DISTRICTS ############################
################################################################################


# 103RD CONGRESS
widths = c(2, 2, 3, 3, 4, 4, 1, 4, 1, 4, 5, 2,5 ,2, 5, 2, 6, 6, 9)
congd_103rd = read.fwf("../data/icpsr/c103_entity/DS0001/06425-0001-Data.txt",
                       widths = widths)

congd_103 = congd_103rd[,c(1:3,17)]
colnames(congd_103) = c("fipstate", "congd_103", "fipscty", "pop")
congd_103 = cnty_fips(congd_103)
congd_103 = congd_103 %>%
  group_by(fipstate, congd_103, fipscty) %>%
  summarise(cty_pop = sum(pop)) %>%
  arrange(fipstate, congd_103)

"dropping county population figures"
congd_103 = congd_103 %>% select(fipstate, congd_103, fipscty)


# 109TH CONGRESSIONAL DISTRICS

congd_109th = read.table("../data/us_census/cong_dist/cd109_natl.txt",
                         skip = 1,
                         sep = ",",
                         header = T)
colnames(congd_109th) = c("fipstate", "fipscty", "congd_109")
congd_109 = cnty_fips(congd_109th)




# 113TH CONGRESSIONAL DISTRICS

congd_113th = read.table("../data/us_census/cong_dist/cd113_natl.txt",
                         skip = 1,
                         sep = ",",
                         header = T)
colnames(congd_113th) = c("fipstate", "fipscty", "congd_113")
congd_113 = cnty_fips(congd_113th)


################################################################################
########################### MERGE WITH DW-NOMINATE #############################
################################################################################

nom_1980c = dw_nom[dw_nom$congress >=98 & dw_nom$congress < 103,]
nom_1990c = dw_nom[dw_nom$congress >=103 & dw_nom$congress < 108,]
nom_2000c = dw_nom[dw_nom$congress >=108 & dw_nom$congress < 113,]
nom_2010c = dw_nom[dw_nom$congress >=113 & dw_nom$congress,]

nom_1990c = merge(nom_1990c, congd_103,
                  by.x = c("fipstate", "district_code"),
                  by.y = c("fipstate", "congd_103"))
nom_1990c = nom_1990c %>% arrange(congress, fipstate, district_code, fipscty)

nom_2000c = merge(nom_2000c, congd_109,
                  by.x = c("fipstate", "district_code"),
                  by.y = c("fipstate", "congd_109"))
nom_2000c = nom_2000c %>% arrange(congress, fipstate, district_code, fipscty)

nom_2010c = merge(nom_2010c, congd_113,
                  by.x = c("fipstate", "district_code"),
                  by.y = c("fipstate", "congd_113"))
nom_2010c = nom_2010c %>% arrange(congress, fipstate, district_code, fipscty)



################################################################################
#################### COUNTY-COMGRESSIONAL DISTRICT CELLS #######################
################################################################################



pop90 <- read.csv("../data/ipums/demog/demo_90.csv")
pop90 <- pop90 %>%
  mutate(adlt_pop = rowSums(.[5:9])) %>%
  select(fipscty, adlt_pop)

pop00 <- read.csv("../data/ipums/demog/demo_00.csv")
pop00 <- pop00 %>%
  mutate(adlt_pop = rowSums(.[5:9])) %>%
  select(fipscty, adlt_pop)

pop10 <- read.csv("../data/ipums/demog/demo_10.csv")
pop10 <- pop10 %>%
  mutate(adlt_pop = rowSums(.[5:9])) %>%
  select(fipscty, adlt_pop)

bool_90s = nom_1990c$congress == 103 & nom_1990c$last_means == 1
bool_00s = nom_2000c$congress == 108 & nom_2000c$last_means == 1
bool_10s = nom_2010c$congress == 113 & nom_2010c$last_means == 1 

col_select = c("fipstate", "district_code", "fipscty")

cnty_pop90 = merge(nom_1990c[bool_90s,col_select], pop90,
                   by = "fipscty", all = F)
cd_pop90 = cnty_pop90 %>%
  group_by(fipstate, district_code) %>%
  summarise(cd_adltpop = sum(adlt_pop))
cd_cnty_wts90 = merge(cnty_pop90, cd_pop90,
                      by = c("fipstate", "district_code")) %>%
  mutate(cd_prop_pop = adlt_pop/cd_adltpop) %>%
  select(col_select, cd_prop_pop, cd_adltpop)
  

cnty_pop00 = merge(nom_2000c[bool_00s,col_select], pop00,,
                   by = "fipscty", all = F)
cd_pop00 = cnty_pop00 %>%
  group_by(fipstate, district_code) %>%
  summarise(cd_adltpop = sum(adlt_pop))
cd_cnty_wts00 = merge(cnty_pop00, cd_pop00,
                      by = c("fipstate", "district_code")) %>%
  mutate(cd_prop_pop = adlt_pop/cd_adltpop) %>%
  select(col_select, cd_prop_pop, cd_adltpop)




cnty_pop10 = merge(nom_2010c[bool_10s,col_select], pop10,,
                   by = "fipscty", all = F)
cd_pop10 = cnty_pop10 %>%
  group_by(fipstate, district_code) %>%
  summarise(cd_adltpop = sum(adlt_pop))
cd_cnty_wts10 = merge(cnty_pop10, cd_pop10,
                      by = c("fipstate", "district_code")) %>%
  mutate(cd_prop_pop = adlt_pop/cd_adltpop) %>%
  select(col_select, cd_prop_pop, cd_adltpop)





nom_1990.cc = merge(nom_1990c, cd_cnty_wts90,
                    by = c("fipstate", "district_code", "fipscty"))
nom_1990b.cc = nom_1990.cc[nom_1990.cc$congress==103,] %>%
  mutate(nominate_beg = nominate_dim1)
nom_1990e.cc = nom_1990.cc[nom_1990.cc$congress==107,] %>%
  mutate(nominate_end = nominate_dim1)
nom_1990.cc = merge(nom_1990b.cc,nom_1990e.cc,
                    by = c("fipstate", "district_code",
                           "fipscty", "cd_prop_pop")) %>%
  mutate(delta_nom = nominate_end - nominate_beg,
         delta_abs_nom = abs(nominate_end) - abs(nominate_beg),
         abs_nominate_beg = abs(nominate_beg),
         abs_nominate_end = abs(nominate_end),
         beg_party = party_code.x,
         end_party = party_code.y,
         cd_adltpop = cd_adltpop.x) %>%
  select(fipstate, district_code, fipscty, nominate_beg,
         nominate_end, delta_nom, delta_abs_nom,
         abs_nominate_beg, abs_nominate_end, cd_prop_pop,
         cd_adltpop, beg_party, end_party)





nom_2000.cc = merge(nom_2000c, cd_cnty_wts00,
                    by = c("fipstate", "district_code", "fipscty"))
nom_2000b.cc = nom_2000.cc[nom_2000.cc$congress==108,] %>%
  mutate(nominate_beg = nominate_dim1)
nom_2000e.cc = nom_2000.cc[nom_2000.cc$congress==112,] %>%
  mutate(nominate_end = nominate_dim1)
nom_2000.cc = merge(nom_2000b.cc,nom_2000e.cc,
                    by = c("fipstate", "district_code",
                           "fipscty", "cd_prop_pop")) %>%
  mutate(delta_nom = nominate_end - nominate_beg,
         delta_abs_nom = abs(nominate_end) - abs(nominate_beg),
         abs_nominate_beg = abs(nominate_beg),
         abs_nominate_end = abs(nominate_end),
         beg_party = party_code.x,
         end_party = party_code.y,
         cd_adltpop = cd_adltpop.x) %>%
  select(fipstate, district_code, fipscty, nominate_beg,
         nominate_end, delta_nom, delta_abs_nom,
         abs_nominate_beg, abs_nominate_end, cd_prop_pop,
         cd_adltpop, beg_party, end_party)





nom_2010.cc = merge(nom_2010c, cd_cnty_wts10,
                    by = c("fipstate", "district_code", "fipscty"))
nom_2010b.cc = nom_2010.cc[nom_2010.cc$congress==113,] %>%
  mutate(nominate_beg = nominate_dim1)
nom_2010e.cc = nom_2010.cc[nom_2010.cc$congress==115,] %>%
  mutate(nominate_end = nominate_dim1)
nom_2010.cc = merge(nom_2010b.cc,nom_2010e.cc,
                    by = c("fipstate", "district_code",
                           "fipscty", "cd_prop_pop")) %>%
  mutate(delta_nom = nominate_end - nominate_beg,
         delta_abs_nom = abs(nominate_end) - abs(nominate_beg),
         abs_nominate_beg = abs(nominate_beg),
         abs_nominate_end = abs(nominate_end),
         beg_party = party_code.x,
         end_party = party_code.y,
         cd_adltpop = cd_adltpop.x) %>%
  select(fipstate, district_code, fipscty, nominate_beg,
         nominate_end, delta_nom, delta_abs_nom,
         abs_nominate_beg, abs_nominate_end, cd_prop_pop,
         cd_adltpop, beg_party, end_party)





dir.create("../data/dw_nominate/nomc3")
write.csv(nom_1990.cc, "../data/dw_nominate/nomc3/nom_1990cc.csv",
          row.names = F)
write.csv(nom_2000.cc, "../data/dw_nominate/nomc3/nom_2000cc.csv",
          row.names = F)
write.csv(nom_2010.cc, "../data/dw_nominate/nomc3/nom_2010cc.csv",
          row.names = F)



################################################################################
################################ HOUSE BY PARTY ################################
################################################################################



hr_103 = nom_1990c[nom_1990c$congress == 103,]
colnames(hr_103) = c("fipstate",
                     "district_code",
                     "hr_103",
                     "party_code",
                     "nom_103",
                     "last_means",
                     "fipscty")
hr_103r = hr_103[hr_103$last_means == 1,] %>%
  select(fipstate, district_code, fipscty, nom_103)

hr_107 = nom_1990c[nom_1990c$congress == 107,]
colnames(hr_107) = c("fipstate",
                     "district_code",
                     "hr_107",
                     "party_code",
                     "nom_107",
                     "last_means",
                     "fipscty")
hr_107r = hr_107[hr_107$last_means == 1,]%>%
  select(fipstate, district_code, fipscty, nom_107)

hr_103.107 = merge(hr_103r, hr_107r, by = c("fipstate",
                                            "district_code",
                                            "fipscty"))
hr_103.107 = hr_103.107 %>%
  mutate(cnom = nom_107 - nom_103)




hr_108 = nom_2000c[nom_2000c$congress == 108,]
colnames(hr_108) = c("fipstate",
                     "district_code",
                     "hr_108",
                     "party_code",
                     "nom_108",
                     "last_means",
                     "fipscty")
hr_108r = hr_108[hr_108$last_means == 1,] %>%
  select(fipstate, district_code, fipscty, nom_108)

hr_112 = nom_2000c[nom_2000c$congress == 112,]
colnames(hr_112) = c("fipstate",
                     "district_code",
                     "hr_112",
                     "party_code",
                     "nom_112",
                     "last_means",
                     "fipscty")
hr_112r = hr_112[hr_112$last_means == 1,]%>%
  select(fipstate, district_code, fipscty, nom_112)

hr_108.112 = merge(hr_108r, hr_112r, by = c("fipstate",
                                            "district_code",
                                            "fipscty"))
hr_108.112 = hr_108.112 %>%
  mutate(cnom = nom_112 - nom_108)





hr_113 = nom_2010c[nom_2010c$congress == 113,]
colnames(hr_113) = c("fipstate",
                     "district_code",
                     "hr_113",
                     "party_code",
                     "nom_113",
                     "last_means",
                     "fipscty")
hr_113r = hr_113[hr_113$last_means == 1,] %>%
  select(fipstate, district_code, fipscty, nom_113)

hr_115 = nom_2010c[nom_2010c$congress == 115,]
colnames(hr_115) = c("fipstate",
                     "district_code",
                     "hr_115",
                     "party_code",
                     "nom_115",
                     "last_means",
                     "fipscty")
hr_115r = hr_115 %>%
  select(fipstate, district_code, fipscty, nom_115)

hr_113.115 = merge(hr_113r, hr_115r, by = c("fipstate",
                                            "district_code",
                                            "fipscty"))
hr_113.115 = hr_113.115 %>%
  mutate(cnom = nom_113 - nom_115)



dir.create("../data/dw_nominate/party_changes/")
write.csv(hr_103.107, "../data/dw_nominate/party_changes/hr_103-107.csv",
          row.names = F)
write.csv(hr_108.112, "../data/dw_nominate/party_changes/hr_108-112.csv",
          row.names = F)
write.csv(hr_113.115, "../data/dw_nominate/party_changes/hr_113-115.csv",
          row.names = F)



