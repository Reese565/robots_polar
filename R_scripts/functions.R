# Title: Functions (IAPP)
# Description: set of functions for cleaning and building IAPP datasets

# Packages
library(dplyr)
library(stringr)
library(foreign)
library(readr)

################################################################################
################################## EMPLOYMENT ##################################
################################################################################


########################### COUNTY BUSINESS PATTERNS ###########################

#' @title clean cbp
#' @description 
#' @param dataframe data frame
#' @return data frame
clean_cbp = function(dataframe, ind_code = 'naics'){
  columns = c('fipstate',
              'fipscty',
              ind_code,
              'empflag',
              'emp')
  dataframe = dataframe[,columns]
  return(dataframe)}

#' @title clean cbp david dorn
#' @description 
#' @param dataframe data frame
#' @return data frame
sic.clean_cbpdd = function(dataframe){
  
  dataframe$code4 = as.character(dataframe$code4)
  dataframe$code4 = str_pad(dataframe$code4,
                            width = 4,
                            pad = '0',
                            side = 'left')
  dataframe$code2 = as.character(dataframe$code2)
  dataframe$code2 = str_pad(dataframe$code2,
                            width = 2,
                            pad = '0',
                            side = 'left')
  dataframe$code2 = str_pad(dataframe$code2,
                            width = 4,
                            pad = '0',
                            side = 'right')
  
  return(dataframe)
}

#' @title clean cbp david dorn
#' @description 
#' @param dataframe data frame
#' @return data frame
sic.agg_cbpdd = function(dataframe){
  
  "Excluding auxilary and administrative industry employment"
  
  admin_aux_bool = grep('[1-9]0001', dataframe$code2)
  dataframe = dataframe[-admin_aux_bool,]
  
  slice_index = grep('25[12349][0179]', dataframe$code4)
  slice = dataframe[slice_index,] %>%
    mutate(fipscty = countyid, sic = code4) %>%
    group_by(fipscty, sic) %>%
    summarise(emp = sum(imp_emp))
  
  level_bool = dataframe$level == 3
  df_agg = dataframe[level_bool,] %>%
    mutate(fipscty = countyid, sic = code2) %>%
    group_by(fipscty, sic) %>%
    summarise(emp = sum(imp_emp))
  
  dataframe = rbind(df_agg, slice)
  
  return(dataframe)
}

#' @title clean cbp david dorn
#' @description 
#' @param dataframe data frame
#' @return data frame
naics.clean_cbpdd = function(dataframe){
  
  level_bool = dataframe$level == 5
  dataframe = dataframe[level_bool,]
  
  dataframe = dataframe %>%
    mutate(fipscty = countyid, naics = code2) %>%
    group_by(fipscty, naics) %>%
    summarise(emp = sum(imp_emp))
  
  dataframe$naics = as.character(dataframe$naics)
  dataframe$fipscty = as.character(dataframe$fipscty)
  
  return(dataframe)
}

#' @title replace with lower bound
#' @description replaces EMP_FLAG indicator with lower bound in EMP
#' @param cbp_data data frame of cleaned CBP data
#' @return data frame
replace_with_lb = function(cbp_data, ind_code = 'naics'){
  
  cbp_data$emp[cbp_data$empflag == 'A'] = 0
  cbp_data$emp[cbp_data$empflag == 'B'] = 20
  cbp_data$emp[cbp_data$empflag == 'C'] = 100
  cbp_data$emp[cbp_data$empflag == 'E'] = 250
  cbp_data$emp[cbp_data$empflag == 'F'] = 500
  cbp_data$emp[cbp_data$empflag == 'G'] = 1000
  cbp_data$emp[cbp_data$empflag == 'H'] = 2500
  cbp_data$emp[cbp_data$empflag == 'I'] = 5000
  cbp_data$emp[cbp_data$empflag == 'J'] = 10000
  cbp_data$emp[cbp_data$empflag == 'K'] = 25000
  cbp_data$emp[cbp_data$empflag == 'L'] = 50000
  cbp_data$emp[cbp_data$empflag == 'M'] = 100000
  
  return(cbp_data)
}

#' @title assign isic
#' @description 
#' @param dataframe 
#' @param code
#' @return data frame
assign_isic = function(dataframe, ind_code = 'naics'){
  # if ( 'emp' %in% colnames(dataframe) ){
  #   dataframe = replace_with_lb(dataframe, ind_code)
  # }
  
  
  sic.prefix = c('^0[7-9]00|0100','^1[0-4]00','^2[0-1]00','^2[2-3]00|^3100',
                 '^2400|^25[12349][0179]|^2[6-7]00', '^2900|^2800', '^3[02]00',
                 '^3[3-4]00', '^3500', '^3600|^3800', '^3700',
                 '^3900|^2500', '^4[8-9]00', '^1[5-7]00','^8200',
                 '^4[0-79]00|^[5-9][0-9]00|9999')
  
  naics.prefix = c('^11[0-9]000','^21[0-9]000','^31[1-2]000','^31[3-6]000',
                   '^32[1-3]000|^337000', '^32[4-5]000', '^32[6-7]000',
                   '^33[1-2]000', '^333000', '^33[4-5]000', '^336000',
                   '^339000', '^22[0-9]000','^23[0-9]000','^61[0-9]000',
                   '^[4-9][0-9]{2}000')
  
  isic.rep = c('A', 'B', '10-12', '13-15',
               '16-18','19-21', '22-23',
               '24-25','28','26-27','29-30',
               '31-33','D-E','F', 'P', 'Z')
  
  if (ind_code == 'sic'){prefix = sic.prefix}
  else{prefix = naics.prefix}
  
  dataframe$isic = dataframe[[ind_code]]
  
  for (i in 1:16){
    prefx = prefix[i]
    sub = isic.rep[i]
    dataframe$isic = gsub(prefx, sub, dataframe$isic)
    
  }
  
  return(dataframe)
}

#' @title make county fips
#' @description 
#' @param dataframe 
#' @param 
#' @return data frame
cnty_fips = function(dataframe){
  
  fipscty = str_pad(dataframe$fipscty, width = 3,
                    pad='0', side = 'left')
  fipstate = gsub("^0+", "", dataframe$fipstate)
  
  dataframe$fipscty = paste(fipstate, fipscty, sep = '')
  
  return(dataframe)
}


#' @title isic industries
#' @description 
#' @param dataframe 
#' @param 
#' @return data frame
isic_industries = function(dataframe){
  
  isic.rep = c('A', 'B', '10-12', '13-15',
               '16-18','19-21', '22-23',
               '24-25','28','26-27','29-30',
               '31-33','D-E','F', 'P','Z')
  
  bool = dataframe$isic %in% isic.rep
  dataframe = dataframe[bool,]
  
  dataframe = dataframe %>%
    group_by(fipscty, isic) %>%
    summarise(emp = sum(emp))
  
  return(dataframe)
}

#' @title county employment
#' @description 
#' @param dataframe 
#' @param 
#' @return data frame in thousands
county_emp = function(dataframe){
  
  dataframe = dataframe%>%
    group_by(fipscty) %>%
    summarise(emp = sum(emp)/1000)
  
  return(dataframe)
}

#' @title national industries
#' @description 
#' @param dataframe 
#' @param 
#' @return data frame in thousands
national_isic = function(dataframe){
  
  dataframe = dataframe %>%
    group_by(isic) %>%
    summarise(emp = sum(emp)/1000)
  
  return(dataframe)
}

#' @title build cbpdd
#' @description 
#' @param dataframe
#' @param 
#' @return data frame in thousands
cbpdd_build = function(base = '1990'){
  
  cbp_files = list.files('../data/cbp/cbpdd', pattern="*.csv", full.names=TRUE)
  cbp_strip = gsub('[^0-9]', '', cbp_files)
  cbp_bool = cbp_strip == base
  cbp_path = cbp_files[cbp_bool]
  cbp = read.csv(cbp_path, stringsAsFactors = F)
  
  if (base == '2000'){
    dataframe = naics.clean_cbpdd(cbp)
    dataframe  = assign_isic(dataframe, ind_code = 'naics')
    dataframe = isic_industries(dataframe)
    
    return(dataframe)
  }
  if (base == "1990" | base == "1980") {
    dataframe = sic.clean_cbpdd(cbp)
    dataframe = sic.agg_cbpdd(dataframe)
    dataframe = assign_isic(dataframe, ind_code = 'sic')
    dataframe = isic_industries(dataframe)
    
    return(dataframe)
  }
}



################################### EU KLEMS ###################################

#' de_euklems = read.csv("")
#' 
#' #' @title 
#' #' @description 
#' #' @param dataframe
#' #' @return dataframe
#' euklems_predict_past = function(dataframe){
#'   
#' }


#' @title 
#' @description 
#' @param dataframe
#' @return dataframe
euklems_clean = function(dataframe, base = '1995', all = FALSE){
  
  dataframe = dataframe[,c(2,26:48)]
  rownames(dataframe) = dataframe[,1]
  dataframe[is.na(dataframe)] = 0
  
  Z = c('G','45','46','47','H','49-52','53','I','J','58-60',
        '61','62-63','K','L','M-N','O-U','Q','R-S','T','U')
  dataframe['Z',-1] = colSums(dataframe[Z,-1])
  dataframe['19-21',-1] = colSums(dataframe[c('19','20-21'),-1])
  
  ind_aggs = c('A', 'B', '10-12', '13-15', '16-18',
               '19-21', '22-23', '24-25', '28','26-27',
               '29-30', '31-33', 'D-E', 'F', 'P', 'Z')
  
  dataframe = dataframe[ind_aggs,-1]
  dataframe$isic = rownames(dataframe)
  rownames(dataframe) = NULL
  
  if (all){
    return(dataframe)
  }
  
  yr = paste('EMP', base, sep = '')
  dataframe$emp = dataframe[,yr]
  
  dataframe = dataframe[,c('isic','emp')]
  
  return(dataframe)
}



################################################################################
###################### INTERNATIONAL FEDERATION OF ROBOTS ######################
################################################################################



#' @title clean_ifr
#' @description Cleans IFR data from raw form.
#' @param Unclean (raw) IFR dataframe in csv format as in rawdata/ifr directory
#' @return dataframe of IFR dataset with years aligned and IFR industry codes
clean_ifr = function(dataframe){
  
  rownames(dataframe) <- NULL
  year_names = paste('yr.', as.character(1993:2016), sep='')
  colnames(dataframe) = c('Industry', year_names)
  
  ind = dataframe$Industry
  
  first = gsub("[^0-9A-Z-]","", ind)
  second = gsub("-$", "", first)
  ind = gsub("-[A-Z]+$", "", second)
  
  dataframe = dataframe %>%
    mutate(industry = ind) %>%
    select(industry, year_names)
  
  rownames(dataframe) = dataframe[,1]
  
  return(dataframe)
}

#' @title 
#' @description distribute unspecified manufacturing robots into industries.
#' @param 
#' @return 
redistribute_unsp = function(dataframe){
  
  ind_sel = c("000","A-B","C","D","10-12","13-15","16",
              "17-18","19","20-21","22", "229","23","24","25",
              "28", "289","26-27","29","30","91",
              "E","F","P","90","99")
  
  main_df = dataframe[ind_sel,]
  
  "Distributing unspecified metal into main metal industries."
  metal_ind = c("24","25","28")
  metal_df = main_df[metal_ind,-1]
  met_ind_tot = rowSums(metal_df)
  met_tot = sum(main_df[c("24","25","28"),-1])
  met_ind_dist = met_ind_tot/met_tot
  prop_mat_met = matrix(rep(met_ind_dist, 24), nrow = 3)
  unsp_metal = as.numeric(main_df["289",-1])
  dist_metal = prop_mat_met %*% diag(as.numeric(unsp_metal))
  metal_df_agg = metal_df + dist_metal
  main_df[metal_ind,-1] = metal_df_agg
  main_df = main_df[-c(17),]
  
  "combine unspecified plastics manufacturing."
  
  main_df["22",-1] = main_df["22",-1] + main_df["229",-1]
  main_df = main_df[-c(4,12),]
  
  "distribute all unspecified non-manufacturing robots 
  to known sectors, proportionally."
  
  nrows = nrow(main_df)
  unsp_robs = as.numeric(main_df["99",-1])
  yr_totals = colSums(main_df[-c(1,nrows),-1])
  yr_totals[yr_totals == 0] = 1
  yr_weights = solve(diag(yr_totals))
  prop_mat = as.matrix(main_df[-c(1,nrows),-1]) %*% yr_weights
  dist_unsp = prop_mat %*% diag(unsp_robs)
  
  # ind_tot = rowSums(main_df[-c(1,nrows),-1])
  # tot = sum(ind_tot)
  # dist_ind = ind_tot / tot
  # prop_mat = matrix(rep(dist_ind, 24), length(dist_ind))
  # dist_unsp = prop_mat %*% diag(unsp_robs)
  
  agg_df = dist_unsp + main_df[-c(1,nrows),-1]
  main_df[-c(1,nrows),-1] = agg_df
  
  main_df = main_df[-nrows,]
  rownames(main_df) = NULL
  
  return(main_df)
}

#' @title ifr to isic
#' @description converts ifr industry codes to isic rev.4
#' @param dataframe
#' @return dataframe
ifr_to_isic = function(dataframe){
  
  isic = c('Total', 'A', 'B', '10-12', '13-15', '16', '17-18', '20-21',
           '19', '22', '23', '24', '25', '28', '26-27',
           '29','30', '31-33', 'D-E', 'F', 'P', 'Z')
  rownames(dataframe) = isic
  
  return(dataframe[,-1])
}

#' @title 
#' @description 
#' @param dataframe
#' @return dataframe
ifr_to_euklems = function(dataframe){
  
  aggregates = c('Total', 'A', 'B', '10-12', '13-15', '16-18', '19-21',
                 '22-23', '24-25', '28', '26-27', '29-30',
                 '31-33', 'D-E', 'F', 'P', 'Z')
  
  dataframe["16-18",] = colSums(dataframe[c('16','17-18'),])
  dataframe["19-21",] = colSums(dataframe[c('19','20-21'),])
  dataframe["22-23",] = colSums(dataframe[c('22','23'),])
  dataframe["24-25",] = colSums(dataframe[c('24','25'),])
  dataframe['29-30',] = colSums(dataframe[c('29','30'),])
  
  dataframe = dataframe[aggregates,]
  dataframe$isic = rownames(dataframe)
  rownames(dataframe) = NULL
  
  return(dataframe)
}

#' @title build ifr dataframe
#' @description 
#' @param dataframe
#' @return dataframe
build_ifr = function(dataframe){
  
  dataframe = clean_ifr(dataframe)
  dataframe = redistribute_unsp(dataframe)
  dataframe = ifr_to_isic(dataframe)
  dataframe = ifr_to_euklems(dataframe)
  
  return(dataframe)
}



################################################################################
########################### ROBOTS PER WORKER (RPW) ############################
################################################################################



#' @title robots per worker (thousands)
#' @description calculates robots per thousand workers in each
#' industry and total
#' @param emp_df
#' @param ifr_df 
#' @return dataframe
rob_per_worker = function(emp_df, ifr_df){
  
  emp_tot = data.frame(isic = "Total", emp = sum(emp_df$emp))
  emp_df = rbind( emp_tot, emp_df)
  
  dataframe = merge(emp_df, ifr_df, by = 'isic')[c(16,1:15,17),]
  
  dataframe = dataframe %>%
    select(isic, everything())
  dataframe$emp = as.numeric(dataframe$emp)
  dataframe[,-c(1,2)] = dataframe[,-c(1,2)]/dataframe$emp
  
  return(dataframe)
}


################################################################################
################################## CHANGE RPW ##################################
################################################################################



#' @title change robots per worker
#' @description converts ifr industry codes to isic rev.4
#' @param dataframe
#' @return dataframe
delta_rpw = function(emp_df, ifr_df, begin='1993', end='2016'){
  
  merged = merge(emp_df, ifr_df, by = 'isic')
  
  begin = paste('yr.', begin, sep = '')
  end = paste('yr.', end, sep = '')
  
  delta_robs = merged[[end]] - merged[[begin]]
  
  delta_rpw = delta_robs / merged$emp
  isic = merged$isic
  
  df = data.frame(isic, delta_rpw)
  return(df)
}

#' @title 
#' @description converts ifr industry codes to isic rev.4
#' @param dataframe list of dfs
#' @return dataframe
eu_delta_rpw = function(begin='1993',end='2016', base='1995'){
  
  # Europe KLEMS Dataframe List
  euklems = list.files('../data/eu_klems', pattern="*.csv", full.names=TRUE)
  euklems_dfl = lapply(euklems, read.csv, stringsAsFactors = F)
  names(euklems_dfl) = gsub('[^A-Z]','', euklems)
  eu_bool = names(euklems_dfl) != 'US'
  euklems_dfl = euklems_dfl[eu_bool]
  euklems_dfl = lapply(euklems_dfl, euklems_clean, base = base)
  
  # IFR Europe Dataframe List
  eu_ifr = list.files('../data/ifr/Europe', pattern='*.csv', full.names=T)
  eu_ifr_dfl = lapply(eu_ifr, read.csv, stringsAsFactors = F, na.string = '')
  eu_ifr = gsub('[^A-Z]','', eu_ifr)
  names(eu_ifr_dfl) = gsub('^.','', eu_ifr)
  eu_ifr_dfl = lapply(eu_ifr_dfl, build_ifr)
  
  countries = names(eu_ifr_dfl)
  
  aggregates = c('A', 'B', '10-12', '13-15', '16-18', '19-21',
                 '22-23', '24-25', '28', '26-27', '29-30',
                 '31-33', 'D-E', 'F', 'P', 'Z')
  merged = data.frame(isic = aggregates)
  
  for (i in countries){
    ifr = eu_ifr_dfl[[i]]
    emp = euklems_dfl[[i]]
    
    delta = delta_rpw(emp, ifr, begin, end)
    delta_i = paste('delta', i, sep = '_')
    colnames(delta) = c('isic', delta_i)
    merged = merge(merged, delta, by = 'isic')
  }
  
  return(merged)
}

#' @title 
#' @description 
#' @param dataframe list of dfs
#' @return dataframe
us_delta_rpw = function(begin='1993',
                        end='2016',
                        base='1990'){
  
  # IFR Data
  robs_path = "../data/ifr/NA/NA.csv"
  robs = read.csv(robs_path, stringsAsFactors = F, na.strings = '')
  ifr = build_ifr(robs)
  
  # Employment Data
  cbp = cbpdd_build(base)
  emp = national_isic(cbp)
  
  dataframe = delta_rpw(emp_df = emp,
                        ifr_df = ifr,
                        begin = begin,
                        end = end)
  return(dataframe)
}

#' @title select quantile eu ifr
#' @description 
#' @param dataframe
#' @return dataframe
select_drpw_quantile = function(dataframe, quant = 0.5){
  
  drpw_quant = apply(dataframe[,-1], MARGIN = 1, quantile, quant)
  dataframe$drpw_quant = drpw_quant
  dataframe = dataframe %>% select(isic, drpw_quant)
  
  return(dataframe)
}



################################################################################
########################## COMMUTING ZONE EMPLOYMENT ###########################
################################################################################



#' @title industry cz employment
#' @description 
#' @param dataframe list of dfs
#' @return dataframe
ind_cz_emp = function(emp_df){
  
  # County CZ Crosswalk
  cw_cz_cnty_path = '../data/cw/ddorn/cw_cty_czone.dta'
  cw_cz_fips = read.dta(cw_cz_cnty_path)
  colnames(cw_cz_fips) = c('czone', 'fipscty')
  
  cw_cz_cbp_merge = merge(emp_df, cw_cz_fips, by = 'fipscty')
  
  ind_df = cw_cz_cbp_merge %>%
    group_by(czone, isic) %>%
    summarise(emp = sum(emp))
  
  return(ind_df)
}

#' @title 
#' @description 
#' @param dataframe list of dfs
#' @return dataframe
cz_emp = function(emp_df){
  
  ind_df = ind_cz_emp(emp_df)
  
  cz_emp_df = ind_df %>%
    group_by(czone) %>%
    summarise(tot_emp = sum(emp))
  
  return(cz_emp_df)
}

#' @title 
#' @description 
#' @param dataframe list of dfs
#' @return dataframe
cz_ind_prop = function(emp_df){
  
  ind_df = ind_cz_emp(emp_df)
  cz_emp_df = cz_emp(emp_df)
  
  cz_ind_prop = merge(ind_df, cz_emp_df, by = 'czone') %>%
    mutate(ind_prop = emp/tot_emp) %>%
    select(czone, isic, ind_prop)
  
  return(cz_ind_prop)
}

#' @title 
#' @description 
#' @param dataframe
#' @return dataframe
cz.fips_data = function(dataframe){
  
  cw_cz_cnty_path = '../data/cw/ddorn/cw_cty_czone.dta'
  cw_cz_fips = read.dta(cw_cz_cnty_path)
  colnames(cw_cz_fips) = c('czone', 'fipscty')
  
  cnty_czone = merge(cw_cz_fips, dataframe, by = 'czone')
  
  return(cnty_czone)
}



################################################################################
################### COMMUTING ZONE CHNAGE ROBOTS PER WORKER ####################
################################################################################



#' @title 
#' @description 
#' @param dataframe list of dfs
#' @return dataframe
cz_exp_robs = function(begin = '1993',
                       end = '2016',
                       base = '1990',
                       us = TRUE,
                       eu_quant = 0.5){
  
  # County to CZ Employment
  if (base == '1990'){
    emp_df = cbpdd_build()
  }
  else{
    emp_df = cbpdd_build('2000')
  }
  cz_ind_prop = cz_ind_prop(emp_df)
  
  if (us){
    drpw_df = us_delta_rpw(begin, end, base)
  }
  else{
    if (as.integer(base) < 1995) {
      base = '1995'
    }
    drpw_df = eu_delta_rpw(begin, end, base)
    drpw_df = select_drpw_quantile(drpw_df, eu_quant)
    colnames(drpw_df) = c('isic', 'delta_rpw')
  }
  cz_drpw = merge(cz_ind_prop, drpw_df, by = 'isic') %>%
    group_by(czone) %>%
    summarise(delta_exp_robs = ind_prop %*% delta_rpw)
  
  return(cz_drpw)
}



################################################################################
################################ MANUFACTURING #################################
################################################################################



#' @title 
#' @description 
#' @param dataframe 
#' @return dataframe
clean_manuf_output = function(dataframe){
  
  colnames(dataframe) = c('sic', 'year', 'vship')
  na_bool = is.na(dataframe$vship)
  dataframe[na_bool,3] = 0
  
  slice = dataframe[-grep('25[12349][0179]', dataframe$sic),]
  slice$sic = gsub('[0-9]{2}$', '00', slice$sic)
  dataframe[-grep('25[12349][0179]', dataframe$sic),] = slice
  
  return(dataframe)
}

#' @title 
#' @description 
#' @param dataframe 
#' @return dataframe
manufacturing_isic = function(dataframe){
  
  dataframe = assign_isic(dataframe, ind_code = 'sic') %>%
    group_by(year, isic) %>%
    summarise( vship = sum(vship))
  
  return(dataframe)
}




################################################################################
#################################### TRADE #####################################
################################################################################



#' @title CHANGE NAME!!!
#' @description 
#' @param dataframe 
#' @return dataframe
clean_trade = function(dataframe){
  columns = c("Year",
              "Trade.Flow",
              "Partner",
              "Commodity.Code",
              "Trade.Value..US..")
  dataframe = dataframe[,columns]
  
  newcols = c("year",
              "trade.flow",
              "country",
              "hs6",
              "trade.value")
  
  colnames(dataframe) = newcols
  dataframe = arrange(dataframe, desc(trade.flow), year)
  return(dataframe)}




#' @title industry trade
#' @description
#' @param
#' @return
hs6_to_sicdd <- function(trade_df){
  cw_hs6_sicdd_path = '../data/cw/ddorn/cw_hs6_sic87dd.dta'
  cw_hs6_sicdd = read.dta(cw_hs6_sicdd_path)
  cw_hs6_sicdd$sic87dd = as.character(cw_hs6_sicdd$sic87dd)
  cw_hs6_sicdd = cw_hs6_sicdd[complete.cases(cw_hs6_sicdd),]
  
  cw_trade = merge(trade_df, cw_hs6_sicdd, by = 'hs6')
  cw_trade$sic = rep('i', nrow(cw_trade))
  
  agg_bool = startsWith(cw_trade$sic87dd, '1')
  cw_trade$sic[agg_bool] = str_pad(cw_trade$sic87dd[agg_bool],
                                   width = 4,pad = '0', side = 'left')
  cw_trade$sic[!agg_bool] = str_pad(cw_trade$sic87dd[!agg_bool],
                                    width = 4,pad = '0', side = 'right')
  
  return(cw_trade)
}


iapp_sf_sic_isic <- function(trade_df){
  slice = trade_df[-grep('25[12349][0179]', trade_df$sic),]
  slice$sic = gsub('[0-9]{2}$', '00', slice$sic)
  trade_df[-grep('25[12349][0179]', trade_df$sic),] = slice
  
  return(trade_df)
}


iapp_sf_isic_agg <- function(trade_df){
  trade_df = trade_df %>%
    group_by(year ,isic, trade.flow) %>%
    summarise(value = sum(trade.value))
  
  return(trade_df)
}


#' @title industry trade
#' @description
#' @param
#' @return
industry_trade = function(country = 'china'){
  path = paste('../data/un_comtrade/us-', country, sep = '')
  trade_files = list.files(path, pattern="*.csv",
                           full.names=TRUE)
  trade_dfl = lapply(trade_files, read.csv, stringsAsFactors = F)
  trade_dfl = lapply(trade_dfl, clean_trade)
  
  trade = trade_dfl[[1]]
  
  for (i in 2:length(trade_dfl)){
    trade = rbind(trade, trade_dfl[[i]])
  }
  
  cw_hs6_sicdd_path = '../data/cw/ddorn/cw_hs6_sic87dd.dta'
  cw_hs6_sicdd = read.dta(cw_hs6_sicdd_path)
  cw_hs6_sicdd$sic87dd = as.character(cw_hs6_sicdd$sic87dd)
  cw_hs6_sicdd = cw_hs6_sicdd[complete.cases(cw_hs6_sicdd),]
  
  cw_trade = merge(trade, cw_hs6_sicdd, by = 'hs6')
  cw_trade$sic = rep('i', nrow(cw_trade))
  agg_bool = startsWith(cw_trade$sic87dd, '1')
  cw_trade$sic[agg_bool] = str_pad(cw_trade$sic87dd[agg_bool],
                                   width = 4,pad = '0', side = 'left')
  cw_trade$sic[!agg_bool] = str_pad(cw_trade$sic87dd[!agg_bool],
                                    width = 4,pad = '0', side = 'right')
  
  slice = cw_trade[-grep('25[12349][0179]', cw_trade$sic),]
  slice$sic = gsub('[0-9]{2}$', '00', slice$sic)
  cw_trade[-grep('25[12349][0179]', cw_trade$sic),] = slice
  
  trade_df = assign_isic(cw_trade, ind_code = 'sic') %>%
    group_by(year ,isic, trade.flow) %>%
    summarise(value = sum(trade.value))
  
  return(trade_df)
}

#' @title build imports dataframe
#' @description
#' @param
#' @return
build_imports_df = function(trade_df){
  
  m_bool = trade_df$trade.flow == 'Import'
  
  import_df = trade_df[m_bool,]
  colnames(import_df) = c('year', 'isic', 'flow', 'import.val')
  
  return(import_df)
}

#' @title build exports dataframe
#' @description
#' @param
#' @return
build_exports_df = function(trade_df){
  
  x_bool = trade_df$trade.flow == 'Export'
  export_df = trade_df[x_bool,]
  colnames(export_df) = c('year', 'isic', 'flow', 'export.val')
  
  return(export_df)
}



#' @title 
#' @description
#' @param
#' @return
exp_im_df_build <- function(trade_df){
  
  exports_df = build_exports_df(trade_df)
  imports_df = build_imports_df(trade_df)
  
  exp_imp_df = merge(exports_df, imports_df, by = c('year', 'isic')) %>%
    select(year, isic, export.val, import.val)
  
  return(exp_imp_df)
}



#' @title 
#' @description
#' @param
#' @return
add_domestic_industry_output <- function(exp_imp_df){
  
  # Manufacturing Output
  ind_output = read.csv('../data/NBER-CES/sicdd.csv', stringsAsFactors = F)
  ind_output = clean_manuf_output(ind_output)
  ind_output = manufacturing_isic(ind_output)
  ind_output$vship <- ind_output$vship*10^6
  
  trade_df = merge(ind_output, exp_imp_df, by = c('year', 'isic'), all.y = T)
  
  return(trade_df)
}


#' @title
#' @description
#' @param
#' @return
add_initial_absorbtion = function(trade_df){
  
  exports_df <- trade_df[,c("year", "isic", "export.val")]
  imports_df <- trade_df[,c("year", "isic", "import.val")]
  
  init_obs_df = trade_df %>%
    mutate(init_obs = vship + import.val - export.val)
  
  exports_12.16 = exports_df[exports_df$year > 2011,] %>%
    mutate(vship = 0, import.val = 0, init_obs = 0) %>%
    select(year, isic, vship, export.val, import.val, init_obs)
  
  imports_12.16 = imports_df[exports_df$year > 2011,] %>%
    mutate(vship = 0, export.val = 0, init_obs = 0) %>%
    select(year, isic, vship, export.val, import.val, init_obs)
  
  exp_imp.12_16 = merge(exports_12.16, imports_12.16,
                        by = c('year', 'isic', 'vship', 'init_obs'))
  
  exp_imp.12_16 = exp_imp.12_16 %>%
    mutate(export.val = export.val.x, import.val = import.val.y) %>%
    select(year, isic, vship, export.val, import.val, init_obs)
  
  trade_df = rbind(init_obs_df, exp_imp.12_16)
  
  return(trade_df)
}





#' @title
#' @description
#' @param
#' @return
build_trade_df = function(trade_df){
  
  # Manufacturing Output
  ind_output = read.csv('../data/NBER-CES/sicdd.csv', stringsAsFactors = F)
  ind_output = clean_manuf_output(ind_output)
  ind_output = manufacturing_isic(ind_output)
  ind_output$vship <- ind_output$vship*10^6
  
  # Manufacturing Trade
  exports_df = build_exports_df(trade_df)
  imports_df = build_imports_df(trade_df)
  
  exp_imp_df = merge(exports_df, imports_df, by = c('year', 'isic')) %>%
    select(year, isic, export.val, import.val)
  
  trade_df = merge(ind_output, exp_imp_df, by = c('year', 'isic'), all = F)
  
  init_obs_df = trade_df %>%
    mutate(init_obs = vship + import.val - export.val)
  
  exports_12.16 = exports_df[exports_df$year > 2011,] %>%
    mutate(vship = 0, import.val = 0, init_obs = 0) %>%
    select(year, isic, vship, export.val, import.val, init_obs)
  
  imports_12.16 = imports_df[exports_df$year > 2011,] %>%
    mutate(vship = 0, export.val = 0, init_obs = 0) %>%
    select(year, isic, vship, export.val, import.val, init_obs)
  
  exp_imp.12_16 = merge(exports_12.16, imports_12.16,
                        by = c('year', 'isic', 'vship', 'init_obs'))
  
  exp_imp.12_16 = exp_imp.12_16 %>%
    mutate(export.val = export.val.x, import.val = import.val.y) %>%
    select(year, isic, vship, export.val, import.val, init_obs)
  
  trade_df = rbind(init_obs_df, exp_imp.12_16)
  
  return(trade_df)
}

#' @title delta (change) import penetration
#' @description
#' @param
#' @return
delta_impPen = function(trade_df, begin = '1992', end = '2016'){
  
  beg_bool = trade_df$year == begin
  end_bool = trade_df$year == end
  
  trade_df_beg = trade_df[beg_bool,]
  trade_df_end = trade_df[end_bool,]
  
  delta_ip_df = merge(trade_df_beg, trade_df_end, by = 'isic') %>%
    mutate(delta_ip = (import.val.y - import.val.x)/init_obs.x) %>%
    select(isic, delta_ip)
  
  return(delta_ip_df)
}

delta_impPen_w.lag = function(trade_df,
                              begin = '1992',
                              end = '2016',
                              lag = 0){

  beg_bool = trade_df$year == begin
  end_bool = trade_df$year == end
  
  trade_df_beg = trade_df[beg_bool,]
  trade_df_end = trade_df[end_bool,]
  trade_df_end <- trade_df_end[!is.na(trade_df_end$vship),]
  
  delta_ip_df = merge(trade_df_beg, trade_df_end, by = 'isic')
  
  if (lag > 0){
    lagded_year <- as.character(as.numeric(begin)-lag)
    intabs_df <- trade_df[trade_df$year == lagded_year,c("isic","init_obs")]
    delta_ip_df <- merge(delta_ip_df, intabs_df) %>%
      mutate(delta_ip = (import.val.y - import.val.x)/init_obs) %>%
      select(isic, delta_ip)
    return(delta_ip_df)
  }
  
  delta_ip_df = delta_ip_df %>%
    mutate(delta_ip = (import.val.y - import.val.x)/init_obs.x) %>%
    select(isic, delta_ip)
  
  return(delta_ip_df)
}

#' @title 
#' @description 
#' @param dataframe list of dfs
#' @return dataframe
cz_impPen = function(trade_df, emp_df){

  trade_df <- trade_df[!is.na(trade_df[,2]),]
  cz_ind_props = cz_ind_prop(emp_df)
  
  cz_drpw = merge(cz_ind_props, trade_df, by = 'isic') %>%
    group_by(czone) %>%
    summarise(delta_ip = ind_prop %*% delta_ip)
  
  return(cz_drpw)
}


################################################################################
################################## DEMOGRAPHICS ################################
################################################################################


#' @title 
#' @description 
#' @param dataframe 
#' @return dataframe
county_pop = function(year = '1990'){
  
  ipums_file_paths = list.files('../data/ipums/nhgis003_tot_pop',
                                pattern="*.csv",
                                full.names=TRUE)
  years = str_extract(ipums_file_paths, "[12][0-9]{2}0")
  years_bool = years == year
  ipums_path = ipums_file_paths[years_bool]
  
  pop_variables = c('1990' = 'EWZ001', '2000' = 'F55001', '2010' = 'LGH001')
  pop_var = pop_variables[[year]]
  
  population_df = read.csv(ipums_path)
  population_df$tot_pop = population_df[[pop_var]]
  
  population_df = population_df %>%
    mutate(fipscty = COUNTYA, fipstate = STATEA)
  
  population_df = cnty_fips(population_df) %>%
    select(fipscty, tot_pop)
  
  return(population_df)
}



################################################################################
################################################################################
################################################################################
################################# END OF FILE ##################################
################################################################################
################################################################################
################################################################################