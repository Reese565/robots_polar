# Title:
# Description:
# Inputs:
# Outputs:

source("functions.R")
library(httr)
library(rjson)


################################################################################
############################### UN COMTRADE API ################################
################################################################################

string <- "http://comtrade.un.org/data/cache/partnerAreas.json"
reporters <- fromJSON(file=string)
reporters <- as.data.frame(t(sapply(reporters$results,rbind)))

get.Comtrade <- function(url="http://comtrade.un.org/api/get?",
                         maxrec=50000,
                         type="C",
                         freq="A",
                         px="HS",
                         ps="now",
                         r,
                         p,
                         rg="all",
                         cc="TOTAL",
                         fmt="json"){
  string<- paste(url
                 ,"max=",maxrec,"&" #maximum no. of records returned
                 ,"type=",type,"&" #type of trade (c=commodities)
                 ,"freq=",freq,"&" #frequency
                 ,"px=",px,"&" #classification
                 ,"ps=",ps,"&" #time period
                 ,"r=",r,"&" #reporting area
                 ,"p=",p,"&" #partner country
                 ,"rg=",rg,"&" #trade flow
                 ,"cc=",cc,"&" #classification code
                 ,"fmt=",fmt        #Format
                 ,sep = "")
  
  if(fmt == "csv") {
    raw.data<- read.csv(string,header=TRUE)
    return(list(validation=NULL, data=raw.data))
  } else {
    if(fmt == "json" ) {
      raw.data<- fromJSON(file=string)
      data<- raw.data$dataset
      validation<- unlist(raw.data$validation, recursive=TRUE)
      ndata<- NULL
      if(length(data)> 0) {
        var.names<- names(data[[1]])
        data<- as.data.frame(t( sapply(data,rbind)))
        ndata<- NULL
        for(i in 1:ncol(data)){
          data[sapply(data[,i],is.null),i]<- NA
          ndata<- cbind(ndata, unlist(data[,i]))
        }
        ndata<- as.data.frame(ndata)
        colnames(ndata)<- var.names
      }
      return(list(validation=validation,data =ndata))
    }
  }
}





years = paste(1990:2016, collapse = ",")
usa.1995_2016 <- get.Comtrade(r = "842",
                          ps = "1999,2000,2001",
                          p = "156",
                          rg = "1,2",
                          px = "HS")



usa.1995_2016$data























################################################################################
#################################### TRADE #####################################
################################################################################








#' #' @title 
#' #' @description 
#' #' @param dataframe 
#' #' @return dataframe
#' clean_trade = function(dataframe){
#'   library(dplyr)
#'   columns = c("Year",
#'               "Trade.Flow",
#'               "Partner",
#'               "Commodity.Code",
#'               "Trade.Value..US..")
#'   dataframe = dataframe[,columns]
#'   
#'   newcols = c("year",
#'               "trade.flow",
#'               "country",
#'               "hs6",
#'               "trade.value")
#'   
#'   colnames(dataframe) = newcols
#'   dataframe = arrange(dataframe, desc(trade.flow), year)
#'   return(dataframe)}
#' 
#' #' @title industry trade
#' #' @description
#' #' @param
#' #' @return
#' industry_trade = function(country = 'china'){
#'   path = paste('un_comtrade/us-', country, sep = '')
#'   trade_files = list.files('un_comtrade/us-china/', pattern="*.csv",
#'                            full.names=TRUE)
#'   trade_dfl = lapply(trade_files, read.csv, stringsAsFactors = F)
#'   trade_dfl = lapply(trade_dfl, clean_trade)
#'   
#'   trade = trade_dfl[[1]]
#'   
#'   for (i in 2:length(trade_dfl)){
#'     trade = rbind(trade, trade_dfl[[i]])
#'   }
#'   
#'   cw_hs6_sicdd_path = 'dorn/crosswalks/cw_hs6_sic87dd.dta'
#'   cw_hs6_sicdd = read.dta(cw_hs6_sicdd_path)
#'   cw_hs6_sicdd$sic87dd = as.character(cw_hs6_sicdd$sic87dd)
#'   cw_hs6_sicdd = cw_hs6_sicdd[complete.cases(cw_hs6_sicdd),]
#'   
#'   cw_trade = merge(trade, cw_hs6_sicdd, by = 'hs6')
#'   cw_trade$sic = rep('i', nrow(cw_trade))
#'   agg_bool = startsWith(cw_trade$sic87dd, '1')
#'   cw_trade$sic[agg_bool] = str_pad(cw_trade$sic87dd[agg_bool],
#'                                    width = 4,pad = '0', side = 'left')
#'   cw_trade$sic[!agg_bool] = str_pad(cw_trade$sic87dd[!agg_bool],
#'                                     width = 4,pad = '0', side = 'right')
#'   
#'   slice = cw_trade[-grep('25[12349][0179]', cw_trade$sic),]
#'   slice$sic = gsub('[0-9]{2}$', '00', slice$sic)
#'   cw_trade[-grep('25[12349][0179]', cw_trade$sic),] = slice
#'   
#'   trade = assign_isic(cw_trade, ind_code = 'sic') %>%
#'     group_by(year ,isic, trade.flow) %>%
#'     summarise(value = sum(trade.value))
#'   
#'   return(trade)
#' }
#' 
#' #' @title build imports dataframe
#' #' @description
#' #' @param
#' #' @return
#' build_imports_df = function(trade_df){
#'   
#'   m_bool = trade_df$trade.flow == 'Import'
#'   
#'   import_df = trade_df[m_bool,]
#'   colnames(import_df) = c('year', 'isic', 'flow', 'import.val')
#'   
#'   return(import_df)
#' }
#' 
#' #' @title build exports dataframe
#' #' @description
#' #' @param
#' #' @return
#' build_exports_df = function(trade_df){
#'   
#'   x_bool = trade_df$trade.flow == 'Export'
#'   export_df = trade_df[x_bool,]
#'   colnames(export_df) = c('year', 'isic', 'flow', 'export.val')
#'   
#'   return(export_df)
#' }
#' 
#' #' @title initial obsorbtion
#' #' @description
#' #' @param
#' #' @return
#' build_trade_df = function(trade_df, country = 'china'){
#'   
#'   # Manufacturing Output
#'   ind_output = read.csv('us-census/sicdd.csv', stringsAsFactors = F)
#'   ind_output = clean_manuf_output(ind_output)
#'   ind_output = manufacturing_isic(ind_output)
#'   
#'   # Manufacturing Trade
#'   exports_df = build_exports_df(trade_df)
#'   imports_df = build_imports_df(trade_df)
#'   
#'   exp_imp_df = merge(exports_df, imports_df, by = c('year', 'isic')) %>%
#'     select(year, isic, export.val, import.val)
#'   
#'   trade_df = merge(ind_output, exp_imp_df, by = c('year', 'isic'), all = F)
#'   
#'   init_obs_df = trade_df %>%
#'     mutate(init_obs = vship + import.val - export.val)
#'   
#'   exports_12.16 = exports_df[exports_df$year > 2011,] %>%
#'     mutate(vship = 0, import.val = 0, init_obs = 0) %>%
#'     select(year, isic, vship, export.val, import.val, init_obs)
#'   
#'   imports_12.16 = imports_df[exports_df$year > 2011,] %>%
#'     mutate(vship = 0, export.val = 0, init_obs = 0) %>%
#'     select(year, isic, vship, export.val, import.val, init_obs)
#'   
#'   exp_imp.12_16 = merge(exports_12.16, imports_12.16,
#'                         by = c('year', 'isic', 'vship', 'init_obs')
#'   )
#'   exp_imp.12_16 = exp_imp.12_16 %>%
#'     mutate(export.val = export.val.x, import.val = import.val.y) %>%
#'     select(year, isic, vship, export.val, import.val, init_obs)
#'   
#'   trade_df = rbind(init_obs_df, exp_imp.12_16)
#'   
#'   return(trade_df)
#' }
#' 
#' #' @title delta (change) import penetration
#' #' @description
#' #' @param
#' #' @return
#' delta_impPen = function(trade_df, begin = '1991', end = '2016'){
#'   
#'   beg_bool = trade_df$year == begin
#'   end_bool = trade_df$year == end
#'   
#'   trade_df_beg = trade_df[beg_bool,]
#'   trade_df_end = trade_df[end_bool,]
#'   
#'   delta_ip_df = merge(trade_df_beg, trade_df_end, by = 'isic') %>%
#'     mutate(delta_ip = (import.val.y - import.val.x)/init_obs.x) %>%
#'     select(isic, delta_ip)
#'   
#'   return(delta_ip_df)
#' }
#' 
#' #' @title 
#' #' @description 
#' #' @param dataframe list of dfs
#' #' @return dataframe
#' cz_impPen = function(trade_df, emp_df,
#'                      begin = '1993',
#'                      end = '2016',
#'                      base = '1990'){
#'   
#'   cz_ind_prop = cz_ind_prop(emp_df)
#'   
#'   cz_drpw = merge(cz_ind_prop, trade_df, by = 'isic') %>%
#'     group_by(czone) %>%
#'     summarise(delta_ip = ind_prop %*% delta_ip)
#'   
#'   return(cz_drpw)
#' }
#' 
#' china_trade = industry_trade()
#' china_tradef = build_trade_df(china_trade)
#' china_imp_91.10 = delta_impPen(china_tradef, '1991', '2016')
#' emp_df = cbpdd_build()
#' delta_impPen.china = cz_impPen(china_imp_91.10, emp_df,
#'                                begin = '1993',
#'                                end = '2016',
#'                                base = '1990')
#' 
#' cz.cty_dip = cz.fips_data(delta_impPen.china)
#' us_xpr_cz.fips_04.16 = cz.fips_data(cz_dxr.us04_16)
#' eu_xpr_cz.fips_93.16 = cz.fips_data(cz_dxr.eu93_16)
#' colnames(eu_xpr_cz.fips_93.16) = c('czone', 'fipscty', 'eu_delta_exp_robs')
#' 
#' merged_exp_rob = merge(us_xpr_cz.fips_04.16, eu_xpr_cz.fips_93.16,
#'                        by = c('czone', 'fipscty'))
#' 
#' merged_xr_ip = merge(merged_exp_rob, cz.cty_dip, by = c('czone', 'fipscty'))









