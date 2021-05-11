********************************************************************************
* INUSTRIAL AUTOMATION AND POLITICAL POLARIZATION

* REGRESSIONS
********************************************************************************

* AUTHOR:		Maurice Williams

* DATE:			Nov 1st, 2018 

* DESCRIPTION: 	

* INPUTS:	
	
* OUTPUTS:		

********************************************************************************

* ADMINSITRATIVE

set matsize 1000

********************************************************************************
* LOAD DTA FILE

use "../outputs/data/fsscz.dta"

********************************************************************************


* generating other variables

gen bach_plus = bach + grad_plus


* creating control variable lists

global trade china_deltaip
global economic sh_routine sh_off man_sh
global demo_race black_nh nw_hisp asia_pi other
global demo_age yngad adlt miad1 miad2 sen
global demo_edu no_dip hs_dip some_col
global demographics $demo_age $demo_race bach_plus females

* generating variable labels

label variable delta_exp_robs "$\Delta$ Exposure to Robots"
label variable eu_delta_exp_robs "EU Exposure to Robots"
label variable china_deltaip "IP China"
label variable mex_deltaip "IP Mexico"
label variable females "\% Female"
label variable black_nh "\% Black non-hisp"
label variable nw_hisp "\% White hisp"
label variable asia_pi "\% Asian/Pac."
label variable other "\% Other race"
label variable yngad "\% Age 18-25"
label variable adlt "\% Age 26-35"
label variable miad1 "\% Age 36-45"
label variable miad2 "\% Age 45-64"
label variable sen "\% Age 65+"
label variable bach_plus "\% College Deg."
label variable sh_routine "\% CZ Routine Jobs"
label variable sh_off "Offshorability Index"
label variable man_sh "\% CZ Manufacturing Emp."




********************************************************************************
************************** CZONE FIRST-STAGE RESULTS ***************************
********************************************************************************


reg delta_exp_robs eu_delta_exp_robs eu_mu_deltaip $economic $demographics $political i.census_division if t1==1, vce(cluster czone)
predict eder_90_res if t1==1, residuals
predict eder_90 if t1==1
test eu_delta_exp_robs
*twoway scatter eder_90_res eu_delta_exp_robs

reg china_deltaip eu_delta_exp_robs eu_mu_deltaip $economic $demographics $political i.census_division if t1==1 [aw = tot_pop], vce(cluster czone)
predict edchip_90_res if t1==1, residuals
predict edchip_90 if t1==1
test eu_mu_deltaip
*twoway scatter edchip_90_res edchip_90



reg delta_exp_robs eu_delta_exp_robs eu_mu_deltaip $economic $demographics $political i.census_division if t2==1 [aw = tot_pop], vce(cluster czone)
predict eder_00_res if t2==1, residuals
predict eder_00 if t2==1
test eu_delta_exp_robs
*twoway scatter eder_00_res eder_00

reg china_deltaip eu_delta_exp_robs eu_mu_deltaip $economic $demographics $political i.census_division if t2==1 [aw = tot_pop], vce(cluster czone)
predict edchip_00_res if t2==1, residuals
predict edchip_00 if t2==1
test eu_mu_deltaip
*twoway scatter edchip_00_res edchip_00



reg delta_exp_robs eu_delta_exp_robs eu_mu_deltaip $economic $demographics $political i.census_division if t3==1 [aw = tot_pop], vce(cluster czone)
predict eder_10_res if t3==1, residuals
predict eder_10 if t3==1
test eu_delta_exp_robs
*twoway scatter eder_10_res eder_10

reg china_deltaip eu_delta_exp_robs eu_mu_deltaip $economic $demographics $political i.census_division if t3==1 [aw = tot_pop], vce(cluster czone)
predict edchip_10_res if t3==1, residuals
predict edchip_10 if t3==1
test eu_mu_deltaip
*twoway scatter edchip_10_res edchip_10





********************************************************************************
************************ CHANGE ROBOTS ON CHANGE TRADE *************************
********************************************************************************


reg delta_exp_robs china_deltaip $economic $demographics i.census_division if t1==1 [aw = tot_pop], cluster(czone)
est sto rc


ivreg2 delta_exp_robs (china_deltaip = eu_mu_deltaip) $demographics $economic i.census_division if t1==1 [aw = tot_pop], cluster(czone)
est sto rc1
predict eder_90_res if t1==1, residuals
predict eder_90 if t1==1
twoway scatter eder_90_res eu_delta_exp_robs

reg delta_exp_robs china_deltaip $economic $demographics i.census_division if t2==1 [aw = tot_pop], cluster(czone)
est sto rc2
ivreg2 delta_exp_robs (china_deltaip = eu_mu_deltaip) $demographics $economic i.census_division if t2==1 [aw = tot_pop], cluster(czone)
est sto rc3
reg delta_exp_robs china_deltaip $economic $demographics i.census_division  if t3==1 [aw = tot_pop], cluster(czone)
est sto rc4
ivreg2 delta_exp_robs (china_deltaip = eu_mu_deltaip) $economic $demographics i.census_division if t3==1 [aw = tot_pop], cluster(czone)
est sto rc5

esttab rc rc1 rc2 rc3 rc4 rc5, se stats(widstat N, labels("First-Stage F-statistic" "N")) star( * 0.10 ** 0.05 *** 0.01) drop(*.census_division _cons) label interaction(" $\times$ ")style(tex)  mtitles("OLS" "2SLS-1990" "OLS-2000" "2SLS-2000" "OLS-2010" "2SLS-2010")
esttab rc rc1 rc2 rc3 rc4 rc5 using "../outputs/tables/dr_controls.tex", replace se stats(widstat N, labels("First-Stage F-statistic" "N")) star(* 0.10 ** 0.05 *** 0.01) drop(*.census_division _cons) label interaction(" $\times$ ")style(tex) mtitles("OLS" "2SLS-1990" "OLS-2000" "2SLS-2000" "OLS-2010" "2SLS-2010")
esttab rc rc1 rc2 rc3 rc4 rc5 using "../outputs/tables/dr_controlsp.tex", replace se stats(widstat N, labels("First-Stage F-statistic" "N")) star(* 0.10 ** 0.05 *** 0.01) drop($demo_age $demo_race bach_plus females *.census_division _cons) label interaction(" $\times$ ")style(tex) nolines gaps nomtitles nonumbers nonotes nodepvar fragment



********************************************************************************
************************ CHANGE TRADE ON CHANGE ROBOTS *************************
********************************************************************************


ivreg2 china_deltaip (delta_exp_robs = eu_delta_exp_robs) $demographics $economic i.census_division if t1==1 [aw = tot_pop], cluster(double_cluster)
est sto chip1
reg china_deltaip delta_exp_robs $demographics $economic if t2==1, cluster(double_cluster)
est sto chip2
ivreg2 china_deltaip (delta_exp_robs = eu_delta_exp_robs) $demographics $economic i.census_division if t2==1 [aw = tot_pop], cluster(double_cluster)
est sto chip3
reg china_deltaip delta_exp_robs $demographics $economic i.census_division  if t3==1 [aw = tot_pop], cluster(double_cluster)
est sto chip4
ivreg2 china_deltaip (delta_exp_robs = eu_delta_exp_robs) $demographics $economic i.census_division if t3==1 [aw = tot_pop], cluster(double_cluster)
est sto chip5

esttab chip1 chip2 chip3 chip4 chip5, se stats(widstat N, labels("First-Stage F-statistic" "N")) star(* 0.10 ** 0.05 *** 0.01) drop(*.census_division _cons) label interaction(" $\times$ ")style(tex)  mtitles("2SLS-1990" "OLS-2000" "2SLS-2000" "OLS-2010" "2SLS-2010")
esttab chip1 chip2 chip3 chip4 chip5 using "../outputs/tables/chip.tex", replace se stats(widstat N, labels("First-Stage F-statistic" "N")) star(* 0.10 ** 0.05 *** 0.01) drop(*.census_division  _cons) label interaction(" $\times$ ")style(tex) mtitles("2SLS-1990" "OLS-2000" "2SLS-2000" "OLS-2010" "2SLS-2010")




********************************************************************************
******************************* FIRST-STAGE PLOTS ******************************
********************************************************************************



ivreg2 delta_exp_robs (china_deltaip = eu_mu_deltaip) $demographics $economic i.census_division if t1==1 [aw = tot_pop], cluster(czone)
est sto rc1
predict derfs_90_res if t1==1, residuals
predict derfs_90 if t1==1
twoway scatter derfs_90_res eu_delta_exp_robs if t1==1

ivreg2 delta_exp_robs (china_deltaip = eu_mu_deltaip) $demographics $economic i.census_division if t2==1 [aw = tot_pop], cluster(czone)
est sto rc1
predict derfs_00_res if t2==1, residuals
predict derfs_00 if t2==1
twoway scatter derfs_00_res eu_delta_exp_robs if t2==1

ivreg2 delta_exp_robs (china_deltaip = eu_mu_deltaip) $demographics $economic i.census_division if t3==1 [aw = tot_pop], cluster(czone)
est sto rc1
predict derfs_10_res if t3==1, residuals
predict derfs_10 if t3==1
twoway scatter derfs_10_res eu_delta_exp_robs if t3==1



