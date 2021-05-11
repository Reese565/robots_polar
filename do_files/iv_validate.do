********************************************************************************
* ECON 142C

* PROBLEM SET 5
********************************************************************************

* AUTHOR:		Maurice Williams

* DATE:			Nov 1st, 2018 

* DESCRIPTION: 	

* INPUTS:		
* OUTPUTS:		
********************************************************************************
* LOAD DTA FILE


use "../outputs/data/fss.dta"



********************************************************************************

* generating other variables

gen bach_plus = bach + grad_plus
replace delta_abs_nom = delta_abs_nom*100
replace delta_nom = delta_nom*100

* creating control variable lists

global political1 gop_win_beg#c.win_prop gop_win_beg#c.nominate_beg  gop_win_beg#unopposed_beg
global political2 gop_win_beg#c.win_prop gop_win_beg#c.nominate_beg  gop_win_beg#unopposed_beg
global trade china_deltaip mex_deltaip
global trade_iv AUS_delta_ip DE_delta_ip DEK_delta_ip ESP_delta_ip FIN_delta_ip JAP_delta_ip NZ_delta_ip SUI_delta_ip
global economic sh_routine sh_off man_sh
global demo_race black_nh nw_hisp asia_pi other
global demo_age yngad adlt miad1 miad2 sen
global demo_edu no_dip hs_dip some_col
global demographics $demo_age $demo_race bach_plus females

* creating double cluster for standard errors

egen double_cluster=group(czone dist_id)



reg delta_exp_robs eu_delta_exp_robs eu_mu_deltaip $economic $demographics $political i.census_division if t1==1, vce(cluster double_cluster)
predict eder_90_res if t1==1, residuals
predict eder_90 if t1==1
test eu_delta_exp_robs
*twoway scatter eder_90_res eder_90

reg china_deltaip eu_delta_exp_robs eu_mu_deltaip $economic $demographics $political i.census_division if t1==1, vce(cluster double_cluster)
predict edchip_90_res if t1==1, residuals
predict edchip_90 if t1==1
test eu_mu_deltaip
*twoway scatter edchip_90_res edchip_90



reg delta_exp_robs eu_delta_exp_robs eu_mu_deltaip $economic $demographics $political i.census_division if t2==1, cluster(double_cluster)
predict eder_00_res if t2==1, residuals
predict eder_00 if t2==1
test eu_delta_exp_robs
*twoway scatter eder_00_res eder_00

reg china_deltaip eu_delta_exp_robs eu_mu_deltaip $economic $demographics $political i.census_division if t2==1, vce(cluster double_cluster)
predict edchip_00_res if t2==1, residuals
predict edchip_00 if t2==1
test eu_mu_deltaip
*twoway scatter edchip_00_res edchip_00



reg delta_exp_robs eu_delta_exp_robs eu_mu_deltaip $economic $demographics $political i.census_division if t3==1, vce(cluster double_cluster)
predict eder_10_res if t3==1, residuals
predict eder_10 if t3==1
test eu_delta_exp_robs
*twoway scatter eder_10_res eder_10

reg china_deltaip eu_delta_exp_robs eu_mu_deltaip $economic $demographics $political i.census_division if t3==1, vce(cluster double_cluster)
predict edchip_10_res if t3==1, residuals
predict edchip_10 if t3==1
test eu_mu_deltaip
*twoway scatter edchip_10_res edchip_10




