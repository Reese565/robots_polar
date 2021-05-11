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

use "../outputs/data/fssp_test.dta"

********************************************************************************


* generating other variables

gen bach_plus = bach + grad_plus
gen dem_vs_int = dem_win*dem_vs
gen gop_vs_int = gop_win*gop_vs


* creating control variable lists

global political1 gop_win#c.win_vs 
global trade china_deltaip
global economic sh_routine sh_off man_sh
global demo_race black_nh nw_hisp asia_pi other
global demo_age yngad adlt miad1 miad2 sen
global demo_edu no_dip hs_dip some_college
global demographics $demo_age $demo_race bach_plus females



* creating double cluster for standard errors

egen double_cluster=group(czone fipscty)
egen tripple_cluster=group(czone fipscty p_elections)



* labeling variables

label variable delta_exp_robs "Exposure to Robots"
label variable eu_delta_exp_robs "EU Exposure to Robots"
label variable china_deltaip "IP China"
label variable mex_deltaip "IP Mexico"
label variable black_nh "\% Black non-hisp"
label variable nw_hisp "\% White hisp"
label variable asia_pi "\% Asian/Pac."
label variable asia_pi "\% Other race"
label variable yngad "\% Age 18-25"
label variable adlt "\% Age 26-35"
label variable miad1 "\% Age 36-45"
label variable miad2 "\% Age 45-64"
label variable sen "\% Age 65+"
label variable bach_plus "\% College Deg."
label variable sh_routine "\% CZ Routine Jobs"
label variable sh_off "Offshorability Index"
label variable man_sh "\% CZ Manufacturing Emp."
label variable gop_win "GOP Win"
label variable gop_vs "GOP Vote Share"
label variable dem_vs "Dem Vote Share"
label variable win_vs "Winning Candidate Vote Share"
label variable delta_gop_vs "Change GOP Vote Share"




predict resids_0, residuals
predict yhat_0

twoway scatter resids_0 yhat_0

twoway scatter delta_gop_vs yhat_0

drop resids_0 yhat_0



********************************************************************************
************************** SUMMARY STATISTICS TABLES ***************************
********************************************************************************


estpost sum delta_gop_vs delta_exp_robs china_deltaip eu_delta_exp_robs eu_mu_deltaip gop_win win_vs $demographics $economic if t1==1, detail
esttab using "../outputs/tables/pres/summary_stats/pt1.tex", replace cell( (count(label(N)) mean(label(Mean))  sd(par label(Standard Deviation)) min(label(Min)) p25(label(25$^{th}$ Percentile))  p50(label(Median))  p50(label(75$^{th}$ Percentile))  max(label(Max)) ) ) label style(tex) nonumbers noobs gaps fragment

estpost sum delta_gop_vs delta_exp_robs china_deltaip eu_delta_exp_robs eu_mu_deltaip gop_win win_vs $demographics $economic if t2==1, detail
esttab using "../outputs/tables/pres/summary_stats/pt2.tex", replace cell( (count(label(N)) mean(label(Mean))  sd(par label(Standard Deviation)) min(label(Min)) p25(label(25$^{th}$ Percentile))  p50(label(Median))  p50(label(75$^{th}$ Percentile))  max(label(Max)) ) ) label style(tex) nonumbers noobs gaps fragment

estpost sum delta_gop_vs delta_exp_robs china_deltaip eu_delta_exp_robs eu_mu_deltaip gop_win win_vs $demographics $economic if t3==1, detail
esttab using "../outputs/tables/pres/summary_stats/pt3.tex", replace cell( (count(label(N)) mean(label(Mean))  sd(par label(Standard Deviation)) min(label(Min)) p25(label(25$^{th}$ Percentile))  p50(label(Median))  p50(label(75$^{th}$ Percentile))  max(label(Max)) ) ) label style(tex) nonumbers noobs gaps fragment




********************************************************************************
********************************************************************************
********************************************************************************

* Stacked

reg delta_gop_vs delta_exp_robs china_deltaip $political1 $demographics $economic if t1==1 | t4==1 | t6==1 [aw = vote_total], cluster(tripple_cluster)
est sto sprOLS
ivreg2 delta_gop_vs (delta_exp_robs = eu_delta_exp_robs) i.census_division  if t1==1 | t4==1 | t6==1  [aw = vote_total], cluster(tripple_cluster)
est sto spr1
ivreg2 delta_gop_vs (delta_exp_robs = eu_delta_exp_robs) $political1 i.census_division if t1==1 | t4==1 | t6==1 [aw = vote_total], cluster(tripple_cluster)
est sto spr2
ivreg2 delta_gop_vs (delta_exp_robs = eu_delta_exp_robs) $political1 china_deltaip i.census_division if t1==1 | t4==1 | t6==1 [aw = vote_total], cluster(tripple_cluster)
est sto spr2x
ivreg2 delta_gop_vs (delta_exp_robs china_deltaip = eu_delta_exp_robs eu_mu_deltaip) $political1  i.census_division if t1==1 | t4==1 | t6==1 [aw = vote_total], cluster(tripple_cluster)
est sto spr3
ivreg2 delta_gop_vs (delta_exp_robs china_deltaip = eu_delta_exp_robs eu_mu_deltaip) $political1  $demographics i.census_division if t1==1 | t4==1 | t6==1 [aw = vote_total], cluster(tripple_cluster)
est sto spr4
ivreg2 delta_gop_vs (delta_exp_robs china_deltaip = eu_delta_exp_robs eu_mu_deltaip) $political1  $demographics $economic i.census_division if t1==1 | t4==1 | t6==1 [aw = vote_total], cluster(tripple_cluster)
est sto spr5


esttab sprOLS spr1 spr2 spr2x spr3 spr4 spr5, se stats(widstat N, labels("First-Stage F-statistic" "N")) star(* 0.10 ** 0.05 *** 0.01) drop(*.gop_win* *.census_division _cons) addnotes(my note) label interaction(" $\times$ ")style(tex)  mtitles(OLS 2SLS 2SLS 2SLS 2SLS 2SLS)
esttab sprOLS spr1 spr2 spr2x spr3 spr4 spr5 using "../outputs/tables/pres/pres_stackedp.tex", replace se stats(widstat N, labels("First-Stage F-statistic" "N")) star(* 0.10 ** 0.05 *** 0.01) drop(*.gop_win* china_deltaip $demographics $economic *.census_division _cons) addnotes(my note) label interaction(" $\times$ ")style(tex) nolines gaps nomtitles nonumbers nonotes nodepvar fragment           
esttab sprOLS spr1 spr2 spr2x spr3 spr4 spr5 using "../outputs/tables/pres/pres_stacked.tex", replace se stats(widstat N, labels("First-Stage F-statistic" "N")) star(* 0.10 ** 0.05 *** 0.01) drop(*.gop_win* *.census_division _cons) label interaction(" $\times$ ")style(tex) mtitles(OLS 2SLS 2SLS 2SLS 2SLS 2SLS)


* Stacked Sub

reg delta_gop_vs delta_exp_robs china_deltaip $political1 $demographics $economic if t4==1 | t6==1 [aw = vote_total], cluster(tripple_cluster)
est sto sprOLS
ivreg2 delta_gop_vs (delta_exp_robs = eu_delta_exp_robs) i.census_division  if t4==1 | t6==1  [aw = vote_total], cluster(tripple_cluster)
est sto spr1
ivreg2 delta_gop_vs (delta_exp_robs = eu_delta_exp_robs) $political1 i.census_division if t4==1 | t6==1 [aw = vote_total], cluster(tripple_cluster)
est sto spr2
ivreg2 delta_gop_vs (delta_exp_robs = eu_delta_exp_robs) $political1 china_deltaip i.census_division if t4==1 | t6==1 [aw = vote_total], cluster(tripple_cluster)
est sto spr2x
ivreg2 delta_gop_vs (delta_exp_robs china_deltaip = eu_delta_exp_robs eu_mu_deltaip) $political1  i.census_division if t4==1 | t6==1 [aw = vote_total], cluster(tripple_cluster)
est sto spr3
ivreg2 delta_gop_vs (delta_exp_robs china_deltaip = eu_delta_exp_robs eu_mu_deltaip) $political1  $demographics i.census_division if t4==1 | t6==1 [aw = vote_total], cluster(tripple_cluster)
est sto spr4
ivreg2 delta_gop_vs (delta_exp_robs china_deltaip = eu_delta_exp_robs eu_mu_deltaip) $political1  $demographics $economic i.census_division if t4==1 | t6==1 [aw = vote_total], cluster(tripple_cluster)
est sto spr5


esttab sprOLS spr1 spr2 spr2x spr3 spr4 spr5, se stats(widstat N, labels("First-Stage F-statistic" "N")) star(* 0.10 ** 0.05 *** 0.01) drop(*.gop_win* *.census_division _cons) addnotes(my note) label interaction(" $\times$ ")style(tex)  mtitles(OLS 2SLS 2SLS 2SLS 2SLS 2SLS)
esttab sprOLS spr1 spr2 spr2x spr3 spr4 spr5 using "../outputs/tables/pres/pres_stackedsubp.tex", replace se stats(widstat N, labels("First-Stage F-statistic" "N")) star(* 0.10 ** 0.05 *** 0.01) drop(*.gop_win* china_deltaip $demographics $economic *.census_division _cons) addnotes(my note) label interaction(" $\times$ ")style(tex) nolines gaps nomtitles nonumbers nonotes nodepvar fragment           
esttab sprOLS spr1 spr2 spr2x spr3 spr4 spr5 using "../outputs/tables/pres/pres_stackedsub.tex", replace se stats(widstat N, labels("First-Stage F-statistic" "N")) star(* 0.10 ** 0.05 *** 0.01) drop(*.gop_win* *.census_division _cons) label interaction(" $\times$ ")style(tex) mtitles(OLS 2SLS 2SLS 2SLS 2SLS 2SLS)


* 1992-2000

reg delta_gop_vs delta_exp_robs china_deltaip $political1 $demographics $economic if t1==1 [aw = vote_total], cluster(double_cluster)
est sto prOLS
ivreg2 delta_gop_vs (delta_exp_robs = eu_delta_exp_robs)  if t1==1 [aw = vote_total], cluster(double_cluster)
est sto pr1
ivreg2 delta_gop_vs (delta_exp_robs = eu_delta_exp_robs) $political1 if t1==1 [aw = vote_total], cluster(double_cluster)
est sto pr2
ivreg2 delta_gop_vs (delta_exp_robs = eu_delta_exp_robs) $political1 china_deltaip if t1==1 [aw = vote_total], cluster(double_cluster)
est sto pr2x
ivreg2 delta_gop_vs (delta_exp_robs china_deltaip = eu_delta_exp_robs eu_mu_deltaip) $political1  if t1==1 [aw = vote_total], cluster(double_cluster)
est sto pr3
ivreg2 delta_gop_vs (delta_exp_robs china_deltaip = eu_delta_exp_robs eu_mu_deltaip) $political1  $demographics if t1==1 [aw = vote_total], cluster(double_cluster)
est sto pr4
ivreg2 delta_gop_vs (delta_exp_robs china_deltaip = eu_delta_exp_robs eu_mu_deltaip) $political1  $demographics $economic if t1==1 [aw = vote_total], cluster(double_cluster)
est sto pr5


esttab prOLS pr1 pr2 pr2x pr3 pr4 pr5, se stats(widstat N, labels("First-Stage F-statistic" "N")) star(* 0.10 ** 0.05 *** 0.01) drop(*.gop_win* _cons) addnotes(my note) label interaction(" $\times$ ")style(tex) mtitles(OLS 2SLS 2SLS 2SLS 2SLS 2SLS)
esttab prOLS pr1 pr2 pr2x pr3 pr4 pr5 using "../outputs/tables/pres/pres_92-00p.tex", replace se stats(widstat N, labels("First-Stage F-statistic" "N")) star(* 0.10 ** 0.05 *** 0.01) drop(*.gop_win* china_deltaip $demographics $economic _cons) addnotes(my note) label interaction(" $\times$ ")style(tex) nolines gaps nomtitles nonumbers nonotes nodepvar fragment      
esttab prOLS pr1 pr2 pr2x pr3 pr4 pr5 using "../outputs/tables/pres/pres_92-00.tex", replace se stats(widstat N, labels("First-Stage F-statistic" "N")) star(* 0.10 ** 0.05 *** 0.01) drop(*.gop_win* _cons) label interaction(" $\times$ ")style(tex) mtitles(OLS 2SLS 2SLS 2SLS 2SLS 2SLS)





* 1992-2008

reg delta_gop_vs delta_exp_robs china_deltaip $political1 $demographics $economic if t2==1 [aw = cnty_vs], cluster(double_cluster)
est sto prOLS1
ivreg2 delta_gop_vs (delta_exp_robs = eu_delta_exp_robs) if t2==1 [aw = vote_total], cluster(double_cluster)
est sto pr7
ivreg2 delta_gop_vs (delta_exp_robs = eu_delta_exp_robs) $political1 if t2==1 [aw = vote_total], cluster(double_cluster)
est sto pr8
ivreg2 delta_gop_vs (delta_exp_robs = eu_delta_exp_robs) $political1 china_deltaip if t2==1 [aw = vote_total], cluster(double_cluster)
est sto pr8x
ivreg2 delta_gop_vs (delta_exp_robs china_deltaip = eu_delta_exp_robs eu_mu_deltaip) $political1  if t2==1 [aw = vote_total], cluster(double_cluster)
est sto pr9
ivreg2 delta_gop_vs (delta_exp_robs china_deltaip = eu_delta_exp_robs eu_mu_deltaip) $political1  $demographics if t2==1 [aw = vote_total], cluster(double_cluster)
est sto pr10
ivreg2 delta_gop_vs (delta_exp_robs china_deltaip = eu_delta_exp_robs eu_mu_deltaip) $political1  $demographics $economic if t2==1 [aw = vote_total], cluster(double_cluster)
est sto pr11


esttab prOLS1 pr7 pr8 pr8x pr9 pr10 pr11, se stats(widstat N, labels("First-Stage F-statistic" "N")) star(* 0.10 ** 0.05 *** 0.01) drop(*.gop_win* _cons) addnotes(my note) label interaction(" $\times$ ")style(tex) mtitles(OLS 2SLS 2SLS 2SLS 2SLS 2SLS)
esttab prOLS1 pr7 pr8 pr8x pr9 pr10 pr11 using "../outputs/tables/pres/pres_92-08p.tex", replace se stats(widstat N, labels("First-Stage F-statistic" "N")) star(* 0.10 ** 0.05 *** 0.01) drop(*.gop_win* china_deltaip $demographics $economic man_sh _cons) addnotes(my note) label interaction(" $\times$ ")style(tex)  nolines gaps nomtitles nonumbers nonotes nodepvar fragment
esttab prOLS1 pr7 pr8 pr8x pr9 pr10 pr11 using "../outputs/tables/pres/pres_92-08.tex", replace se stats(widstat N, labels("First-Stage F-statistic" "N")) star(* 0.10 ** 0.05 *** 0.01) drop(*.gop_win* _cons) label interaction(" $\times$ ")style(tex) mtitles(OLS 2SLS 2SLS 2SLS 2SLS 2SLS)




* 1992-2016

reg delta_gop_vs delta_exp_robs china_deltaip $political1 $demographics $economic if t3==1 [aw = vote_total], cluster(double_cluster)
est sto prOLS3
ivreg2 delta_gop_vs (delta_exp_robs = eu_delta_exp_robs) if t3==1 [aw = vote_total], cluster(double_cluster)
est sto pr13
ivreg2 delta_gop_vs (delta_exp_robs = eu_delta_exp_robs) $political1 if t3==1 [aw = vote_total], cluster(double_cluster)
est sto pr14
ivreg2 delta_gop_vs (delta_exp_robs = eu_delta_exp_robs) $political1 china_deltaip if t3==1 [aw = vote_total], cluster(double_cluster)
est sto pr14x
ivreg2 delta_gop_vs (delta_exp_robs china_deltaip = eu_delta_exp_robs eu_mu_deltaip) $political1  if t3==1 [aw = vote_total], cluster(double_cluster)
est sto pr15
ivreg2 delta_gop_vs (delta_exp_robs china_deltaip = eu_delta_exp_robs eu_mu_deltaip) $political1  $demographics if t3==1 [aw = vote_total], cluster(double_cluster)
est sto pr16
ivreg2 delta_gop_vs (delta_exp_robs china_deltaip = eu_delta_exp_robs eu_mu_deltaip) $political1  $political1 $demographics $economic if t3==1 [aw = vote_total], cluster(double_cluster)
est sto pr17


esttab prOLS3 pr13 pr14 pr14x pr15 pr16 pr17, se stats(widstat N, labels("First-Stage F-statistic" "N")) star(* 0.10 ** 0.05 *** 0.01) drop(*.gop_win* _cons) addnotes(my note) label interaction(" $\times$ ")style(tex)  mtitles(OLS 2SLS 2SLS 2SLS 2SLS 2SLS)
esttab prOLS3 pr13 pr14 pr14x pr15 pr16 pr17 using "../outputs/tables/pres/pres_92-16p.tex", replace se stats(widstat N, labels("First-Stage F-statistic" "N")) star(* 0.10 ** 0.05 *** 0.01) drop(*.gop_win* china_deltaip $demographics $economic _cons) addnotes(my note) label interaction(" $\times$ ")style(tex) nolines gaps nomtitles nonumbers nonotes nodepvar fragment
esttab prOLS3 pr13 pr14 pr14x pr15 pr16 pr17 using "../outputs/tables/pres/pres_92-16.tex", replace se stats(widstat N, labels("First-Stage F-statistic" "N")) star(* 0.10 ** 0.05 *** 0.01) drop(*.gop_win* _cons) label interaction(" $\times$ ")style(tex) mtitles(OLS 2SLS 2SLS 2SLS 2SLS 2SLS)




* 2000-2008

reg delta_gop_vs delta_exp_robs china_deltaip $political1 $demographics $economic if t4==1 [aw = vote_total], cluster(double_cluster)
est sto prOLS4
ivreg2 delta_gop_vs (delta_exp_robs = eu_delta_exp_robs) if t4==1 [aw = vote_total], cluster(double_cluster)
est sto pr19
ivreg2 delta_gop_vs (delta_exp_robs = eu_delta_exp_robs) $political1 if t4==1 [aw = vote_total], cluster(double_cluster)
est sto pr20
ivreg2 delta_gop_vs (delta_exp_robs = eu_delta_exp_robs) $political1 china_deltaip if t4==1 [aw = vote_total], cluster(double_cluster)
est sto pr20x
ivreg2 delta_gop_vs (delta_exp_robs china_deltaip = eu_delta_exp_robs eu_mu_deltaip) $political1  if t4==1 [aw = vote_total], cluster(double_cluster)
est sto pr21
ivreg2 delta_gop_vs (delta_exp_robs china_deltaip = eu_delta_exp_robs eu_mu_deltaip) $political1  $demographics if t4==1 [aw = vote_total], cluster(double_cluster)
est sto pr22
ivreg2 delta_gop_vs (delta_exp_robs china_deltaip = eu_delta_exp_robs eu_mu_deltaip) $political1  $demographics $economic if t4==1 [aw = vote_total], cluster(double_cluster)
est sto pr23


esttab prOLS4 pr19 pr20 pr20x pr21 pr22 pr23, se stats(widstat N, labels("First-Stage F-statistic" "N")) star(* 0.10 ** 0.05 *** 0.01) drop(*.gop_win* _cons) addnotes(my note) label interaction(" $\times$ ")style(tex) mtitles(OLS 2SLS 2SLS 2SLS 2SLS 2SLS)
esttab prOLS4 pr19 pr20 pr20x pr21 pr22 pr23 using "../outputs/tables/pres/pres_00-08p.tex", replace se stats(widstat N, labels("First-Stage F-statistic" "N")) star(* 0.10 ** 0.05 *** 0.01) drop(*.gop_win* china_deltaip $demographics $economic  _cons) addnotes(my note) label interaction(" $\times$ ")style(tex) nolines gaps nomtitles nonumbers nonotes nodepvar fragment
esttab prOLS4 pr19 pr20 pr20x pr21 pr22 pr23 using "../outputs/tables/pres/pres_00-08.tex", replace se stats(widstat N, labels("First-Stage F-statistic" "N")) star(* 0.10 ** 0.05 *** 0.01) drop(*.gop_win* _cons) label interaction(" $\times$ ")style(tex) mtitles(OLS 2SLS 2SLS 2SLS 2SLS 2SLS)



* 2000-2016

reg delta_gop_vs delta_exp_robs china_deltaip $political1 $demographics $economic if t5==1 [aw = vote_total], cluster(double_cluster)
est sto prOLS5
ivreg2 delta_gop_vs (delta_exp_robs = eu_delta_exp_robs) if t5==1 [aw = vote_total], cluster(double_cluster)
est sto pr25
ivreg2 delta_gop_vs (delta_exp_robs = eu_delta_exp_robs) $political1 if t5==1 [aw = vote_total], cluster(double_cluster)
est sto pr26
ivreg2 delta_gop_vs (delta_exp_robs = eu_delta_exp_robs) $political1 china_deltaip if t5==1 [aw = vote_total], cluster(double_cluster)
est sto pr26x
ivreg2 delta_gop_vs (delta_exp_robs china_deltaip = eu_delta_exp_robs eu_mu_deltaip) $political1  if t5==1 [aw = vote_total], cluster(double_cluster)
est sto pr27
ivreg2 delta_gop_vs (delta_exp_robs china_deltaip = eu_delta_exp_robs eu_mu_deltaip) $political1  $demographics if t5==1 [aw = vote_total], cluster(double_cluster)
est sto pr28
ivreg2 delta_gop_vs (delta_exp_robs china_deltaip = eu_delta_exp_robs eu_mu_deltaip) $political1  $demographics $economic if t5==1 [aw = vote_total], cluster(double_cluster)
est sto pr29


esttab prOLS5 pr25 pr26 pr26x pr27 pr28 pr29, se stats(widstat N, labels("First-Stage F-statistic" "N")) star(* 0.10 ** 0.05 *** 0.01) drop(*.gop_win* _cons) addnotes(my note) label interaction(" $\times$ ")style(tex) mtitles(OLS 2SLS 2SLS 2SLS 2SLS 2SLS)
esttab prOLS5 pr25 pr26 pr26x pr27 pr28 pr29 using "../outputs/tables/pres/pres_00-16p.tex", replace se stats(widstat N, labels("First-Stage F-statistic" "N")) star(* 0.10 ** 0.05 *** 0.01) drop(*.gop_win* china_deltaip $demographics $economic _cons) addnotes(my note) label interaction(" $\times$ ")style(tex) nolines gaps nomtitles nonumbers nonotes nodepvar fragment
esttab prOLS5 pr25 pr26 pr26x pr27 pr28 pr29 using "../outputs/tables/pres/pres_00-16.tex", replace se stats(widstat N, labels("First-Stage F-statistic" "N")) star(* 0.10 ** 0.05 *** 0.01) drop(*.gop_win* _cons)  label interaction(" $\times$ ")style(tex) mtitles(OLS 2SLS 2SLS 2SLS 2SLS 2SLS)



* 2008-2016

reg delta_gop_vs delta_exp_robs china_deltaip $political1 $demographics $economic if t6==1 [aw = vote_total], cluster(double_cluster)
est sto prOLS6
ivreg2 delta_gop_vs (delta_exp_robs = eu_delta_exp_robs) if t6==1 [aw = vote_total], cluster(double_cluster)
est sto pr31
ivreg2 delta_gop_vs (delta_exp_robs = eu_delta_exp_robs) $political1 if t6==1 [aw = vote_total], cluster(double_cluster)
est sto pr32
ivreg2 delta_gop_vs (delta_exp_robs = eu_delta_exp_robs) $political1 china_deltaip if t6==1 [aw = vote_total], cluster(double_cluster)
est sto pr32x
ivreg2 delta_gop_vs (delta_exp_robs china_deltaip = eu_delta_exp_robs eu_mu_deltaip) $political1  if t6==1 [pw = vote_total], cluster(double_cluster)
est sto pr33
ivreg2 delta_gop_vs (delta_exp_robs china_deltaip = eu_delta_exp_robs eu_mu_deltaip) $political1  $demographics if t6==1 [pw = vote_total], cluster(double_cluster)
est sto pr34
ivreg2 delta_gop_vs (delta_exp_robs china_deltaip = eu_delta_exp_robs eu_mu_deltaip) $political1  $demographics $economic if t6==1 [pw = vote_total], cluster(double_cluster)
est sto pr35


esttab prOLS6 pr31 pr32 pr32x pr33 pr34 pr35, se stats(widstat N, labels("First-Stage F-statistic" "N")) star(* 0.10 ** 0.05 *** 0.01) drop(*.gop_win* _cons) addnotes(my note) label interaction(" $\times$ ")style(tex) mtitles(OLS 2SLS 2SLS 2SLS 2SLS 2SLS)
esttab prOLS6 pr31 pr32 pr32x pr33 pr34 pr35 using "../outputs/tables/pres/pres_08-16.tex", replace se stats(widstat N, labels("First-Stage F-statistic" "N")) star(* 0.10 ** 0.05 *** 0.01) drop(*.gop_win* _cons) label interaction(" $\times$ ")style(tex)  mtitles(OLS 2SLS 2SLS 2SLS 2SLS 2SLS)
esttab prOLS6 pr31 pr32 pr32x pr33 pr34 pr35 using "../outputs/tables/pres/pres_08-16p.tex", replace se stats(widstat N, labels("First-Stage F-statistic" "N")) star(* 0.10 ** 0.05 *** 0.01) drop(*.gop_win* china_deltaip $demographics $economic man_sh _cons) addnotes(my note) label interaction($\times$) nolines gaps nomtitles nonumbers nonotes nodepvar fragment






esttab prOLS pr1 pr2 pr2x pr3 pr4 pr5 prOLS4 pr19 pr20 pr20x pr21 pr22 pr23 using "../outputs/tables/pres/pres_fp.tex", replace se stats(widstat N, labels("First-Stage F-statistic" "N")) star(* 0.10 ** 0.05 *** 0.01) drop(*.gop_win* china_deltaip $demographics $economic _cons) addnotes(my note) label interaction(" $\times$ ")style(tex) extracols(8) nolines gaps nomtitles nonumbers nonotes nodepvar fragment      

esttab prOLS1 pr7 pr8 pr8x pr9 pr10 pr11 prOLS5 pr25 pr26 pr26x pr27 pr28 pr29 using "../outputs/tables/pres/pres_f1p.tex", replace se stats(widstat N, labels("First-Stage F-statistic" "N")) star(* 0.10 ** 0.05 *** 0.01) drop(*.gop_win* china_deltaip $demographics $economic man_sh _cons) addnotes(my note) label interaction(" $\times$ ")style(tex) extracols(8)  nolines gaps nomtitles nonumbers nonotes nodepvar fragment

esttab prOLS3 pr13 pr14 pr14x pr15 pr16 pr17 prOLS6 pr31 pr32 pr32x pr33 pr34 pr35 using "../outputs/tables/pres/pres_f2p.tex", replace se stats(widstat N, labels("First-Stage F-statistic" "N")) star(* 0.10 ** 0.05 *** 0.01) drop(*.gop_win* china_deltaip $demographics $economic _cons) addnotes(my note) label interaction(" $\times$ ")style(tex) extracols(8) nolines gaps nomtitles nonumbers nonotes nodepvar fragment






