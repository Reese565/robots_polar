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

use "../outputs/data/fss.dta"

********************************************************************************




* generating other variables

gen bach_plus = bach + grad_plus
replace delta_abs_nom = delta_abs_nom*100
replace delta_nom = delta_nom*100
gen win_vs_int = win_prop*gop_win_beg
gen nominate_beg_int = nominate_beg*gop_win_beg
gen unopposed_beg_int = unopposed_beg*gop_win_beg
gen delta_exp_robs2 = delta_exp_robs^2
gen eu_delta_exp_robs2 = eu_delta_exp_robs^2

* creating control variable lists

global political1 gop_win_beg win_vs_int nominate_beg_int unopposed_beg_int
global political2 gop_win_beg#c.win_prop gop_win_beg#c.nominate_beg  gop_win_beg#unopposed_beg
global political1 gop_win_beg#c.win_prop gop_win_beg#c.nominate_beg  gop_win_beg#unopposed_beg
global trade china_deltaip
global economic sh_routine sh_off man_sh
global demo_race black_nh nw_hisp asia_pi other
global demo_age yngad adlt miad1 miad2 sen
global demo_edu no_dip hs_dip some_col
global demographics $demo_age $demo_race bach_plus females


* creating double cluster for standard errors

egen double_cluster=group(czone dist_id)
egen tripple_cluster=group(czone dist_id census)


* labeling variables

label variable delta_abs_nom "Change Absolute Nominate"
label variable delta_nom "Change Nominate"
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
label variable gop_win_beg "GOP Win"
label variable win_prty_delta_sh "Change Winning Party Vote Share"
label variable gop_delta_sh "Change GOP Vote Share"
label variable dem_delta_sh "Change Democratic Party Vote Share"
label variable delta_win_margin "Change Win Margin"
label variable unopposed_beg "Unopposed"
label variable nominate_beg "Beg. Nominate"
label variable abs_nominate_beg "Beg. Abs. Nominate"
label variable win_prop "Winning Candidates Vote Share"




* predictions and residuals

predict resids_0, residuals
predict yhat_0

twoway scatter resids_0 yhat_0
kdensity resids_0
qnorm resids_0

twoway scatter delta_abs_nom yhat_0
twoway scatter delta_nom yhat_0
twoway scatter win_prty_delta_sh yhat_0
twoway scatter gop_delta_sh yhat_0
twoway scatter dem_delta_sh yhat_0
twoway scatter delta_win_margin yhat_0

drop resids_0 yhat_0

esttab, se star(* 0.10 ** 0.05 *** 0.01) drop($political2 $trade $economic $demographics _cons) addnotes() mtitle()




reg delta_abs_nom $trade man_sh $economic $demo_age $demo_race bach_plus females, cluster(double_cluster)

reg china_deltaip mex_deltaip delta_exp_robs man_sh $economic $demo_age $demo_race bach_plus females, cluster(double_cluster)
reg mex_deltaip china_deltaip delta_exp_robs man_sh $economic $demo_age $demo_race bach_plus females, cluster(double_cluster)
reg man_sh delta_exp_robs $trade $economic $demo_age $demo_race bach_plus female, cluster(double_cluster)

reg delta_abs_nom $trade $political2 $economic $demographics [aw = cd_prop_pop], cluster(tripple_cluster)


********************************************************************************
************************** SUMMARY STATISTICS TABLES ***************************
********************************************************************************


estpost sum delta_nom delta_abs_nom delta_exp_robs china_deltaip eu_delta_exp_robs eu_mu_deltaip nominate_beg win_prop gop_win_beg unopposed_beg $demographics $economic if t1==1, detail
esttab using "../outputs/tables/house/summary_stats/ht1.tex", replace cell( (count(label(N)) mean(label(Mean))  sd(par label(Standard Deviation)) min(label(Min)) p25(label(25$^{th}$ Percentile))  p50(label(Median))  p50(label(75$^{th}$ Percentile))  max(label(Max)) ) ) label style(tex) nonumbers noobs gaps fragment

estpost sum delta_nom delta_abs_nom delta_exp_robs china_deltaip eu_delta_exp_robs eu_mu_deltaip nominate_beg win_prop gop_win_beg unopposed_beg $demographics $economic if t2==1, detail
esttab using "../outputs/tables/house/summary_stats/ht2.tex", replace cell( (count(label(N)) mean(label(Mean))  sd(par label(Standard Deviation)) min(label(Min)) p25(label(25$^{th}$ Percentile))  p50(label(Median))  p50(label(75$^{th}$ Percentile))  max(label(Max)) ) ) label style(tex) nonumbers noobs gaps fragment

estpost sum delta_nom delta_abs_nom delta_exp_robs china_deltaip eu_delta_exp_robs eu_mu_deltaip nominate_beg win_prop gop_win_beg unopposed_beg $demographics $economic if t3==1, detail
esttab using "../outputs/tables/house/summary_stats/ht3.tex", replace cell( (count(label(N)) mean(label(Mean))  sd(par label(Standard Deviation)) min(label(Min)) p25(label(25$^{th}$ Percentile))  p50(label(Median))  p50(label(75$^{th}$ Percentile))  max(label(Max)) ) ) label style(tex) nonumbers noobs gaps fragment


********************************************************************************
************************** CHANGE IN NOMINATE SCORES ***************************
********************************************************************************

* Stacked Differences

* OLS
reg delta_nom delta_exp_robs china_deltaip $political2 $demographics $economic i.census_division [aw = cd_prop_pop], cluster(tripple_cluster)
est sto m
* IV Regressions
ivreg2 delta_nom (delta_exp_robs = eu_delta_exp_robs) i.census_division [aw = cd_prop_pop], cluster(tripple_cluster)
est sto m1
ivreg2 delta_nom (delta_exp_robs = eu_delta_exp_robs) $political2 i.census_division [aw = cd_prop_pop], cluster(tripple_cluster)
est sto m2
ivreg2 delta_nom (delta_exp_robs = eu_delta_exp_robs) $political2 china_deltaip i.census_division [aw = cd_prop_pop], cluster(tripple_cluster)
est sto m2x
ivreg2 delta_nom (delta_exp_robs china_deltaip = eu_delta_exp_robs eu_mu_deltaip) $political2 i.census_division [aw = cd_prop_pop], cluster(tripple_cluster)
est sto m3
ivreg2 delta_nom (delta_exp_robs china_deltaip = eu_delta_exp_robs eu_mu_deltaip) $political2 $demographics i.census_division [aw = cd_prop_pop], cluster(tripple_cluster)
est sto m4
ivreg2 delta_nom (delta_exp_robs china_deltaip = eu_delta_exp_robs eu_mu_deltaip) $political2 $demographics $economic i.census_division [aw = cd_prop_pop], cluster(tripple_cluster)
est sto m5


esttab m m1 m2 m2x m3 m4 m5, se stats(widstat N, labels("First-Stage F-statistic" "N")) star(* 0.10 ** 0.05 *** 0.01) drop(*.gop_win_beg* *.census_division _cons) label interaction(" $\times$ ")style(tex)  mtitles(OLS 2SLS 2SLS 2SLS 2SLS 2SLS 2SLS)
esttab m m1 m2 m2x m3 m4 m5 using "../outputs/tables/house/nominate/nom_stacked.tex", replace se stats(widstat N, labels("First-Stage F-statistic" "N")) star(* 0.10 ** 0.05 *** 0.01) drop(*.gop_win_beg* *.census_division _cons) label interaction(" $\times$ ")style(tex) mtitles(OLS 2SLS 2SLS 2SLS 2SLS 2SLS)
esttab m m1 m2 m2x m3 m4 m5 using "../outputs/tables/house/nominate/nom_stackedp.tex", replace se stats(widstat N, labels("First-Stage F-statistic" "N")) star(* 0.10 ** 0.05 *** 0.01) drop(*.gop_win_beg* china_deltaip  $economic $demographics *.census_division _cons) addnotes(my note) label interaction(" $\times$ ")style(tex) nolines gaps nomtitles nonumbers nonotes nodepvar fragment


* Stacked Differences Sub

* OLS
reg delta_nom delta_exp_robs china_deltaip $political2 $demographics $economic i.census_division [aw = cd_prop_pop], cluster(tripple_cluster)
est sto mm
* IV Regressions
ivreg2 delta_nom (delta_exp_robs = eu_delta_exp_robs) i.census_division if t2==1 | t3==1 [aw = cd_prop_pop], cluster(tripple_cluster)
est sto mm1
ivreg2 delta_nom (delta_exp_robs = eu_delta_exp_robs) $political2 i.census_division  if t2==1 | t3==1 [aw = cd_prop_pop], cluster(tripple_cluster)
est sto mm2
ivreg2 delta_nom (delta_exp_robs = eu_delta_exp_robs) $political2 china_deltaip i.census_division  if t2==1 | t3==1 [aw = cd_prop_pop], cluster(tripple_cluster)
est sto mm2x
ivreg2 delta_nom (delta_exp_robs china_deltaip = eu_delta_exp_robs eu_mu_deltaip) $political2  i.census_division if t2==1 | t3==1 [aw = cd_prop_pop], cluster(tripple_cluster)
est sto mm3
ivreg2 delta_nom (delta_exp_robs china_deltaip = eu_delta_exp_robs eu_mu_deltaip) $political2  $demographics i.census_division  if t2==1 | t3==1 [aw = cd_prop_pop], cluster(tripple_cluster)
est sto mm4
ivreg2 delta_nom (delta_exp_robs china_deltaip = eu_delta_exp_robs eu_mu_deltaip) $political2  $demographics $economic i.census_division  if t2==1 | t3==1 [aw = cd_prop_pop], cluster(tripple_cluster)
est sto mm5


esttab mm mm1 mm2 mm2x mm3 mm4 mm5, se stats(widstat N, labels("First-Stage F-statistic" "N")) star(* 0.10 ** 0.05 *** 0.01) drop(*.gop_win_beg* *.census_division _cons) label interaction(" $\times$ ")style(tex)  mtitles(OLS 2SLS 2SLS 2SLS 2SLS 2SLS 2SLS)
esttab mm mm1 mm2 mm2x mm3 mm4 mm5 using "../outputs/tables/house/nominate/nom_stackedsub.tex", replace se stats(widstat N, labels("First-Stage F-statistic" "N")) star(* 0.10 ** 0.05 *** 0.01) drop(*.gop_win_beg* *.census_division _cons) label interaction(" $\times$ ")style(tex) mtitles(OLS 2SLS 2SLS 2SLS 2SLS 2SLS)
esttab mm mm1 mm2 mm2x mm3 mm4 mm5 using "../outputs/tables/house/nominate/nom_stackedsubp.tex", replace se stats(widstat N, labels("First-Stage F-statistic" "N")) star(* 0.10 ** 0.05 *** 0.01) drop(*.gop_win_beg* china_deltaip  $economic $demographics *.census_division _cons) addnotes(my note) label interaction(" $\times$ ")style(tex) nolines gaps nomtitles nonumbers nonotes nodepvar fragment


* Stacked Differences

* OLS
reg delta_abs_nom delta_exp_robs china_deltaip $political2 $demographics $economic i.census_division [aw = cd_prop_pop], cluster(tripple_cluster)
est sto m6
* IV Regressions
ivreg2 delta_abs_nom (delta_exp_robs = eu_delta_exp_robs) i.census_division [aw = cd_prop_pop], cluster(tripple_cluster)
est sto m7
ivreg2 delta_abs_nom (delta_exp_robs = eu_delta_exp_robs) $political2 i.census_division [aw = cd_prop_pop], cluster(tripple_cluster)
est sto m8
ivreg2 delta_abs_nom (delta_exp_robs = eu_delta_exp_robs) china_deltaip $political2 i.census_division [aw = cd_prop_pop], cluster(tripple_cluster)
est sto m8x
ivreg2 delta_abs_nom (delta_exp_robs china_deltaip = eu_delta_exp_robs eu_mu_deltaip) $political2  i.census_division [aw = cd_prop_pop], cluster(tripple_cluster)
est sto m9
ivreg2 delta_abs_nom (delta_exp_robs china_deltaip = eu_delta_exp_robs eu_mu_deltaip) $political2  $demographics i.census_division [aw = cd_prop_pop], cluster(tripple_cluster)
est sto m10
ivreg2 delta_abs_nom (delta_exp_robs china_deltaip = eu_delta_exp_robs eu_mu_deltaip) $political2  $demographics $economic i.census_division [aw = cd_prop_pop], cluster(tripple_cluster)
est sto m11


esttab m6 m7 m8 m8x m9 m10 m11, se stats(widstat N, labels("First-Stage F-statistic" "N")) star(* 0.10 ** 0.05 *** 0.01) drop(*.gop_win_beg* *.census_division _cons) label interaction(" $\times$ ")style(tex)  mtitles(OLS 2SLS 2SLS 2SLS 2SLS 2SLS 2SLS) 
esttab m6 m7 m8 m8x m9 m10 m11 using "../outputs/tables/house/nominate/anom_stacked.tex", replace se stats(widstat N, labels("First-Stage F-statistic" "N")) star(* 0.10 ** 0.05 *** 0.01) drop(*.gop_win_beg* *.census_division _cons) label interaction(" $\times$ ")style(tex) mtitles(OLS 2SLS 2SLS 2SLS 2SLS 2SLS 2SLS)
esttab m6 m7 m8 m8x m9 m10 m11 using "../outputs/tables/house/nominate/anom_stackedp.tex", replace se stats(widstat N, labels("First-Stage F-statistic" "N")) star(* 0.10 ** 0.05 *** 0.01) drop(*.gop_win_beg* china_deltaip  $economic $demographics *.census_division _cons) addnotes(my note) label interaction(" $\times$ ")style(tex) nolines gaps nomtitles nonumbers nonotes nodepvar fragment

* Stacked Differences Sub

* OLS
reg delta_abs_nom delta_exp_robs china_deltaip $political2 $demographics $economic i.census_division [aw = cd_prop_pop], cluster(tripple_cluster)
est sto mm6
* IV Regressions
ivreg2 delta_abs_nom (delta_exp_robs = eu_delta_exp_robs) i.census_division if t2==1 | t3==1 [aw = cd_prop_pop], cluster(tripple_cluster)
est sto mm7
ivreg2 delta_abs_nom (delta_exp_robs = eu_delta_exp_robs) $political2 i.census_division if t2==1 | t3==1 [aw = cd_prop_pop], cluster(tripple_cluster)
est sto mm8
ivreg2 delta_abs_nom (delta_exp_robs = eu_delta_exp_robs) china_deltaip $political2 i.census_division if t2==1 | t3==1 [aw = cd_prop_pop], cluster(tripple_cluster)
est sto mm8x
ivreg2 delta_abs_nom (delta_exp_robs china_deltaip = eu_delta_exp_robs eu_mu_deltaip) $political2  i.census_division if t2==1 | t3==1 [aw = cd_prop_pop], cluster(tripple_cluster)
est sto mm9
ivreg2 delta_abs_nom (delta_exp_robs china_deltaip = eu_delta_exp_robs eu_mu_deltaip) $political2  $demographics i.census_division if t2==1 | t3==1 [aw = cd_prop_pop], cluster(tripple_cluster)
est sto mm10
ivreg2 delta_abs_nom (delta_exp_robs china_deltaip = eu_delta_exp_robs eu_mu_deltaip) $political2  $demographics $economic i.census_division if t2==1 | t3==1 [aw = cd_prop_pop], cluster(tripple_cluster)
est sto mm11


esttab mm6 mm7 mm8 mm8x mm9 mm10 mm11, se stats(widstat N, labels("First-Stage F-statistic" "N")) star(* 0.10 ** 0.05 *** 0.01) drop(*.gop_win_beg* *.census_division _cons) label interaction(" $\times$ ")style(tex)  mtitles(OLS 2SLS 2SLS 2SLS 2SLS 2SLS)
esttab mm6 mm7 mm8 mm8x mm9 mm10 mm11 using "../outputs/tables/house/nominate/anom_stackedsub.tex", replace se stats(widstat N, labels("First-Stage F-statistic" "N")) star(* 0.10 ** 0.05 *** 0.01) drop(*.gop_win_beg* *.census_division _cons) label interaction(" $\times$ ")style(tex) mtitles(OLS 2SLS 2SLS 2SLS 2SLS 2SLS)
esttab mm6 mm7 mm8 mm8x mm9 mm10 mm11 using "../outputs/tables/house/nominate/anom_stackedsubp.tex", replace se stats(widstat N, labels("First-Stage F-statistic" "N")) star(* 0.10 ** 0.05 *** 0.01) drop(*.gop_win_beg* china_deltaip  $economic $demographics *.census_division _cons) addnotes(my note) label interaction(" $\times$ ")style(tex) nolines gaps nomtitles nonumbers nonotes nodepvar fragment


********************************************************************************

*** 1990's Geographic Integrity

* OLS
reg delta_nom delta_exp_robs china_deltaip $political2 $demographics $economic i.census_division if t1==1 [aw = cd_prop_pop], cluster(double_cluster)
est sto mm7
* IV Regressions
ivreg2 delta_nom (delta_exp_robs = eu_delta_exp_robs) i.census_division if t1==1 [aw = cd_prop_pop], cluster(double_cluster)
est sto mm8
ivreg2 delta_nom (delta_exp_robs = eu_delta_exp_robs) $political1 i.census_division if t1==1 [aw = cd_prop_pop], cluster(double_cluster)
est sto mm9
ivreg2 delta_nom (delta_exp_robs = eu_delta_exp_robs) $political1 china_deltaip i.census_division if t1==1 [aw = cd_prop_pop], cluster(double_cluster)
est sto mm9x
ivreg2 delta_nom (delta_exp_robs china_deltaip = eu_delta_exp_robs eu_mu_deltaip) $political1  i.census_division if t1==1 [aw = cd_prop_pop], cluster(double_cluster)
est sto mm10
ivreg2 delta_nom (delta_exp_robs china_deltaip = eu_delta_exp_robs eu_mu_deltaip) $political1  $demographics i.census_division if t1==1 [aw = cd_prop_pop], cluster(double_cluster)
est sto mm11
ivreg2 delta_nom (delta_exp_robs china_deltaip = eu_delta_exp_robs eu_mu_deltaip) $political1  $political2 $demographics $economic i.census_division if t1==1 [aw = cd_prop_pop], cluster(double_cluster)
est sto mm12


esttab mm7 mm8 mm9 mm9x mm10 mm11 mm12, se stats(widstat N, labels("First-Stage F-statistic" "N")) star(* 0.10 ** 0.05 *** 0.01) drop(*.gop_win_beg* *.census_division _cons) addnotes(my note) label interaction(" $\times$ ")style(tex) mtitles(OLS 2SLS 2SLS 2SLS 2SLS 2SLS 2SLS)
esttab mm7 mm8 mm9 mm9x mm10 mm11 mm12 using "../outputs/tables/house/nominate/nom_90.tex", replace se stats(widstat N, labels("First-Stage F-statistic" "N")) star(* 0.10 ** 0.05 *** 0.01) drop(*.gop_win_beg* *.census_division _cons) label interaction(" $\times$ ")style(tex) mtitles(OLS 2SLS 2SLS 2SLS 2SLS 2SLS 2SLS)
esttab mm7 mm8 mm9 mm9x mm10 mm11 mm12 using "../outputs/tables/house/nominate/nom_90p.tex", replace se stats(widstat N, labels("First-Stage F-statistic" "N")) star(* 0.10 ** 0.05 *** 0.01) drop(*.gop_win_beg* china_deltaip $economic $demographics *.census_division _cons) addnotes(my note) label interaction(" $\times$ ")style(tex) nolines gaps nomtitles nonumbers nonotes nodepvar fragment


* Quantile Regressions

*sqreg delta_nom delta_exp_robs $political $trade $economic $demographics if t1==1, q(.1 .25 .5 .75 .9)


** Change in Absolute Nominate Scores


* OLS
reg delta_abs_nom delta_exp_robs china_deltaip $political2 $demographics $economic i.census_division if t1==1 [aw = cd_prop_pop], cluster(double_cluster)
est sto m7
* IV Regressions
ivreg2 delta_abs_nom (delta_exp_robs = eu_delta_exp_robs) i.census_division if t1==1 [aw = cd_prop_pop], cluster(double_cluster)
est sto m8
ivreg2 delta_abs_nom (delta_exp_robs = eu_delta_exp_robs) $political1 i.census_division if t1==1 [aw = cd_prop_pop], cluster(double_cluster)
est sto m9
ivreg2 delta_abs_nom (delta_exp_robs = eu_delta_exp_robs) $political1 china_deltaip i.census_division if t1==1 [aw = cd_prop_pop], cluster(double_cluster)
est sto m9x
ivreg2 delta_abs_nom (delta_exp_robs china_deltaip = eu_delta_exp_robs eu_mu_deltaip) $political1  i.census_division if t1==1 [aw = cd_prop_pop], cluster(double_cluster)
est sto m10
ivreg2 delta_abs_nom (delta_exp_robs china_deltaip = eu_delta_exp_robs eu_mu_deltaip) $political1  $demographics i.census_division if t1==1 [aw = cd_prop_pop], cluster(double_cluster)
est sto m11
ivreg2 delta_abs_nom (delta_exp_robs china_deltaip = eu_delta_exp_robs eu_mu_deltaip) $political1  $political2 $demographics $economic i.census_division if t1==1 [aw = cd_prop_pop], cluster(double_cluster)
est sto m12


esttab m7 m8 m9 m9x m10 m11 m12, se stats(widstat N, labels("First-Stage F-statistic" "N")) star(* 0.10 ** 0.05 *** 0.01) drop(*.gop_win_beg* *.census_division _cons) addnotes(my note) label interaction(" $\times$ ")style(tex) mtitles(OLS 2SLS 2SLS 2SLS 2SLS 2SLS)
esttab m7 m8 m9 m9x m10 m11 m12 using "../outputs/tables/house/nominate/anom_90.tex", replace se stats(widstat N, labels("First-Stage F-statistic" "N")) star(* 0.10 ** 0.05 *** 0.01) drop(*.gop_win_beg* *.census_division _cons) label interaction(" $\times$ ")style(tex) mtitles(OLS 2SLS 2SLS 2SLS 2SLS 2SLS)
esttab m7 m8 m9 m9x m10 m11 m12 using "../outputs/tables/house/nominate/anom_90p.tex", replace se stats(widstat N, labels("First-Stage F-statistic" "N")) star(* 0.10 ** 0.05 *** 0.01) drop(*.gop_win_beg* china_deltaip $economic $demographics *.census_division _cons) addnotes(my note) label interaction(" $\times$ ")style(tex) nolines gaps nomtitles nonumbers nonotes nodepvar fragment


* Quantile Regressions

*sqreg delta_abs_nom delta_exp_robs $political $trade $economic $demographics if t1==1, q(.1 .25 .5 .75 .9)


********************************************************************************


*** 2000's Geographic Integrity ***


* OLS

reg delta_nom delta_exp_robs china_deltaip $political2 $demographics $economic i.census_division if t2 == 1 [aw = cd_prop_pop], cluster(double_cluster)
est sto m14
* IV Regressions
ivreg2 delta_nom (delta_exp_robs = eu_delta_exp_robs) i.census_division if t2==1 [aw = cd_prop_pop], cluster(double_cluster)
est sto m15
ivreg2 delta_nom (delta_exp_robs = eu_delta_exp_robs) $political1 i.census_division if t2==1 [aw = cd_prop_pop], cluster(double_cluster)
est sto m16
ivreg2 delta_nom (delta_exp_robs = eu_delta_exp_robs) $political1 china_deltaip i.census_division if t2==1 [aw = cd_prop_pop], cluster(double_cluster)
est sto m16x
ivreg2 delta_nom (delta_exp_robs china_deltaip = eu_delta_exp_robs eu_mu_deltaip) $political1  i.census_division if t2==1 [aw = cd_prop_pop], cluster(double_cluster)
est sto m17
ivreg2 delta_nom (delta_exp_robs china_deltaip = eu_delta_exp_robs eu_mu_deltaip) $political1  $demographics i.census_division if t2==1 [aw = cd_prop_pop], cluster(double_cluster)
est sto m18
ivreg2 delta_nom (delta_exp_robs china_deltaip = eu_delta_exp_robs eu_mu_deltaip) $political1  $demographics $economic i.census_division if t2==1 [aw = cd_prop_pop], cluster(double_cluster)
est sto m19


esttab m14 m15 m16 m16x m17 m18 m19, se stats(widstat N, labels("First-Stage F-statistic" "N")) star(* 0.10 ** 0.05 *** 0.01) drop(*.gop_win_beg* *.census_division _cons) label interaction(" $\times$ ")style(tex) mtitles(OLS 2SLS 2SLS 2SLS 2SLS 2SLS 2SLS)
esttab m14 m15 m16 m16x m17 m18 m19 using "../outputs/tables/house/nominate/nom_00.tex", replace se stats(widstat N, labels("First-Stage F-statistic" "N")) star(* 0.10 ** 0.05 *** 0.01) drop(*.gop_win_beg* *.census_division _cons) label interaction(" $\times$ ")style(tex) mtitles(OLS 2SLS 2SLS 2SLS 2SLS 2SLS 2SLS)
esttab m14 m15 m16 m16x m17 m18 m19 using "../outputs/tables/house/nominate/nom_00p.tex", replace se stats(widstat N, labels("First-Stage F-statistic" "N")) star(* 0.10 ** 0.05 *** 0.01) drop(*.gop_win_beg* china_deltaip $economic $demographics *.census_division _cons) addnotes(my note) label interaction(" $\times$ ")style(tex) nolines gaps nomtitles nonumbers nonotes nodepvar fragment


* Quantile Regressions

*sqreg delta_nom delta_exp_robs $political $trade $economic $demographics if t2==1, q(.1 .25 .5 .75 .9)


** Change in Absolute Nominate Scores


* OLS
reg delta_abs_nom delta_exp_robs china_deltaip $political2 $demographics $economic i.census_division if t2==1 [aw = cd_prop_pop], cluster(double_cluster)
est sto m21
* IV Regressions
ivreg2 delta_abs_nom (delta_exp_robs = eu_delta_exp_robs) i.census_division if t2==1 [aw = cd_prop_pop], cluster(double_cluster)
est sto m22
ivreg2 delta_abs_nom (delta_exp_robs = eu_delta_exp_robs) $political1 i.census_division if t2==1 [aw = cd_prop_pop], cluster(double_cluster)
est sto m23
ivreg2 delta_abs_nom (delta_exp_robs = eu_delta_exp_robs) $political1 china_deltaip i.census_division if t2==1 [aw = cd_prop_pop], cluster(double_cluster)
est sto m23x
ivreg2 delta_abs_nom (delta_exp_robs china_deltaip = eu_delta_exp_robs eu_mu_deltaip) $political1  i.census_division if t2==1 [aw = cd_prop_pop], cluster(double_cluster)
est sto m24
ivreg2 delta_abs_nom (delta_exp_robs china_deltaip = eu_delta_exp_robs eu_mu_deltaip) $political1  $demographics i.census_division if t2==1 [aw = cd_prop_pop], cluster(double_cluster)
est sto m25
ivreg2 delta_abs_nom (delta_exp_robs china_deltaip = eu_delta_exp_robs eu_mu_deltaip) $political1  $demographics $economic  i.census_division if t2==1 [aw = cd_prop_pop], cluster(double_cluster)
est sto m26


esttab m21 m22 m23 m23x m24 m25 m26, se stats(widstat N, labels("First-Stage F-statistic" "N")) star(* 0.10 ** 0.05 *** 0.01) drop(*.gop_win_beg* *.census_division _cons) label interaction(" $\times$ ")style(tex) mtitles(OLS 2SLS 2SLS 2SLS 2SLS 2SLS 2SLS)
esttab m21 m22 m23 m23x m24 m25 m26 using "../outputs/tables/house/nominate/anom_00.tex", replace se stats(widstat N, labels("First-Stage F-statistic" "N")) star(* 0.10 ** 0.05 *** 0.01) drop(*.gop_win_beg* *.census_division _cons) label interaction(" $\times$ ")style(tex) mtitles(OLS 2SLS 2SLS 2SLS 2SLS 2SLS 2SLS)
esttab m21 m22 m23 m23x m24 m25 m26 using "../outputs/tables/house/nominate/anom_00p.tex", replace se stats(widstat N, labels("First-Stage F-statistic" "N")) star(* 0.10 ** 0.05 *** 0.01) drop(*.gop_win_beg* china_deltaip $economic $demographics *.census_division _cons) addnotes(my note) label interaction(" $\times$ ")style(tex) nolines gaps nomtitles nonumbers nonotes nodepvar fragment



* Quantile Regressions

sqreg delta_abs_nom delta_exp_robs $political1  $demographics $economic  i.census_division  if t2==1, q(.1 .25 .5 .75 .9)





********************************************************************************

*** 2010's Geographic Integrity



* OLS
reg delta_nom delta_exp_robs china_deltaip $political2 $demographics $economic i.census_division if t3 == 1 [aw = cd_prop_pop], cluster(double_cluster)
est sto m28
* IV Regressions
ivreg2 delta_nom (delta_exp_robs = eu_delta_exp_robs) i.census_division if t3==1 [aw = cd_prop_pop], cluster(double_cluster)
est sto m29
ivreg2 delta_nom (delta_exp_robs = eu_delta_exp_robs) $political1 i.census_division if t3==1 [aw = cd_prop_pop], cluster(double_cluster)
est sto m30
ivreg2 delta_nom (delta_exp_robs = eu_delta_exp_robs) $political1 china_deltaip i.census_division if t3==1 [aw = cd_prop_pop], cluster(double_cluster)
est sto m30x
ivreg2 delta_nom (delta_exp_robs china_deltaip = eu_delta_exp_robs eu_mu_deltaip) $political1  i.census_division if t3==1 [aw = cd_prop_pop], cluster(double_cluster)
est sto m31
ivreg2 delta_nom (delta_exp_robs china_deltaip = eu_delta_exp_robs eu_mu_deltaip) $political1  $demographics i.census_division  if t3==1 [aw = cd_prop_pop], cluster(double_cluster)
est sto m32
ivreg2 delta_nom (delta_exp_robs china_deltaip = eu_delta_exp_robs eu_mu_deltaip) $political1  $demographics $economic i.census_division if t3==1 [aw = cd_prop_pop], cluster(double_cluster)
est sto m33


esttab m28 m29 m30 m30x m31 m32 m33, se stats(widstat N, labels("First-Stage F-statistic" "N")) star(* 0.10 ** 0.05 *** 0.01) drop(*.gop_win_beg* *.census_division _cons) label interaction(" $\times$ ")style(tex) mtitles(OLS 2SLS 2SLS 2SLS 2SLS 2SLS 2SLS)
esttab m28 m29 m30 m30x m31 m32 m33 using "../outputs/tables/house/nominate/nom_10.tex", replace se stats(widstat N, labels("First-Stage F-statistic" "N")) star(* 0.10 ** 0.05 *** 0.01) drop(*.gop_win_beg* *.census_division _cons) label interaction(" $\times$ ")style(tex) mtitles(OLS 2SLS 2SLS 2SLS 2SLS 2SLS 2SLS)
esttab m28 m29 m30 m30x m31 m32 m33 using "../outputs/tables/house/nominate/nom_10p.tex", replace se stats(widstat N, labels("First-Stage F-statistic" "N")) star(* 0.10 ** 0.05 *** 0.01) drop(*.gop_win_beg* china_deltaip $economic $demographics *.census_division _cons) addnotes(my note) label interaction(" $\times$ ")style(tex) nolines gaps nomtitles nonumbers nonotes nodepvar fragment


* Quantile Regressions

*sqreg delta_nom delta_exp_robs $political $trade $economic $demographics if t3==1, q(.1 .25 .5 .75 .9)


** Change in Absolute Nominate Scores


* OLS
reg delta_abs_nom delta_exp_robs china_deltaip $political2 $demographics $economic i.census_division if t3==1 [aw = cd_prop_pop], cluster(double_cluster)
est sto m35
* IV Regressions
ivreg2 delta_abs_nom (delta_exp_robs = eu_delta_exp_robs) i.census_division if t3==1 [aw = cd_prop_pop], cluster(double_cluster)
est sto m36
ivreg2 delta_abs_nom (delta_exp_robs = eu_delta_exp_robs) $political1 i.census_division if t3==1 [aw = cd_prop_pop], cluster(double_cluster)
est sto m37
ivreg2 delta_abs_nom (delta_exp_robs = eu_delta_exp_robs) $political1 china_deltaip i.census_division if t3==1 [aw = cd_prop_pop], cluster(double_cluster)
est sto m37x
ivreg2 delta_abs_nom (delta_exp_robs china_deltaip = eu_delta_exp_robs eu_mu_deltaip) $political1  i.census_division if t3==1 [aw = cd_prop_pop], cluster(double_cluster)
est sto m38
ivreg2 delta_abs_nom (delta_exp_robs china_deltaip = eu_delta_exp_robs eu_mu_deltaip) $political1  $demographics i.census_division if t3==1 [aw = cd_prop_pop], cluster(double_cluster)
est sto m39
ivreg2 delta_abs_nom (delta_exp_robs china_deltaip = eu_delta_exp_robs eu_mu_deltaip) $political1  $demographics $economic i.census_division if t3==1 [aw = cd_prop_pop], cluster(double_cluster)
est sto m40


esttab m35 m36 m37 m37x m38 m39 m40, se stats(widstat N, labels("First-Stage F-statistic" "N")) star(* 0.10 ** 0.05 *** 0.01) drop(*.gop_win_beg* *.census_division _cons) label interaction(" $\times$ ")style(tex) mtitles(OLS 2SLS 2SLS 2SLS 2SLS 2SLS 2SLS)
esttab m35 m36 m37 m37x m38 m39 m40 using "../outputs/tables/house/nominate/anom_10.tex", replace se stats(widstat N, labels("First-Stage F-statistic" "N")) star(* 0.10 ** 0.05 *** 0.01) drop(*.gop_win_beg* *.census_division _cons) label interaction(" $\times$ ")style(tex) mtitles(OLS 2SLS 2SLS 2SLS 2SLS 2SLS 2SLS)
esttab m35 m36 m37 m37x m38 m39 m40 using "../outputs/tables/house/nominate/anom_10p.tex", replace se stats(widstat N, labels("First-Stage F-statistic" "N")) star(* 0.10 ** 0.05 *** 0.01) drop(*.gop_win_beg* china_deltaip $economic $demographics *.census_division _cons) addnotes(my note) label interaction(" $\times$ ")style(tex) nolines gaps nomtitles nonumbers nonotes nodepvar fragment


* Quantile Regressions

*sqreg delta_abs_nom delta_exp_robs $political $trade $economic $demographics if t3==1 [aw = cd_prop_pop], q(.1 .25 .5 .75 .9)





* Stacked Differences, Full

esttab m m1 m2 m2x m3 m4 m5 m6 m7 m8 m8x m9 m10 m11 using "../outputs/tables/house/nominate/nom-anom_s.tex", replace se stats(widstat N, labels("First-Stage F-statistic" "N")) star(* 0.10 ** 0.05 *** 0.01) drop(*.gop_win_beg* china_deltaip  $economic $demographics *.census_division _cons) addnotes(my note) label interaction(" $\times$ ")style(tex) extracols(8) nolines gaps nomtitles nonumbers nonotes nodepvar fragment

* 1990's, Full

esttab mm7 mm8 mm9 mm9x mm10 mm11 mm12 m7 m8 m9 m9x m10 m11 m12 using "../outputs/tables/house/nominate/nom-anom_90.tex", replace se stats(widstat N, labels("First-Stage F-statistic" "N")) star(* 0.10 ** 0.05 *** 0.01) drop(*.gop_win_beg* china_deltaip $economic $demographics *.census_division _cons) addnotes(my note) label interaction(" $\times$ ")style(tex) extracols(8) nolines gaps nomtitles nonumbers nonotes nodepvar fragment

* 2000's, Full

esttab m14 m15 m16 m16x m17 m18 m19 m21 m22 m23 m23x m24 m25 m26 using "../outputs/tables/house/nominate/nom-anom_00.tex", replace se stats(widstat N, labels("First-Stage F-statistic" "N")) star(* 0.10 ** 0.05 *** 0.01) drop(*.gop_win_beg* china_deltaip $economic $demographics *.census_division _cons) addnotes(my note) label interaction(" $\times$ ")style(tex) extracols(8) nolines gaps nomtitles nonumbers nonotes nodepvar fragment

* 2010's, Full

esttab m28 m29 m30 m30x m31 m32 m33 m35 m36 m37 m37x m38 m39 m40 using "../outputs/tables/house/nominate/nom-anom_10.tex", replace se stats(widstat N, labels("First-Stage F-statistic" "N")) star(* 0.10 ** 0.05 *** 0.01) drop(*.gop_win_beg* china_deltaip $economic $demographics *.census_division _cons) addnotes(my note) label interaction(" $\times$ ")style(tex) extracols(8) nolines gaps nomtitles nonumbers nonotes nodepvar fragment




********************************************************************************
************************** CHANGE IN WIN VOTE SHARES ***************************
********************************************************************************


* Stacked Difference 2001-2010, 2011-2016

* OLS
reg win_prty_delta_sh delta_exp_robs china_deltaip $political2 $demographics $economic i.census_division [aw = cd_prop_pop] , cluster(tripple_cluster)
est sto w
* IV Regressions
ivreg2 win_prty_delta_sh (delta_exp_robs = eu_delta_exp_robs) i.census_division if t2==1 | t3==1 [aw = cd_prop_pop], cluster(tripple_cluster)
est sto w1
ivreg2 win_prty_delta_sh (delta_exp_robs = eu_delta_exp_robs) $political1 i.census_division if t2==1 | t3==1 [aw = cd_prop_pop], cluster(tripple_cluster)
est sto w2
ivreg2 win_prty_delta_sh (delta_exp_robs = eu_delta_exp_robs) $political1 china_deltaip i.census_division if t2==1 | t3==1 [aw = cd_prop_pop], cluster(tripple_cluster)
est sto w2x
ivreg2 win_prty_delta_sh (delta_exp_robs china_deltaip = eu_delta_exp_robs eu_mu_deltaip) $political1  i.census_division if t2==1 | t3==1 [aw = cd_prop_pop], cluster(tripple_cluster)
est sto w3
ivreg2 win_prty_delta_sh (delta_exp_robs china_deltaip = eu_delta_exp_robs eu_mu_deltaip) $political1  $demographics i.census_division if t2==1 | t3==1 [aw = cd_prop_pop], cluster(tripple_cluster)
est sto w4
ivreg2 win_prty_delta_sh (delta_exp_robs china_deltaip = eu_delta_exp_robs eu_mu_deltaip) $political1  $demographics $economic i.census_division if t2==1 | t3==1 [aw = cd_prop_pop], cluster(tripple_cluster)
est sto w5


esttab w w1 w2 w2x w3 w4 w5, se stats(widstat N, labels("First-Stage F-statistic" "N")) star(* 0.10 ** 0.05 *** 0.01) drop(*.gop_win_beg* *.census_division _cons) label interaction(" $\times$ ")style(tex) mtitles(OLS 2SLS 2SLS 2SLS 2SLS 2SLS 2SLS)
esttab w w1 w2 w2x w3 w4 w5 using "../outputs/tables/house/vote_shares/win_vs_stacked.tex", replace se stats(widstat N, labels("First-Stage F-statistic" "N")) star(* 0.10 ** 0.05 *** 0.01) drop(*.gop_win_beg* *.census_division _cons) label interaction(" $\times$ ")style(tex) mtitles(OLS 2SLS 2SLS 2SLS 2SLS 2SLS 2SLS)
esttab w w1 w2 w2x w3 w4 w5 using "../outputs/tables/house/vote_shares/win_vs_stackedp.tex", replace se stats(widstat N, labels("First-Stage F-statistic" "N")) star(* 0.10 ** 0.05 *** 0.01) drop(*.gop_win_beg* china_deltaip $economic $demographics *.census_division _cons) label interaction(" $\times$ ")style(tex) nolines gaps nomtitles nonumbers nonotes nodepvar fragment


* 1990's

* OLS
reg win_prty_delta_sh delta_exp_robs china_deltaip $political1 $demographics $economic i.census_division if t1==1 [aw = cd_prop_pop], cluster(double_cluster)
est sto w7
* IV Regressions
ivreg2 win_prty_delta_sh (delta_exp_robs = eu_delta_exp_robs) i.census_division if t1==1 [aw = cd_prop_pop], cluster(double_cluster)
est sto w8
ivreg2 win_prty_delta_sh (delta_exp_robs = eu_delta_exp_robs) $political1 i.census_division if t1==1 [aw = cd_prop_pop], cluster(double_cluster)
est sto w9
ivreg2 win_prty_delta_sh (delta_exp_robs = eu_delta_exp_robs) $political1 china_deltaip i.census_division if t1==1 [aw = cd_prop_pop], cluster(double_cluster)
est sto w9x
ivreg2 win_prty_delta_sh (delta_exp_robs china_deltaip = eu_delta_exp_robs eu_mu_deltaip) $political1  $political1 i.census_division if t1==1 [aw = cd_prop_pop], cluster(double_cluster)
est sto w10
ivreg2 win_prty_delta_sh (delta_exp_robs china_deltaip = eu_delta_exp_robs eu_mu_deltaip) $political1  $political1 $demographics i.census_division if t1==1 [aw = cd_prop_pop], cluster(double_cluster)
est sto w11
ivreg2 win_prty_delta_sh (delta_exp_robs china_deltaip = eu_delta_exp_robs eu_mu_deltaip) $political1  $political1 $demographics $economic i.census_division if t1==1 [aw = cd_prop_pop], cluster(double_cluster)
est sto w12


esttab w7 w8 w9 w9x w10 w11 w12, se stats(widstat N, labels("First-Stage F-statistic" "N")) star(* 0.10 ** 0.05 *** 0.01) drop(*.gop_win_beg* *.census_division  _cons) label interaction(" $\times$ ")style(tex) mtitles(OLS 2SLS 2SLS 2SLS 2SLS 2SLS 2SLS)
esttab w7 w8 w9 w9x w10 w11 w12 using "../outputs/tables/house/vote_shares/win_vs_90.tex", replace se stats(widstat N, labels("First-Stage F-statistic" "N")) star(* 0.10 ** 0.05 *** 0.01) drop(*.gop_win_beg* *.census_division _cons) label interaction(" $\times$ ")style(tex) mtitles(OLS 2SLS 2SLS 2SLS 2SLS 2SLS 2SLS)
esttab w7 w8 w9 w9x w10 w11 w12 using "../outputs/tables/house/vote_shares/win_vs_90p.tex", replace se stats(widstat N, labels("First-Stage F-statistic" "N")) star(* 0.10 ** 0.05 *** 0.01) drop(*.gop_win_beg* china_deltaip $demographics $economic *.census_division _cons) label interaction(" $\times$ ")style(tex) nolines gaps nomtitles nonumbers nonotes nodepvar fragment





* 2000's

* OLS
reg win_prty_delta_sh delta_exp_robs china_deltaip $political2 $demographics $economic i.census_division if t2==1 [aw = cd_prop_pop], cluster(double_cluster)
est sto w14
* IV Regressions
ivreg2 win_prty_delta_sh (delta_exp_robs = eu_delta_exp_robs) i.census_division if t2==1 [aw = cd_prop_pop], cluster(double_cluster)
est sto w15
ivreg2 win_prty_delta_sh (delta_exp_robs = eu_delta_exp_robs) $political1 i.census_division if t2==1 [aw = cd_prop_pop], cluster(double_cluster)
est sto w16
ivreg2 win_prty_delta_sh (delta_exp_robs = eu_delta_exp_robs) $political1 china_deltaip i.census_division if t2==1 [aw = cd_prop_pop], cluster(double_cluster)
est sto w16x
ivreg2 win_prty_delta_sh (delta_exp_robs china_deltaip = eu_delta_exp_robs eu_mu_deltaip) $political1  $political1 i.census_division if t2==1 [aw = cd_prop_pop], cluster(double_cluster)
est sto w17
ivreg2 win_prty_delta_sh (delta_exp_robs china_deltaip = eu_delta_exp_robs eu_mu_deltaip) $political1  $political1 $demographics i.census_division if t2==1 [aw = cd_prop_pop], cluster(double_cluster)
est sto w18
ivreg2 win_prty_delta_sh (delta_exp_robs china_deltaip = eu_delta_exp_robs eu_mu_deltaip) $political1  $political1 $demographics $economic i.census_division if t2==1 [aw = cd_prop_pop], cluster(double_cluster)
est sto w19


esttab w14 w15 w16 w16x w17 w18 w19, se stats(widstat N, labels("First-Stage F-statistic" "N")) star(* 0.10 ** 0.05 *** 0.01) drop(*.gop_win_beg* *.census_division _cons) label interaction(" $\times$ ")style(tex) mtitles(OLS 2SLS 2SLS 2SLS 2SLS 2SLS)
esttab w14 w15 w16 w16x w17 w18 w19 using "../outputs/tables/house/vote_shares/win_vs_00.tex", replace se stats(widstat N, labels("First-Stage F-statistic" "N")) star(* 0.10 ** 0.05 *** 0.01) drop(*.gop_win_beg* *.census_division _cons) label interaction(" $\times$ ")style(tex) mtitles(OLS 2SLS 2SLS 2SLS 2SLS 2SLS)
esttab w14 w15 w16 w16x w17 w18 w19 using "../outputs/tables/house/vote_shares/win_vs_00p.tex", replace se stats(widstat N, labels("First-Stage F-statistic" "N")) star(* 0.10 ** 0.05 *** 0.01) drop(*.gop_win_beg* china_deltaip $economic $demographics *.census_division _cons) label interaction(" $\times$ ")style(tex) nolines gaps nomtitles nonumbers nonotes nodepvar fragment




* 2010's

* OLS
reg win_prty_delta_sh delta_exp_robs china_deltaip $political2 $demographics $economic i.census_division if t3==1 [aw = cd_prop_pop], cluster(double_cluster)
est sto w21
* IV Regressions
ivreg2 win_prty_delta_sh (delta_exp_robs = eu_delta_exp_robs) i.census_division if t3==1 [aw = cd_prop_pop], cluster(double_cluster)
est sto w22
ivreg2 win_prty_delta_sh (delta_exp_robs = eu_delta_exp_robs) $political1 i.census_division if t3==1 [aw = cd_prop_pop], cluster(double_cluster)
est sto w23
ivreg2 win_prty_delta_sh (delta_exp_robs = eu_delta_exp_robs) $political1 china_deltaip i.census_division if t3==1 [aw = cd_prop_pop], cluster(double_cluster)
est sto w23x
ivreg2 win_prty_delta_sh (delta_exp_robs china_deltaip = eu_delta_exp_robs eu_mu_deltaip) $political1  i.census_division if t3==1 [aw = cd_prop_pop], cluster(double_cluster)
est sto w24
ivreg2 win_prty_delta_sh (delta_exp_robs china_deltaip = eu_delta_exp_robs eu_mu_deltaip) $political1  $demographics i.census_division if t3==1 [aw = cd_prop_pop], cluster(double_cluster)
est sto w25
ivreg2 win_prty_delta_sh (delta_exp_robs china_deltaip = eu_delta_exp_robs eu_mu_deltaip) $political1  $demographics $economic i.census_division if t3==1 [aw = cd_prop_pop], cluster(double_cluster)
est sto w26


esttab w21 w22 w23 w23x w24 w25 w26, se stats(widstat N, labels("First-Stage F-statistic" "N")) star(* 0.10 ** 0.05 *** 0.01) drop(*.gop_win_beg* *.census_division _cons) label interaction(" $\times$ ")style(tex) mtitles(OLS 2SLS 2SLS 2SLS 2SLS 2SLS)
esttab w21 w22 w23 w23x w24 w25 w26 using "../outputs/tables/house/vote_shares/win_vs_10.tex", replace se stats(widstat N, labels("First-Stage F-statistic" "N")) star(* 0.10 ** 0.05 *** 0.01) drop(*.gop_win_beg* *.census_division _cons) label interaction(" $\times$ ")style(tex) mtitles(OLS 2SLS 2SLS 2SLS 2SLS 2SLS)
esttab w21 w22 w23 w23x w24 w25 w26 using "../outputs/tables/house/vote_shares/win_vs_10p.tex", replace se stats(widstat N, labels("First-Stage F-statistic" "N")) star(* 0.10 ** 0.05 *** 0.01) drop(*.gop_win_beg* china_deltaip $demographics $economic *.census_division _cons) label interaction(" $\times$ ")style(tex) nolines gaps nomtitles nonumbers nonotes nodepvar fragment









********************************************************************************
************************* CHANGE IN PARTY VOTE SHARES **************************
********************************************************************************

** Republican **


* Stacked Difference

* OLS
reg gop_delta_sh delta_exp_robs china_deltaip $political2 $demographics $economic i.census_division [aw = cd_prop_pop], cluster(tripple_cluster)
est sto r
* IV Regressions
ivreg2 gop_delta_sh (delta_exp_robs = eu_delta_exp_robs) i.census_division  [aw = cd_prop_pop], cluster(tripple_cluster)
est sto r1
ivreg2 gop_delta_sh (delta_exp_robs = eu_delta_exp_robs) $political1 i.census_division  [aw = cd_prop_pop], cluster(tripple_cluster)
est sto r2
ivreg2 gop_delta_sh (delta_exp_robs = eu_delta_exp_robs) $political1 china_deltaip i.census_division  [aw = cd_prop_pop], cluster(tripple_cluster)
est sto r2x
ivreg2 gop_delta_sh (delta_exp_robs china_deltaip = eu_delta_exp_robs eu_mu_deltaip) $political1  i.census_division  [aw = cd_prop_pop], cluster(tripple_cluster)
est sto r3
ivreg2 gop_delta_sh (delta_exp_robs china_deltaip = eu_delta_exp_robs eu_mu_deltaip) $political1  $demographics i.census_division  [aw = cd_prop_pop], cluster(tripple_cluster)
est sto r4
ivreg2 gop_delta_sh (delta_exp_robs china_deltaip = eu_delta_exp_robs eu_mu_deltaip) $political1  $demographics $economic i.census_division  [aw = cd_prop_pop], cluster(tripple_cluster)
est sto r5


esttab r r1 r2 r2x r3 r4 r5, se stats(widstat N, labels("First-Stage F-statistic" "N")) star(* 0.10 ** 0.05 *** 0.01) drop(*.gop_win_beg* *.census_division  _cons) label interaction(" $\times$ ")style(tex) mtitles(OLS 2SLS 2SLS 2SLS 2SLS 2SLS)
esttab r r1 r2 r2x r3 r4 r5 using "../outputs/tables/house/vote_shares/gop_vs_stacked.tex", replace se stats(widstat N, labels("First-Stage F-statistic" "N")) star(* 0.10 ** 0.05 *** 0.01) drop(*.gop_win_beg* *.census_division _cons) label interaction(" $\times$ ")style(tex) mtitles(OLS 2SLS 2SLS 2SLS 2SLS 2SLS)
esttab r r1 r2 r2x r3 r4 r5 using "../outputs/tables/house/vote_shares/gop_vs_stackedp.tex", replace se stats(widstat N, labels("First-Stage F-statistic" "N")) star(* 0.10 ** 0.05 *** 0.01) drop(*.gop_win_beg* china_deltaip $demographics $economic *.census_division  _cons) addnotes(my note) label interaction(" $\times$ ")style(tex) nolines gaps nomtitles nonumbers nonotes nodepvar fragment


* Stacked Difference Sub

* OLS
reg gop_delta_sh delta_exp_robs china_deltaip $political2 $demographics $economic i.census_division if t2==1 | t3==1 [aw = cd_prop_pop], cluster(tripple_cluster)
est sto rr
* IV Regressions
ivreg2 gop_delta_sh (delta_exp_robs = eu_delta_exp_robs) i.census_division if t2==1 | t3==1 [aw = cd_prop_pop], cluster(tripple_cluster)
est sto rr1
ivreg2 gop_delta_sh (delta_exp_robs = eu_delta_exp_robs) $political1 i.census_division if t2==1 | t3==1 [aw = cd_prop_pop], cluster(tripple_cluster)
est sto rr2
ivreg2 gop_delta_sh (delta_exp_robs = eu_delta_exp_robs) $political1 china_deltaip i.census_division if t2==1 | t3==1 [aw = cd_prop_pop], cluster(tripple_cluster)
est sto rr2x
ivreg2 gop_delta_sh (delta_exp_robs china_deltaip = eu_delta_exp_robs eu_mu_deltaip) $political1  i.census_division if t2==1 | t3==1 [aw = cd_prop_pop], cluster(tripple_cluster)
est sto rr3
ivreg2 gop_delta_sh (delta_exp_robs china_deltaip = eu_delta_exp_robs eu_mu_deltaip) $political1  $demographics i.census_division if t2==1 | t3==1 [aw = cd_prop_pop], cluster(tripple_cluster)
est sto rr4
ivreg2 gop_delta_sh (delta_exp_robs china_deltaip = eu_delta_exp_robs eu_mu_deltaip) $political1  $demographics $economic i.census_division if t2==1 | t3==1 [aw = cd_prop_pop], cluster(tripple_cluster)
est sto rr5


esttab rr rr1 rr2 rr2x rr3 rr4 rr5, se stats(widstat N, labels("First-Stage F-statistic" "N")) star(* 0.10 ** 0.05 *** 0.01) drop(*.gop_win_beg* *.census_division  _cons) label interaction(" $\times$ ")style(tex) mtitles(OLS 2SLS 2SLS 2SLS 2SLS 2SLS)
esttab rr rr1 rr2 rr2x rr3 rr4 rr5 using "../outputs/tables/house/vote_shares/gop_vs_stackedsub.tex", replace se stats(widstat N, labels("First-Stage F-statistic" "N")) star(* 0.10 ** 0.05 *** 0.01) drop(*.gop_win_beg* *.census_division _cons) label interaction(" $\times$ ")style(tex) mtitles(OLS 2SLS 2SLS 2SLS 2SLS 2SLS)
esttab rr rr1 rr2 rr2x rr3 rr4 rr5 using "../outputs/tables/house/vote_shares/gop_vs_stackedsubp.tex", replace se stats(widstat N, labels("First-Stage F-statistic" "N")) star(* 0.10 ** 0.05 *** 0.01) drop(*.gop_win_beg* china_deltaip $demographics $economic *.census_division  _cons) addnotes(my note) label interaction(" $\times$ ")style(tex) nolines gaps nomtitles nonumbers nonotes nodepvar fragment



* 1990

* OLS
reg gop_delta_sh delta_exp_robs china_deltaip $political2 $demographics $economic i.census_division if t1==1 [aw = cd_prop_pop], cluster(tripple_cluster)
est sto rr6
* IV Regressions
ivreg2 gop_delta_sh (delta_exp_robs = eu_delta_exp_robs) i.census_division if t1==1  [aw = cd_prop_pop], cluster(tripple_cluster)
est sto rr7
ivreg2 gop_delta_sh (delta_exp_robs = eu_delta_exp_robs) $political1 i.census_division  if t1==1 [aw = cd_prop_pop], cluster(tripple_cluster)
est sto rr8
ivreg2 gop_delta_sh (delta_exp_robs = eu_delta_exp_robs) $political1 china_deltaip i.census_division  if t1==1 [aw = cd_prop_pop], cluster(tripple_cluster)
est sto rr8x
ivreg2 gop_delta_sh (delta_exp_robs china_deltaip = eu_delta_exp_robs eu_mu_deltaip) $political1  i.census_division if t1==1  [aw = cd_prop_pop], cluster(tripple_cluster)
est sto rr9
ivreg2 gop_delta_sh (delta_exp_robs china_deltaip = eu_delta_exp_robs eu_mu_deltaip) $political1  $demographics i.census_division if t1==1  [aw = cd_prop_pop], cluster(tripple_cluster)
est sto rr10
ivreg2 gop_delta_sh (delta_exp_robs china_deltaip = eu_delta_exp_robs eu_mu_deltaip) $political1  $demographics $economic i.census_division if t1==1  [aw = cd_prop_pop], cluster(tripple_cluster)
est sto rr11


esttab rr6 rr7 rr8 rr8x rr9 rr10 rr11, se stats(widstat N, labels("First-Stage F-statistic" "N")) star(* 0.10 ** 0.05 *** 0.01) drop(*.gop_win_beg* *.census_division  _cons) label interaction(" $\times$ ")style(tex) mtitles(OLS 2SLS 2SLS 2SLS 2SLS 2SLS)
esttab rr6 rr7 rr8 rr8x rr9 rr10 rr11 using "../outputs/tables/house/vote_shares/gop_vs_90.tex", replace se stats(widstat N, labels("First-Stage F-statistic" "N")) star(* 0.10 ** 0.05 *** 0.01) drop(*.gop_win_beg* *.census_division _cons) label interaction(" $\times$ ")style(tex) mtitles(OLS 2SLS 2SLS 2SLS 2SLS 2SLS)
esttab rr6 rr7 rr8 rr8x rr9 rr10 rr11 using "../outputs/tables/house/vote_shares/gop_vs_90p.tex", replace se stats(widstat N, labels("First-Stage F-statistic" "N")) star(* 0.10 ** 0.05 *** 0.01) drop(*.gop_win_beg* china_deltaip $demographics $economic *.census_division  _cons) addnotes(my note) label interaction(" $\times$ ")style(tex) nolines gaps nomtitles nonumbers nonotes nodepvar fragment


* 2000

* OLS
reg gop_delta_sh delta_exp_robs china_deltaip $political2 $demographics $economic i.census_division [aw = cd_prop_pop], cluster(tripple_cluster)
est sto r6
* IV Regressions
ivreg2 gop_delta_sh (delta_exp_robs = eu_delta_exp_robs) i.census_division if t2==1  [aw = cd_prop_pop], cluster(tripple_cluster)
est sto r7
ivreg2 gop_delta_sh (delta_exp_robs = eu_delta_exp_robs) $political1 i.census_division  if t2==1 [aw = cd_prop_pop], cluster(tripple_cluster)
est sto r8
ivreg2 gop_delta_sh (delta_exp_robs = eu_delta_exp_robs) $political1 china_deltaip i.census_division  if t2==1 [aw = cd_prop_pop], cluster(tripple_cluster)
est sto r8x
ivreg2 gop_delta_sh (delta_exp_robs china_deltaip = eu_delta_exp_robs eu_mu_deltaip) $political1  i.census_division if t2==1  [aw = cd_prop_pop], cluster(tripple_cluster)
est sto r9
ivreg2 gop_delta_sh (delta_exp_robs china_deltaip = eu_delta_exp_robs eu_mu_deltaip) $political1  $demographics i.census_division if t2==1  [aw = cd_prop_pop], cluster(tripple_cluster)
est sto r10
ivreg2 gop_delta_sh (delta_exp_robs china_deltaip = eu_delta_exp_robs eu_mu_deltaip) $political1  $demographics $economic i.census_division if t2==1  [aw = cd_prop_pop], cluster(tripple_cluster)
est sto r11


esttab r6 r7 r8 r8x r9 r10 r11, se stats(widstat N, labels("First-Stage F-statistic" "N")) star(* 0.10 ** 0.05 *** 0.01) drop(*.gop_win_beg* *.census_division  _cons) label interaction(" $\times$ ")style(tex) mtitles(OLS 2SLS 2SLS 2SLS 2SLS 2SLS)
esttab r6 r7 r8 r8x r9 r10 r11 using "../outputs/tables/house/vote_shares/gop_vs_00.tex", replace se stats(widstat N, labels("First-Stage F-statistic" "N")) star(* 0.10 ** 0.05 *** 0.01) drop(*.gop_win_beg* *.census_division _cons) label interaction(" $\times$ ")style(tex) mtitles(OLS 2SLS 2SLS 2SLS 2SLS 2SLS)
esttab r6 r7 r8 r8x r9 r10 r11 using "../outputs/tables/house/vote_shares/gop_vs_00p.tex", replace se stats(widstat N, labels("First-Stage F-statistic" "N")) star(* 0.10 ** 0.05 *** 0.01) drop(*.gop_win_beg* china_deltaip $demographics $economic *.census_division  _cons) addnotes(my note) label interaction(" $\times$ ")style(tex) nolines gaps nomtitles nonumbers nonotes nodepvar fragment


* 2010

* OLS
reg gop_delta_sh delta_exp_robs china_deltaip $political2 $demographics $economic i.census_division [aw = cd_prop_pop], cluster(tripple_cluster)
est sto r12
* IV Regressions
ivreg2 gop_delta_sh (delta_exp_robs = eu_delta_exp_robs) i.census_division if t3==1  [aw = cd_prop_pop], cluster(tripple_cluster)
est sto r13
ivreg2 gop_delta_sh (delta_exp_robs = eu_delta_exp_robs) $political1 i.census_division  if t3==1 [aw = cd_prop_pop], cluster(tripple_cluster)
est sto r14
ivreg2 gop_delta_sh (delta_exp_robs = eu_delta_exp_robs) $political1 china_deltaip i.census_division  if t3==1 [aw = cd_prop_pop], cluster(tripple_cluster)
est sto r14x
ivreg2 gop_delta_sh (delta_exp_robs china_deltaip = eu_delta_exp_robs eu_mu_deltaip) $political1  i.census_division if t3==1  [aw = cd_prop_pop], cluster(tripple_cluster)
est sto r15
ivreg2 gop_delta_sh (delta_exp_robs china_deltaip = eu_delta_exp_robs eu_mu_deltaip) $political1  $demographics i.census_division if t3==1  [aw = cd_prop_pop], cluster(tripple_cluster)
est sto r16
ivreg2 gop_delta_sh (delta_exp_robs china_deltaip = eu_delta_exp_robs eu_mu_deltaip) $political1  $demographics $economic i.census_division if t3==1  [aw = cd_prop_pop], cluster(tripple_cluster)
est sto r17


esttab r12 r13 r14 r14x r15 r16 r17, se stats(widstat N, labels("First-Stage F-statistic" "N")) star(* 0.10 ** 0.05 *** 0.01) drop(*.gop_win_beg* *.census_division  _cons) label interaction(" $\times$ ")style(tex) mtitles(OLS 2SLS 2SLS 2SLS 2SLS 2SLS)
esttab r12 r13 r14 r14x r15 r16 r17 using "../outputs/tables/house/vote_shares/gop_vs_10.tex", replace se stats(widstat N, labels("First-Stage F-statistic" "N")) star(* 0.10 ** 0.05 *** 0.01) drop(*.gop_win_beg* *.census_division _cons) label interaction(" $\times$ ")style(tex) mtitles(OLS 2SLS 2SLS 2SLS 2SLS 2SLS)
esttab r12 r13 r14 r14x r15 r16 r17 using "../outputs/tables/house/vote_shares/gop_vs_10p.tex", replace se stats(widstat N, labels("First-Stage F-statistic" "N")) star(* 0.10 ** 0.05 *** 0.01) drop(*.gop_win_beg* china_deltaip $demographics $economic *.census_division  _cons) addnotes(my note) label interaction(" $\times$ ")style(tex) nolines gaps nomtitles nonumbers nonotes nodepvar fragment




** Democrat **


* Stacked Difference

* OLS
reg dem_delta_sh delta_exp_robs china_deltaip $political2 $demographics $economic i.census_division  [aw = cd_prop_pop], cluster(tripple_cluster)
est sto dem
* IV Regressions
ivreg2 dem_delta_sh (delta_exp_robs = eu_delta_exp_robs) i.census_division  [aw = cd_prop_pop], cluster(tripple_cluster)
est sto d1
ivreg2 dem_delta_sh (delta_exp_robs = eu_delta_exp_robs) $political1 i.census_division  [aw = cd_prop_pop], cluster(tripple_cluster)
est sto d2
ivreg2 dem_delta_sh (delta_exp_robs = eu_delta_exp_robs) $political1 china_deltaip i.census_division  [aw = cd_prop_pop], cluster(tripple_cluster)
est sto d2x
ivreg2 dem_delta_sh (delta_exp_robs china_deltaip = eu_delta_exp_robs eu_mu_deltaip) $political1  i.census_division   [aw = cd_prop_pop], cluster(tripple_cluster)
est sto d3
ivreg2 dem_delta_sh (delta_exp_robs china_deltaip = eu_delta_exp_robs eu_mu_deltaip) $political1  $demographics i.census_division  [aw = cd_prop_pop], cluster(tripple_cluster)
est sto d4
ivreg2 dem_delta_sh (delta_exp_robs china_deltaip = eu_delta_exp_robs eu_mu_deltaip) $political1  $demographics $economic i.census_division   [aw = cd_prop_pop], cluster(tripple_cluster)
est sto d5


esttab dem d1 d2 d2x d3 d4 d5, se stats(widstat N, labels("First-Stage F-statistic" "N")) star(* 0.10 ** 0.05 *** 0.01) drop(*.gop_win_beg* *.census_division _cons) label interaction(" $\times$ ")style(tex) mtitles(OLS 2SLS 2SLS 2SLS 2SLS 2SLS)
esttab dem d1 d2 d2x d3 d4 d5 using "../outputs/tables/house/vote_shares/dem_vs_stacked.tex", replace se stats(widstat N, labels("First-Stage F-statistic" "N")) star(* 0.10 ** 0.05 *** 0.01) drop(*.gop_win_beg* *.census_division _cons) label interaction(" $\times$ ")style(tex) mtitles(OLS 2SLS 2SLS 2SLS 2SLS 2SLS)
esttab dem d1 d2 d2x d3 d4 d5 using "../outputs/tables/house/vote_shares/dem_vs_stackedp.tex", replace se stats(widstat N, labels("First-Stage F-statistic" "N")) star(* 0.10 ** 0.05 *** 0.01) drop(*.gop_win_beg* china_deltaip $economic $demographics *.census_division  _cons) addnotes(my note) label interaction(" $\times$ ")style(tex) nolines gaps nomtitles nonumbers nonotes nodepvar fragment



* Stacked Difference Sub

* OLS
reg dem_delta_sh delta_exp_robs china_deltaip $political2 $demographics $economic i.census_division if t2==1 | t3==1 [aw = cd_prop_pop], cluster(tripple_cluster)
est sto ddem
* IV Regressions
ivreg2 dem_delta_sh (delta_exp_robs = eu_delta_exp_robs) i.census_division if t2==1 | t3==1 [aw = cd_prop_pop], cluster(tripple_cluster)
est sto dd1
ivreg2 dem_delta_sh (delta_exp_robs = eu_delta_exp_robs) $political1 i.census_division if t2==1 | t3==1 [aw = cd_prop_pop], cluster(tripple_cluster)
est sto dd2
ivreg2 dem_delta_sh (delta_exp_robs = eu_delta_exp_robs) $political1 china_deltaip i.census_division if t2==1 | t3==1 [aw = cd_prop_pop], cluster(tripple_cluster)
est sto dd2x
ivreg2 dem_delta_sh (delta_exp_robs china_deltaip = eu_delta_exp_robs eu_mu_deltaip) $political1  i.census_division  if t2==1 | t3==1 [aw = cd_prop_pop], cluster(tripple_cluster)
est sto dd3
ivreg2 dem_delta_sh (delta_exp_robs china_deltaip = eu_delta_exp_robs eu_mu_deltaip) $political1  $demographics i.census_division if t2==1 | t3==1 [aw = cd_prop_pop], cluster(tripple_cluster)
est sto dd4
ivreg2 dem_delta_sh (delta_exp_robs china_deltaip = eu_delta_exp_robs eu_mu_deltaip) $political1  $demographics $economic i.census_division  if t2==1 | t3==1 [aw = cd_prop_pop], cluster(tripple_cluster)
est sto dd5


esttab ddem dd1 dd2 dd2x dd3 dd4 dd5, se stats(widstat N, labels("First-Stage F-statistic" "N")) star(* 0.10 ** 0.05 *** 0.01) drop(*.gop_win_beg* *.census_division _cons) label interaction(" $\times$ ")style(tex) mtitles(OLS 2SLS 2SLS 2SLS 2SLS 2SLS)
esttab ddem dd1 dd2 dd2x dd3 dd4 dd5 using "../outputs/tables/house/vote_shares/dem_vs_stackedsub.tex", replace se stats(widstat N, labels("First-Stage F-statistic" "N")) star(* 0.10 ** 0.05 *** 0.01) drop(*.gop_win_beg* *.census_division _cons) label interaction(" $\times$ ")style(tex) mtitles(OLS 2SLS 2SLS 2SLS 2SLS 2SLS)
esttab ddem dd1 dd2 dd2x dd3 dd4 dd5 using "../outputs/tables/house/vote_shares/dem_vs_stackedsubp.tex", replace se stats(widstat N, labels("First-Stage F-statistic" "N")) star(* 0.10 ** 0.05 *** 0.01) drop(*.gop_win_beg* china_deltaip $economic $demographics *.census_division  _cons) addnotes(my note) label interaction(" $\times$ ")style(tex) nolines gaps nomtitles nonumbers nonotes nodepvar fragment


* 1990's

* OLS
reg dem_delta_sh delta_exp_robs china_deltaip $political2 $demographics $economic if t1==1 [aw = cd_prop_pop], cluster(tripple_cluster)
est sto dd6
* IV Regressions
ivreg2 dem_delta_sh (delta_exp_robs = eu_delta_exp_robs) i.census_division if t1==1   [aw = cd_prop_pop], cluster(double_cluster)
est sto dd7
ivreg2 dem_delta_sh (delta_exp_robs = eu_delta_exp_robs) $political1 i.census_division if t1==1   [aw = cd_prop_pop], cluster(double_cluster)
est sto dd8
ivreg2 dem_delta_sh (delta_exp_robs = eu_delta_exp_robs) $political1 china_deltaip i.census_division if t1==1   [aw = cd_prop_pop], cluster(double_cluster)
est sto dd8x
ivreg2 dem_delta_sh (delta_exp_robs china_deltaip = eu_delta_exp_robs eu_mu_deltaip) $political1  i.census_division if t1==1  [aw = cd_prop_pop], cluster(double_cluster)
est sto dd9
ivreg2 dem_delta_sh (delta_exp_robs china_deltaip = eu_delta_exp_robs eu_mu_deltaip) $political1  $demographics i.census_division  if t1==1  [aw = cd_prop_pop], cluster(double_cluster)
est sto dd10
ivreg2 dem_delta_sh (delta_exp_robs china_deltaip = eu_delta_exp_robs eu_mu_deltaip) $political1  $demographics $economic i.census_division if t1==1  [aw = cd_prop_pop], cluster(double_cluster)
est sto dd11


esttab dd6 dd7 dd8 dd8x dd9 dd10 dd11, se stats(widstat N, labels("First-Stage F-statistic" "N")) star(* 0.10 ** 0.05 *** 0.01) drop(*.gop_win_beg* *.census_division _cons) label interaction(" $\times$ ")style(tex) mtitles(OLS 2SLS 2SLS 2SLS 2SLS 2SLS)
esttab dd6 dd7 dd8 dd8x dd9 dd10 dd11 using "../outputs/tables/house/vote_shares/dem_vs_90.tex", replace se stats(widstat N, labels("First-Stage F-statistic" "N")) star(* 0.10 ** 0.05 *** 0.01) drop(*.gop_win_beg* *.census_division _cons) label interaction(" $\times$ ")style(tex) mtitles(OLS 2SLS 2SLS 2SLS 2SLS 2SLS)
esttab dd6 dd7 dd8 dd8x dd9 dd10 dd11 using "../outputs/tables/house/vote_shares/dem_vs_90p.tex", replace se stats(widstat N, labels("First-Stage F-statistic" "N")) star(* 0.10 ** 0.05 *** 0.01) drop(*.gop_win_beg* china_deltaip $economic $demographics  *.census_division _cons) addnotes(my note) label interaction(" $\times$ ")style(tex) nolines gaps nomtitles nonumbers nonotes nodepvar fragment



* 2000's

* OLS
reg dem_delta_sh delta_exp_robs china_deltaip $political2 $demographics $economic if t2==1 [aw = cd_prop_pop], cluster(tripple_cluster)
est sto d6
* IV Regressions
ivreg2 dem_delta_sh (delta_exp_robs = eu_delta_exp_robs) i.census_division if t2==1   [aw = cd_prop_pop], cluster(double_cluster)
est sto d7
ivreg2 dem_delta_sh (delta_exp_robs = eu_delta_exp_robs) $political1 i.census_division if t2==1   [aw = cd_prop_pop], cluster(double_cluster)
est sto d8
ivreg2 dem_delta_sh (delta_exp_robs = eu_delta_exp_robs) $political1 china_deltaip i.census_division if t2==1   [aw = cd_prop_pop], cluster(double_cluster)
est sto d8x
ivreg2 dem_delta_sh (delta_exp_robs china_deltaip = eu_delta_exp_robs eu_mu_deltaip) $political1  i.census_division if t2==1  [aw = cd_prop_pop], cluster(double_cluster)
est sto d9
ivreg2 dem_delta_sh (delta_exp_robs china_deltaip = eu_delta_exp_robs eu_mu_deltaip) $political1  $demographics i.census_division  if t2==1  [aw = cd_prop_pop], cluster(double_cluster)
est sto d10
ivreg2 dem_delta_sh (delta_exp_robs china_deltaip = eu_delta_exp_robs eu_mu_deltaip) $political1  $demographics $economic i.census_division if t2==1  [aw = cd_prop_pop], cluster(double_cluster)
est sto d11


esttab d6 d7 d8 d8x d9 d10 d11, se stats(widstat N, labels("First-Stage F-statistic" "N")) star(* 0.10 ** 0.05 *** 0.01) drop(*.gop_win_beg* *.census_division _cons) label interaction(" $\times$ ")style(tex) mtitles(OLS 2SLS 2SLS 2SLS 2SLS 2SLS)
esttab d6 d7 d8 d8x d9 d10 d11 using "../outputs/tables/house/vote_shares/dem_vs_00.tex", replace se stats(widstat N, labels("First-Stage F-statistic" "N")) star(* 0.10 ** 0.05 *** 0.01) drop(*.gop_win_beg* *.census_division _cons) label interaction(" $\times$ ")style(tex) mtitles(OLS 2SLS 2SLS 2SLS 2SLS 2SLS)
esttab d6 d7 d8 d8x d9 d10 d11 using "../outputs/tables/house/vote_shares/dem_vs_00p.tex", replace se stats(widstat N, labels("First-Stage F-statistic" "N")) star(* 0.10 ** 0.05 *** 0.01) drop(*.gop_win_beg* china_deltaip $economic $demographics  *.census_division _cons) addnotes(my note) label interaction(" $\times$ ")style(tex) nolines gaps nomtitles nonumbers nonotes nodepvar fragment


* 2010's

* OLS
reg dem_delta_sh delta_exp_robs china_deltaip $political2 $demographics $economic i.census_division if  t3==1 [aw = cd_prop_pop], cluster(tripple_cluster)
est sto d12
* IV Regressions
ivreg2 dem_delta_sh (delta_exp_robs = eu_delta_exp_robs) i.census_division  if  t3==1  [aw = cd_prop_pop], cluster(double_cluster)
est sto d13
ivreg2 dem_delta_sh (delta_exp_robs = eu_delta_exp_robs) $political1 i.census_division  if  t3==1  [aw = cd_prop_pop], cluster(double_cluster)
est sto d14
ivreg2 dem_delta_sh (delta_exp_robs = eu_delta_exp_robs) $political1 china_deltaip i.census_division  if  t3==1  [aw = cd_prop_pop], cluster(double_cluster)
est sto d14x
ivreg2 dem_delta_sh (delta_exp_robs china_deltaip = eu_delta_exp_robs eu_mu_deltaip) $political1  i.census_division  if  t3==1  [aw = cd_prop_pop], cluster(double_cluster)
est sto d15
ivreg2 dem_delta_sh (delta_exp_robs china_deltaip = eu_delta_exp_robs eu_mu_deltaip) $political1  $demographics i.census_division  if  t3==1  [aw = cd_prop_pop], cluster(double_cluster)
est sto d16
ivreg2 dem_delta_sh (delta_exp_robs china_deltaip = eu_delta_exp_robs eu_mu_deltaip) $political1  $demographics $economic i.census_division  if  t3==1  [aw = cd_prop_pop], cluster(double_cluster)
est sto d17


esttab d12 d13 d14 d14x d15 d16 d17, se stats(widstat N, labels("First-Stage F-statistic" "N")) star(* 0.10 ** 0.05 *** 0.01) drop(*.gop_win_beg* *.census_division _cons) label interaction(" $\times$ ")style(tex) mtitles(OLS 2SLS 2SLS 2SLS 2SLS 2SLS)
esttab d12 d13 d14 d14x d15 d16 d17 using "../outputs/tables/house/vote_shares/dem_vs_10.tex", replace se stats(widstat N, labels("First-Stage F-statistic" "N")) star(* 0.10 ** 0.05 *** 0.01) drop(*.gop_win_beg* *.census_division _cons) label interaction(" $\times$ ")style(tex) mtitles(OLS 2SLS 2SLS 2SLS 2SLS 2SLS)
esttab d12 d13 d14 d14x d15 d16 d17 using "../outputs/tables/house/vote_shares/dem_vs_10p.tex", replace se stats(widstat N, labels("First-Stage F-statistic" "N")) star(* 0.10 ** 0.05 *** 0.01) drop(*.gop_win_beg* china_deltaip $economic $demographics *.census_division  _cons) addnotes(my note) label interaction(" $\times$ ")style(tex) nolines gaps nomtitles nonumbers nonotes nodepvar fragment






* Stacked Difference

esttab dem d1 d2 d2x d3 d4 d5 r r1 r2 r2x r3 r4 r5 using "../outputs/tables/house/vote_shares/prty_vs_stackedp.tex", replace se stats(widstat N, labels("First-Stage F-statistic" "N")) star(* 0.10 ** 0.05 *** 0.01) drop(*.gop_win_beg* china_deltaip $economic $demographics *.census_division  _cons) addnotes(my note) label interaction(" $\times$ ")style(tex) extracols(8) nolines gaps nomtitles nonumbers nonotes nodepvar fragment


* Stacked Difference Sub

esttab ddem dd1 dd2 dd2x dd3 dd4 dd5 rr rr1 rr2 rr2x rr3 rr4 rr5 using "../outputs/tables/house/vote_shares/prty_vs_stackedsubp.tex", replace se stats(widstat N, labels("First-Stage F-statistic" "N")) star(* 0.10 ** 0.05 *** 0.01) drop(*.gop_win_beg* china_deltaip $economic $demographics *.census_division  _cons) addnotes(my note) label interaction(" $\times$ ")style(tex) extracols(8) nolines gaps nomtitles nonumbers nonotes nodepvar fragment


* 1990's

esttab dd6 dd7 dd8 dd8x dd9 dd10 dd11 rr6 rr7 rr8 rr8x rr9 rr10 rr11 using "../outputs/tables/house/vote_shares/prty_vs_90p.tex", replace se stats(widstat N, labels("First-Stage F-statistic" "N")) star(* 0.10 ** 0.05 *** 0.01) drop(*.gop_win_beg* china_deltaip $economic $demographics  *.census_division _cons) label interaction(" $\times$ ")style(tex) extracols(8) nolines gaps nomtitles nonumbers nonotes nodepvar fragment


* 2000's

esttab d6 d7 d8 d8x d9 d10 d11 r6 r7 r8 r8x r9 r10 r11 using "../outputs/tables/house/vote_shares/prty_vs_00p.tex", replace se stats(widstat N, labels("First-Stage F-statistic" "N")) star(* 0.10 ** 0.05 *** 0.01) drop(*.gop_win_beg* china_deltaip $economic $demographics  *.census_division _cons) addnotes(my note) label interaction(" $\times$ ")style(tex) extracols(8) nolines gaps nomtitles nonumbers nonotes nodepvar fragment


* 2010's

esttab d12 d13 d14 d14x d15 d16 d17 r12 r13 r14 r14x r15 r16 r17 using "../outputs/tables/house/vote_shares/prty_vs_10p.tex", replace se stats(widstat N, labels("First-Stage F-statistic" "N")) star(* 0.10 ** 0.05 *** 0.01) drop(*.gop_win_beg* china_deltaip $economic $demographics *.census_division  _cons) addnotes(my note) label interaction(" $\times$ ")style(tex) extracols(8) nolines gaps nomtitles nonumbers nonotes nodepvar fragment



********************************************************************************
*************************** CHANGE IN WINNER MARGIN ****************************
********************************************************************************


* Stacked Difference

* OLS
reg delta_win_margin delta_exp_robs china_deltaip $political1 $demographics $economic i.census_division [aw = cd_prop_pop], cluster(tripple_cluster)
est sto wmOL1
* IV Regressions
ivreg2 delta_win_margin (delta_exp_robs = eu_delta_exp_robs) i.census_division [aw = cd_prop_pop], cluster(tripple_cluster)
est sto wm1
ivreg2 delta_win_margin (delta_exp_robs = eu_delta_exp_robs) $political1 i.census_division  [aw = cd_prop_pop], cluster(tripple_cluster)
est sto wm2
ivreg2 delta_win_margin (delta_exp_robs = eu_delta_exp_robs) $political1 china_deltaip i.census_division  [aw = cd_prop_pop], cluster(tripple_cluster)
est sto wm2x
ivreg2 delta_win_margin (delta_exp_robs china_deltaip = eu_delta_exp_robs eu_mu_deltaip) $political1  i.census_division [aw = cd_prop_pop], cluster(tripple_cluster)
est sto wm3
ivreg2 delta_win_margin (delta_exp_robs china_deltaip = eu_delta_exp_robs eu_mu_deltaip) $political1  $demographics i.census_division [aw = cd_prop_pop], cluster(tripple_cluster)
est sto wm4
ivreg2 delta_win_margin (delta_exp_robs china_deltaip = eu_delta_exp_robs eu_mu_deltaip) $political1  $demographics $economic i.census_division [aw = cd_prop_pop], cluster(tripple_cluster)
est sto wm5

esttab wmOL1 wm1 wm2 wm2x wm3 wm4 wm5, se stats(widstat N, labels("First-Stage F-statistic" "N")) star(* 0.10 ** 0.05 *** 0.01) drop(*.gop_win_beg* *.census_division _cons) label interaction(" $\times$ ")style(tex) mtitles(OLS 2SLS 2SLS 2SLS 2SLS 2SLS)
esttab wmOL1 wm1 wm2 wm2x wm3 wm4 wm5 using "../outputs/tables/house/vote_shares/win_margin_stacked.tex", replace se stats(widstat N, labels("First-Stage F-statistic" "N")) star(* 0.10 ** 0.05 *** 0.01) drop(*.census_division _cons) label interaction(" $\times$ ")style(tex) mtitles(OLS 2SLS 2SLS 2SLS 2SLS 2SLS 2SLS) fragment gaps nonotes nodepvar
esttab wmOL1 wm1 wm2 wm2x wm3 wm4 wm5 using "../outputs/tables/house/vote_shares/win_margin_stackedp.tex", replace se stats(widstat N, labels("First-Stage F-statistic" "N")) star(* 0.10 ** 0.05 *** 0.01) drop(*.gop_win_beg* china_deltaip $demographics $economic *.census_division _cons) label interaction(" $\times$ ")style(tex) nolines gaps nomtitles nonumbers nonotes nodepvar fragment 


* Stacked Difference Sub

* OLS
reg delta_win_margin delta_exp_robs china_deltaip $political1 $demographics $economic i.census_division [aw = cd_prop_pop], cluster(tripple_cluster)
est sto wmOL1
* IV Regressions
ivreg2 delta_win_margin (delta_exp_robs = eu_delta_exp_robs) i.census_division if t2==1 | t3==1 [aw = cd_prop_pop], cluster(tripple_cluster)
est sto wm1
ivreg2 delta_win_margin (delta_exp_robs = eu_delta_exp_robs) $political1 i.census_division if t2==1 | t3==1 [aw = cd_prop_pop], cluster(tripple_cluster)
est sto wm2
ivreg2 delta_win_margin (delta_exp_robs = eu_delta_exp_robs) $political1 china_deltaip i.census_division if t2==1 | t3==1 [aw = cd_prop_pop], cluster(tripple_cluster)
est sto wm2x
ivreg2 delta_win_margin (delta_exp_robs china_deltaip = eu_delta_exp_robs eu_mu_deltaip) $political1  i.census_division if t2==1 | t3==1 [aw = cd_prop_pop], cluster(tripple_cluster)
est sto wm3
ivreg2 delta_win_margin (delta_exp_robs china_deltaip = eu_delta_exp_robs eu_mu_deltaip) $political1  $demographics i.census_division if t2==1 | t3==1 [aw = cd_prop_pop], cluster(tripple_cluster)
est sto wm4
ivreg2 delta_win_margin (delta_exp_robs china_deltaip = eu_delta_exp_robs eu_mu_deltaip) $political1  $demographics $economic i.census_division if t2==1 | t3==1 [aw = cd_prop_pop], cluster(tripple_cluster)
est sto wm5

esttab wmOL1 wm1 wm2 wm2x wm3 wm4 wm5, se stats(widstat N, labels("First-Stage F-statistic" "N")) star(* 0.10 ** 0.05 *** 0.01) drop(*.gop_win_beg* *.census_division _cons) label interaction(" $\times$ ")style(tex) mtitles(OLS 2SLS 2SLS 2SLS 2SLS 2SLS)
esttab wmOL1 wm1 wm2 wm2x wm3 wm4 wm5 using "../outputs/tables/house/vote_shares/win_margin_stackedsub.tex", replace se stats(widstat N, labels("First-Stage F-statistic" "N")) star(* 0.10 ** 0.05 *** 0.01) drop(*.gop_win_beg* *.census_division _cons) label interaction(" $\times$ ")style(tex) mtitles(OLS 2SLS 2SLS 2SLS 2SLS 2SLS)
esttab wmOL1 wm1 wm2 wm2x wm3 wm4 wm5 using "../outputs/tables/house/vote_shares/win_margin_stackedsubp.tex", replace se stats(widstat N, labels("First-Stage F-statistic" "N")) star(* 0.10 ** 0.05 *** 0.01) drop(*.gop_win_beg* china_deltaip $demographics $economic *.census_division _cons) label interaction(" $\times$ ")style(tex) nolines gaps nomtitles nonumbers nonotes nodepvar fragment




* 1990

reg delta_win_margin delta_exp_robs china_deltaip $political1 $demographics $economic i.census_division if t1==1  [aw = cd_prop_pop], cluster(tripple_cluster)
est sto wmOL1
ivreg2 delta_win_margin (delta_exp_robs = eu_delta_exp_robs) i.census_division if t1==1 [aw = cd_prop_pop], cluster(tripple_cluster)
est sto wm1
ivreg2 delta_win_margin (delta_exp_robs = eu_delta_exp_robs) $political1 i.census_division if t1==1 [aw = cd_prop_pop], cluster(tripple_cluster)
est sto wm2
ivreg2 delta_win_margin (delta_exp_robs = eu_delta_exp_robs) $political1 china_deltaip i.census_division if t1==1 [aw = cd_prop_pop], cluster(tripple_cluster)
est sto wm2x
ivreg2 delta_win_margin (delta_exp_robs china_deltaip = eu_delta_exp_robs eu_mu_deltaip) $political1  i.census_division if t1==1 [aw = cd_prop_pop], cluster(tripple_cluster)
est sto wm3
ivreg2 delta_win_margin (delta_exp_robs china_deltaip = eu_delta_exp_robs eu_mu_deltaip) $political1   $demographics i.census_division if t1==1 [aw = cd_prop_pop], cluster(tripple_cluster)
est sto wm4
ivreg2 delta_win_margin (delta_exp_robs china_deltaip = eu_delta_exp_robs eu_mu_deltaip) $political1  $demographics $economic i.census_division if t1==1 [aw = cd_prop_pop], cluster(tripple_cluster)
est sto wm5


esttab wmOL1 wm1 wm2 wm2x wm3 wm4 wm5, se stats(widstat N, labels("First-Stage F-statistic" "N")) star(* 0.10 ** 0.05 *** 0.01) drop(*.gop_win_beg* *.census_division _cons) label interaction(" $\times$ ")style(tex) mtitles(OLS 2SLS 2SLS 2SLS 2SLS 2SLS)
esttab wmOL1 wm1 wm2 wm2x wm3 wm4 wm5 using "../outputs/tables/house/vote_shares/win_margin90.tex", replace se stats(widstat N, labels("First-Stage F-statistic" "N")) star(* 0.10 ** 0.05 *** 0.01) drop(*.gop_win_beg* *.census_division _cons) label interaction(" $\times$ ")style(tex) mtitles(OLS 2SLS 2SLS 2SLS 2SLS 2SLS)
esttab wmOL1 wm1 wm2 wm2x wm3 wm4 wm5 using "../outputs/tables/house/vote_shares/win_margin90p.tex", replace se stats(widstat N, labels("First-Stage F-statistic" "N")) star(* 0.10 ** 0.05 *** 0.01) drop(*.gop_win_beg* china_deltaip $demographics $economic *.census_division  _cons) addnotes(my note) label interaction(" $\times$ ")style(tex) nolines gaps nomtitles nonumbers nonotes nodepvar fragment


* 2000

reg delta_win_margin delta_exp_robs china_deltaip $political1 $demographics $economic i.census_division if t2==1 [aw = cd_prop_pop], cluster(tripple_cluster)
est sto wmOL1
ivreg2 delta_win_margin (delta_exp_robs = eu_delta_exp_robs) i.census_division if t2==1 [aw = cd_prop_pop], cluster(tripple_cluster)
est sto wm1
ivreg2 delta_win_margin (delta_exp_robs = eu_delta_exp_robs) $political1 i.census_division if t2==1 [aw = cd_prop_pop], cluster(tripple_cluster)
est sto wm2
ivreg2 delta_win_margin (delta_exp_robs = eu_delta_exp_robs) $political1 china_deltaip i.census_division if t2==1 [aw = cd_prop_pop], cluster(tripple_cluster)
est sto wm2x
ivreg2 delta_win_margin (delta_exp_robs china_deltaip = eu_delta_exp_robs eu_mu_deltaip) $political1  i.census_division if t2==1 [aw = cd_prop_pop], cluster(tripple_cluster)
est sto wm3
ivreg2 delta_win_margin (delta_exp_robs china_deltaip = eu_delta_exp_robs eu_mu_deltaip) $political1   $demographics i.census_division if t2==1 [aw = cd_prop_pop], cluster(tripple_cluster)
est sto wm4
ivreg2 delta_win_margin (delta_exp_robs china_deltaip = eu_delta_exp_robs eu_mu_deltaip) $political1  $demographics $economic i.census_division if t2==1 [aw = cd_prop_pop], cluster(tripple_cluster)
est sto wm5


esttab wmOL1 wm1 wm2 wm2x wm3 wm4 wm5, se stats(widstat N, labels("First-Stage F-statistic" "N")) star(* 0.10 ** 0.05 *** 0.01) drop(*.gop_win_beg* *.census_division _cons) label interaction(" $\times$ ")style(tex) mtitles(OLS 2SLS 2SLS 2SLS 2SLS 2SLS)
esttab wmOL1 wm1 wm2 wm2x wm3 wm4 wm5 using "../outputs/tables/house/vote_shares/win_margin00.tex", replace se stats(widstat N, labels("First-Stage F-statistic" "N")) star(* 0.10 ** 0.05 *** 0.01) drop(*.gop_win_beg* *.census_division _cons) label interaction(" $\times$ ")style(tex) mtitles(OLS 2SLS 2SLS 2SLS 2SLS 2SLS)
esttab wmOL1 wm1 wm2 wm2x wm3 wm4 wm5 using "../outputs/tables/house/vote_shares/win_margin00p.tex", replace se stats(widstat N, labels("First-Stage F-statistic" "N")) star(* 0.10 ** 0.05 *** 0.01) drop(*.gop_win_beg* china_deltaip $demographics $economic *.census_division  _cons) addnotes(my note) label interaction(" $\times$ ")style(tex) nolines gaps nomtitles nonumbers nonotes nodepvar fragment



* 2010

reg delta_win_margin delta_exp_robs china_deltaip $political1 $demographics $economic  i.census_division if t3==1 [aw = cd_prop_pop], cluster(tripple_cluster)
est sto wmOL1
ivreg2 delta_win_margin (delta_exp_robs = eu_delta_exp_robs) i.census_division if t3==1 [aw = cd_prop_pop], cluster(tripple_cluster)
est sto wm1
ivreg2 delta_win_margin (delta_exp_robs = eu_delta_exp_robs) $political1 i.census_division if t3==1 [aw = cd_prop_pop], cluster(tripple_cluster)
est sto wm2
ivreg2 delta_win_margin (delta_exp_robs = eu_delta_exp_robs) $political1 china_deltaip i.census_division if t3==1 [aw = cd_prop_pop], cluster(tripple_cluster)
est sto wm2x
ivreg2 delta_win_margin (delta_exp_robs china_deltaip = eu_delta_exp_robs eu_mu_deltaip) $political1 i.census_division  if t3==1 [aw = cd_prop_pop], cluster(tripple_cluster)
est sto wm3
ivreg2 delta_win_margin (delta_exp_robs china_deltaip = eu_delta_exp_robs eu_mu_deltaip) $political1 $demographics i.census_division if t3==1 [aw = cd_prop_pop], cluster(tripple_cluster)
est sto wm4
ivreg2 delta_win_margin (delta_exp_robs china_deltaip = eu_delta_exp_robs eu_mu_deltaip) $political1 $demographics $economic i.census_division if t3==1 [aw = cd_prop_pop], cluster(tripple_cluster)
est sto wm5


esttab wmOL1 wm1 wm2 wm2x wm3 wm4 wm5, se stats(widstat N, labels("First-Stage F-statistic" "N")) star(* 0.10 ** 0.05 *** 0.01) drop(*.gop_win_beg* *.census_division _cons) label interaction(" $\times$ ")style(tex) mtitles(OLS 2SLS 2SLS 2SLS 2SLS 2SLS)
esttab wmOL1 wm1 wm2 wm2x wm3 wm4 wm5 using "../outputs/tables/house/vote_shares/win_margin10.tex", replace se stats(widstat N, labels("First-Stage F-statistic" "N")) star(* 0.10 ** 0.05 *** 0.01) drop(*.gop_win_beg* *.census_division _cons) label interaction(" $\times$ ")style(tex) mtitles(OLS 2SLS 2SLS 2SLS 2SLS 2SLS)
esttab wmOL1 wm1 wm2 wm2x wm3 wm4 wm5 using "../outputs/tables/house/vote_shares/win_margin10p.tex", replace se stats(widstat N, labels("First-Stage F-statistic" "N")) star(* 0.10 ** 0.05 *** 0.01) drop(*.gop_win_beg* china_deltaip $demographics $economic *.census_division _cons) addnotes(my note) label interaction(" $\times$ ")style(tex) nolines gaps nomtitles nonumbers nonotes nodepvar fragment





