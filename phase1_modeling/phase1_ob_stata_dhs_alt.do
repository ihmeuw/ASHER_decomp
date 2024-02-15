clear


** change this filepath to in date 
cd "/Volumes/share/scratch/projects/hssa/asher/phase1/data/2024-02-13"

import delimited ob_input_prepped_df_dhs.csv
save full_data.dta, replace

** filter only single lonely strata
*keep if  strata_unique  !=  "gh4 2003 156"
egen country_round = concat(country year), punct(" ")

** try country by country 
* cm needs special specification for method type 
* np needs special specification for method type 
* rw needs special specification for method type 
* mw normal
* gh normal

* for 15-24 
foreach country_code in "cm" "np" "gh" "rw" "mw" {
	clear
	** change this filepath to in date 
	cd "/Volumes/share/scratch/projects/hssa/asher/phase1/data/2024-02-13"

	use full_data.dta
	
	keep if country == "`country_code'"  // Keep only the data for the current country_code
	save "`country_code'_15_24_alt.dta", replace  // Save the subset for the country
	use "`country_code'_15_24_alt.dta", clear

	
	svyset psu_unique [pweight=pweight]
	
/* general 
	oaxaca any_birth_preg rural wealth_index educ_single_yrs attend_school curr_cohabit ideal_child desire_child_teen mcpr fp_exp_media knowledge_mod never_had_intercourse  sex_activity_last_4_weeks, by(baseline) weight(1) detail svy noisily
*/

if "`country_code'" == "cm"  {
	oaxaca any_birth_preg rural wealth_index educ_single_yrs attend_school curr_cohabit ideal_child desire_child_teen mcpr fp_exp_media knowledge_mod never_had_intercourse  sex_activity_last_4_weeks, by(baseline) weight(1) detail svy noisily
}

if "`country_code'" == "np"  {
oaxaca any_birth_preg rural wealth_index educ_single_yrs attend_school curr_cohabit ideal_child desire_child_teen mcpr fp_exp_media knowledge_mod never_had_intercourse  sex_activity_last_4_weeks, by(baseline) weight(1) detail svy noisily
}

if "`country_code'" == "gh"  {
oaxaca any_birth_preg rural wealth_index educ_single_yrs attend_school curr_cohabit ideal_child desire_child_teen mcpr fp_exp_media knowledge_mod never_had_intercourse  sex_activity_last_4_weeks, by(baseline) weight(1) detail svy noisily
}

if "`country_code'" == "rw"  {
	oaxaca any_birth_preg rural wealth_index educ_single_yrs attend_school curr_cohabit ideal_child desire_child_teen mcpr fp_exp_media knowledge_mod never_had_intercourse  sex_activity_last_4_weeks, by(baseline) weight(1) detail svy noisily
}

if "`country_code'" == "mw"  {
oaxaca any_birth_preg rural wealth_index educ_single_yrs attend_school curr_cohabit ideal_child desire_child_teen mcpr fp_exp_media knowledge_mod never_had_intercourse  sex_activity_last_4_weeks, by(baseline) weight(1) detail svy noisily
}



estimates store test
*esttab using "Regression tables.csv", b(2) ci(2) compress label replace

gen file_name_coef = "alt_dhs_oaxaca_15_24_coef_detail_" +  "`country_code'" + ".csv"
local file_name_coef = file_name_coef[1]

gen file_name_ci = "alt_dhs_oaxaca_15_24_ci_detail_" +  "`country_code'" + ".csv"
local file_name_ci = file_name_ci[1]

gen plot_name = "alt_dhs_15_24_detail_" +  "`country_code'" + ".png"
local plot_name = plot_name[1]

** cd to outdir: change this date to today's date
cd "/Volumes/share/scratch/projects/hssa/asher/phase1/results/ob/2024-02-14"

* store coefficients
esttab test using "`file_name_coef'", cells(b)  replace
* store confidence intervals
esttab test using "`file_name_ci'", ci(2) replace

/*
coefplot (., keep(explained:*)), bylabel("Explained") || ///
(., keep(unexplained:*)),  bylabel("Unexplained")  || , ///
drop(*:_cons) recast(bar) barwidth(0.5) citop ciopts(recast(rcap) color(black)) ///
byopts(cols(1) title("`country_code' 15-24") )  xline(0, lpattern(dash)) xlabel(, grid glstyle(minor_grid) glpattern(dash)) ///
*/

graph export "`plot_name'", replace
}




* for 15-19
foreach country_code in "cm" "np" "gh" "rw" "mw"{
	clear
	** change this filepath to in date 
	cd "/Volumes/share/scratch/projects/hssa/asher/phase1/data/2024-02-13"

	use full_data.dta
	
	keep if country == "`country_code'"  // Keep only the data for the current country_code
	keep if age < 20 & age > 14 
	save "`country_code'_15_19.dta", replace  // Save the subset for the country
	use "`country_code'_15_19.dta", clear

	
	svyset psu_unique [pweight=pweight]

if "`country_code'" == "cm"  {
oaxaca any_birth_preg rural wealth_index educ_single_yrs attend_school curr_cohabit ideal_child desire_child_teen mcpr fp_exp_media knowledge_mod never_had_intercourse  sex_activity_last_4_weeks, by(baseline) weight(1) detail svy noisily
}

if "`country_code'" == "np"  {
oaxaca any_birth_preg rural wealth_index educ_single_yrs attend_school curr_cohabit ideal_child desire_child_teen mcpr fp_exp_media knowledge_mod never_had_intercourse  sex_activity_last_4_weeks, by(baseline) weight(1) detail svy noisily
}

* age excluded from lasso 
if "`country_code'" == "gh"  {
oaxaca any_birth_preg rural wealth_index educ_single_yrs attend_school curr_cohabit ideal_child desire_child_teen mcpr fp_exp_media knowledge_mod never_had_intercourse  sex_activity_last_4_weeks, by(baseline) weight(1) detail svy noisily
}


if "`country_code'" == "rw"  {
oaxaca any_birth_preg rural wealth_index educ_single_yrs attend_school curr_cohabit ideal_child desire_child_teen mcpr fp_exp_media knowledge_mod never_had_intercourse  sex_activity_last_4_weeks, by(baseline) weight(1) detail svy noisily
}

if "`country_code'" == "mw"  {
oaxaca any_birth_preg rural wealth_index educ_single_yrs attend_school curr_cohabit ideal_child desire_child_teen mcpr fp_exp_media knowledge_mod never_had_intercourse  sex_activity_last_4_weeks, by(baseline) weight(1) detail svy noisily
}




estimates store test
*esttab using "Regression tables.csv", b(2) ci(2) compress label replace

gen file_name_coef = "alt_dhs_oaxaca_15_19_coef_detail_" +  "`country_code'" + ".csv"
local file_name_coef = file_name_coef[1]

gen file_name_ci = "alt_dhs_oaxaca_15_19_ci_detail_" +  "`country_code'" + ".csv"
local file_name_ci = file_name_ci[1]

gen plot_name = "alt_dhs_15_19_detail_" +  "`country_code'" + ".png"
local plot_name = plot_name[1]

** cd to outdir: change this date to today's date
cd "/Volumes/share/scratch/projects/hssa/asher/phase1/results/ob/2024-02-14"

* store coefficients
esttab test using "`file_name_coef'", cells(b)  replace
* store confidence intervals
esttab test using "`file_name_ci'", ci(2) replace

/*

coefplot (., keep(explained:*)), bylabel("Explained") || ///
(., keep(unexplained:*)),  bylabel("Unexplained")  || , ///
drop(*:_cons) recast(bar) barwidth(0.5) citop ciopts(recast(rcap) color(black)) ///
byopts(cols(1) title("`country_code' 15-19") ) xline(0, lpattern(dash)) xlabel(, grid glstyle(minor_grid) glpattern(dash)), ///

graph export "`plot_name'", replace
*/
}

*
