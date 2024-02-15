clear

ssc install decomp
cd "/Users/bintzc/Desktop/asher_local"
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
foreach country_code in "cm" "np" "gh" "rw" "mw"{
	clear
	use full_data.dta
	
	keep if country == "`country_code'"  // Keep only the data for the current country_code
	save "`country_code'_15_24.dta", replace  // Save the subset for the country
	use "`country_code'_15_24.dta", clear

	
	svyset psu_unique [pweight=pweight]
	
/* general 
	oaxaca any_birth_preg wealth_index age educ_single_yrs ideal_child mcpr unmet_need curr_cohabit fp_exp_media knowledge_mod desire_child_teen fp_hcw12m fp_facility12m attend_school never_had_intercourse rural sex_activity_last_4_weeks no_sex_activity_last_4_weeks long_acting_method_mod short_acting_method_mod no_method other_method_trad, by(baseline) categorical(long_acting_method_mod short_acting_method_mod no_method other_method_trad) detail svy noisily
*/

if "`country_code'" == "cm"  {
oaxaca any_birth_preg wealth_index educ_single_yrs ideal_child mcpr fp_exp_media knowledge_mod desire_child_teen fp_hcw12m fp_facility12m attend_school never_had_intercourse rural long_acting_method_mod short_acting_method_mod other_method_trad, by(baseline) detail svy noisily
}

if "`country_code'" == "np"  {
oaxaca any_birth_preg educ_single_yrs ideal_child mcpr unmet_need curr_cohabit fp_exp_media fp_facility12m never_had_intercourse rural short_acting_method_mod, by(baseline) detail svy noisily
}

if "`country_code'" == "gh"  {
oaxaca any_birth_preg wealth_index educ_single_yrs ideal_child mcpr unmet_need curr_cohabit fp_exp_media knowledge_mod desire_child_teen fp_hcw12m fp_facility12m attend_school never_had_intercourse rural sex_activity_last_4_weeks, by(baseline) detail svy noisily
}
/* age allowed
if "`country_code'" == "rw"  {
oaxaca any_birth_preg wealth_index age educ_single_yrs ideal_child unmet_need curr_cohabit knowledge_mod desire_child_teen fp_hcw12m attend_school never_had_intercourse rural sex_activity_last_4_weeks long_acting_method_mod short_acting_method_mod other_method_trad, by(baseline) detail svy noisily
}
*/
* age excluded
if "`country_code'" == "rw"  {
oaxaca any_birth_preg wealth_index educ_single_yrs unmet_need curr_cohabit knowledge_mod desire_child_teen fp_hcw12m attend_school never_had_intercourse rural sex_activity_last_4_weeks short_acting_method_mod other_method_trad, by(baseline) detail svy noisily
}

if "`country_code'" == "mw"  {
oaxaca any_birth_preg wealth_index educ_single_yrs ideal_child mcpr unmet_need curr_cohabit fp_exp_media knowledge_mod desire_child_teen fp_hcw12m fp_facility12m attend_school never_had_intercourse rural long_acting_method_mod no_method, by(baseline)
}



estimates store test
*esttab using "Regression tables.csv", b(2) ci(2) compress label replace

gen file_name_coef = "oaxaca_15_24_coef_detail_" +  "`country_code'" + ".csv"
local file_name_coef = file_name_coef[1]

gen file_name_ci = "oaxaca_15_24_ci_detail_" +  "`country_code'" + ".csv"
local file_name_ci = file_name_ci[1]

gen plot_name = "dhs_15_24_detail_" +  "`country_code'" + ".png"
local plot_name = plot_name[1]

* store coefficients
esttab test using "`file_name_coef'", cells(b)  replace
* store confidence intervals
esttab test using "`file_name_ci'", ci(2) replace

coefplot (., keep(endowments:*)), bylabel("Endowments") title("`country_code' 15-24")|| ///
(., keep(coefficients:*)),  bylabel("Coefficients")  || , ///
drop(*:_cons) recast(bar) barwidth(0.5) citop ciopts(recast(rcap) color(black)) ///
byopts(cols(1)) xline(0, lpattern(dash)) xlabel(, grid glstyle(minor_grid) glpattern(dash)) ///


graph export "`plot_name'", replace
}




* for 15-19
foreach country_code in "cm" "np" "gh" "rw" "mw"{
	clear
	use full_data.dta
	
	keep if country == "`country_code'"  // Keep only the data for the current country_code
	keep if age < 20 & age > 14 
	save "`country_code'_15_19.dta", replace  // Save the subset for the country
	use "`country_code'_15_19.dta", clear

	
	svyset psu_unique [pweight=pweight]

if "`country_code'" == "cm"  {
oaxaca any_birth_preg educ_single_yrs curr_cohabit fp_exp_media attend_school never_had_intercourse rural sex_activity_last_4_weeks long_acting_method_mod short_acting_method_mod knowledge_mod ideal_child, by(baseline) detail svy noisily
}

if "`country_code'" == "np"  {
oaxaca any_birth_preg educ_single_yrs ideal_child desire_child_teen fp_facility12m short_acting_method_mod, by(baseline) detail svy noisily
}

* age excluded from lasso 
if "`country_code'" == "gh"  {
	oaxaca any_birth_preg wealth_index educ_single_yrs ideal_child unmet_need curr_cohabit fp_exp_media desire_child_teen fp_facility12m never_had_intercourse rural, by(baseline) detail svy noisily
}

/* age allowed
if "`country_code'" == "gh"  {
	oaxaca any_birth_preg wealth_index age educ_single_yrs ideal_child unmet_need curr_cohabit fp_exp_media desire_child_teen fp_facility12m never_had_intercourse rural long_acting_method_mod, by(baseline) detail svy noisily
}

*/

if "`country_code'" == "rw"  {
oaxaca any_birth_preg educ_single_yrs unmet_need curr_cohabit desire_child_teen never_had_intercourse short_acting_method_mod other_method_trad, by(baseline) detail svy noisily
}

if "`country_code'" == "mw"  {
oaxaca any_birth_preg wealth_index educ_single_yrs ideal_child fp_exp_media knowledge_mod desire_child_teen fp_hcw12m fp_facility12m attend_school never_had_intercourse rural other_method_trad, by(baseline)
}


estimates store test
*esttab using "Regression tables.csv", b(2) ci(2) compress label replace

gen file_name_coef = "oaxaca_15_19_coef_detail_" +  "`country_code'" + ".csv"
local file_name_coef = file_name_coef[1]

gen file_name_ci = "oaxaca_15_19_ci_detail_" +  "`country_code'" + ".csv"
local file_name_ci = file_name_ci[1]

gen plot_name = "dhs_15_19_detail_" +  "`country_code'" + ".png"
local plot_name = plot_name[1]

* store coefficients
esttab test using "`file_name_coef'", cells(b)  replace
* store confidence intervals
esttab test using "`file_name_ci'", ci(2) replace

coefplot (., keep(endowments:*)), bylabel("Endowments") title("`country_code' 15-19")|| ///
(., keep(coefficients:*)),  bylabel("Coefficients")  || , ///
drop(*:_cons) recast(bar) barwidth(0.5) citop ciopts(recast(rcap) color(black)) ///
byopts(cols(1)) xline(0, lpattern(dash)) xlabel(, grid glstyle(minor_grid) glpattern(dash)) ///


graph export "`plot_name'", replace
}
