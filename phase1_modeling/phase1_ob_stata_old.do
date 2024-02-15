clear

ssc install decomp
cd "/Users/bintzc/Desktop/asher_local"
import delimited ob_input_prepped_df_mics.csv
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
foreach country_code in "mw" "rw" "cm" "gh" "np" {
	clear
	use full_data.dta
	
	keep if country == "`country_code'"  // Keep only the data for the current country_code
	save "`country_code'_15_24.dta", replace  // Save the subset for the country
	use "`country_code'_15_24.dta", clear

	
	svyset psu_unique [pweight=pweight]
	
	if "`country_code'" == "mw" | "`country_code'" =="gh" | "`country_code'" == "np" | "`country_code'" == "cm" {
oaxaca any_birth_preg age educ_single_yrs mcpr unmet_need curr_cohabit fp_exp_media desire_child_teen attend_school never_had_intercourse rural sex_activity_last_4_weeks wealth_index long_acting_method_mod short_acting_method_mod no_method other_method_trad, by(baseline) categorical(long_acting_method_mod short_acting_method_mod no_method other_method_trad) detail svy noisily
}

if "`country_code'" == "rw"  {
oaxaca any_birth_preg age educ_single_yrs mcpr unmet_need curr_cohabit fp_exp_media desire_child_teen attend_school never_had_intercourse rural sex_activity_last_4_weeks wealth_index short_acting_method_mod no_method, by(baseline) detail svy noisily
}



estimates store test
*esttab using "Regression tables.csv", b(2) ci(2) compress label replace

gen file_name_coef = "oaxaca_15_24_coef_detail_" +  "`country_code'" + ".csv"
local file_name_coef = file_name_coef[1]

gen file_name_ci = "oaxaca_15_24_ci_detail_" +  "`country_code'" + ".csv"
local file_name_ci = file_name_ci[1]

gen plot_name = "mics_15_24_detail_" +  "`country_code'" + ".png"
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
foreach country_code in "mw" "rw" "cm" "gh" "np" {
	clear
	use full_data.dta
	
	keep if country == "`country_code'"  // Keep only the data for the current country_code
	keep if age < 20 & age > 14 
	save "`country_code'_15_19.dta", replace  // Save the subset for the country
	use "`country_code'_15_19.dta", clear

	
	svyset psu_unique [pweight=pweight]
	
	if "`country_code'" == "mw" | "`country_code'" =="gh" | "`country_code'" == "np" | "`country_code'" == "cm" {
oaxaca any_birth_preg age educ_single_yrs mcpr unmet_need curr_cohabit fp_exp_media desire_child_teen attend_school never_had_intercourse rural sex_activity_last_4_weeks wealth_index long_acting_method_mod short_acting_method_mod no_method other_method_trad, by(baseline) categorical(long_acting_method_mod short_acting_method_mod no_method other_method_trad) detail svy noisily
}

if "`country_code'" == "rw"  {
oaxaca any_birth_preg age educ_single_yrs mcpr unmet_need curr_cohabit fp_exp_media desire_child_teen attend_school never_had_intercourse rural sex_activity_last_4_weeks wealth_index short_acting_method_mod no_method, by(baseline) detail svy noisily
}



estimates store test
*esttab using "Regression tables.csv", b(2) ci(2) compress label replace

gen file_name_coef = "oaxaca_15_19_coef_detail_" +  "`country_code'" + ".csv"
local file_name_coef = file_name_coef[1]

gen file_name_ci = "oaxaca_15_19_ci_detail_" +  "`country_code'" + ".csv"
local file_name_ci = file_name_ci[1]

gen plot_name = "mics_15_19_detail_" +  "`country_code'" + ".png"
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
