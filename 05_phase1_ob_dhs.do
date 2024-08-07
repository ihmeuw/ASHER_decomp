*==============================================================================
* Title: ASHER Oaxaca-Blinder analysis for DHS data
* Date: March 21, 2024
* Description: This do file conducts an Oaxaca-Blinder analysis for 5 countries using two years of data (baseline and endline)
*
* Data Source: DHS surveys 
* Notes: 
* - This analysis is meant for baseline and endline years only for each country 
* - Ensure that in date and out date file paths are specified 
* - There is a separate .do file for running the analysis using MICS data for the endline for Malawi
*==============================================================================

clear

** change this filepath to in date 
cd "filepath"

import delimited ob_input_prepped_df_dhs.csv
save full_data.dta, replace

egen country_round = concat(country year), punct(" ")

* for 15-24 
foreach country_code in  "gh" "cm" "np" "rw" {
	clear
	** change this filepath to in date 
  cd "filepath"

	use full_data.dta
	
	keep if country == "`country_code'"  // Keep only the data for the current country_code
	save "`country_code'_15_24.dta", replace  // Save the subset for the country
	use "`country_code'_15_24.dta", clear

	
	svyset psu_unique [pweight=pweight]

oaxaca any_birth_preg_2_yr_dhs  age educ_single_yrs   curr_cohabit  unmet_need wealth_dummies1 wealth_dummies2 wealth_dummies3 wealth_dummies4 wealth_dummies5 had_intercourse, by(baseline) categorical(wealth_dummies1 wealth_dummies2 wealth_dummies3 wealth_dummies4 wealth_dummies5) weight(1) detail svy noisily relax

estimates store test

gen file_name_coef = "alt_dhs_oaxaca_15_24_coef_detail_" +  "`country_code'" + ".csv"
local file_name_coef = file_name_coef[1]

gen file_name_ci = "alt_dhs_oaxaca_15_24_ci_detail_" +  "`country_code'" + ".csv"
local file_name_ci = file_name_ci[1]

gen plot_name = "alt_dhs_15_24_detail_" +  "`country_code'" + ".png"
local plot_name = plot_name[1]

** cd to outdir: change this date to today's date
cd "filepath"

* store coefficients
esttab test using "`file_name_coef'", cells(b)  replace
* store confidence intervals
esttab test using "`file_name_ci'", ci(4) replace

* regress pooled 
svy: regress any_birth_preg_2_yr_dhs  age educ_single_yrs  curr_cohabit  unmet_need wealth_dummies2 wealth_dummies3 wealth_dummies4 wealth_dummies5 had_intercourse 

estimates store test_pooled

gen file_name_ci_test = "alt_dhs_oaxaca_15_24_ci_detail_test_" +  "`country_code'" + ".csv"
local file_name_ci_test = file_name_ci_test[1]

* store confidence intervals
esttab test_pooled using "`file_name_ci_test'", ci(4) replace

gen file_name_ci_pooled_table = "alt_dhs_oaxaca_15_24_ci_detail_pooled_table_" +  "`country_code'" + ".csv"
local file_name_ci_pooled_table = file_name_ci_pooled_table[1]

* store confidence intervals for methods appendix
esttab test_pooled using "`file_name_ci_pooled_table'", se star nogaps replace


* regress endline only
keep if baseline == 0
svy: regress any_birth_preg_2_yr_dhs  age educ_single_yrs  curr_cohabit  unmet_need wealth_dummies2 wealth_dummies3 wealth_dummies4 wealth_dummies5 had_intercourse  

estimates store test_endline

gen file_name_ci_endline = "alt_dhs_oaxaca_15_24_ci_detail_endline_" +  "`country_code'" + ".csv"
local file_name_ci_endline = file_name_ci_endline[1]

* store confidence intervals
esttab test_endline using "`file_name_ci_endline'", ci(4) replace

gen file_name_ci_endline_table = "alt_dhs_oaxaca_15_24_ci_detail_endline_table_" +  "`country_code'" + ".csv"
local file_name_ci_endline_table = file_name_ci_endline_table[1]

* store confidence intervals for methods appendix
esttab test_endline using "`file_name_ci_endline_table'", se star nogaps replace

* regress baseline only
clear
	** change this filepath to in date 
	cd "filepath"

	use full_data.dta
	
	keep if country == "`country_code'"  // Keep only the data for the current country_code
	save "`country_code'_15_24.dta", replace  // Save the subset for the country
	use "`country_code'_15_24.dta", clear

	svyset psu_unique [pweight=pweight]
	
keep if baseline == 1
svy: regress any_birth_preg_2_yr_dhs  age educ_single_yrs  curr_cohabit  unmet_need wealth_dummies2 wealth_dummies3 wealth_dummies4 wealth_dummies5 had_intercourse  

** cd to outdir: change this date to today's date
cd "filepath"
estimates store test_baseline

gen file_name_ci_baseline = "alt_dhs_oaxaca_15_24_ci_detail_baseline_" +  "`country_code'" + ".csv"
local file_name_ci_baseline = file_name_ci_baseline[1]

* store confidence intervals
esttab test_baseline using "`file_name_ci_baseline'", ci(4) replace

gen file_name_ci_baseline_table = "alt_dhs_oaxaca_15_24_ci_detail_baseline_table_" +  "`country_code'" + ".csv"
local file_name_ci_baseline_table = file_name_ci_baseline_table[1]

* store confidence intervals for methods appendix
esttab test_baseline using "`file_name_ci_baseline_table'", se star nogaps replace


}


* for 15-19
foreach country_code in  "gh" "cm" "np" "rw"  {
	clear
	** change this filepath to in date 
	cd "filepath"

	use full_data.dta
	
	keep if country == "`country_code'"  // Keep only the data for the current country_code
	keep if age < 20 & age > 14 
	save "`country_code'_15_19.dta", replace  // Save the subset for the country
	use "`country_code'_15_19.dta", clear

	
	svyset psu_unique [pweight=pweight]

oaxaca any_birth_preg_2_yr_dhs  age educ_single_yrs  curr_cohabit  unmet_need wealth_dummies1 wealth_dummies2 wealth_dummies3 wealth_dummies4 wealth_dummies5 had_intercourse, by(baseline) categorical(wealth_dummies1 wealth_dummies2 wealth_dummies3 wealth_dummies4 wealth_dummies5) weight(1) detail svy noisily relax

estimates store test

gen file_name_coef = "alt_dhs_oaxaca_15_19_coef_detail_" +  "`country_code'" + ".csv"
local file_name_coef = file_name_coef[1]

gen file_name_ci = "alt_dhs_oaxaca_15_19_ci_detail_" +  "`country_code'" + ".csv"
local file_name_ci = file_name_ci[1]

gen plot_name = "alt_dhs_15_19_detail_" +  "`country_code'" + ".png"
local plot_name = plot_name[1]

** cd to outdir: change this date to today's date
cd "filepath"

* store coefficients
esttab test using "`file_name_coef'", cells(b)  replace
* store confidence intervals
esttab test using "`file_name_ci'", ci(4) replace

* regress pooled 
svy: regress any_birth_preg_2_yr_dhs  age educ_single_yrs curr_cohabit  unmet_need wealth_dummies2 wealth_dummies3 wealth_dummies4 wealth_dummies5 had_intercourse 

estimates store test_pooled

gen file_name_ci_test = "alt_dhs_oaxaca_15_19_ci_detail_test_" +  "`country_code'" + ".csv"
local file_name_ci_test = file_name_ci_test[1]

* store confidence intervals
esttab test_pooled using "`file_name_ci_test'", ci(4) replace

gen file_name_ci_pooled_table = "alt_dhs_oaxaca_15_19_ci_detail_pooled_table_" +  "`country_code'" + ".csv"
local file_name_ci_pooled_table = file_name_ci_pooled_table[1]

* store confidence intervals for methods appendix
esttab test_pooled using "`file_name_ci_pooled_table'", se star nogaps replace

* regress endline only
keep if baseline == 0
svy: regress any_birth_preg_2_yr_dhs  age educ_single_yrs curr_cohabit  unmet_need wealth_dummies2 wealth_dummies3 wealth_dummies4 wealth_dummies5 had_intercourse  

estimates store test_endline

gen file_name_ci_endline = "alt_dhs_oaxaca_15_19_ci_detail_endline_" +  "`country_code'" + ".csv"
local file_name_ci_endline = file_name_ci_endline[1]

* store confidence intervals
esttab test_endline using "`file_name_ci_endline'", ci(4) replace 

gen file_name_ci_endline_table = "alt_dhs_oaxaca_15_19_ci_detail_endline_table_" +  "`country_code'" + ".csv"
local file_name_ci_endline_table = file_name_ci_endline_table[1]

* store confidence intervals for methods appendix
esttab test_endline using "`file_name_ci_endline_table'", se star nogaps replace


* regress baseline only
	clear
	** change this filepath to in date 
	cd "filepath"

	use full_data.dta
	
	keep if country == "`country_code'"  // Keep only the data for the current country_code
	keep if age < 20 & age > 14 
	save "`country_code'_15_19.dta", replace  // Save the subset for the country
	use "`country_code'_15_19.dta", clear
	
	svyset psu_unique [pweight=pweight]
	
keep if baseline == 1
svy: regress any_birth_preg_2_yr_dhs  age educ_single_yrs  curr_cohabit  unmet_need wealth_dummies2 wealth_dummies3 wealth_dummies4 wealth_dummies5 had_intercourse  

** cd to outdir: change this date to today's date
cd "filepath"
estimates store test_baseline

gen file_name_ci_baseline = "alt_dhs_oaxaca_15_19_ci_detail_baseline_" +  "`country_code'" + ".csv"
local file_name_ci_baseline = file_name_ci_baseline[1]

* store confidence intervals
esttab test_baseline using "`file_name_ci_baseline'", ci(4) replace

gen file_name_ci_baseline_table = "alt_dhs_oaxaca_15_19_ci_detail_baseline_table_" +  "`country_code'" + ".csv"
local file_name_ci_baseline_table = file_name_ci_baseline_table[1]

* store confidence intervals for methods appendix
esttab test_baseline using "`file_name_ci_baseline_table'", se star nogaps replace
}


