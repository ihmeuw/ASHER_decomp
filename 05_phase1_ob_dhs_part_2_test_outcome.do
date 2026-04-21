*==============================================================================
* Title: ASHER Oaxaca-Blinder analysis for DHS data
* Author: NAME
* Description: This do file conducts an Oaxaca-Blinder analysis for 4 countries using two years of data (baseline and endline): part 2, outcome of any birth or pregnancy in the last two years among women who have had sex 
*
* Data Source: DHS surveys 
* Notes: 
* - This analysis is meant for baseline and endline years only for each country 
* - Ensure that in date and out date file paths are specified 
* - There is a separate .do file for running the analysis using MICS data for the endline for Malawi
*==============================================================================


*==============================================================================
** OUTCOME C *****
*==============================================================================
clear

** change this filepath to in date 
cd "FILEPATH"

import delimited ob_input_prepped_df_outcome_c_ob_dhs.csv
save full_data.dta, replace

* for 15-19
foreach country_code in   "gh" "cm" "np" "rw"  {
	clear
	** change this filepath to in date 
	cd "FILEPATH"

	use full_data.dta
	
	keep if country == "`country_code'"  // Keep only the data for the current country_code
	keep if age < 20 & age > 14 
	keep if had_intercourse == 1 // keep only women who have had sex 
	save "`country_code'_15_19.dta", replace  // Save the subset for the country
	use "`country_code'_15_19.dta", clear

	
	svyset psu_unique [pweight=pweight]

oaxaca outcome_c_dhs  age educ_single_yrs age_1st_sex_imp  curr_cohabit  unmet_need wealth_dummies1 wealth_dummies2 wealth_dummies3 wealth_dummies4 wealth_dummies5 beating_just, by(baseline) categorical(wealth_dummies1 wealth_dummies2 wealth_dummies3 wealth_dummies4 wealth_dummies5) weight(1) detail svy noisily relax

estimates store test

gen file_name_coef = "alt_dhs_oaxaca_15_19_coef_detail_part_2_outcome_c_" +  "`country_code'" + ".csv"
local file_name_coef = file_name_coef[1]

gen file_name_ci = "alt_dhs_oaxaca_15_19_ci_detail_part_2_outcome_c_" +  "`country_code'" + ".csv"
local file_name_ci = file_name_ci[1]

** cd to outdir: change this date to today's date
cd "FILEPATH"

* store coefficients
esttab test using "`file_name_coef'", cells(b)  replace
* store confidence intervals
esttab test using "`file_name_ci'", ci(4) replace

** logit 
oaxaca outcome_c_dhs  age educ_single_yrs age_1st_sex_imp  curr_cohabit  unmet_need wealth_dummies1 wealth_dummies2 wealth_dummies3 wealth_dummies4 wealth_dummies5 beating_just, by(baseline) categorical(wealth_dummies1 wealth_dummies2 wealth_dummies3 wealth_dummies4 wealth_dummies5) weight(1) detail svy noisily relax logit

estimates store test

replace file_name_coef = "alt_dhs_oaxaca_15_19_coef_detail_part_2_outcome_c_logit_" +  "`country_code'" + ".csv"
local file_name_coef = file_name_coef[1]

replace file_name_ci = "alt_dhs_oaxaca_15_19_ci_detail_part_2_outcome_c_logit_" +  "`country_code'" + ".csv"
local file_name_ci = file_name_ci[1]

* store coefficients
esttab test using "`file_name_coef'", cells(b)  replace
* store confidence intervals
esttab test using "`file_name_ci'", ci(4) replace


* regress endline only
keep if baseline == 0
svy: regress outcome_c_dhs  age educ_single_yrs age_1st_sex_imp curr_cohabit  unmet_need wealth_dummies2 wealth_dummies3 wealth_dummies4 wealth_dummies5 beating_just  

estimates store test_endline

gen file_name_ci_endline = "alt_dhs_oaxaca_15_19_ci_detail_endline_part_2_outcome_c_" +  "`country_code'" + ".csv"
local file_name_ci_endline = file_name_ci_endline[1]

* store confidence intervals
esttab test_endline using "`file_name_ci_endline'", ci(4) replace

* logit
svy: logit outcome_c_dhs  age educ_single_yrs age_1st_sex_imp curr_cohabit  unmet_need wealth_dummies2 wealth_dummies3 wealth_dummies4 wealth_dummies5 beating_just  

estimates store test_endline_logit

gen file_name_ci_endline_logit = "alt_dhs_oaxaca_15_19_ci_detail_endline_part_2_outcome_c_logit_" +  "`country_code'" + ".csv"
local file_name_ci_endline_logit = file_name_ci_endline_logit[1]

* store confidence intervals
esttab test_endline_logit using "`file_name_ci_endline_logit'", ci(4) replace

}



