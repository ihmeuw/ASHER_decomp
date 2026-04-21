*==============================================================================
* Title: ASHER Oaxaca-Blinder analysis for DHS data
* Author: NAME (adapted from NAME)
* Description: This do file conducts an Oaxaca-Blinder analysis for 4 countries using two years of data (baseline and endline)
*
* Data Source: DHS surveys 
* Notes: 
* - This analysis is meant for baseline and endline years only for each country 
* - Ensure that in date and out date file paths are specified 
* - There is a separate .do file for running the analysis using MICS data for the endline for Malawi
*==============================================================================


** OUTCOME B

clear

** change this filepath to in date 
cd "FILEPATH"

import delimited ob_input_prepped_df_outcome_b_dhs.csv
save full_data.dta, replace

* for 15-19
foreach country_code in  "gh" "mw" "np" "rw"  {
	clear
	** change this filepath to in date 
	cd "FILEPATH"

	use full_data.dta
	
	keep if country == "`country_code'"  // Keep only the data for the current country_code
	keep if age < 20 & age > 14 
	save "`country_code'_15_19.dta", replace  // Save the subset for the country
	use "`country_code'_15_19.dta", clear

	
	svyset psu_unique [pweight=pweight]

** cd to outdir: change this date to today's date
cd "FILEPATH"

* regress endline only
keep if baseline == 0
svy: regress outcome_b_dhs  age educ_single_yrs curr_cohabit mod_contra_outcome_b wealth_dummies2 wealth_dummies3 wealth_dummies4 wealth_dummies5 had_intercourse_unmarried beating_just  

estimates store test_endline

gen file_name_ci_endline = "outcome_b_dhs_oaxaca_15_19_ci_detail_endline_contra_" +  "`country_code'" + ".csv"
local file_name_ci_endline = file_name_ci_endline[1]

* store confidence intervals
esttab test_endline using "`file_name_ci_endline'", ci(4) replace 

gen file_name_ci_endline_table = "outcome_b_dhs_oaxaca_15_19_ci_detail_endline_table_contra_" +  "`country_code'" + ".csv"
local file_name_ci_endline_table = file_name_ci_endline_table[1]

* store confidence intervals for methods appendix
esttab test_endline using "`file_name_ci_endline_table'", se star nogaps replace
}

** OUTCOME C

clear

** change this filepath to in date 
cd "FILEPATH"

import delimited ob_input_prepped_df_outcome_c_dhs.csv
save full_data.dta, replace

* for 15-19
foreach country_code in  "gh" "mw" "np" "rw"  {
	clear
	** change this filepath to in date 
	cd "FILEPATH"

	use full_data.dta
	
	keep if country == "`country_code'"  // Keep only the data for the current country_code
	keep if age < 20 & age > 14 
	save "`country_code'_15_19.dta", replace  // Save the subset for the country
	use "`country_code'_15_19.dta", clear

	
	svyset psu_unique [pweight=pweight]

** cd to outdir: change this date to today's date
cd "FILEPATH"

* regress endline only
keep if baseline == 0
svy: regress outcome_c_dhs  age educ_single_yrs curr_cohabit mod_contra_outcome_c wealth_dummies2 wealth_dummies3 wealth_dummies4 wealth_dummies5 had_intercourse_unmarried beating_just  

estimates store test_endline

gen file_name_ci_endline = "outcome_c_dhs_oaxaca_15_19_ci_detail_endline_contra_" +  "`country_code'" + ".csv"
local file_name_ci_endline = file_name_ci_endline[1]

* store confidence intervals
esttab test_endline using "`file_name_ci_endline'", ci(4) replace 

gen file_name_ci_endline_table = "outcome_c_dhs_oaxaca_15_19_ci_detail_endline_table_contra_" +  "`country_code'" + ".csv"
local file_name_ci_endline_table = file_name_ci_endline_table[1]

* store confidence intervals for methods appendix
esttab test_endline using "`file_name_ci_endline_table'", se star nogaps replace
}
