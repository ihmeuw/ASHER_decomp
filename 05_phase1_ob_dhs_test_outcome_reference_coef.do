*==============================================================================
* Title: ASHER Oaxaca-Blinder analysis for DHS data
* Author: NAME
* Description: This do file conducts an Oaxaca-Blinder analysis for 4 countries using two years of data (baseline and endline)
* Outcome A: (1) any birth in the last two years, (0) never pregnant 
* Outcome B: (1) first birth in the last two years, (0) never pregnant 
* Outcome C: (1) any birth in the last two years, (0) no birth in the last two years 
*
* Data Source: DHS surveys 
* Notes: 
* - This analysis is meant for baseline and endline years only for each country 
* - Ensure that in date and out date file paths are specified 
* - There is a separate .do file for running the analysis using MICS data for the endline for Malawi
*==============================================================================


*==============================================================================
** OUTCOME A *****
*==============================================================================

clear

** change this filepath to in date 
cd "FILEPATH"


import delimited ob_input_prepped_df_outcome_a_ob_dhs.csv
save full_data.dta, replace

* for 15-19
foreach country_code in  "gh" "cm" "np" "rw"  {
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

* Baseline reference endline coefficients
oaxaca outcome_a_dhs  age educ_single_yrs  curr_cohabit  unmet_need wealth_dummies1 wealth_dummies2 wealth_dummies3 wealth_dummies4 wealth_dummies5 had_intercourse_unmarried beating_just, by(baseline) categorical(wealth_dummies1 wealth_dummies2 wealth_dummies3 wealth_dummies4 wealth_dummies5) weight(1) detail svy noisily relax

estimates store test

gen file_name_coef = "alt_dhs_oaxaca_15_19_coef_detail_outcome_a_baseline_ref_endline_coef_" +  "`country_code'" + ".csv"
local file_name_coef = file_name_coef[1]

gen file_name_ci = "alt_dhs_oaxaca_15_19_ci_detail_outcome_a_baseline_ref_endline_coef_" +  "`country_code'" + ".csv"
local file_name_ci = file_name_ci[1]

gen plot_name = "alt_dhs_15_19_detail_outcome_a_baseline_ref_endline_coef_" +  "`country_code'" + ".png"
local plot_name = plot_name[1]

* store coefficients
esttab test using "`file_name_coef'", cells(b)  replace
* store confidence intervals
esttab test using "`file_name_ci'", ci(4) replace

* Baseline reference baseline coefficients
oaxaca outcome_a_dhs  age educ_single_yrs  curr_cohabit  unmet_need wealth_dummies1 wealth_dummies2 wealth_dummies3 wealth_dummies4 wealth_dummies5 had_intercourse_unmarried beating_just, by(baseline) categorical(wealth_dummies1 wealth_dummies2 wealth_dummies3 wealth_dummies4 wealth_dummies5) weight(0) detail svy noisily relax

estimates store test

replace file_name_coef = "alt_dhs_oaxaca_15_19_coef_detail_outcome_a_baseline_ref_baseline_coef_" +  "`country_code'" + ".csv"
local file_name_coef = file_name_coef[1]

replace file_name_ci = "alt_dhs_oaxaca_15_19_ci_detail_outcome_a_baseline_ref_baseline_coef_" +  "`country_code'" + ".csv"
local file_name_ci = file_name_ci[1]

replace plot_name = "alt_dhs_15_19_detail_outcome_a_baseline_ref_baseline_coef_" +  "`country_code'" + ".png"
local plot_name = plot_name[1]

* store coefficients
esttab test using "`file_name_coef'", cells(b)  replace
* store confidence intervals
esttab test using "`file_name_ci'", ci(4) replace

* Baseline reference pooled coefficients
oaxaca outcome_a_dhs  age educ_single_yrs  curr_cohabit  unmet_need wealth_dummies1 wealth_dummies2 wealth_dummies3 wealth_dummies4 wealth_dummies5 had_intercourse_unmarried beating_just, by(baseline) categorical(wealth_dummies1 wealth_dummies2 wealth_dummies3 wealth_dummies4 wealth_dummies5) pooled detail svy noisily relax

estimates store test

replace file_name_coef = "alt_dhs_oaxaca_15_19_coef_detail_outcome_a_baseline_ref_pooled_coef_" +  "`country_code'" + ".csv"
local file_name_coef = file_name_coef[1]

replace file_name_ci = "alt_dhs_oaxaca_15_19_ci_detail_outcome_a_baseline_ref_pooled_coef_" +  "`country_code'" + ".csv"
local file_name_ci = file_name_ci[1]

replace plot_name = "alt_dhs_15_19_detail_outcome_a_baseline_ref_pooled_coef_" +  "`country_code'" + ".png"
local plot_name = plot_name[1]

* store coefficients
esttab test using "`file_name_coef'", cells(b)  replace
* store confidence intervals
esttab test using "`file_name_ci'", ci(4) replace

* Endline reference endline coefficients
oaxaca outcome_a_dhs  age educ_single_yrs  curr_cohabit  unmet_need wealth_dummies1 wealth_dummies2 wealth_dummies3 wealth_dummies4 wealth_dummies5 had_intercourse_unmarried beating_just, by(endline) categorical(wealth_dummies1 wealth_dummies2 wealth_dummies3 wealth_dummies4 wealth_dummies5) weight(1) detail svy noisily relax

estimates store test

replace file_name_coef = "alt_dhs_oaxaca_15_19_coef_detail_outcome_a_endline_ref_endline_coef_" +  "`country_code'" + ".csv"
local file_name_coef = file_name_coef[1]

replace file_name_ci = "alt_dhs_oaxaca_15_19_ci_detail_outcome_a_endline_ref_endline_coef_" +  "`country_code'" + ".csv"
local file_name_ci = file_name_ci[1]

replace plot_name = "alt_dhs_15_19_detail_outcome_a_baseline_ref_endline_coef_" +  "`country_code'" + ".png"
local plot_name = plot_name[1]

* store coefficients
esttab test using "`file_name_coef'", cells(b)  replace
* store confidence intervals
esttab test using "`file_name_ci'", ci(4) replace

* Endline reference baseline coefficients
oaxaca outcome_a_dhs  age educ_single_yrs  curr_cohabit  unmet_need wealth_dummies1 wealth_dummies2 wealth_dummies3 wealth_dummies4 wealth_dummies5 had_intercourse_unmarried beating_just, by(endline) categorical(wealth_dummies1 wealth_dummies2 wealth_dummies3 wealth_dummies4 wealth_dummies5) weight(0) detail svy noisily relax

estimates store test

replace file_name_coef = "alt_dhs_oaxaca_15_19_coef_detail_outcome_a_endline_ref_baseline_coef_" +  "`country_code'" + ".csv"
local file_name_coef = file_name_coef[1]

replace file_name_ci = "alt_dhs_oaxaca_15_19_ci_detail_outcome_a_endline_ref_baseline_coef_" +  "`country_code'" + ".csv"
local file_name_ci = file_name_ci[1]

replace plot_name = "alt_dhs_15_19_detail_outcome_a_baseline_ref_endline_coef_" +  "`country_code'" + ".png"
local plot_name = plot_name[1]

* store coefficients
esttab test using "`file_name_coef'", cells(b)  replace
* store confidence intervals
esttab test using "`file_name_ci'", ci(4) replace

* Endline reference pooled coefficients
oaxaca outcome_a_dhs  age educ_single_yrs  curr_cohabit  unmet_need wealth_dummies1 wealth_dummies2 wealth_dummies3 wealth_dummies4 wealth_dummies5 had_intercourse_unmarried beating_just, by(endline) categorical(wealth_dummies1 wealth_dummies2 wealth_dummies3 wealth_dummies4 wealth_dummies5) pooled detail svy noisily relax

estimates store test

replace file_name_coef = "alt_dhs_oaxaca_15_19_coef_detail_outcome_a_endline_ref_pooled_coef_" +  "`country_code'" + ".csv"
local file_name_coef = file_name_coef[1]

replace file_name_ci = "alt_dhs_oaxaca_15_19_ci_detail_outcome_a_endline_ref_pooled_coef_" +  "`country_code'" + ".csv"
local file_name_ci = file_name_ci[1]

replace plot_name = "alt_dhs_15_19_detail_outcome_a_endline_ref_pooled_coef_" +  "`country_code'" + ".png"
local plot_name = plot_name[1]

* store coefficients
esttab test using "`file_name_coef'", cells(b)  replace
* store confidence intervals
esttab test using "`file_name_ci'", ci(4) replace

* regress endline only
keep if baseline == 0
svy: regress outcome_a_dhs  age educ_single_yrs  curr_cohabit  unmet_need wealth_dummies2 wealth_dummies3 wealth_dummies4 wealth_dummies5 had_intercourse_unmarried beating_just  

estimates store test_endline

gen file_name_ci_endline = "alt_dhs_oaxaca_15_19_ci_detail_endline_outcome_a_" +  "`country_code'" + ".csv"
local file_name_ci_endline = file_name_ci_endline[1]

* store confidence intervals
esttab test_endline using "`file_name_ci_endline'", ci(4) replace

gen file_name_ci_endline_table = "alt_dhs_oaxaca_15_19_ci_detail_endline_table_outcome_a_" +  "`country_code'" + ".csv"
local file_name_ci_endline_table = file_name_ci_endline_table[1]

* store confidence intervals for methods appendix
esttab test_endline using "`file_name_ci_endline_table'", se star nogaps replace

}

*==============================================================================
** OUTCOME B *****
*==============================================================================
clear

** change this filepath to in date 
cd "FILEPATH"

import delimited ob_input_prepped_df_outcome_b_ob_dhs.csv
save full_data.dta, replace

* for 15-19
foreach country_code in  "gh" "cm" "np" "rw"  {

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

* Baseline reference endline coefficients
oaxaca outcome_b_dhs  age educ_single_yrs  curr_cohabit  unmet_need wealth_dummies1 wealth_dummies2 wealth_dummies3 wealth_dummies4 wealth_dummies5 had_intercourse_unmarried beating_just, by(baseline) categorical(wealth_dummies1 wealth_dummies2 wealth_dummies3 wealth_dummies4 wealth_dummies5) weight(1) detail svy noisily relax

estimates store test

gen file_name_coef = "alt_dhs_oaxaca_15_19_coef_detail_outcome_b_baseline_ref_endline_coef_" +  "`country_code'" + ".csv"
local file_name_coef = file_name_coef[1]

gen file_name_ci = "alt_dhs_oaxaca_15_19_ci_detail_outcome_b_baseline_ref_endline_coef_" +  "`country_code'" + ".csv"
local file_name_ci = file_name_ci[1]

gen plot_name = "alt_dhs_15_19_detail_outcome_b_baseline_ref_endline_coef_" +  "`country_code'" + ".png"
local plot_name = plot_name[1]

* store coefficients
esttab test using "`file_name_coef'", cells(b)  replace
* store confidence intervals
esttab test using "`file_name_ci'", ci(4) replace

* Baseline reference baseline coefficients
oaxaca outcome_b_dhs  age educ_single_yrs  curr_cohabit  unmet_need wealth_dummies1 wealth_dummies2 wealth_dummies3 wealth_dummies4 wealth_dummies5 had_intercourse_unmarried beating_just, by(baseline) categorical(wealth_dummies1 wealth_dummies2 wealth_dummies3 wealth_dummies4 wealth_dummies5) weight(0) detail svy noisily relax

estimates store test

replace file_name_coef = "alt_dhs_oaxaca_15_19_coef_detail_outcome_b_baseline_ref_baseline_coef_" +  "`country_code'" + ".csv"
local file_name_coef = file_name_coef[1]

replace file_name_ci = "alt_dhs_oaxaca_15_19_ci_detail_outcome_b_baseline_ref_baseline_coef_" +  "`country_code'" + ".csv"
local file_name_ci = file_name_ci[1]

replace plot_name = "alt_dhs_15_19_detail_outcome_b_baseline_ref_baseline_coef_" +  "`country_code'" + ".png"
local plot_name = plot_name[1]

* store coefficients
esttab test using "`file_name_coef'", cells(b)  replace
* store confidence intervals
esttab test using "`file_name_ci'", ci(4) replace

* Baseline reference pooled coefficients
oaxaca outcome_b_dhs  age educ_single_yrs  curr_cohabit  unmet_need wealth_dummies1 wealth_dummies2 wealth_dummies3 wealth_dummies4 wealth_dummies5 had_intercourse_unmarried beating_just, by(baseline) categorical(wealth_dummies1 wealth_dummies2 wealth_dummies3 wealth_dummies4 wealth_dummies5) pooled detail svy noisily relax

estimates store test

replace file_name_coef = "alt_dhs_oaxaca_15_19_coef_detail_outcome_b_baseline_ref_pooled_coef_" +  "`country_code'" + ".csv"
local file_name_coef = file_name_coef[1]

replace file_name_ci = "alt_dhs_oaxaca_15_19_ci_detail_outcome_b_baseline_ref_pooled_coef_" +  "`country_code'" + ".csv"
local file_name_ci = file_name_ci[1]

replace plot_name = "alt_dhs_15_19_detail_outcome_b_baseline_ref_pooled_coef_" +  "`country_code'" + ".png"
local plot_name = plot_name[1]

* store coefficients
esttab test using "`file_name_coef'", cells(b)  replace
* store confidence intervals
esttab test using "`file_name_ci'", ci(4) replace

* Endline reference endline coefficients
oaxaca outcome_b_dhs  age educ_single_yrs  curr_cohabit  unmet_need wealth_dummies1 wealth_dummies2 wealth_dummies3 wealth_dummies4 wealth_dummies5 had_intercourse_unmarried beating_just, by(endline) categorical(wealth_dummies1 wealth_dummies2 wealth_dummies3 wealth_dummies4 wealth_dummies5) weight(1) detail svy noisily relax

estimates store test

replace file_name_coef = "alt_dhs_oaxaca_15_19_coef_detail_outcome_b_endline_ref_endline_coef_" +  "`country_code'" + ".csv"
local file_name_coef = file_name_coef[1]

replace file_name_ci = "alt_dhs_oaxaca_15_19_ci_detail_outcome_b_endline_ref_endline_coef_" +  "`country_code'" + ".csv"
local file_name_ci = file_name_ci[1]

replace plot_name = "alt_dhs_15_19_detail_outcome_b_baseline_ref_endline_coef_" +  "`country_code'" + ".png"
local plot_name = plot_name[1]

* store coefficients
esttab test using "`file_name_coef'", cells(b)  replace
* store confidence intervals
esttab test using "`file_name_ci'", ci(4) replace

* Endline reference baseline coefficients
oaxaca outcome_b_dhs  age educ_single_yrs  curr_cohabit  unmet_need wealth_dummies1 wealth_dummies2 wealth_dummies3 wealth_dummies4 wealth_dummies5 had_intercourse_unmarried beating_just, by(endline) categorical(wealth_dummies1 wealth_dummies2 wealth_dummies3 wealth_dummies4 wealth_dummies5) weight(0) detail svy noisily relax

estimates store test

replace file_name_coef = "alt_dhs_oaxaca_15_19_coef_detail_outcome_b_endline_ref_baseline_coef_" +  "`country_code'" + ".csv"
local file_name_coef = file_name_coef[1]

replace file_name_ci = "alt_dhs_oaxaca_15_19_ci_detail_outcome_b_endline_ref_baseline_coef_" +  "`country_code'" + ".csv"
local file_name_ci = file_name_ci[1]

replace plot_name = "alt_dhs_15_19_detail_outcome_b_baseline_ref_endline_coef_" +  "`country_code'" + ".png"
local plot_name = plot_name[1]

* store coefficients
esttab test using "`file_name_coef'", cells(b)  replace
* store confidence intervals
esttab test using "`file_name_ci'", ci(4) replace

* Endline reference pooled coefficients
oaxaca outcome_b_dhs  age educ_single_yrs  curr_cohabit  unmet_need wealth_dummies1 wealth_dummies2 wealth_dummies3 wealth_dummies4 wealth_dummies5 had_intercourse_unmarried beating_just, by(endline) categorical(wealth_dummies1 wealth_dummies2 wealth_dummies3 wealth_dummies4 wealth_dummies5) pooled detail svy noisily relax

estimates store test

replace file_name_coef = "alt_dhs_oaxaca_15_19_coef_detail_outcome_b_endline_ref_pooled_coef_" +  "`country_code'" + ".csv"
local file_name_coef = file_name_coef[1]

replace file_name_ci = "alt_dhs_oaxaca_15_19_ci_detail_outcome_b_endline_ref_pooled_coef_" +  "`country_code'" + ".csv"
local file_name_ci = file_name_ci[1]

replace plot_name = "alt_dhs_15_19_detail_outcome_b_endline_ref_pooled_coef_" +  "`country_code'" + ".png"
local plot_name = plot_name[1]

* store coefficients
esttab test using "`file_name_coef'", cells(b)  replace
* store confidence intervals
esttab test using "`file_name_ci'", ci(4) replace


* regress endline only
keep if baseline == 0
svy: regress outcome_b_dhs  age educ_single_yrs  curr_cohabit  unmet_need wealth_dummies2 wealth_dummies3 wealth_dummies4 wealth_dummies5 had_intercourse_unmarried beating_just  

estimates store test_endline

gen file_name_ci_endline = "alt_dhs_oaxaca_15_19_ci_detail_endline_outcome_b_" +  "`country_code'" + ".csv"
local file_name_ci_endline = file_name_ci_endline[1]

* store confidence intervals
esttab test_endline using "`file_name_ci_endline'", ci(4) replace

gen file_name_ci_endline_table = "alt_dhs_oaxaca_15_19_ci_detail_endline_table_outcome_b_" +  "`country_code'" + ".csv"
local file_name_ci_endline_table = file_name_ci_endline_table[1]

* store confidence intervals for methods appendix
esttab test_endline using "`file_name_ci_endline_table'", se star nogaps replace

}

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
	save "`country_code'_15_19.dta", replace  // Save the subset for the country
	use "`country_code'_15_19.dta", clear

	
	svyset psu_unique [pweight=pweight]

	** cd to outdir: change this date to today's date
cd "FILEPATH"

* Baseline reference endline coefficients
oaxaca outcome_c_dhs  age educ_single_yrs  curr_cohabit  unmet_need wealth_dummies1 wealth_dummies2 wealth_dummies3 wealth_dummies4 wealth_dummies5 had_intercourse_unmarried beating_just, by(baseline) categorical(wealth_dummies1 wealth_dummies2 wealth_dummies3 wealth_dummies4 wealth_dummies5) weight(1) detail svy noisily relax

estimates store test

gen file_name_coef = "alt_dhs_oaxaca_15_19_coef_detail_outcome_c_baseline_ref_endline_coef_" +  "`country_code'" + ".csv"
local file_name_coef = file_name_coef[1]

gen file_name_ci = "alt_dhs_oaxaca_15_19_ci_detail_outcome_c_baseline_ref_endline_coef_" +  "`country_code'" + ".csv"
local file_name_ci = file_name_ci[1]

gen plot_name = "alt_dhs_15_19_detail_outcome_c_baseline_ref_endline_coef_" +  "`country_code'" + ".png"
local plot_name = plot_name[1]

* store coefficients
esttab test using "`file_name_coef'", cells(b)  replace
* store confidence intervals
esttab test using "`file_name_ci'", ci(4) replace

* store coefficients
esttab test using "`file_name_coef'", cells(b)  replace
* store confidence intervals
esttab test using "`file_name_ci'", ci(4) replace

* Baseline reference baseline coefficients
oaxaca outcome_c_dhs  age educ_single_yrs  curr_cohabit  unmet_need wealth_dummies1 wealth_dummies2 wealth_dummies3 wealth_dummies4 wealth_dummies5 had_intercourse_unmarried beating_just, by(baseline) categorical(wealth_dummies1 wealth_dummies2 wealth_dummies3 wealth_dummies4 wealth_dummies5) weight(0) detail svy noisily relax

estimates store test

replace file_name_coef = "alt_dhs_oaxaca_15_19_coef_detail_outcome_c_baseline_ref_baseline_coef_" +  "`country_code'" + ".csv"
local file_name_coef = file_name_coef[1]

replace file_name_ci = "alt_dhs_oaxaca_15_19_ci_detail_outcome_c_baseline_ref_baseline_coef_" +  "`country_code'" + ".csv"
local file_name_ci = file_name_ci[1]

replace plot_name = "alt_dhs_15_19_detail_outcome_c_baseline_ref_baseline_coef_" +  "`country_code'" + ".png"
local plot_name = plot_name[1]

* store coefficients
esttab test using "`file_name_coef'", cells(b)  replace
* store confidence intervals
esttab test using "`file_name_ci'", ci(4) replace

* Baseline reference pooled coefficients
oaxaca outcome_c_dhs  age educ_single_yrs  curr_cohabit  unmet_need wealth_dummies1 wealth_dummies2 wealth_dummies3 wealth_dummies4 wealth_dummies5 had_intercourse_unmarried beating_just, by(baseline) categorical(wealth_dummies1 wealth_dummies2 wealth_dummies3 wealth_dummies4 wealth_dummies5) pooled detail svy noisily relax

estimates store test

replace file_name_coef = "alt_dhs_oaxaca_15_19_coef_detail_outcome_c_baseline_ref_pooled_coef_" +  "`country_code'" + ".csv"
local file_name_coef = file_name_coef[1]

replace file_name_ci = "alt_dhs_oaxaca_15_19_ci_detail_outcome_c_baseline_ref_pooled_coef_" +  "`country_code'" + ".csv"
local file_name_ci = file_name_ci[1]

replace plot_name = "alt_dhs_15_19_detail_outcome_c_baseline_ref_pooled_coef_" +  "`country_code'" + ".png"
local plot_name = plot_name[1]

* store coefficients
esttab test using "`file_name_coef'", cells(b)  replace
* store confidence intervals
esttab test using "`file_name_ci'", ci(4) replace

* Endline reference endline coefficients
oaxaca outcome_c_dhs  age educ_single_yrs  curr_cohabit  unmet_need wealth_dummies1 wealth_dummies2 wealth_dummies3 wealth_dummies4 wealth_dummies5 had_intercourse_unmarried beating_just, by(endline) categorical(wealth_dummies1 wealth_dummies2 wealth_dummies3 wealth_dummies4 wealth_dummies5) weight(1) detail svy noisily relax

estimates store test

replace file_name_coef = "alt_dhs_oaxaca_15_19_coef_detail_outcome_c_endline_ref_endline_coef_" +  "`country_code'" + ".csv"
local file_name_coef = file_name_coef[1]

replace file_name_ci = "alt_dhs_oaxaca_15_19_ci_detail_outcome_c_endline_ref_endline_coef_" +  "`country_code'" + ".csv"
local file_name_ci = file_name_ci[1]

replace plot_name = "alt_dhs_15_19_detail_outcome_c_baseline_ref_endline_coef_" +  "`country_code'" + ".png"
local plot_name = plot_name[1]

* store coefficients
esttab test using "`file_name_coef'", cells(b)  replace
* store confidence intervals
esttab test using "`file_name_ci'", ci(4) replace

* Endline reference baseline coefficients
oaxaca outcome_c_dhs  age educ_single_yrs  curr_cohabit  unmet_need wealth_dummies1 wealth_dummies2 wealth_dummies3 wealth_dummies4 wealth_dummies5 had_intercourse_unmarried beating_just, by(endline) categorical(wealth_dummies1 wealth_dummies2 wealth_dummies3 wealth_dummies4 wealth_dummies5) weight(0) detail svy noisily relax

estimates store test

replace file_name_coef = "alt_dhs_oaxaca_15_19_coef_detail_outcome_c_endline_ref_baseline_coef_" +  "`country_code'" + ".csv"
local file_name_coef = file_name_coef[1]

replace file_name_ci = "alt_dhs_oaxaca_15_19_ci_detail_outcome_c_endline_ref_baseline_coef_" +  "`country_code'" + ".csv"
local file_name_ci = file_name_ci[1]

replace plot_name = "alt_dhs_15_19_detail_outcome_c_baseline_ref_endline_coef_" +  "`country_code'" + ".png"
local plot_name = plot_name[1]

* store coefficients
esttab test using "`file_name_coef'", cells(b)  replace
* store confidence intervals
esttab test using "`file_name_ci'", ci(4) replace

* Endline reference pooled coefficients
oaxaca outcome_c_dhs  age educ_single_yrs  curr_cohabit  unmet_need wealth_dummies1 wealth_dummies2 wealth_dummies3 wealth_dummies4 wealth_dummies5 had_intercourse_unmarried beating_just, by(endline) categorical(wealth_dummies1 wealth_dummies2 wealth_dummies3 wealth_dummies4 wealth_dummies5) pooled detail svy noisily relax

estimates store test

replace file_name_coef = "alt_dhs_oaxaca_15_19_coef_detail_outcome_c_endline_ref_pooled_coef_" +  "`country_code'" + ".csv"
local file_name_coef = file_name_coef[1]

replace file_name_ci = "alt_dhs_oaxaca_15_19_ci_detail_outcome_c_endline_ref_pooled_coef_" +  "`country_code'" + ".csv"
local file_name_ci = file_name_ci[1]

replace plot_name = "alt_dhs_15_19_detail_outcome_c_endline_ref_pooled_coef_" +  "`country_code'" + ".png"
local plot_name = plot_name[1]

* store coefficients
esttab test using "`file_name_coef'", cells(b)  replace
* store confidence intervals
esttab test using "`file_name_ci'", ci(4) replace

* regress pooled
svy: regress outcome_c_dhs  age educ_single_yrs  curr_cohabit  unmet_need wealth_dummies2 wealth_dummies3 wealth_dummies4 wealth_dummies5 had_intercourse_unmarried beating_just  

estimates store test_pooled

gen file_name_ci_pooled = "alt_dhs_oaxaca_15_19_ci_detail_pooled_outcome_c_" +  "`country_code'" + ".csv"
local file_name_ci_pooled = file_name_ci_pooled[1]

* store confidence intervals
esttab test_pooled using "`file_name_ci_pooled'", ci(4) replace

* regress endline only
keep if baseline == 0
svy: regress outcome_c_dhs  age educ_single_yrs  curr_cohabit  unmet_need wealth_dummies2 wealth_dummies3 wealth_dummies4 wealth_dummies5 had_intercourse_unmarried beating_just  

estimates store test_endline

gen file_name_ci_endline = "alt_dhs_oaxaca_15_19_ci_detail_endline_outcome_c_" +  "`country_code'" + ".csv"
local file_name_ci_endline = file_name_ci_endline[1]

* store confidence intervals
esttab test_endline using "`file_name_ci_endline'", ci(4) replace

gen file_name_ci_endline_table = "alt_dhs_oaxaca_15_19_ci_detail_endline_table_outcome_c_" +  "`country_code'" + ".csv"
local file_name_ci_endline_table = file_name_ci_endline_table[1]

* store confidence intervals for methods appendix
esttab test_endline using "`file_name_ci_endline_table'", se star nogaps replace

* regress baseline only to compare coefficients to endline 
cd "FILEPATH"
use "`country_code'_15_19.dta", clear
svyset psu_unique [pweight=pweight]

keep if baseline == 1
svy: regress outcome_c_dhs  age educ_single_yrs  curr_cohabit  unmet_need wealth_dummies2 wealth_dummies3 wealth_dummies4 wealth_dummies5 had_intercourse_unmarried beating_just  

estimates store test_baseline

gen file_name_ci_baseline = "alt_dhs_oaxaca_15_19_ci_detail_baseline_outcome_c_" +  "`country_code'" + ".csv"
local file_name_ci_baseline = file_name_ci_baseline[1]

** cd to outdir: change this date to today's date
cd "FILEPATH"

* store confidence intervals
esttab test_baseline using "`file_name_ci_baseline'", ci(4) replace

gen file_name_ci_baseline_table = "alt_dhs_oaxaca_15_19_ci_detail_baseline_table_outcome_c_" +  "`country_code'" + ".csv"
local file_name_ci_baseline_table = file_name_ci_baseline_table[1]

* store confidence intervals for methods appendix
esttab test_baseline using "`file_name_ci_baseline_table'", se star nogaps replace


}
