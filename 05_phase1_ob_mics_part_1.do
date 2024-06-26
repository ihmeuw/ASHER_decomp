*==============================================================================
* Title: ASHER Oaxaca-Blinder analysis for MICS data endline in Malawi
* Date: March 21, 2024
* Description: This do file conducts an Oaxaca-Blinder analysis for Malawi using two years of data (baseline and endline): part 1 of two part model with outcome of sexual activity 
* with DHS data as the baseline and MICS data as the endline
*
* Data Source: DHS and MICS surveys 
* Notes: 
* - This analysis is meant for baseline and endline years only for Malawi
* - Ensure that in date and out date file paths are specified 
* - There is a separate .do file for running the analysis using DHS data for the endline for Malawi and 4 other countries 
*==============================================================================

clear

** change this filepath to in date 
cd "filepath"

import delimited ob_input_prepped_df_mics.csv
save full_data.dta, replace

egen country_round = concat(country year), punct(" ")

* for 15-24 
foreach country_code in  "mw" {
	clear
	** change this filepath to in date 
	cd "filepath"

	use full_data.dta
	
	keep if country == "`country_code'"  // Keep only the data for the current country_code
	save "`country_code'_15_24.dta", replace  // Save the subset for the country
	use "`country_code'_15_24.dta", clear
	
	svyset psu_unique [pweight=pweight]

oaxaca had_intercourse  age educ_single_yrs  curr_cohabit unmet_need wealth_dummies1 wealth_dummies2 wealth_dummies3 wealth_dummies4 wealth_dummies5, by(baseline) categorical(wealth_dummies1 wealth_dummies2 wealth_dummies3 wealth_dummies4 wealth_dummies5) weight(1) detail svy noisily relax


estimates store test

gen file_name_coef = "alt_mics_oaxaca_15_24_coef_detail_part_1_" +  "`country_code'" + ".csv"
local file_name_coef = file_name_coef[1]

gen file_name_ci = "alt_mics_oaxaca_15_24_ci_detail_part_1_" +  "`country_code'" + ".csv"
local file_name_ci = file_name_ci[1]

gen plot_name = "alt_mics_15_24_detail_part_1_" +  "`country_code'" + ".png"
local plot_name = plot_name[1]

** cd to outdir: change this date to today's date
cd "filepath"

* store coefficients
esttab test using "`file_name_coef'", cells(b)  replace
* store confidence intervals
esttab test using "`file_name_ci'", ci(4) replace

* regress pooled 

svy: regress had_intercourse  age educ_single_yrs  curr_cohabit unmet_need wealth_dummies2 wealth_dummies3 wealth_dummies4 wealth_dummies5


estimates store test_pooled

gen file_name_ci_test = "alt_mics_oaxaca_15_24_ci_detail_test_part_1_" +  "`country_code'" + ".csv"
local file_name_ci_test = file_name_ci_test[1]

* store confidence intervals
esttab test_pooled using "`file_name_ci_test'", ci(4) replace

* regress endline only
keep if baseline == 0

svy: regress had_intercourse  age educ_single_yrs  curr_cohabit unmet_need wealth_dummies2 wealth_dummies3 wealth_dummies4 wealth_dummies5

estimates store test_endline

gen file_name_ci_endline = "alt_mics_oaxaca_15_24_ci_detail_endline_part_1_" +  "`country_code'" + ".csv"
local file_name_ci_endline = file_name_ci_endline[1]

* store confidence intervals
esttab test_endline using "`file_name_ci_endline'", ci(4) replace

}


* for 15-19
foreach country_code in  "mw" {
	clear
	** change this filepath to in date 
	cd "filepath"

	use full_data.dta
	
	keep if country == "`country_code'"  // Keep only the data for the current country_code
	keep if age < 20 & age > 14 
	save "`country_code'_15_19.dta", replace  // Save the subset for the country
	use "`country_code'_15_19.dta", clear

	
	svyset psu_unique [pweight=pweight]

oaxaca had_intercourse  age educ_single_yrs  curr_cohabit unmet_need wealth_dummies1 wealth_dummies2 wealth_dummies3 wealth_dummies4 wealth_dummies5, by(baseline) categorical(wealth_dummies1 wealth_dummies2 wealth_dummies3 wealth_dummies4 wealth_dummies5) weight(1) detail svy noisily relax

estimates store test
*esttab using "Regression tables.csv", b(2) ci(4) compress label replace

gen file_name_coef = "alt_mics_oaxaca_15_19_coef_detail_part_1_" +  "`country_code'" + ".csv"
local file_name_coef = file_name_coef[1]

gen file_name_ci = "alt_mics_oaxaca_15_19_ci_detail_part_1_" +  "`country_code'" + ".csv"
local file_name_ci = file_name_ci[1]

gen plot_name = "alt_mics_15_19_detail_part_1_" +  "`country_code'" + ".png"
local plot_name = plot_name[1]

** cd to outdir: change this date to today's date
cd "filepath"

* store coefficients
esttab test using "`file_name_coef'", cells(b)  replace
* store confidence intervals
esttab test using "`file_name_ci'", ci(4) replace

* regress pooled 

svy: regress had_intercourse  age educ_single_yrs  curr_cohabit unmet_need wealth_dummies2 wealth_dummies3 wealth_dummies4 wealth_dummies5

estimates store test_pooled

gen file_name_ci_test = "alt_mics_oaxaca_15_19_ci_detail_test_part_1_" +  "`country_code'" + ".csv"
local file_name_ci_test = file_name_ci_test[1]

* store confidence intervals
esttab test_pooled using "`file_name_ci_test'", ci(4) replace

* regress endline only
keep if baseline == 0

svy: regress had_intercourse  age educ_single_yrs  curr_cohabit unmet_need wealth_dummies2 wealth_dummies3 wealth_dummies4 wealth_dummies5

estimates store test_endline

gen file_name_ci_endline = "alt_mics_oaxaca_15_19_ci_detail_endline_part_1_" +  "`country_code'" + ".csv"
local file_name_ci_endline = file_name_ci_endline[1]

* store confidence intervals
esttab test_endline using "`file_name_ci_endline'", ci(4) replace

}


