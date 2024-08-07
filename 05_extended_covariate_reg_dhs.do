*==============================================================================
* Title: ASHER Oaxaca-Blinder analysis for DHS data
* Date: May 1, 2024
* Description: This do file runs a regression for 4 countries using two years of data (baseline and endline) with an extended set of covariates
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

* for 15-19
foreach country_code in  "gh" "cm" "np" "rw" {
	clear
	** change this filepath to in date 
	cd "filepath"
	
	use full_data.dta
	keep if age < 20 & age > 14 

	keep if country == "`country_code'"  // Keep only the data for the current country_code
	svyset psu_unique [pweight=pweight]

	svy: regress any_birth_preg_2_yr_dhs  age educ_single_yrs curr_cohabit  unmet_need wealth_dummies2 wealth_dummies3 wealth_dummies4 wealth_dummies5 had_intercourse mcpr rural attend_school fp_exp_media knowledge_mod ideal_child desire_child_teen mean_yrs_schooling_head 
	estimates store test_pooled

	gen file_name_ci_pooled_table = "extended_dhs_oaxaca_15_19_ci_detail_pooled_table_" +  "`country_code'" + ".csv"
local file_name_ci_pooled_table = file_name_ci_pooled_table[1]

** change this filepath to out dir 
cd "filepath"

* store confidence intervals for methods appendix
esttab test_pooled using "`file_name_ci_pooled_table'", se star nogaps replace

}
