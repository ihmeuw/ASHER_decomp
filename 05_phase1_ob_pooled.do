*==============================================================================
* Title: ASHER Oaxaca-Blinder analysis for DHS and MICS outcomes using pooled data
* Date: June 26, 2024
* Description: This do file conducts an Oaxaca-Blinder analysis for 5 countries using pooled data

*==============================================================================

clear

** change this filepath to in date 
cd "filepath"

import delimited ob_input_prepped_df_pooled_dhs.csv
save full_data.dta, replace

** run DHS outcome for all countries except Malawi  
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

	* regress pooled 
	svy: regress any_birth_preg_2_yr_dhs  age educ_single_yrs curr_cohabit  unmet_need wealth_dummies2 wealth_dummies3 wealth_dummies4 wealth_dummies5 had_intercourse beating_just 

	estimates store test_pooled

	* change to outdir (today's date')
  cd "filepath"
	
	gen file_name_ci_test = "dhs_oaxaca_15_19_pooled_reg_" +  "`country_code'" + ".csv"
	local file_name_ci_test = file_name_ci_test[1]

	* store confidence intervals
	esttab test_pooled using "`file_name_ci_test'", ci(4) replace

}


** run MICS outcome for Malawi  
	clear

	** change this filepath to in date 
	cd "filepath"

	import delimited ob_input_prepped_df_pooled_mics.csv
	save full_data.dta, replace
	use full_data.dta

	keep if country == "mw"  // Keep only the data for the current country_code
	keep if age < 20 & age > 14 
	save "mw_15_19.dta", replace  // Save the subset for the country
	use "mw_15_19.dta", clear

	svyset psu_unique [pweight=pweight]

	* regress pooled 
	svy: regress any_birth_preg_2_yr_mics  age educ_single_yrs curr_cohabit  unmet_need wealth_dummies2 wealth_dummies3 wealth_dummies4 wealth_dummies5 had_intercourse beating_just 

	estimates store test_pooled
	
	* change to outdir (today's date')
cd "filepath"
	
	gen file_name_ci_test = "mics_oaxaca_15_19_pooled_reg_mw.csv" 
	local file_name_ci_test = file_name_ci_test[1]

	* store confidence intervals
	esttab test_pooled using "`file_name_ci_test'", ci(4) replace

