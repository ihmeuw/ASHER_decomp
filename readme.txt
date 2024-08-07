# IHME ASHER Decomposition

This code was written to conduct a decomposition analysis in support of the Adolescent Sexual and Reproductive Health and Rights Exemplars (ASHER) project. The duration of this project was from October 2023 to April 2024. For any questions or comments, please contact the project lead, Annie Haakenstad, at ahaak@uw.edu.

Supporting Members: Annie Haakenstad, Corinne Bintz, Olivia Angelino, Abby Chapin & Steve Lim

Order of operations:
01 scripts: data preparation; extract and clean variables from original DHS and MICS files
02 script: use Principal Component Analysis to construct country-specific wealth indices across years/surveys
03 scripts: merge prepared extraction files for DHS and MICS
04 scripts: prepare data for Oaxaca Blinder and cohort modeling, construct additional variables 
05 scripts: run and test various Oaxaca Blinder and cox proportional hazard cohort models