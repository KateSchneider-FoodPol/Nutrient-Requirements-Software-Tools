/* 
Kate Schneider (kate.schneider@tufts.edu)
Last modified: 26 November 2020
Purpose: Software tools to compile DRI nutrient requirements recalculated for
	WHO growth references and with optional adjustment if including children 6-23 
	months.	The final data shape is strucutured appropriately as the nutrient requirement
	inputs to the protocol for calculating the Cost of Nutrient Adequacy (CoNA),
	described in the companion software tools article Bai et al (2020).
Supporting Explanatory Documentation: 
	o Schneider, K. & Herforth, A. 2020. "Software tools for practical application 
		of human nutrient requirements in food-based social science research". 
		Gates Open Research.
	o Input datasets "Notes" files
Input data: 
	Supplement 1: 1_NutrientRequirements_Notes
	Supplement 2: 2_WHOGrowthCharts_Notes
	Supplement 3: 3_NutrientRequirements6-23months_Notes
Output datasets: 
	EAR+UL+AMDR+CDRR_allgroups
	EAR+UL+AMDR+CDRR+BF_allgroups
	EAR+UL+AMDR+CDRR+BF+WHOGrowth_allgroups
	Part 5 gives example code to extract the nutrient requirements for a single
		age-sex group at a specific level of physical activity. This is produces
		the data in a format suited to merging with the food prices and composition 
		data in preparation for linear programming in the CoNA protocol.
		
Licence:
Copyright 2020 Kate Schneider

   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at

     http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License.
		
*/

// PART 0 // FILE MANAGEMENT
global myfiles "yourfilepath"

* Working directory
cd "$myfiles"
cap log close
log using "Nut Reqts_`c(current_date)'", replace

***Log of Nutrient Requirements Compilation
di "`c(current_date)' `c(current_time)'"

// PART 1 // Import and reshape

* Micronutrients EAR 
clear 
insheet using "$myfiles\1_NutrientRequirements_MicronutrientsEAR.csv", case
	// Rename variables (nutr_no follows the "Units_Notes" sheet)
	// Generate variables for nutrients without EAR
	gen ear1=.
	gen ear2=.
	gen ear3=.
	gen ear4=.
	gen ear5=.
	rename vitA ear6
	gen ear7=.
	rename vitC ear8
	rename vitE ear9
	rename thiamin ear10
	rename riboflavin ear11
	rename niacin ear12
	rename vitB6 ear13
	rename folate ear14
	rename vitB12 ear15
	rename calcium ear16
	rename copper ear17
	rename iron ear18
	rename magnesium ear19
	rename phosphorus ear20
	rename selenium ear21
	rename zinc ear22
	rename sodium ear23

	// Make sure all variables are numeric
	describe
	foreach v of varlist ear* {
		destring `v', replace
		}
	// Reshape long
	reshape long ear, i(lifestage sex age_lower_yrs age_upper_yrs) j(nutr_no)
	replace ear=. if nutr_no==3 // carbohydrates are repeated in macronutrients
	tempfile Micronutrients_EAR
		save `Micronutrients_EAR', replace

* Micronutrients UL 
clear
insheet using "$myfiles\1_NutrientRequirements_MicronutrientsUL.csv", case
	// Rename variables (nutr_no follows the "Units_Notes" sheet)
	// Generate as missing for nutrients without UL
	forval i=1/5 {
		gen ul`i'=.
		}
	rename vitA ul6
	rename retinol ul7
	rename vitC ul8
	rename vitE ul9
	rename thiamin ul10
	rename riboflavin ul11
	rename niacin ul12
	rename vitB6 ul13
	rename folate ul14
	rename vitB12 ul15
	rename calcium ul16
	rename copper ul17
	rename iron ul18
	rename magnesium ul19
	rename phosphorus ul20
	rename selenium ul21
	rename zinc ul22
	rename sodium ul23

	// Make sure all variables are numeric
	describe
	forval i=6/23 {
		destring ul`i', replace
		}
	// Reshape to long form
	reshape long ul, i(age_sex_grp) j(nutr_no)
keep age_sex_grp nutr_no ul
tempfile Micronutrients_UL
	save `Micronutrients_UL', replace

* Macronutrients EAR 
clear 
insheet using "$myfiles\1_NutrientRequirements_MacronutrientsEAR.csv", case
drop lifestage sex age_lower_yrs age_upper_yrs
rename refweight_kg refweight_protein_kg
gen nutr_no=4
tempfile Macronutrients_EAR
	save `Macronutrients_EAR', replace

* Macronutrients AMDR
clear
insheet using "$myfiles\1_NutrientRequirements_MacronutrientsAMDR.csv", case
cap drop if age_sex_grp==.
	// Rename variables to reshape long
	rename lipids_lower amdr_lower5
	rename lipids_upper amdr_upper5
	rename carbohydrate_lower amdr_lower3
	rename carbohydrate_upper amdr_upper3
	rename protein_lower amdr_lower4
	rename protein_upper amdr_upper4
	reshape long amdr_lower amdr_upper, i(age_sex_grp lifestage sex age_lower_yrs age_upper_yrs) ///
		j(nutr_no)
keep age_sex_grp nutr_no amdr_lower amdr_upper
tempfile Macronutrients_AMDR
	save `Macronutrients_AMDR', replace

* Energy
clear
insheet using "$myfiles\1_NutrientRequirements_Energy.csv", case
gen nutr_no=2
encode PA_level, gen(pal)
	tab pal
	tab pal, nolabel
	recode pal (1=3) (2=2) (3=1) (4=4)
		cap lab drop pal
		lab def pal ///
		1 "Sedentary" ///
		2 "Low active" ///
		3 "Active" ///
		4 "Very active" 
	lab val pal pal
	tab pal
keep age_sex_grp energy nutr_no pal
order age_sex_grp nutr_no pal energy, first
sort age_sex_grp nutr_no pal
tempfile Energy
	save `Energy', replace

* Units Notes - imported long by nutrient, merge with UL keeping relevant variables only
clear
insheet using "$myfiles\1_NutrientRequirements_UnitsNotes.csv", case
keep nutr_no ul_note unit amdr_unit
merge m:m nutr_no using `Micronutrients_UL'
drop _merge
order age_sex_grp nutr_no unit ul, first
	/* NOTE: the sodium UL is the not a Tolerable Upper Intake Level (UL), it is the 
		Chronic Disease Risk Reduction (CDRR) level as established in the 2019 
		update to the DRIs for sodium and potassium (IOM 2019). 
		However it functions in practice as an upper bound constraint for the 
		definition of healthy and adequate diets and therefore is included with 
		the ULs for analytical convenience.
		*/
tempfile Micronutrients_UL
	save `Micronutrients_UL', replace

// PART 2 // MERGE ALL
clear
* Merge datasets except energy
	use `Micronutrients_EAR'
	// Micronutrients ULs
	merge 1:1 age_sex_grp nutr_no using `Micronutrients_UL'
	distinct age_sex_grp nutr_no
	drop _merge
	// Macronutrients EAR
	merge m:1 age_sex_grp nutr_no using `Macronutrients_EAR'
	drop _merge 
		foreach v in protein carbohydrate{
			destring `v', replace
			}
		replace ear=protein if nutr_no==4
		replace ear=carbohydrate if nutr_no==3
	// Macronutrients AMDR
		* Note the macronutrients unit is % total calories
	merge m:1 age_sex_grp nutr_no using `Macronutrients_AMDR'
		di 25*3 // Check - should be matched
	drop _merge
	sort age_sex_grp nutr_no 
	distinct age_sex_grp nutr_no
	// Energy
	merge m:m age_sex_grp nutr_no using `Energy'
	drop _merge
	order age_sex_grp nutr_no pal ear ul protein amdr_lower amdr_upper energy, first
	sort age_sex_grp nutr_no pal
		* Expand 
		expand 4 if inlist(nutr_no,3,4,5)
		sort age_sex_grp nutr_no pal
		bys age_sex_grp nutr_no: egen v1=seq()
		forval i=1/4 {
			replace pal=`i' if (v1==`i' & inlist(nutr_no,3,4,5))
			}
		destring energy, replace
		bys age_sex_grp: replace energy=energy[_n-4] if (energy==. & nutr_no==3)
		bys age_sex_grp: replace energy=energy[_n-8] if (energy==. & nutr_no==4)
		bys age_sex_grp: replace energy=energy[_n-12] if (energy==. & nutr_no==5)
			list age_sex_grp energy nutr_no if inlist(nutr_no,2,3,4,5) // Check
		drop v1 protein_perkg refweight_protein_kg
		* Replace missing EAR with value of energy
		replace ear=energy if ear==. & nutr_no==2

* Replace AMDRs as grams of each macronutrient, per PAL
	* Note: AMDRs are currently expressed as a percent
	foreach v in amdr_lower amdr_upper {
		destring `v', replace
		}
	// Convert to percent and multiply by total energy
	gen amdr_kcal_lower=energy*amdr_lower/100 if inlist(nutr_no,3,4,5)
	gen amdr_kcal_upper=energy*amdr_upper/100 if inlist(nutr_no,3,4,5)
	// Divide the total calories per macronutrient by the kcal per gram
		* Protein and Carbohydrates have 4 calories per gram
		* Lipids have 9 calories per gram
	replace amdr_lower=amdr_kcal_lower/4 if inlist(nutr_no,3,4)
	replace amdr_lower=amdr_kcal_lower/9 if nutr_no==5
	replace amdr_upper=amdr_kcal_upper/4 if inlist(nutr_no,3,4)
	replace amdr_upper=amdr_kcal_upper/9 if nutr_no==5
	replace amdr_lower=. if !inlist(nutr_no,3,4,5)
	replace amdr_upper=. if !inlist(nutr_no,3,4,5)
	// Result: total grams 
	replace amdr_unit="g" if inlist(nutr_no,3,4,5)
	
* Make sure all variables are numeric
describe
encode lifestage, gen(lifestage2)
	drop lifestage
	rename lifestage2 lifestage
tab sex
encode sex, gen(sex2)
	tab sex2, nolabel
	drop sex
	rename sex2 sex
	tab sex
	recode sex (2=0)
	replace sex=. if sex==3
	tab sex
	lab def sex 0 "Male" 1 "Female"
	lab val sex sex
foreach v in age_lower_yrs age_upper_yrs {
	destring `v', replace
	}
* Order and Sort
drop protein carbohydrate energy amdr_kcal_lower amdr_kcal_upper
order age_sex_grp nutr_no pal ear ul amdr_lower amdr_upper , first
sort age_sex_grp nutr_no pal
	
* Label nutrients
gen nutr="."
replace nutr="Price" if nutr_no==1
replace nutr="Energy" if nutr_no==2
replace nutr="Carbohydrate" if nutr_no==3
replace nutr="Protein" if nutr_no==4
replace nutr="Lipids" if nutr_no==5
replace nutr="Vit_A" if nutr_no==6
replace nutr="Retinol" if nutr_no==7
replace nutr="Vit_C" if nutr_no==8
replace nutr="Vit_E" if nutr_no==9
replace nutr="Thiamin" if nutr_no==10
replace nutr="Riboflavin" if nutr_no==11
replace nutr="Niacin" if nutr_no==12
replace nutr="Vit_B6" if nutr_no==13
replace nutr="Folate" if nutr_no==14
replace nutr="Vit_B12" if nutr_no==15
replace nutr="Calcium" if nutr_no==16
replace nutr="Copper" if nutr_no==17
replace nutr="Iron" if nutr_no==18
replace nutr="Magnesium" if nutr_no==19
replace nutr="Phosphorus" if nutr_no==20
replace nutr="Selenium" if nutr_no==21
replace nutr="Zinc" if nutr_no==22
replace nutr="Sodium" if nutr_no==23
order nutr, after(nutr_no)

replace ear=0 if nutr=="Price"

order age_sex_grp nutr_no nutr ear ul amdr_lower amdr_upper pal unit, first
save EAR+UL+AMDR+CDRR_allgroups, replace

// PART 3 // USE WHO GROWTH CHARTS BASED REFERENCE WEIGHTS 
clear
insheet using "$myfiles\2_WHOGrowthCharts_EnergyEER.csv", case 
keep age_sex_grp PA_level who_energy iom_energy
encode PA_level, gen(pal)
recode pal (1=3) (3=1)
drop PA_level
lab drop pal
destring who_energy, replace
tab pal, sum(who_energy)
gen nutr_no=2
tempfile whoenergy
	save `whoenergy'
	
clear
insheet using "$myfiles\2_WHOGrowthCharts_ProteinEAR.csv", case 
keep age_sex_grp who_protein
gen nutr_no=4
tempfile whoprotein
	save `whoprotein'

use EAR+UL+AMDR+CDRR_allgroups, clear
merge m:1 age_sex_grp nutr_no pal using `whoenergy'
drop _merge
merge m:1 age_sex_grp nutr_no using `whoprotein'
destring _all, replace
replace ear=who_energy if nutr_no==2
replace ear=who_protein if nutr_no==4
drop _merge who_* iom*

save EAR+UL+AMDR+CDRR_WHOgrowthref_allgroups, replace

// PART 4 // ADJUSTMENTS FOR THE CONTRIBUTION OF BREASTMILK 
/* If calculating nutrient requirements for infants 6-23 months still breastfeeding,
		adjustment must be made to reduce nutrient requirements to only the percentage 
		required from food
*/

clear
insheet using "$myfiles\3_NutrientRequirements6-23months_6-23moFoodNeeds.csv", case 
keep age_sex_grp nutr_no pctfromfood bf_match_id
tempfile bf
	save `bf'

* Using DRI reference weights
	use EAR+UL+AMDR+CDRR_allgroups, clear
	expand 2 if age_sex_grp==2, gen(bf_match_id)
		replace bf_match_id=. if !inlist(age_sex_grp,2,3)
		recode bf_match_id (0=1) (1=2)
	expand 2 if age_sex_grp==3, gen(group3split)
		replace bf_match_id=3 if age_sex_grp==3 & group3split==0
		replace bf_match_id=. if age_sex_grp==3 & group3split==1
		drop group3split
		tab2 age_sex_grp bf_match_id // check
	merge m:1 age_sex_grp bf_match_id nutr_no using `bf'
		drop _merge

	* Reduce needs of children 6-23 months to only what is required from food
		// Note WHO recommends continued breastfeeding through 2 years
		replace pctfromfood=. if !inlist(age_sex_grp,2,3)
		replace ear=ear*(pctfromfood/100) if pctfromfood!=. & inlist(bf_match_id,1,2,3)
		
	save EAR+UL+AMDR+CDRR+BF_allgroups, replace

* Using WHO Growth Charts for Reference Weights
	use EAR+UL+AMDR+CDRR_WHOgrowthref_allgroups, clear
	expand 2 if age_sex_grp==2, gen(bf_match_id)
		replace bf_match_id=. if !inlist(age_sex_grp,2,3)
		recode bf_match_id (0=1) (1=2)
	expand 2 if age_sex_grp==3, gen(group3split)
		replace bf_match_id=3 if age_sex_grp==3 & group3split==0
		replace bf_match_id=. if age_sex_grp==3 & group3split==1
		drop group3split
		tab2 age_sex_grp bf_match_id // check
	merge m:1 age_sex_grp bf_match_id nutr_no using `bf'
		drop _merge

	* Reduce needs of children 6-23 months to only what is required from food
		// Note WHO recommends continued breastfeeding through 2 years
		replace pctfromfood=. if !inlist(age_sex_grp,2,3)
		replace ear=ear*(pctfromfood/100) if pctfromfood!=. & inlist(bf_match_id,1,2,3)
		drop pctfromfood
		
	save EAR+UL+AMDR+CDRR+BF_WHOgrowthref_allgroups, replace
	
// PART 5 // EXTRACT INDIVIDUAL DATA FILES 
* To import for linear programming:
* Extract separate data files for each age-sex group at desired PAL
	* Example using Group 16 Adult Female (age range: 19-30), DRI Reference Weights and PAL=3
	use "$myfiles\EAR+UL+AMDR+CDRR_allgroups", clear
		keep if age_sex_grp==16
		tab nutr
		drop if inlist(pal,1,2,4)
		tab nutr_no
		keep nutr_no nutr ear ul amdr_lower amdr_upper unit ul_note
		save NR16_DRIWeight_PAL3, replace

	* Example using Group 10 Adult Male (age range: 19-30), WHO Reference Weights and PAL=4 (developing country farmer)
	use "$myfiles\EAR+UL+AMDR+CDRR_WHOgrowthref_allgroups", clear
		keep if age_sex_grp==10
		tab nutr
		drop if inlist(pal,1,2,3)
		tab nutr_no
		keep nutr_no nutr ear ul amdr_lower amdr_upper unit ul_note
		save NR10_WHOWeight_PAL4, replace

	* Example using Group 3 (Infants 12-23 months still breastfeeding), WHO Reference Weights and breastfeeding
	use "$myfiles\EAR+UL+AMDR+CDRR+BF_WHOgrowthref_allgroups", clear
		keep if age_sex_grp==3
		drop if inlist(pal,2,3,4) // PAL does not affect energy requirements for this group, keep any one
		keep nutr_no nutr ear ul amdr_lower amdr_upper unit ul_note
		save NR3_WHOWeight_ContBF, replace

