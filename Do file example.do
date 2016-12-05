/*-----------------------------------------------------------				
*	Goal:			Description and analysis of TB SP visit

*	Input:			1) SP_dataset_05102016.dta;
					
*	Output:			1) TB_anaylsis_final.do
										
*   Author(s):      Hao Xue  
*	Created: 		2016-06-09
*   Last Modified: 	2016-06-26 Hao
-----------------------------------------------------------*/

/*-----------------------------------------------------------
 Note: primary steps of this do file
 
	Step 1: Table 1: Provider Characteristics
	Step 2: Regression	
		Step 2.1: Table 2 Nodrug and process
	Step 3: Table 3: Management of Township Hospital Doctors Conditional on Being Shown CXR by SP
	Step 4: Table S1: Impact of provider qualifications and provider characteristics on the main standardized patient outcomes
	Step 5: Figure 1: Impact of provider certification on main standardized patient outcomes
	Step 6: Figure 2: Impact of reward on main standardized patient outcomes
	Step 7: Figure 3: Know-Do Gap
	Step 8: Figure S1: Proportion of provides for chehclists--SPV
	Step 9: Table 4: System-level analysis
-----------------------------------------------------------*/

clear all
set more off
capture log close
set maxvar  30000


* Load adofiles
global data "/Users/apple/Dropbox (REAP)/Standardized_Patients_II/Std_Patient_2/Papers/2_TB_Detection/Das et al Lancet ID"		

	* These files are created by the authors for the purposes of this study and are not publicly available for general use.
	* These files are not guaranteed to produce appropriate statistics other than those contained in this replication file.
		qui do "$data/adofiles/chartable.ado"
		qui do "$data/adofiles/betterbar.ado"
		
	* In addition, this dofile relies on two other publicly available STATA extensions: 
		* estadd, in package st0085_2 from http://www.stata-journal.com/software/sj14-2
		* xml_tab, in package dm0037 from http://www.stata-journal.com/software/sj8-3

global TB "/Users/apple/Dropbox/Standardized_Patients_II/Std_Patient_2/Papers/TB_Detection/"

cd "$TB"

/*-------
Step 1: Table 1: Provider Characteristics
--------*/
use "TB_analysis.dta",replace
		
	foreach var of varlist CXR fulltime parttime rewarddocyes rewarddoc clitraintbyes clitraintb {
		replace `var'=0 if level=="Village" & `var'==.
	} 
	* note: Village Clinics do not have those variables, need to be delete them in the table

	local binaries " CXR stethoscope thermometer managetb fulltime parttime susrefer_town susrefer_county susrefer_city susrefer_cdc rewardclinyes rewarddocyes clitraintbyes pracdoc rewarddocyes male hiedu traintbyes "
			
	qui foreach var of varlist totpat outpat numdoc CXR stethoscope thermometer managetb fulltime parttime nummanage  ///
	numsuspect susrefer_town susrefer_county susrefer_city susrefer_cdc rewardclinyes rewardclin rewarddocyes rewarddoc clitraintbyes clitraintb ///
	pracdoc rewarddocyes age male hiedu income patientload traintbyes traintb numcough {
	  
		cap mat drop var
			
			foreach caselogic in =="Village" =="Township" {  
			
				if regexm("`binaries'"," `var' ") count if `var'  == 1 & level `caselogic'
				if regexm("`binaries'"," `var' ") local n = `r(N)' 
						else local n = .
				
				sum `var' if level `caselogic'
					if regexm("`binaries'"," `var' ") local mean = 100 * `r(mean)'
						else local mean = `r(mean)'
			
				if regexm("`binaries'"," `var' ") ci `var' if level `caselogic', b wilson 
					else ci `var' if level `caselogic'
						
					local lower = `r(lb)'
						if regexm("`binaries'"," `var' ") local lower = 100 * `lower'
					local upper = `r(ub)'
						if regexm("`binaries'"," `var' ") local upper = 100 * `upper'
						
				mat var = nullmat(var) , [`n',`mean',`lower',`upper']
				
				}
					
			mat results = nullmat(results) \ var
			
			}
		

		local columns `" "Number" "Percentage or Mean" "Lower 95% CI" "Upper 95% CI" "'
		
		#delimit ;
		local rows `" 
			"Number of patients in catchment area" 
			"Number of patients seen in calendar year 2014"
			"Number of physicians working full time at the acility"
			"Facility has chest radiograph machine"
			"Facility has stethoscope"
			"Facility has thermometer"
			"Facility provides Management of TB Patients"
			"Facility has full-time doctors for TB Management"
			"Facility has part-time doctors for TB Managmeent"
			"Number of TB Patients Managed at end of calendar year 2014"
			"Number of suspected TB patients in calender year 2014"
			"Township Health Center"
			"County Hospital"
			"City Hospital"
			"CDC"
			"Facility receives reward"
			"If yes, facility reward amount (yuan)"
			"Doctor receives reward"
			"If yes, doctor reward amount (yuan)"
			"Facility-level Tuberculosis-specific training in 2014"
			"If yes, times"
			"Practicing Physician Certificate"
			"Receive reward for discovered TB patients"
			"Provider age (years)"
			"Male provider"
			"Provider education, upper secondary or higher"
			"Provider month salary (1,000 yuan)"
			"Patient load (patients)"
			"Received Tuberculosis-specific training in 2014"
			"If yes, times"
			"Patients with persistent cough in past two weeks"
			"' ;
		#delimit cr
			
		xml_tab ///
			results ///
			using "$TB/TB_tables.xls" ///
			, replace ///
			title("Table 1: Provider Characteristics") sheet("Table 1") ///
			rnames(`rows') ///
			showeq ceq("Village Clinics" "Village Clinics" "Village Clinics" "Village Clinics" "Township Health Centers" "Township Health Centers" "Township Health Centers" "Township Health Centers") /// 
			cnames(`columns' `columns' ) ///
			lines(COL_NAMES 3 LAST_ROW 3)  format((SCLB0) (SCCB0 NCRR2))

	

/*-------
Step 2: Regression
--------*/		
	/*-------
	Step 2.1: Table 2 Nodrug and process
	--------*/	
	
	global doccha "pracdoc age male patientload"
	
	eststo clear

	qui foreach out of varlist diagtime_min diagtime arq arqe {
		eststo: reg 	`out' nodrug_b nodrug_a angina  		if THC == 1, 				  vce(robust)
		eststo: areg 	`out' nodrug_b nodrug_a angina  		if THC == 1, absorb(towncode) vce(robust)
		eststo: areg 	`out' nodrug_b nodrug_a angina $doccha 	if THC == 1, absorb(towncode) vce(robust)

		eststo: reg 	`out' nodrug_b nodrug_a angina tuberc 			i.countycode if VC == 1, 				   vce(robust)
		eststo: areg 	`out' nodrug_b nodrug_a angina tuberc 			i.countycode if VC == 1, absorb(groupcode) vce(robust)
		eststo: areg 	`out' nodrug_b nodrug_a angina tuberc $doccha 	i.countycode if VC == 1, absorb(groupcode) vce(robust)

		eststo: reg 	`out' nodrug_b nodrug_a angina tuberc 			i.countycode if MVC == 1, 				    vce(robust)
		eststo: areg 	`out' nodrug_b nodrug_a angina tuberc 			i.countycode if MVC == 1, absorb(groupcode) vce(robust)
		eststo: areg 	`out' nodrug_b nodrug_a angina tuberc $doccha 	i.countycode if MVC == 1, absorb(groupcode) vce(robust)


		eststo: reg		`out' nodrug_b nodrug_a angina tuberc THC MVC	 					 , 					 vce(robust)
		eststo: reg 	`out' nodrug_b nodrug_a angina tuberc THC MVC 			i.countycode , 					 vce(robust)
		eststo: areg 	`out' nodrug_b nodrug_a angina tuberc THC MVC 			i.countycode , absorb(groupcode) vce(robust)
		eststo: areg 	`out' nodrug_b nodrug_a angina tuberc THC MVC $doccha 	i.countycode , absorb(groupcode) vce(robust)
	}

	esttab using "$TB/nodrug_process.csv¡°, b(%9.3fc) se(%9.3fc) starlevels( * 0.1 ** 0.05 *** 0.01) ///
			ar2(2) keep(nodrug_b nodrug_a pracdoc angina tuberc THC MVC) replace 
			
			
Stop

*Have a lovely day!			
