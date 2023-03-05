** Riccardo Dal Cero
** Labour economics project
clear 
** Our working directory today is:
cd "/Users/riccardodalcero/Library/CloudStorage/OneDrive-UniversitaCattolicaSacroCuore-ICATT/Materials/Labour/project/"

*+++++++++++++++++++*
*Rearenging the dataset*
*+++++++++++++++++++*
** importing the individual dataset for dependet worker

use "STATA/comp.dta"
*log file
*log using STATA.log, replace 
drop if anno<1981
drop if anno==2020
*mergin the data
merge 1:m nquest nord anno  using "STATA/ldip.dta", generate(newv)
replace nonoc=0 if missing(nonoc)
drop if newv!=3 & eta>30
drop if eta>26 & ylm==0
drop if eta>26 & missing(ylm)
drop if eta<=15  /*drop observation before elementary school */
drop if eta>=65 /*drop observation that are in the retirment age */
*getting log real annual wages
merge m:1 anno using "STATA/defl.dta"
drop if _merge !=3

*label and rename

rename studio educ
label define educlabels 1 "no edu" 2 "primary" 3 "lower secondary" 4 "secondary technical - 3 years" 5 "secondary" 6 "first level degree"
label values educ educlabels
rename eta age
label var age "Age"
rename sesso sex
label var sex "Sex (female==1)"
rename anno year 
label var year "Year"
label var educ "Education level"

*retrive wages from income
gen y = ((ylm+ylnm)* defl)*12 
replace y=10 if y<=0 | missing(y)
gen l_y = log(y) 
gen hours = oretot*12*4
gen l_wage = log(y/hours)
egen mean_y     =   mean(y)
drop if missing(anasc)
drop if age>61

*define cohort for year of birth
gen cohort = ceil((anasc-1925)/5)
replace sex = sex-1
gen years  = ceil((year-1988)/5)
egen mean_ly    =   mean(l_y), by(educ cohort age)
egen mean_hours =   mean(oretot*12*4), by(educ cohort age)
gen mean_lhours =   log(mean_hours)
gen mean_lwage =   log(mean_y/mean_hours)
gen wage = y/hours
replace mean_hours=0 if missing(mean_hours)
gen time_e = mean_hours / 2000
gen age2    =   age^2


*Mean log wages of each (s,c,t)
label var y             "Annual Income"
label var l_y           "log(Annual Income)"
label var l_wage        "Earnings per year in log levels"
label var hours         "Work Hours"
label var mean_hours    "Mean of work hour"
label var mean_lwage    "Mean of log(wage)"
label var mean_ly       "Mean of log(Income)"
label var time_e        "Time endowments"
label var cohort        "Cohort"




*get some descriptive statistics 

ssc install estout, replace

*Now let’s generate a simple summary statistics table of cohorts
est clear  // clear the stored estimates
tab cohort
esttab using "cohort.tex", replace booktabs
est clear  // clear the stored estimates
// ttest difference for wages
gen     group1 =0 if educ==2 & sex==0 //lower education and male
replace group1 =1 if educ==2 & sex==1 //lower education and female
ttest   wage, by(group1) //difference in  wages for gender with low education
drop    group1

gen     group1 =0 if educ==3 & sex==0 //higher education and male
replace group1 =1 if educ==3 & sex==1 //higher education and female
ttest   wage, by(group1) //difference in  wages for gender with higher education
drop    group1

gen     group1 =0 if educ==2 & sex==0 //low education male
replace group1 =1 if educ==3& sex==0 //higher education and male
ttest   wage, by(group1) //difference in  wages for lower education for male
drop    group1

gen     group1 =0 if educ==2 & sex==1 //low education female
replace group1 =1 if educ==3& sex==1 //higher education and female
ttest   wage, by(group1) //difference in  wages for  higer education for female
drop    group1

//differences for annual working hour
gen     group1 =0 if educ==2 & sex==0 //lower education and male
replace group1 =1 if educ==2 & sex==1 //lower education and female
ttest   hours, by(group1) //difference in  whours for gender with low education
drop    group1

gen     group1 =0 if educ==3 & sex==0 //higher education and male
replace group1 =1 if educ==3 & sex==1 //higher education and female
ttest   hours, by(group1) //difference in  whours for gender with higher education
drop    group1

gen     group1 =0 if educ==2 & sex==0 //low education male
replace group1 =1 if educ==3& sex==0 //higher education and male
ttest   hours, by(group1) //difference in  whours for lower education for male
drop    group1

gen     group1 =0 if educ==2 & sex==1 //low education female
replace group1 =1 if educ==3& sex==1 //higher education and female
ttest   hours, by(group1) //difference in  whours for  higer education for female
drop    group1

*++++++++++++++++++++++++++++++++++++*
*generating table to export in latex+*
*++++++++++++++++++++++++++++++++++++*

gen group1 =0 if educ==2 & sex==0
replace group1 =1 if educ==2 & sex==1 //lower education and female
eststo grp1: estpost tabstat wage hours if educ ==2 & sex==0 , c(stat) stat(mean sd) 
eststo grp2: estpost tabstat wage hours if educ ==2 & sex==1 , c(stat) stat(mean sd) 
eststo grp3: estpost tabstat wage hours if educ ==5 & sex==0 , c(stat) stat(mean sd) 
eststo grp4: estpost tabstat wage hours if educ ==5 & sex==1, c(stat) stat(mean sd) 
ereturn list // list the stored locals
//export in latex
esttab grp* using "./table1.tex", replace ///
  main(mean %8.2fc) aux(sd %8.2fc) nostar nonumber unstack ///
   compress nonote noobs gap label booktabs   ///
   collabels(none) mtitle("All" "L. Sec." "Sec." "L. Sec Female" "Sec. Female") ///
title("\label{desc-table1}Descriptive statistics")

*also export summary statistics by some control groups educ and cohort
est clear
estpost tabstat mean_lwage mean_hours mean_y sex if cohort==10 & year==2008, by(educ) c(stat) stat(mean sd n) nototal
esttab using "./table2.tex", replace ///
  main(mean %8.2fc) aux(sd %8.2fc) nostar nonumber unstack ///
   compress nonote noobs gap label booktabs   ///
   collabels(none) ///
   nomtitles
//eqlabels() ///  




********************
* Wage age profiles*
********************
*some naive inspections
graph twoway (scatter mean_lwage  age  if educ==5 ) (scatter mean_lwage  age  if educ==3), by(cohort, title("Mean wage by cohorts"))  legend(label(1 "Secondary")) legend(label(2 "Lower secondary"))
graph export "graph.png", replace

est clear
eststo: reg  l_wage  age age2  i.cohort i.years if educ==5 
predict w_1
eststo: reg  l_wage  age age2  i.cohort i.years if educ==3 
predict w_2

esttab using "./regression1.tex", replace  ///
 b(3) se(3) nomtitle label star(* 0.10 ** 0.05 *** 0.01) ///
 booktabs  ///
 title("Regression table \label{reg1}")   ///
 addnotes("")

graph twoway (scatter w_1 age) (scatter w_2 age), by(cohort, title("Wage age profiles cond. on education")) legend(label(1 "Secondary")) legend(label(2 "Lower secondary"))
graph export "graph1.png", replace


*replicating adding gender effects

est clear
eststo: reg l_wage  age age2  i.cohort i.years if educ==5 & sex==0
predict w_m_h
eststo: reg l_wage  age age2  i.cohort i.years if educ==5 & sex==1
predict w_f_h
eststo: reg l_wage  age age2  i.cohort i.years if educ==3 & sex==0
predict w_m_l
eststo: reg l_wage  age age2  i.cohort i.years if educ==3 & sex==1
predict w_f_l
esttab using "./regression2.tex", replace  ///
 b(5) se(5) nomtitle label star(* 0.10 ** 0.05 *** 0.01) ///
 booktabs  ///
 title("Regression table \label{reg2}")   ///
 addnotes("")
graph twoway (scatter w_m_l age) (scatter w_f_l age), by(cohort, title("Wage profiles of low secondary school and gender gap"))  legend(label(1 "Male")) legend(label(2 "Female"))
graph export "graph2.png", replace
graph twoway (scatter w_m_h age) (scatter w_f_h age), by(cohort, title("Wage profiles of secondary school and gender gap"))  legend(label(1 "Male")) legend(label(2 "Female"))
graph export "graph3.png", replace



*********************
* Hours age profile*
*********************
est clear
eststo: reg  mean_lhours  age age2  i.cohort i.years if educ==5 
predict h_1
eststo: reg  mean_lhours  age age2  i.cohort i.years if educ==3 
predict h_2

esttab using "./regression3.tex", replace  ///
 b(3) se(3) nomtitle label star(* 0.10 ** 0.05 *** 0.01) ///
 booktabs  ///
 title("Regression table \label{reg3}")   ///
 addnotes("")

graph twoway (scatter h_1 age) (scatter h_2 age), by(cohort, title("Hour age profiles cond. on education")) legend(label(1 "Secondary")) legend(label(2 "Lower secondary"))
graph export "graph4.png", replace

*++++++++++++++++++++++++++++*
*+differentiating for gender+*
*++++++++++++++++++++++++++++*
est clear
eststo: reg mean_lhours  age age2  i.cohort i.years if educ==5 & sex==0
predict h_m_h
eststo: reg mean_lhours  age age2  i.cohort i.years if educ==5 & sex==1
predict h_f_h
eststo: reg mean_lhours  age age2  i.cohort i.years if educ==3 & sex==0
predict h_m_l
eststo: reg mean_lhours  age age2  i.cohort i.years if educ==3 & sex==1
predict h_f_l
esttab using "./regression4.tex", replace  ///
 b(5) se(5) nomtitle label star(* 0.10 ** 0.05 *** 0.01) ///
 booktabs  ///
 title("Regression table \label{reg4}")   ///
 addnotes("")
graph twoway (scatter w_m_l age) (scatter w_f_l age), by(cohort, title("Hour age profiles of lower-secondary school and gender gap"))  legend(label(1 "Male")) legend(label(2 "Female"))
graph export "graph5.png", replace
graph twoway (scatter w_m_h age) (scatter w_f_h age), by(cohort, title("Hour age profiles of secondary school and gender gap"))  legend(label(1 "Male")) legend(label(2 "Female"))
graph export "graph6.png", replace

