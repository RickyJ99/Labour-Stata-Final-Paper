** Riccardo Dal Cero
** Labour economics project
clear 
** Our working directory today is:
cd "/Users/riccardodalcero/Library/CloudStorage/OneDrive-UniversitaCattolicaSacroCuore-ICATT/Materials/Labour/project/"
*all plot are commented to run fast the code
*all the export .tex and .png are commented 
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
drop if eta>26 & ylm==0 // drop if is unemployed but with age >26
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
rename sesso female
label var female "Gender (female==1)"
rename anno year 
label var year "Year"
label var educ "Education level"

*define group as education and geneder
replace female = female-1
gen group_l=.
gen group_h=.
*female and lower education
replace group_l=1 if female==1 & educ==3 
*female and higher education
replace group_h=1 if female==1 & educ==5
*male and lower education
replace group_l=0 if female==0 & educ==3 
*male and higher education
replace group_h=0 if female==0 & educ==5

*retrive wages from income
gen y = ((ylm+ylnm)* defl)*12 
replace y=10 if y<=0 | missing(y)
gen l_y = log(y) 
gen hours = oretot*12*4
gen l_hours =log(hours)
gen l_wage = log(y/hours)
egen mean_y     =   mean(y)
drop if missing(anasc)
drop if age>61

*define cohort for year of birth
gen cohort = ceil((anasc-1925)/5)
gen years  = ceil((year-1988)/5)
egen mean_ly    =   mean(l_y), by(educ cohort age)
egen mean_hours =   mean(oretot*12*4), by(educ cohort age)
replace mean_hours=1 if missing(mean_hours)
egen mean_hours_sex =   mean(oretot*12*4), by(educ cohort age female)
replace mean_hours_sex=1 if missing(mean_hours_sex)
gen mean_lhours =   log(mean_hours)
gen mean_lhours_sex = log(mean_hours_sex)

gen mean_lwage =   log(mean_y/mean_hours)
gen wage = y/hours

gen time_e = mean_hours / 2000
gen age2    =   age^2

gen child=0
replace child=1 if ncomp>2 | ncomp==2 & ncomp-nperc>0
gen maritalst=0
replace maritalst=1 if staciv==1

gen whitecollar = 0
replace whitecollar = 1 if qualp10n==2 
gen bluecollar = 0
replace bluecollar = 1 if qualp10n==1
gen manager = 0
replace manager =1 if qualp10n==3 | qualp10n == 4
gen south=0
replace south = 1 if area3 == 3 
gen north=0
replace north =1 if area3 == 1
*id
egen id=group(nquest anasc ireg female)
// sort data by id and year
sort id year

// mark duplicates by id and year
duplicates tag id year, gen(dup)

// drop duplicates
keep if dup==0
xtset id year

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
//*esttab using "cohort.tex", replace booktabs
est clear  // clear the stored estimates
// ttest difference for wages
ttest   wage, by(group_l) //difference in  wages for gender with low education

ttest   wage, by(group_h) //difference in  wages for gender with higher education


gen     group1 =0 if educ==2 & female==0 //low education male
replace group1 =1 if educ==3& female==0 //higher education and male
ttest   wage, by(group1) //difference in  wages for lower education for male
drop    group1

gen     group1 =0 if educ==2 & female==1 //low education female
replace group1 =1 if educ==3& female==1 //higher education and female
ttest   wage, by(group1) //difference in  wages for  higer education for female
drop    group1

//differences for annual working hour
gen     group1 =0 if educ==2 & female==0 //lower education and male
replace group1 =1 if educ==2 & female==1 //lower education and female
ttest   hours, by(group1) //difference in  whours for gender with low education
drop    group1


ttest   hours, by(group_h) //difference in  whours for gender with higher education



ttest   hours, by(group_l) //difference in  whours for lower education for male


gen     group1 =0 if educ==2 & female==1 //low education female
replace group1 =1 if educ==3& female==1 //higher education and female
ttest   hours, by(group1) //difference in  whours for  higer education for female
drop    group1

*++++++++++++++++++++++++++++++++++++*
*generating table to export in latex+*
*++++++++++++++++++++++++++++++++++++*

gen group1 =0 if educ==2 & female==0
replace group1 =1 if educ==2 & female==1 //lower education and female
eststo grp1: estpost tabstat wage hours if educ ==2 & female==0 , c(stat) stat(mean sd) 
eststo grp2: estpost tabstat wage hours if educ ==2 & female==1 , c(stat) stat(mean sd) 
eststo grp3: estpost tabstat wage hours if educ ==5 & female==0 , c(stat) stat(mean sd) 
eststo grp4: estpost tabstat wage hours if educ ==5 & female==1, c(stat) stat(mean sd) 
ereturn list // list the stored locals
//export in latex
/**esttab grp* using "./table1.tex", replace ///
  main(mean %8.2fc) aux(sd %8.2fc) nostar nonumber unstack ///
   compress nonote noobs gap label booktabs   ///
   collabels(none) mtitle("All" "L. Sec." "Sec." "L. Sec Female" "Sec. Female") ///
title("\label{desc-table1}Descriptive statistics")*/

*also export summary statistics by some control groups educ and cohort
/*est clear
estpost tabstat mean_lwage mean_hours mean_y female if cohort==10 & year==2008, by(educ) c(stat) stat(mean sd n) nototal
*esttab using "./table2.tex", replace ///
  main(mean %8.2fc) aux(sd %8.2fc) nostar nonumber unstack ///
   compress nonote noobs gap label booktabs   ///
   collabels(none) ///
   nomtitles*/
//eqlabels() ///  




********************
* Wage age profiles*
********************
*some naive inspections
*graph twoway (scatter mean_lwage  age  if educ==5 ) (scatter mean_lwage  age  if educ==3), by(cohort, title("Mean wage by cohorts"))  legend(label(1 "Secondary")) legend(label(2 "Lower secondary"))
*graph export "./latex/graph.png", replace

est clear
eststo: xtreg  l_wage  age age2 i.cohort i.years if educ==5  
predict w_1
eststo: xtreg  l_wage  age age2  i.cohort i.years if educ==3 
predict w_2

*esttab using "./regression1.tex", replace  ///
 stats(r2 r2_a r2_w) nomtitle label star(* 0.10 ** 0.05 *** 0.01) ///
 booktabs  ///
 title("Regression on \(log(wage)\)\label{Tab:reg1}")   ///
addnotes("Regression on \(log(wage)\) for lower secondary group (1) and for secondary school ")

*graph twoway (lowess w_1 age)  (lowess w_1 age) , by(cohort, title("Wage age profiles cond. on education")) legend(label(1 "Secondary")) legend(label(2 "Lower secondary"))
*graph export "./latex/graph1.png", replace



*replicating adding gender effects

est clear
eststo: xtreg l_wage  age age2  i.cohort i.years if group_h==0
predict w_m_h
eststo: xtreg l_wage  age age2  i.cohort i.years if group_h==1
predict w_f_h
eststo: xtreg l_wage  age age2  i.cohort i.years if group_l==0
predict w_m_l
eststo: xtreg l_wage  age age2  i.cohort i.years if group_l==1
predict w_f_l
*esttab using "./regression2.tex", replace  ///
 stats(r2 r2_a r2_w) b(5) se(5) nomtitle label star(* 0.10 ** 0.05 *** 0.01) ///
 booktabs  ///
 title("Regression table \label{reg2}")   ///
 addnotes("")
*graph twoway (lowess w_f_l age) (lowess w_m_l age) (lowess w_f_h age) (lowess w_m_h age), by(cohort,title("Wage profiles per education vs gender gap"))  legend(label(1 "Female L.S.")) legend(label(2 "Male L.S.")) legend(label(3 "Female S.")) legend(label(4 "Male S."))
*graph export "./latex/graph2.png", replace
*graph twoway (lowess w_f_l age if cohort==6) (lowess w_m_l age if cohort==6) (lowess w_f_h age if cohort==6) (lowess w_m_h age if cohort==6),title("Wage profiles per education vs gender gap")  legend(label(1 "Female L.S.")) legend(label(2 "Male L.S.")) legend(label(3 "Female S.")) legend(label(4 "Male S."))
*graph export "./latex/graph2_1.png", replace


*********************
* Hours age profile*
*********************
est clear
eststo: xtreg  mean_lhours  age age2  i.cohort i.years if educ==5 
predict h_1
eststo: xtreg  mean_lhours  age age2  i.cohort i.years if educ==3 
predict h_2

*esttab using "./regression3.tex", replace  ///
  stats(r2 r2_a r2_w) b(3) se(3) nomtitle label star(* 0.10 ** 0.05 *** 0.01) ///
 booktabs  ///
 title("Regression table \label{reg3}")   ///
 addnotes("")

*graph twoway (lowess h_1 age) (lowess h_2 age), by(cohort, title("Hour age profiles cond. on education")) legend(label(1 "Secondary")) legend(label(2 "Lower secondary"))
*graph export "./graph4.png", replace


*++++++++++++++++++++++++++++*
*+differentiating for gender+*
*++++++++++++++++++++++++++++*
est clear
eststo: xtreg l_hours  age age2  i.cohort i.years if group_h==0
predict h_m_h
eststo: xtreg l_hours  age age2  i.cohort i.years if group_h==1
predict h_f_h
eststo: xtreg l_hours age age2  i.cohort i.years if group_l==0
predict h_m_l
eststo: xtreg l_hours  age age2  i.cohort i.years if group_l==1
predict h_f_l
*esttab using "./regression4.tex", replace  ///
 stats(r2 r2overall r2_w) b(5) se(5) nomtitle label star(* 0.10 ** 0.05 *** 0.01) ///
 booktabs  ///
 title("Regression table \label{reg4}")   ///
 addnotes("")
*graph twoway (lowess h_f_l age) (lowess h_m_l age) (lowess h_f_h age) (lowess h_m_h age), by(cohort,title("Hours-age profiles per education vs gender gap"))  legend(label(1 "Female L.S.")) legend(label(2 "Male L.S.")) legend(label(3 "Female S.")) legend(label(4 "Male S."))
*graph export "./latex/graph5.png", replace
*graph twoway (lowess h_f_l age if cohort==7) (lowess h_m_l age if cohort==7) (lowess h_f_h age if cohort==7) (lowess h_m_h age if cohort==7), title("Hours-age profiles per education vs gender gap")  legend(label(1 "Female L.S.")) legend(label(2 "Male L.S.")) legend(label(3 "Female S.")) legend(label(4 "Male S."))
*graph export "./latex/graph5_1.png", replace

*****
*Oxaca decomposition
*****/
ssc install oaxaca
eststo: oaxaca l_wage  age age2,by(group_l) weight(1)
**esttab using "./latex/oax1.tex", replace
eststo: oaxaca l_wage  age age2,by(group_h) weight(1)
*esttab using "./latex/oax2.tex", replace
est clear
eststo: oaxaca mean_lhours  age age2,by(group_l) weight(1)
*esttab using "./latex/oax3.tex", replace
est clear
eststo: oaxaca mean_lhours  age age2,by(group_h) weight(1)
*esttab using "./latex/oax4.tex", replace