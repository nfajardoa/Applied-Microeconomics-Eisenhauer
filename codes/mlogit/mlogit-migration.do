
*** APPLIED MICROECONOMICS: Multinomial Logit Estimations
set scheme plotplainblind
cd Z:
clear all
import delimited "Z:\filtered_data_2601_iscoskill.csv", encoding(UTF-8) 
gen experience = experience_fulltime + 0.5*experience_parttime
replace education_east = 0 if education_east == .
replace education_abroad = 0 if education_abroad == .
replace years_germany = 0 if years_germany == .
replace married_german = 0 if married_german == . 
gen children = .
replace children = 1 if sumkids > 0
replace children = 0 if children != 1
gen parent_isco = max(isco_modified_mother, isco_modified_father)
gen generation = .
replace generation = 1 if gebjahr <= 1964
replace generation = 2 if gebjahr > 1964 & gebjahr <= 1973
replace generation = 3 if gebjahr > 1973

keep if culture == 1 | culture == 3 |  culture == 4 | culture == 6

**** Augmented model
** Drop missing points
foreach i in isco_modified female years_education experience language_cost culture education_east education_abroad years_germany married_german children parent_isco generation east {
	drop if `i' == .
}

** Model 1
mlogit isco_modified i.female years_education experience c.experience#c.experience language_cost i.culture i.education_east i.education_abroad years_germany c.years_germany#c.years_germany i.married_german i.children i.parent_isco i.generation i.east, baseoutcome(1) vce(cluster cid)
estadd fitstat, save
eststo mlog_aisco2
margins, dydx(*) post
eststo mlog_aisco2_m

** Model 2
mlogit isco_modified i.female years_education experience c.experience#c.experience i.migback language_cost i.culture i.education_east i.education_abroad years_germany c.years_germany#c.years_germany i.married_german i.children i.parent_isco i.generation i.east, baseoutcome(1) vce(cluster cid)
estadd fitstat
eststo mlog_aisco3
margins, dydx(*) post
eststo mlog_aisco3_m

** Model 3
mlogit isco_modified i.female years_education experience c.experience#c.experience i.migback i.culture i.education_east i.education_abroad years_germany c.years_germany#c.years_germany i.married_german i.children i.parent_isco i.generation i.east, baseoutcome(1) vce(cluster cid)
estadd fitstat
eststo mlog_aisco1
margins, dydx(*) post
eststo mlog_aisco1_m

*Log-odds table
esttab mlog_aisco1 mlog_aisco2 mlog_aisco3 using lodds_augmented3isco.tex, replace collabels(none) noomitted nobaselevels noconstant nogaps cells(b(fmt(3)star) se(fmt(3)par)) compress unstack order(1.female years_education experience c.experience#c.experience 3.culture 4.culture 6.culture 1.education_east 1.education_abroad years_germany c.years_germany#c.years_germany 1.married_german 1.children 2.parent_isco 3.parent_isco 4.parent_isco 5.parent_isco 2.generation 3.generation 1.east) scalars(r2_mf r2_mfadj r2_ct r2_ctadj aic0 bic_p) booktabs fragment alignment(S) eqlabels("\textsc{\small blue-collar}" "\textsc{\small white-collar}" "\textsc{\small technical}" "\textsc{\small professional}") varlabels(1.female "Female" years_education "Years of education" experience "Work experience" c.experience#c.experience "Sqr. Work experience" 3.culture "Western european" 4.culture "Eastern european" 6.culture "Turkish/Greek" 1.education_east "Education East" 1.education_abroad "Education Abroad" years_germany "Years in Germany" c.years_germany#c.years_germany "Sqr. Years in Germany" 1.married_german "Married German" 1.children "Children" 2.parent_isco "Blue-collar Parent" 3.parent_isco "White-collar Parent" 4.parent_isco "Technician Parent" 5.parent_isco "Professional Parent" 2.generation "Gen. X" 3.generation "Millenial" 1.east "East" 2.migback "Direct migrant" 3.migback "Indirect migrant" language_cost "Language distance")

*Margins table selected model
esttab mlog_aisco1_m using ame_augmentedisco.csv, replace se collabels(none) noomitted nobaselevels noconstant nogaps compress unstack noobs

*Margins comparison tables
esttab mlog_aisco1_m mlog_aisco2_m mlog_aisco3_m using ame_augmented3isco.csv, replace se collabels(none) noomitted nobaselevels noconstant nogaps compress unstack noobs

*Selected Model
mlogit isco_modified i.female years_education experience c.experience#c.experience i.migback i.culture i.education_east i.education_abroad years_germany c.years_germany#c.years_germany i.married_german i.children i.parent_isco i.generation i.east, baseoutcome(1) vce(cluster cid)

*Marginal effects plots
mcp experience migback, margopts(predict(outcome(#1))) at1(0 (5) 35) plot(legend(off) xlabel(0 (5) 35) ylabel(#5) xtitle("") title(Elementary) aspectratio(1))
graph save Graph "1.gph", replace 
mcp experience migback, margopts(predict(outcome(#2))) at1(0 (5) 35) plot(legend(off) xlabel(0 (5) 35) ylabel(#5) xtitle("") title(Blue-collar) aspectratio(1))
graph save Graph "2.gph", replace
mcp experience migback, margopts(predict(outcome(#3))) at1(0 (5) 35) plot(legend(off) xlabel(0 (5) 35) ylabel(#5) xtitle("") title(White-collar) aspectratio(1))
graph save Graph "3.gph", replace 
mcp experience migback, margopts(predict(outcome(#4))) at1(0 (5) 35) plot(legend(off) xlabel(0 (5) 35) ylabel(#5) xtitle("") title(Technical) aspectratio(1))
graph save Graph "4.gph", replace 
mcp experience migback, margopts(predict(outcome(#5))) at1(0 (5) 35) plot(legend(off) xlabel(0 (5) 35) ylabel(#5) xtitle("") title(Professional) aspectratio(1))
graph save Graph "5.gph", replace 
*Combined Graph
graph combine 1.gph 2.gph 3.gph 4.gph 5.gph, ycom xcom com rows(1) xsize(6.7) ysize(1.5) plotregion(margin(0 0 0 0)) graphregion(margin(0 0 0 0)) iscale(1.5) imargin(0 0 -5 -5)
graph export experience_fullisco.png, width(2600) replace

*Margina effects plots
mcp years_education migback, margopts(predict(outcome(#1))) at1(7 (1) 18) plot(legend(off) xlabel(6 (2) 18) ylabel(#5) xtitle("") title(Elementary) aspectratio(1))
graph save Graph "1.gph", replace 
mcp years_education migback, margopts(predict(outcome(#2))) at1(7 (1) 18) plot(legend(off) xlabel(6 (2) 18) ylabel(#5) xtitle("") title(Blue-collar) aspectratio(1))
graph save Graph "2.gph", replace
mcp years_education migback, margopts(predict(outcome(#3))) at1(7 (1) 18) plot(legend(off) xlabel(6 (2) 18) ylabel(#5) xtitle("") title(White-collar) aspectratio(1))
graph save Graph "3.gph", replace 
mcp years_education migback, margopts(predict(outcome(#4))) at1(7 (1) 18) plot(legend(off) xlabel(6 (2) 18) ylabel(#5) xtitle("") title(Technical) aspectratio(1))
graph save Graph "4.gph", replace 
mcp years_education migback, margopts(predict(outcome(#5))) at1(7 (1) 18) plot(legend(off) xlabel(6 (2) 18) ylabel(#5) xtitle("") title(Professional) aspectratio(1))
graph save Graph "5.gph", replace 
graph combine 1.gph 2.gph 3.gph 4.gph 5.gph, ycom xcom com rows(1) xsize(6.7) ysize(1.5) plotregion(margin(0 0 0 0)) graphregion(margin(0 0 0 0)) iscale(1.5) imargin(0 0 -5 -5)
graph export years_education_fullisco.png, width(2600) replace

*Marginal effects plots
mcp years_germany migback, margopts(predict(outcome(#1))) at1(0 (6) 42) plot(legend(off) xlabel(0 (6) 42) ylabel(#5) xtitle("") title(Elementary) aspectratio(1))
graph save Graph "1.gph", replace 
mcp years_germany migback, margopts(predict(outcome(#2))) at1(0 (6) 42) plot(legend(off) xlabel(0 (6) 42) ylabel(#5) xtitle("") title(Blue-collar) aspectratio(1))
graph save Graph "2.gph", replace
mcp years_germany migback, margopts(predict(outcome(#3))) at1(0 (6) 42) plot(legend(off) xlabel(0 (6) 42) ylabel(#5) xtitle("") title(White-collar) aspectratio(1))
graph save Graph "3.gph", replace 
mcp years_germany migback, margopts(predict(outcome(#4))) at1(0 (6) 42) plot(legend(off) xlabel(0 (6) 42) ylabel(#5) xtitle("") title(Technical) aspectratio(1))
graph save Graph "4.gph", replace 
mcp years_germany migback, margopts(predict(outcome(#5))) at1(0 (6) 42) plot(legend(off) xlabel(0 (6) 42) ylabel(#5) xtitle("") title(Professional) aspectratio(1))
graph save Graph "5.gph", replace 
graph combine 1.gph 2.gph 3.gph 4.gph 5.gph, ycom xcom com rows(1) xsize(6.7) ysize(1.5) plotregion(margin(0 0 0 0)) graphregion(margin(0 0 0 0)) iscale(1.5) imargin(0 0 -5 -5)
graph export years_germany_fullisco.png, width(2600) replace

estimates clear

* Simpler model
* Drop missing values
foreach i in isco_modified female years_education experience migback language_cost culture {
	drop if `i' == .
}

*Model2
mlogit isco_modified i.female years_education experience c.experience#c.experience language_cost i.culture, baseoutcome(1) vce(cluster cid)
estadd fitstat, save
eststo mlog_sisco2
margins, dydx(*) post
eststo mlog_sisco2_m

*Model 3
mlogit isco_modified i.female years_education experience c.experience#c.experience i.migback language_cost i.culture, baseoutcome(1) vce(cluster cid)
estadd fitstat
eststo mlog_sisco3
margins, dydx(*) post
eststo mlog_sisco3_m

*Model 1
mlogit isco_modified i.female years_education experience c.experience#c.experience i.migback i.culture, baseoutcome(1) vce(cluster cid) 
estadd fitstat
eststo mlog_sisco1
margins, dydx(*) post
eststo mlog_sisco1_m

*Log-odds table
esttab mlog_sisco1 mlog_sisco2 mlog_sisco3 using lodds_simple3isco.tex, replace collabels(none) noomitted nobaselevels noconstant nogaps cells(b(fmt(3)star) se(fmt(3)par)) compress unstack order(1.female years_education experience c.experience#c.experience 3.culture 4.culture 6.culture) scalars(r2_mf r2_mfadj r2_ct r2_ctadj aic0 bic_p) booktabs fragment alignment(S) eqlabels("\textsc{\small blue-collar}" "\textsc{\small white-collar}" "\textsc{\small technical}" "\textsc{\small professional}") varlabels(1.female "Female" years_education "Years of education" experience "Work experience" c.experience#c.experience "Sqr. Work experience" 3.culture "Western european" 4.culture "Eastern european" 6.culture "Turkish/Greek" 2.migback "Direct migrant" 3.migback "Indirect migrant" language_cost "Language distance")

*Margins table selected
esttab mlog_sisco1_m using ame_simpleisco.csv, replace se collabels(none) noomitted nobaselevels noconstant nogaps compress unstack noobs

*Margins table comparison
esttab mlog_sisco1_m mlog_sisco2_m mlog_sisco3_m using ame_simple3isco.csv, replace se collabels(none) noomitted nobaselevels noconstant nogaps compress unstack noobs

** MARGINAL EFFECTS PLOTS
*Selected model
mlogit isco_modified i.female years_education experience c.experience#c.experience i.migback i.culture, baseoutcome(1) vce(cluster cid) 

mcp experience migback, margopts(predict(outcome(#1))) at1(0 (5) 35) plot(legend(off) xlabel(0 (5) 35) ylabel(#5) xtitle("") title(Elementary) aspectratio(1))
graph save Graph "1.gph", replace 
mcp experience migback, margopts(predict(outcome(#2))) at1(0 (5) 35) plot(legend(off) xlabel(0 (5) 35) ylabel(#5) xtitle("") title(Blue-collar) aspectratio(1))
graph save Graph "2.gph", replace
mcp experience migback, margopts(predict(outcome(#3))) at1(0 (5) 35) plot(legend(off) xlabel(0 (5) 35) ylabel(#5) xtitle("") title(White-collar) aspectratio(1))
graph save Graph "3.gph", replace 
mcp experience migback, margopts(predict(outcome(#4))) at1(0 (5) 35) plot(legend(off) xlabel(0 (5) 35) ylabel(#5) xtitle("") title(Technical) aspectratio(1))
graph save Graph "4.gph", replace 
mcp experience migback, margopts(predict(outcome(#5))) at1(0 (5) 35) plot(legend(off) xlabel(0 (5) 35) ylabel(#5) xtitle("") title(Professional) aspectratio(1))
graph save Graph "5.gph", replace 
graph combine 1.gph 2.gph 3.gph 4.gph 5.gph, ycom xcom com rows(1) xsize(6.7) ysize(1.5) plotregion(margin(0 0 0 0)) graphregion(margin(0 0 0 0)) iscale(1.5) imargin(0 0 -5 -5)
graph export experience_simpleisco.png, width(2600) replace

mcp years_education migback, margopts(predict(outcome(#1))) at1(7 (1) 18) plot(legend(off) xlabel(6 (2) 18) ylabel(#5) xtitle("") title(Elementary) aspectratio(1))
graph save Graph "1.gph", replace 
mcp years_education migback, margopts(predict(outcome(#2))) at1(7 (1) 18) plot(legend(off) xlabel(6 (2) 18) ylabel(#5) xtitle("") title(Blue-collar) aspectratio(1))
graph save Graph "2.gph", replace
mcp years_education migback, margopts(predict(outcome(#3))) at1(7 (1) 18) plot(legend(off) xlabel(6 (2) 18) ylabel(#5) xtitle("") title(White-collar) aspectratio(1))
graph save Graph "3.gph", replace 
mcp years_education migback, margopts(predict(outcome(#4))) at1(7 (1) 18) plot(legend(off) xlabel(6 (2) 18) ylabel(#5) xtitle("") title(Technical) aspectratio(1))
graph save Graph "4.gph", replace 
mcp years_education migback, margopts(predict(outcome(#5))) at1(7 (1) 18) plot(legend(off) xlabel(6 (2) 18) ylabel(#5) xtitle("") title(Professional) aspectratio(1))
graph save Graph "5.gph", replace 
graph combine 1.gph 2.gph 3.gph 4.gph 5.gph, ycom xcom com rows(1) xsize(6.7) ysize(1.5) plotregion(margin(0 0 0 0)) graphregion(margin(0 0 0 0)) iscale(1.5) imargin(0 0 -5 -5)
graph export years_education_simpleisco.png, width(2600) replace

*** LANGUAGE SKILL

clear all
import delimited "Z:\filtered_data_2601_language_skill2.csv", encoding(UTF-8) 
gen experience = experience_fulltime + 0.5*experience_parttime
replace education_east = 0 if education_east == .
replace education_abroad = 0 if education_abroad == .
replace years_germany = 0 if years_germany == .
replace married_german = 0 if married_german == . 
gen children = .
replace children = 1 if sumkids > 0
replace children = 0 if children != 1
gen parent_skill = max(x5_skill2_mother, x5_skill2_father)
gen generation = .
replace generation = 1 if gebjahr <= 1964
replace generation = 2 if gebjahr > 1964 & gebjahr <= 1973
replace generation = 3 if gebjahr > 1973

keep if culture == 1 | culture == 3 |  culture == 4 | culture == 6

* Augmented model
foreach i in x5_skill2 female years_education experience language_cost culture education_east education_abroad years_germany married_german children parent_skill generation east {
	drop if `i' == .
}

mlogit x5_skill2 i.female years_education experience c.experience#c.experience language_cost i.culture i.education_east i.education_abroad years_germany c.years_germany#c.years_germany i.married_german i.children i.parent_skill i.generation i.east, baseoutcome(1) vce(cluster cid)
estadd fitstat, save
eststo mlog_askill2
margins, dydx(*) post
eststo mlog_askill2_m

mlogit x5_skill2 i.female years_education experience c.experience#c.experience i.migback language_cost i.culture i.education_east i.education_abroad years_germany c.years_germany#c.years_germany i.married_german i.children i.parent_skill i.generation i.east, baseoutcome(1) vce(cluster cid)
estadd fitstat, save
eststo mlog_askill3
margins, dydx(*) post
eststo mlog_askill3_m

mlogit x5_skill2 i.female years_education experience c.experience#c.experience i.migback i.culture i.education_east i.education_abroad years_germany c.years_germany#c.years_germany i.married_german i.children i.parent_skill i.generation i.east, baseoutcome(1) vce(cluster cid)
estadd fitstat, save
eststo mlog_askill1
margins, dydx(*) post
eststo mlog_askill1_m

esttab mlog_askill1 mlog_askill2 mlog_askill3 using lodds_augmented3skill.tex, replace collabels(none) noomitted nobaselevels noconstant nogaps cells(b(fmt(3)star) se(fmt(3)par)) compress unstack order(1.female years_education experience c.experience#c.experience 3.culture 4.culture 6.culture 1.education_east 1.education_abroad years_germany c.years_germany#c.years_germany 1.married_german 1.children 2.parent_isco 3.parent_isco 4.parent_isco 5.parent_isco 2.generation 3.generation 1.east) scalars(r2_mf r2_mfadj r2_ct r2_ctadj aic0 bic_p) booktabs fragment alignment(S) eqlabels("\textsc{\small blue-collar}" "\textsc{\small white-collar}" "\textsc{\small technical}" "\textsc{\small professional}") varlabels(1.female "Female" years_education "Years of education" experience "Work experience" c.experience#c.experience "Sqr. Work experience" 3.culture "Western european" 4.culture "Eastern european" 6.culture "Turkish/Greek" 1.education_east "Education East" 1.education_abroad "Education Abroad" years_germany "Years in Germany" c.years_germany#c.years_germany "Sqr. Years in Germany" 1.married_german "Married German" 1.children "Children" 2.parent_isco "Blue-collar Parent" 3.parent_isco "White-collar Parent" 4.parent_isco "Technician Parent" 5.parent_isco "Professional Parent" 2.generation "Gen. X" 3.generation "Millenial" 1.east "East" 2.migback "Direct migrant" 3.migback "Indirect migrant" language_cost "Language distance")

esttab mlog_askill1_m using ame_augmentedskill.csv, replace se collabels(none) noomitted nobaselevels noconstant nogaps compress unstack noobs

esttab mlog_askill1_m mlog_askill2_m mlog_askill3_m using ame_augmented3skill.csv, replace se collabels(none) noomitted nobaselevels noconstant nogaps compress unstack noobs

mlogit x5_skill2 i.female years_education experience c.experience#c.experience i.migback i.culture i.education_east i.education_abroad years_germany c.years_germany#c.years_germany i.married_german i.children i.parent_skill i.generation i.east, baseoutcome(1) vce(cluster cid)

mcp experience migback, margopts(predict(outcome(#1))) at1(0 (5) 35) plot(legend(off) xlabel(0 (5) 35) ylabel(#5) xtitle("") title(Elementary) aspectratio(1))
graph save Graph "1.gph", replace 
mcp experience migback, margopts(predict(outcome(#2))) at1(0 (5) 35) plot(legend(off) xlabel(0 (5) 35) ylabel(#5) xtitle("") title(Blue-collar) aspectratio(1))
graph save Graph "2.gph", replace
mcp experience migback, margopts(predict(outcome(#3))) at1(0 (5) 35) plot(legend(off) xlabel(0 (5) 35) ylabel(#5) xtitle("") title(White-collar) aspectratio(1))
graph save Graph "3.gph", replace 
mcp experience migback, margopts(predict(outcome(#4))) at1(0 (5) 35) plot(legend(off) xlabel(0 (5) 35) ylabel(#5) xtitle("") title(Technical) aspectratio(1))
graph save Graph "4.gph", replace 
mcp experience migback, margopts(predict(outcome(#5))) at1(0 (5) 35) plot(legend(off) xlabel(0 (5) 35) ylabel(#5) xtitle("") title(Professional) aspectratio(1))
graph save Graph "5.gph", replace 
graph combine 1.gph 2.gph 3.gph 4.gph 5.gph, ycom xcom com rows(1) xsize(6.7) ysize(1.5) plotregion(margin(0 0 0 0)) graphregion(margin(0 0 0 0)) iscale(1.5) imargin(0 0 -5 -5)
graph export experience_fullskill.png, width(2600) replace

mcp years_education migback, margopts(predict(outcome(#1))) at1(7 (1) 18) plot(legend(off) xlabel(6 (2) 18) ylabel(#5) xtitle("") title(Elementary) aspectratio(1))
graph save Graph "1.gph", replace 
mcp years_education migback, margopts(predict(outcome(#2))) at1(7 (1) 18) plot(legend(off) xlabel(6 (2) 18) ylabel(#5) xtitle("") title(Blue-collar) aspectratio(1))
graph save Graph "2.gph", replace
mcp years_education migback, margopts(predict(outcome(#3))) at1(7 (1) 18) plot(legend(off) xlabel(6 (2) 18) ylabel(#5) xtitle("") title(White-collar) aspectratio(1))
graph save Graph "3.gph", replace 
mcp years_education migback, margopts(predict(outcome(#4))) at1(7 (1) 18) plot(legend(off) xlabel(6 (2) 18) ylabel(#5) xtitle("") title(Technical) aspectratio(1))
graph save Graph "4.gph", replace 
mcp years_education migback, margopts(predict(outcome(#5))) at1(7 (1) 18) plot(legend(off) xlabel(6 (2) 18) ylabel(#5) xtitle("") title(Professional) aspectratio(1))
graph save Graph "5.gph", replace 
graph combine 1.gph 2.gph 3.gph 4.gph 5.gph, ycom xcom com rows(1) xsize(6.7) ysize(1.5) plotregion(margin(0 0 0 0)) graphregion(margin(0 0 0 0)) iscale(1.5) imargin(0 0 -5 -5)
graph export years_education_fullskill.png, width(2600) replace

mcp years_germany migback, margopts(predict(outcome(#1))) at1(0 (6) 42) plot(legend(off) xlabel(0 (6) 42) ylabel(#5) xtitle("") title(Elementary) aspectratio(1))
graph save Graph "1.gph", replace 
mcp years_germany migback, margopts(predict(outcome(#2))) at1(0 (6) 42) plot(legend(off) xlabel(0 (6) 42) ylabel(#5) xtitle("") title(Blue-collar) aspectratio(1))
graph save Graph "2.gph", replace
mcp years_germany migback, margopts(predict(outcome(#3))) at1(0 (6) 42) plot(legend(off) xlabel(0 (6) 42) ylabel(#5) xtitle("") title(White-collar) aspectratio(1))
graph save Graph "3.gph", replace 
mcp years_germany migback, margopts(predict(outcome(#4))) at1(0 (6) 42) plot(legend(off) xlabel(0 (6) 42) ylabel(#5) xtitle("") title(Technical) aspectratio(1))
graph save Graph "4.gph", replace 
mcp years_germany migback, margopts(predict(outcome(#5))) at1(0 (6) 42) plot(legend(off) xlabel(0 (6) 42) ylabel(#5) xtitle("") title(Professional) aspectratio(1))
graph save Graph "5.gph", replace 
graph combine 1.gph 2.gph 3.gph 4.gph 5.gph, ycom xcom com rows(1) xsize(6.7) ysize(1.5) plotregion(margin(0 0 0 0)) graphregion(margin(0 0 0 0)) iscale(1.5) imargin(0 0 -5 -5)
graph export years_germany_fullskill.png, width(2600) replace

estimates clear

* Simpler model
foreach i in x5_skill2 female years_education experience migback language_cost culture {
	drop if `i' == .
}

mlogit x5_skill2 i.female years_education experience c.experience#c.experience language_cost i.culture, baseoutcome(1) vce(cluster cid)
estadd fitstat, save
eststo mlog_sskill2
margins, dydx(*) post
eststo mlog_sskill2_m

mlogit x5_skill2 i.female years_education experience c.experience#c.experience i.migback language_cost i.culture, baseoutcome(1) vce(cluster cid)
estadd fitstat
eststo mlog_sskill3
margins, dydx(*) post
eststo mlog_sskill3_m

mlogit x5_skill2 i.female years_education experience c.experience#c.experience i.migback i.culture, baseoutcome(1) vce(cluster cid) 
estadd fitstat
eststo mlog_sskill1
margins, dydx(*) post
eststo mlog_sskill1_m

esttab mlog_sskill1 mlog_sskill2 mlog_sskill3 using lodds_simple3skill.tex, replace collabels(none) noomitted nobaselevels noconstant nogaps cells(b(fmt(3)star) se(fmt(3)par)) compress unstack order(1.female years_education experience c.experience#c.experience 3.culture 4.culture 6.culture) scalars(r2_mf r2_mfadj r2_ct r2_ctadj aic0 bic_p) booktabs fragment alignment(S) eqlabels("\textsc{\small blue-collar}" "\textsc{\small white-collar}" "\textsc{\small technical}" "\textsc{\small professional}") varlabels(1.female "Female" years_education "Years of education" experience "Work experience" c.experience#c.experience "Sqr. Work experience" 3.culture "Western european" 4.culture "Eastern european" 6.culture "Turkish/Greek" 2.migback "Direct migrant" 3.migback "Indirect migrant" language_cost "Language distance")

esttab mlog_sskill1_m using ame_simpleskill.csv, replace se collabels(none) noomitted nobaselevels noconstant nogaps compress unstack noobs

esttab mlog_sskill1_m mlog_sskill2_m mlog_sskill3_m using ame_simple3skill.csv, replace se collabels(none) noomitted nobaselevels noconstant nogaps compress unstack noobs

** MARGINAL EFFECTS PLOT

mlogit x5_skill2 i.female years_education experience c.experience#c.experience i.migback i.culture, baseoutcome(1) vce(cluster cid) 

mcp experience migback, margopts(predict(outcome(#1))) at1(0 (5) 35) plot(legend(off) xlabel(0 (5) 35) ylabel(#5) xtitle("") title(Elementary) aspectratio(1))
graph save Graph "1.gph", replace 
mcp experience migback, margopts(predict(outcome(#2))) at1(0 (5) 35) plot(legend(off) xlabel(0 (5) 35) ylabel(#5) xtitle("") title(Blue-collar) aspectratio(1))
graph save Graph "2.gph", replace
mcp experience migback, margopts(predict(outcome(#3))) at1(0 (5) 35) plot(legend(off) xlabel(0 (5) 35) ylabel(#5) xtitle("") title(White-collar) aspectratio(1))
graph save Graph "3.gph", replace 
mcp experience migback, margopts(predict(outcome(#4))) at1(0 (5) 35) plot(legend(off) xlabel(0 (5) 35) ylabel(#5) xtitle("") title(Technical) aspectratio(1))
graph save Graph "4.gph", replace 
mcp experience migback, margopts(predict(outcome(#5))) at1(0 (5) 35) plot(legend(off) xlabel(0 (5) 35) ylabel(#5) xtitle("") title(Professional) aspectratio(1))
graph save Graph "5.gph", replace 
graph combine 1.gph 2.gph 3.gph 4.gph 5.gph, ycom xcom com rows(1) xsize(6.7) ysize(1.5) plotregion(margin(0 0 0 0)) graphregion(margin(0 0 0 0)) iscale(1.5) imargin(0 0 -5 -5)
graph export experience_simpleskill.png, width(2600) replace

mcp years_education migback, margopts(predict(outcome(#1))) at1(7 (1) 18) plot(legend(off) xlabel(6 (2) 18) ylabel(#5) xtitle("") title(Elementary) aspectratio(1))
graph save Graph "1.gph", replace 
mcp years_education migback, margopts(predict(outcome(#2))) at1(7 (1) 18) plot(legend(off) xlabel(6 (2) 18) ylabel(#5) xtitle("") title(Blue-collar) aspectratio(1))
graph save Graph "2.gph", replace
mcp years_education migback, margopts(predict(outcome(#3))) at1(7 (1) 18) plot(legend(off) xlabel(6 (2) 18) ylabel(#5) xtitle("") title(White-collar) aspectratio(1))
graph save Graph "3.gph", replace 
mcp years_education migback, margopts(predict(outcome(#4))) at1(7 (1) 18) plot(legend(off) xlabel(6 (2) 18) ylabel(#5) xtitle("") title(Technical) aspectratio(1))
graph save Graph "4.gph", replace 
mcp years_education migback, margopts(predict(outcome(#5))) at1(7 (1) 18) plot(legend(off) xlabel(6 (2) 18) ylabel(#5) xtitle("") title(Professional) aspectratio(1))
graph save Graph "5.gph", replace 
graph combine 1.gph 2.gph 3.gph 4.gph 5.gph, ycom xcom com rows(1) xsize(6.7) ysize(1.5) plotregion(margin(0 0 0 0)) graphregion(margin(0 0 0 0)) iscale(1.5) imargin(0 0 -5 -5)
graph export years_education_simpleskill.png, width(2600) replace
