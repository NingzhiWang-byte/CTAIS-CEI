
**************************
**       BASELINE       **
**************************


use data.dta,clear


* Summary statistics *
outreg2 using Table1.doc if used == 1, replace sum(detail) keep(exp_assets2 lnexpense CTAIS Size Lev ROA CashHold BM lnGDP_per AverageSalary FiscalInc) title(Decriptive statistics)

* Baseline regression *
reghdfe exp_assets2 CTAIS if used == 1,absorb(code ind_yr) cluster(code)
outreg2 using Table2.doc,replace tstat bdec(3) tdec(2) ctitle(EnvInvest/Assets)
reghdfe exp_assets2 CTAIS Size Lev ROA CashHold BM lnGDP_per AverageSalary FiscalInc ,absorb(code ind_yr) cluster(code)
outreg2 using Table2.doc,append tstat bdec(3) tdec(2) ctitle(EnvInvest/Assets)
reghdfe lnexpense CTAIS if used == 1,absorb(code ind_yr) cluster(code)
outreg2 using Table2.doc,append tstat bdec(3) tdec(2) ctitle(EnvInvest/Assets)
reghdfe lnexpense CTAIS Size Lev ROA CashHold BM lnGDP_per AverageSalary FiscalInc ,absorb(code ind_yr) cluster(code)
outreg2 using Table2.doc,append tstat bdec(3) tdec(2) ctitle(EnvInvest/Assets)

**************************
**       PARALLEL       **
**************************
reghdfe exp_assets pre2 pre1 current post1 post2 post3 ,absorb(code ind_yr ) cluster(code)
outreg2 using Table3.doc,replace tstat bdec(3) tdec(2) ctitle(EnvInvest/Assets)
reghdfe exp_assets pre2 pre1 current post1 post2 post3 Size Lev ROA CashHold BM lnGDP_per AverageSalary FiscalInc ,absorb(code ind_yr) cluster(code)
est store Dynamic1
outreg2 using Table3.doc,append tstat bdec(3) tdec(2) ctitle(EnvInvest/Assets)
reghdfe lnexpense pre2 pre1 current post1 post2 post3 ,absorb(code ind_yr ) cluster(code)
outreg2 using Table3.doc,append tstat bdec(3) tdec(2) ctitle(lnENV)
reghdfe lnexpense pre2 pre1 current post1 post2 post3 Size Lev ROA CashHold BM lnGDP_per AverageSalary FiscalInc ,absorb(code ind_yr) cluster(code)
est store Dynamic2
outreg2 using Table3.doc,append tstat bdec(3) tdec(2) ctitle(lnENV)


**************************
**       CHANNEL        **
**************************
* Analyst following *
use data.dta,clear
bys year: egen Analyst_50 = pctile(Analystnum),p(50)
gen AnalystH = Analystnum>Analyst_50
gen AnalystL = Analystnum<=Analyst_50
reghdfe exp_assets2 CTAIS Size Lev ROA CashHold BM lnGDP_per AverageSalary FiscalInc if AnalystH == 1,absorb(code ind_yr) cluster(code)
outreg2 using Table4a.doc,replace tstat bdec(3) tdec(2) ctitle(AnalystH)
reghdfe lnexpense CTAIS Size Lev ROA CashHold BM lnGDP_per AverageSalary FiscalInc if AnalystH == 1,absorb(code ind_yr) cluster(code)
outreg2 using Table4a.doc,append tstat bdec(3) tdec(2) ctitle(AnalystkH)
reghdfe exp_assets2 CTAIS Size Lev ROA CashHold BM lnGDP_per AverageSalary FiscalInc if AnalystL == 1,absorb(code ind_yr) cluster(code)
outreg2 using Table4a.doc,append tstat bdec(3) tdec(2) ctitle(AnalystL)
reghdfe lnexpense CTAIS Size Lev ROA CashHold BM lnGDP_per AverageSalary FiscalInc if AnalystL == 1,absorb(code ind_yr) cluster(code)
outreg2 using Table4a.doc,append tstat bdec(3) tdec(2) ctitle(AnalystL)


* Bank bargain power: Banknums *
bys province: egen count50 = pctile(fifteenkm_banknum),p(50)
gen countH = fifteenkm_banknum>count50
reghdfe exp_assets2 CTAIS Size Lev ROA CashHold BM lnGDP_per AverageSalary FiscalInc if countH == 1,absorb(code ind_yr) cluster(code)
outreg2 using Table4a.doc,append tstat bdec(3) tdec(2) ctitle(BankNumH)
reghdfe lnexpense CTAIS Size Lev ROA CashHold BM lnGDP_per AverageSalary FiscalInc if countH == 1 ,absorb(code ind_yr) cluster(code)
outreg2 using Table4a.doc,append tstat bdec(3) tdec(2) ctitle(BankNumH)
reghdfe exp_assets2 CTAIS Size Lev ROA CashHold BM lnGDP_per AverageSalary FiscalInc if countH == 0,absorb(code ind_yr) cluster(code)
outreg2 using Table4a.doc,append tstat bdec(3) tdec(2) ctitle(BankNumL)
reghdfe lnexpense CTAIS Size Lev ROA CashHold BM lnGDP_per AverageSalary FiscalInc if countH == 0,absorb(code ind_yr) cluster(code)
outreg2 using Table4a.doc,append tstat bdec(3) tdec(2) ctitle(BankNumL)


*** Media coverage ***
bys year: egen Media50 = pctile(Media),p(50)
gen MediaH = Media>Media50
reghdfe exp_assets2 CTAIS Size Lev ROA CashHold BM lnGDP_per AverageSalary FiscalInc if MediaH == 1,absorb(code ind_yr) cluster(code)
outreg2 using Table4a.doc,append tstat bdec(3) tdec(2) ctitle(MediaH)
reghdfe lnexpense CTAIS Size Lev ROA CashHold BM lnGDP_per AverageSalary FiscalInc if MediaH == 1,absorb(code ind_yr) cluster(code)
outreg2 using Table4a.doc,append tstat bdec(3) tdec(2) ctitle(MediaH)
reghdfe exp_assets2 CTAIS Size Lev ROA CashHold BM lnGDP_per AverageSalary FiscalInc if MediaH == 0,absorb(code ind_yr) cluster(code)
outreg2 using Table4a.doc,append tstat bdec(3) tdec(2) ctitle(MediaL)
reghdfe lnexpense CTAIS Size Lev ROA CashHold BM lnGDP_per AverageSalary FiscalInc if MediaH == 0,absorb(code ind_yr) cluster(code)
outreg2 using Table4a.doc,append tstat bdec(3) tdec(2) ctitle(MediaL)



*** EM_Jones ***
use data.dta,clear
bys year: egen Score50 = pctile(EM_Jones),p(50)
gen ScoreH = EM_Jones>=Score50
xtset code year
reghdfe exp_assets2 CTAIS Size Lev ROA CashHold BM lnGDP_per AverageSalary FiscalInc if ScoreH == 1,absorb(code ind_yr) cluster(code)
outreg2 using Table4b.doc,replace tstat bdec(3) tdec(2) ctitle(EMJonesH)
reghdfe lnexpense CTAIS Size Lev ROA CashHold BM lnGDP_per AverageSalary FiscalInc if ScoreH == 1,absorb(code ind_yr) cluster(code)
outreg2 using Table4b.doc,append tstat bdec(3) tdec(2) ctitle(EMJonesH)
reghdfe exp_assets2 CTAIS Size Lev ROA CashHold BM lnGDP_per AverageSalary FiscalInc if ScoreH == 0,absorb(code ind_yr) cluster(code)
outreg2 using Table4b.doc,append tstat bdec(3) tdec(2) ctitle(EMJonesL)
reghdfe lnexpense CTAIS Size Lev ROA CashHold BM lnGDP_per AverageSalary FiscalInc if ScoreH == 0,absorb(code ind_yr) cluster(code)
outreg2 using Table4b.doc,append tstat bdec(3) tdec(2) ctitle(EMJonesL)


* DD *
bys year: egen DD50 = pctile(DD),p(50)
gen DDH = DD>=DD50
reghdfe exp_assets2 CTAIS Size Lev ROA CashHold BM lnGDP_per AverageSalary FiscalInc if DDH == 1,absorb(code ind_yr) cluster(code)
outreg2 using Table4b.doc,append tstat bdec(3) tdec(2) ctitle(EMDDH)
reghdfe lnexpense CTAIS Size Lev ROA CashHold BM lnGDP_per AverageSalary FiscalInc if DDH == 1,absorb(code ind_yr) cluster(code)
outreg2 using Table4b.doc,append tstat bdec(3) tdec(2) ctitle(EMDDH)
reghdfe exp_assets2 CTAIS Size Lev ROA CashHold BM lnGDP_per AverageSalary FiscalInc if DDH == 0,absorb(code ind_yr) cluster(code)
outreg2 using Table4b.doc,append tstat bdec(3) tdec(2) ctitle(EMDDL)
reghdfe lnexpense CTAIS Size Lev ROA CashHold BM lnGDP_per AverageSalary FiscalInc if DDH == 0,absorb(code ind_yr) cluster(code)
outreg2 using Table4b.doc,append tstat bdec(3) tdec(2) ctitle(EMDDL)

* Disclose level: 1 is best, 4 is worst *
use "data.dta",clear
bys year: egen Disclose50 = pctile(EvaluationResult),p(50)
gen DiscloseL = EvaluationResult>Disclose50
xtset code year
reghdfe exp_assets2 CTAIS Size Lev ROA CashHold BM lnGDP_per AverageSalary FiscalInc if DiscloseL == 1,absorb(code ind_yr) cluster(code)
outreg2 using Table4b.doc,append tstat bdec(3) tdec(2) ctitle(DiscloseL)
reghdfe lnexpense CTAIS Size Lev ROA CashHold BM lnGDP_per AverageSalary FiscalInc if DiscloseL == 1,absorb(code ind_yr) cluster(code)
outreg2 using Table4b.doc,append tstat bdec(3) tdec(2) ctitle(DiscloseL)
reghdfe exp_assets2 CTAIS Size Lev ROA CashHold BM lnGDP_per AverageSalary FiscalInc if DiscloseL == 0,absorb(code ind_yr) cluster(code)
outreg2 using Table4b.doc,append tstat bdec(3) tdec(2) ctitle(DiscloseH)
reghdfe lnexpense CTAIS Size Lev ROA CashHold BM lnGDP_per AverageSalary FiscalInc if DiscloseL == 0,absorb(code ind_yr) cluster(code)
outreg2 using Table4b.doc,append tstat bdec(3) tdec(2) ctitle(DiscloseH)

**************************
**       Heterogeneity        **
**************************
* SOEs *
reghdfe exp_assets2 CTAIS Size Lev ROA CashHold BM lnGDP_per AverageSalary FiscalInc if ContrshrNature == 1,absorb(code ind_yr) cluster(code)
outreg2 using Table5a.doc,replace tstat bdec(3) tdec(2) ctitle(SOEs)
reghdfe lnexpense CTAIS Size Lev ROA CashHold BM lnGDP_per AverageSalary FiscalInc if ContrshrNature == 1,absorb(code ind_yr) cluster(code)
outreg2 using Table5a.doc,append tstat bdec(3) tdec(2) ctitle(SOEs)
reghdfe exp_assets2 CTAIS Size Lev ROA CashHold BM lnGDP_per AverageSalary FiscalInc if ContrshrNature == 0,absorb(code ind_yr) cluster(code)
outreg2 using Table5a.doc,append tstat bdec(3) tdec(2) ctitle(Non-SOEs)
reghdfe lnexpense CTAIS Size Lev ROA CashHold BM lnGDP_per AverageSalary FiscalInc if ContrshrNature == 0,absorb(code ind_yr) cluster(code)
outreg2 using Table5a.doc,append tstat bdec(3) tdec(2) ctitle(Non-SOEs)

* non-polluting firms *
gen ifhp = 0
replace ifhp=1 if Industry_complete=="B06" | Industry_complete=="B07" | Industry_complete=="B08" | Industry_complete=="B09" | Industry_complete=="B10" | Industry_complete=="B11" | Industry_complete=="B12" | Industry_complete=="C17"| Industry_complete=="C18" | Industry_complete=="C19" | Industry_complete=="C22" |  Industry_complete=="C25" | Industry_complete=="C26" | Industry_complete=="C27" | Industry_complete=="C28" | Industry_complete=="C29" |  Industry_complete=="C31" | Industry_complete=="C32"| Industry_complete=="D44" 
reghdfe exp_assets2 CTAIS Size Lev ROA CashHold BM lnGDP_per AverageSalary FiscalInc if ifhp == 1,absorb(code ind_yr) cluster(code)
outreg2 using Table5a.doc,append tstat bdec(3) tdec(2) ctitle(Polluting firms)
reghdfe lnexpense CTAIS Size Lev ROA CashHold BM lnGDP_per AverageSalary FiscalInc if ifhp == 1,absorb(code ind_yr) cluster(code)
outreg2 using Table5a.doc,append tstat bdec(3) tdec(2) ctitle(Polluting firms)
reghdfe exp_assets2 CTAIS Size Lev ROA CashHold BM lnGDP_per AverageSalary FiscalInc if ifhp == 0,absorb(code ind_yr) cluster(code)
outreg2 using Table5a.doc,append tstat bdec(3) tdec(2) ctitle(Non-Polluting firms)
reghdfe lnexpense CTAIS Size Lev ROA CashHold BM lnGDP_per AverageSalary FiscalInc if ifhp == 0,absorb(code ind_yr) cluster(code)
outreg2 using Table5a.doc,append tstat bdec(3) tdec(2) ctitle(Non-polluting firms)



***************************************
**        Additional analysis        **
***************************************
reghdfe clean_energy_assets CTAIS Size Lev ROA CashHold BM lnGDP_per AverageSalary FiscalInc ,absorb(code ind_yr) cluster(code)
outreg2 using Table5b.doc,replace tstat bdec(3) tdec(2) ctitle(CleanEnergy)
reghdfe lnclean_energy CTAIS Size Lev ROA CashHold BM lnGDP_per AverageSalary FiscalInc  ,absorb(code ind_yr) cluster(code)
outreg2 using Table5b.doc,append tstat bdec(3) tdec(2) ctitle(lnCleanEnergy)
reghdfe ecology_restore_assets CTAIS Size Lev ROA CashHold BM lnGDP_per AverageSalary FiscalInc  ,absorb(code ind_yr) cluster(code)
outreg2 using Table5b.doc,append tstat bdec(3) tdec(2) ctitle(EcologyRestore)
reghdfe lnecology_restore CTAIS Size Lev ROA CashHold BM lnGDP_per AverageSalary FiscalInc  ,absorb(code ind_yr) cluster(code)
outreg2 using Table5b.doc,append tstat bdec(3) tdec(2) ctitle(lnEcologyRestore)
reghdfe general_envir_assets CTAIS Size Lev ROA CashHold BM lnGDP_per AverageSalary FiscalInc  ,absorb(code ind_yr) cluster(code)
outreg2 using Table5b.doc,append tstat bdec(3) tdec(2) ctitle(GeneralEnv)
reghdfe lngeneral_envir CTAIS Size Lev ROA CashHold BM lnGDP_per AverageSalary FiscalInc ,absorb(code ind_yr) cluster(code)
outreg2 using Table5b.doc,append tstat bdec(3) tdec(2) ctitle(lnGeneralEnv)



**************************
**        ROBUST        **
**************************
* Business tax to Value-added tax *
use data.dta,clear
drop if Industry_complete == "G53" | Industry_complete == "G54" | Industry_complete == "G55" | Industry_complete == "G56" | Industry_complete == "G57" | Industry_complete == "G58" | Industry_complete == "G59"  | Industry_complete == "I63" | Industry_complete == "I64" | Industry_complete == "I65" | Industry_complete == "L72" | Industry_complete == "L71"
drop if industry_2 == "M"
drop if industry_2 == "E"
drop if industry_2 == "K"
drop if industry_2 == "O"
drop if industry_2 == "H"
xtset code year
reghdfe exp_assets2 CTAIS Size Lev ROA CashHold BM lnGDP_per AverageSalary FiscalInc , absorb(code ind_yr) cluster(code)
outreg2 using Table6a.doc,replace tstat bdec(3) tdec(2) ctitle(Controlling influence of business tax to value-added tax policy)
reghdfe lnexpense CTAIS Size Lev ROA CashHold BM lnGDP_per AverageSalary FiscalInc , absorb(code ind_yr) cluster(code)
outreg2 using Table6a.doc,append tstat bdec(3) tdec(2) ctitle(aa)

* Fixed assets acceleration *
use data.dta,clear
drop if Industry_complete == "C17" | Industry_complete == "C19" |Industry_complete == "C20" |Industry_complete == "C21" | Industry_complete == "C22" | Industry_complete == "C24" | Industry_complete == "C25" |Industry_complete == "C26" | Industry_complete == "C27" | Industry_complete == "C28" | Industry_complete == "C29" |Industry_complete == "C30" |Industry_complete == "C31" |industry_2 == "D"
xtset code year
reghdfe exp_assets2 CTAIS Size Lev ROA CashHold BM lnGDP_per AverageSalary FiscalInc , absorb(code ind_yr) cluster(code)
outreg2 using Table6a.doc,append tstat bdec(3) tdec(2) ctitle(Controlling influence of accelerated depreciation policy for fixed assets)
reghdfe lnexpense CTAIS Size Lev ROA CashHold BM lnGDP_per AverageSalary FiscalInc , absorb(code ind_yr) cluster(code)
outreg2 using Table6a.doc,append tstat bdec(3) tdec(2) ctitle(aa)

* Nature resource transfer business tax *
use data.dta,clear
drop if industry_2 == "A" | industry_2 == "B" | industry_2 == "E"
xtset code year
reghdfe exp_assets2 CTAIS Size Lev ROA CashHold BM lnGDP_per AverageSalary FiscalInc , absorb(code ind_yr) cluster(code)
outreg2 using Table6a.doc,append tstat bdec(3) tdec(2) ctitle(Controlling influence of the transfer of natural resource usage rights also requires the collection of business tax)
reghdfe lnexpense CTAIS Size Lev ROA CashHold BM lnGDP_per AverageSalary FiscalInc , absorb(code ind_yr) cluster(code)
outreg2 using Table6a.doc,append tstat bdec(3) tdec(2) ctitle(aa)



* out-of-province subsidies *
use data.dta,clear
xtset code year
reghdfe exp_assets2 CTAIS Size Lev ROA CashHold BM lnGDP_per AverageSalary FiscalInc  if subt!=1,absorb(code ind_yr) cluster(code)
outreg2 using Table6b.doc,replace tstat bdec(3) tdec(2) ctitle(Controling subsidies in different places)
reghdfe lnexpense CTAIS Size Lev ROA CashHold BM lnGDP_per AverageSalary FiscalInc  if subt!=1,absorb(code ind_yr) cluster(code)
outreg2 using Table6b.doc,append tstat bdec(3) tdec(2) ctitle(aa)


* first-tier cities *
use data.dta,clear
drop if city == "上海市"
drop if city == "北京市"
drop if city == "深圳市"
drop if city == "广州市"
xtset code year
reghdfe exp_assets2 CTAIS Size Lev ROA CashHold BM lnGDP_per AverageSalary FiscalInc ,absorb(code ind_yr) cluster(code)
outreg2 using Table6b.doc,append tstat bdec(3) tdec(2) ctitle(Controling first-tier cities)
reghdfe lnexpense CTAIS Size Lev ROA CashHold BM lnGDP_per AverageSalary FiscalInc ,absorb(code ind_yr) cluster(code)
outreg2 using Table6b.doc,append tstat bdec(3) tdec(2) ctitle(aa)

* province linear trend *
use data.dta,clear
lab var province "newProvince"
egen newProvince = group(province), label
gen prov_yr = newProvince*year
xtset code year
reghdfe exp_assets2 CTAIS Size Lev ROA CashHold BM lnGDP_per AverageSalary FiscalInc prov_yr,absorb(code ind_yr) cluster(code)
outreg2 using Table6b.doc,append tstat bdec(3) tdec(2) ctitle(Controling province linear trend)
reghdfe lnexpense CTAIS Size Lev ROA CashHold BM lnGDP_per AverageSalary FiscalInc prov_yr,absorb(code ind_yr) cluster(code)
outreg2 using Table6b.doc,append tstat bdec(3) tdec(2) ctitle(Controling province linear trend)



*** Controling additional variables ***
** TaxAvoidance **
use data.dta,clear
reghdfe exp_assets2 CTAIS DDBTD Size Lev ROA CashHold BM lnGDP_per AverageSalary FiscalInc ,absorb(code ind_yr) cluster(code)
outreg2 using Table6c.doc,replace tstat bdec(3) tdec(2) ctitle(Controling TaxAvoidance)
reghdfe lnexpense CTAIS DDBTD Size Lev ROA CashHold BM lnGDP_per AverageSalary FiscalInc ,absorb(code ind_yr) cluster(code)
outreg2 using Table6c.doc,append tstat bdec(3) tdec(2) ctitle(Controling Tax)

** Earning management **
reghdfe exp_assets2 CTAIS EM_Jones Size Lev ROA CashHold BM lnGDP_per AverageSalary FiscalInc ,absorb(code ind_yr) cluster(code)
outreg2 using Table6c.doc,append tstat bdec(3) tdec(2) ctitle(Controling earning management)
reghdfe lnexpense CTAIS EM_Jones Size Lev ROA CashHold BM lnGDP_per AverageSalary FiscalInc ,absorb(code ind_yr) cluster(code)
outreg2 using Table6c.doc,append tstat bdec(3) tdec(2) ctitle(Controling earning management)

** AuditFee **
replace AuditFee = ln(AuditFee+1)
reghdfe exp_assets2 CTAIS AuditFee Size Lev ROA CashHold BM lnGDP_per AverageSalary FiscalInc ,absorb(code ind_yr) cluster(code)
outreg2 using Table6c.doc,append tstat bdec(3) tdec(2) ctitle(Controling audit fee)
reghdfe lnexpense CTAIS AuditFee Size Lev ROA CashHold BM lnGDP_per AverageSalary FiscalInc ,absorb(code ind_yr) cluster(code)
outreg2 using Table6c.doc,append tstat bdec(3) tdec(2) ctitle(Controling audit fee)

** CapitalInvestment **
replace CapitalInvestment = ln(CapitalInvestment+1)
reghdfe exp_assets2 CTAIS CapitalInvestment Size Lev ROA CashHold BM lnGDP_per AverageSalary FiscalInc ,absorb(code ind_yr) cluster(code)
outreg2 using Table6c.doc,append tstat bdec(3) tdec(2) ctitle(Controling capital investment)
reghdfe lnexpense CTAIS CapitalInvestment Size Lev ROA CashHold BM lnGDP_per AverageSalary FiscalInc ,absorb(code ind_yr) cluster(code)
outreg2 using Table6c.doc,append tstat bdec(3) tdec(2) ctitle(Controling capital investment)

** All vars **
reghdfe exp_assets2 CTAIS DDBTD EM_Jones AuditFee CapitalInvestment Size Lev ROA CashHold BM lnGDP_per AverageSalary FiscalInc ,absorb(code ind_yr) cluster(code)
outreg2 using Table6c.doc,append tstat bdec(3) tdec(2) ctitle(Controling capital investment)
reghdfe lnexpense CTAIS DDBTD EM_Jones AuditFee CapitalInvestment Size Lev ROA CashHold BM lnGDP_per AverageSalary FiscalInc ,absorb(code ind_yr) cluster(code)
outreg2 using Table6c.doc,append tstat bdec(3) tdec(2) ctitle(Controling capital investment)

* Placebo test *
mat b = J(5000,1,0)
mat se = J(5000,1,0)
mat p = J(5000,1,0)

forvalues i = 1/5000{
	use data_td4.dta, clear
	xtset code year
	bsample 1, strata(newProvince) 
	keep year
	save matchyear.dta, replace
	mkmat year, matrix(sampleyear)
	use data_td4.dta, clear
	xtset code year
	gen DID = 0
	foreach j of numlist 1/31 {
		replace DID = 1 if (newProvince == `j' & year >= sampleyear[`j',1])
	}
	qui reghdfe exp_assets2 DID L.Size L.ROA L.Lev L.CashHold L.BM L.lnGDP_per L.AverageSalary L.FiscalInc, absorb(code ind_yr) cluster(code)
	mat b[`i',1] = _b[DID]
	mat se[`i',1] = _se[DID]
	scalar df_r = e(N) - e(df_m) -1
	mat p[`i',1] = 2*ttail(df_r,abs(_b[DID]/_se[DID]))
}


svmat b, names(coef)
svmat se, names(se)
svmat p, names(pvalue)

drop if pvalue1 == .

twoway (scatter pvalue1 coef1, xline(0 -0.03, lwidth(0.2) lp(shortdash)) xlabel(-0.05(0.01)0.1, grid) xtitle(Coefs) ytitle(pvalue) msymbol(smcircle_hollow) mcolor(orange) legend(off)) (kdensity coef1, title(Placebo Test))












