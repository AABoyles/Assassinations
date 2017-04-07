************
* Make tables for Hit or Miss?
* Final AEJ version
* August 20, 2008
************

version 9.2
clear
set mem 300m
set more off
capture  log close
log using MakeTablesAndFigures, t replace

local fixedeffectvars = "weapondum* numserdum* " 		


****************************
* Build variables
****************************

*** Get data
use mergeddata, clear
keep if year >= 1875

* Make tenure variable
g clock = ceil(sumten / 365)

*** Check if there are multiple attempts/successes per given country-year
g event_marker = 0
replace event_marker = 1 if result !=.
sort cowcode year
by cowcode year: egen count_events = sum(event_marker)
by cowcode year: egen num_seriousevents = sum(numseriousattemptsleaderyear )
sum count_events 

g death = 0
replace death = 1 if result>=11 & result<=19
by cowcode year: egen death_event = max(death)

g seriouswoundedtemp = 0
replace seriouswoundedtemp = 1 if result >= 11 & result <= 22
by cowcode year: egen seriouswounded_event = max(seriouswoundedtemp)

g woundedbystandertemp = 0 
replace woundedbystandertemp = 1 if result >= 11 & result <= 23
replace woundedbystandertemp = 1 if woundedinattempt > 0 & woundedinattempt != .
by cowcode year: egen woundedbystander_event = max(woundedbystandertemp)

g woundedtemp = 0
replace woundedtemp = 1 if result >= 11 & result <= 23
by cowcode year: egen wounded_event = max(woundedtemp)

g seriousattempttemp = 0
replace seriousattempttemp = 1 if result >= 11 & result <= 29
by cowcode year: egen seriousattempt_event = max(seriousattempttemp)

sort cowcode year obsid
by cowcode: g tempoutyear = 1 if obsid[_n] != obsid[_n+1]
g naturaldeathtemp = 1 if exit == 2 & tempoutyear == 1 & clock >= 0 & clock != .
by cowcode year: egen naturaldeath_event = max(naturaldeathtemp)
replace event_marker = 1 if naturaldeathtemp == 1

*** Define a year by the worst outcome:  if there was a successful assassination, then this is a treatment
g attempt = 0
replace attempt = 1 if count_events>=1
g seriouswounded = 0
replace seriouswounded = 1 if count_events>=1 & seriouswounded_event == 1
g woundedbystander = 0
replace woundedbystander = 1 if count_events>=1 & woundedbystander_event == 1
g wounded = 0
replace wounded = 1 if count_events>=1 & wounded_event == 1
g seriousattempt = 0
replace seriousattempt = 1 if count_events>=1 & seriousattempt_event == 1
g success = 0
replace success = 1 if count_events>=1 & death_event==1
g naturaldeath = 0
replace naturaldeath = 1 if naturaldeath_event == 1 


* Figure out how many leader changes per year

sort cowcode year obsid
by cowcode: g leadchange = (obsid != obsid[_n-1]) & obsid != "" & obsid[_n-1] != ""
by cowcode  year: egen numleadchange = sum(leadchange) 


* Figure out how many regular / irregular exits
g outdate = date(enddate,"dmy")
g out_yr = year(outdate)

g exitany = 1 if year == out_yr & out_yr != .
g exitregular = (exitany & exit == 1 ) & exitany != . & exit != .
g exitnaturaldeath = (exitany & (exit == 2 | exit == 2.1)) & exitany != . & exit != .
g exitirregular = (exitany & (exit == 3 | exit == 2.2 | exit == 4)) & exitany != . & exit != .

for any any regular naturaldeath irregular: by cowcode year: egen numexitX = sum(exitX)

* Put into country year format, with markers for assassinations and attempts
gsort +cowcode +year -event_marker +obsid
by cowcode year: keep if _n == 1


*** Mark years between successive attempts / successes
by cowcode:  g yr1 = year if count_events>=1
by cowcode:  replace yr1 = year if _n==1
by cowcode:  g yr2 = year if count_events>=1
by cowcode:  replace yr2 = year if _n==_N

by cowcode:  replace yr1 = yr1[_n-1] if yr1[_n-1]!=. & yr1==.
gsort cowcode -year
by cowcode: replace yr2 = yr2[_n-1] if yr2[_n-1]!=. & yr2==. 

sort cowcode year
g distance = yr2 - yr1
replace distance = distance[_n-1] if count_events>=1
replace distance = distance[_n+1] if count_events>=1 & distance[_n+1] < distance[_n-1]

*** create varialbe so that we can drop events other than the first attempt on each leader as robustness
capture drop temp*
g temp1 = 1 if result != .
sort cowcode obsid year
by cowcode obsid: g temp2 = sum(temp1) if result != .
g firstattempt = (temp2 == 1) if temp2 != .
drop temp*



* Compute percent leader change variables
encode cowcode, g(countrynum)
tsset countrynum year

foreach Y of var numleadchange numexit*  {
	for num 1 5 10 20: g `Y'X1 = f.`Y'
	for num 2/5: replace `Y'51 = fX.`Y' + `Y'51 
	for num 2/10: replace `Y'101 = fX.`Y' + `Y'101
	for num 2/20: replace `Y'201 = fX.`Y' + `Y'201 
	
	g `Y'2010 = `Y'201 - `Y'101

	***********
	* Make another version excluding the current leader
	***********
	for num 1 5 10 20: g `Y'NCX1 = f.`Y' if f.year > out_yr
	for num 1 5 10 20: replace `Y'NCX1 = 0 if f.year <= out_yr
	
	for num 2/5: replace `Y'NC51 = fX.`Y' + `Y'NC51  if fX.year > out_yr
	for num 2/10: replace `Y'NC101 = fX.`Y' + `Y'NC101 if fX.year > out_yr
	for num 2/20: replace `Y'NC201 = fX.`Y' + `Y'NC201 if fX.year > out_yr
	
	g `Y'NC2010 = `Y'NC201 - `Y'NC101

}
g numexitanylast5 = l.numexitany + l1.numexitany + l2.numexitany + l3.numexitany + l4.numexitany + l5.numexitany

for any regular irregular: g perexitX11 = numexitX11 / (numexitregular11 + numexitirregular11)
for any regular irregular: g perexitX51 = numexitX51 / (numexitregular51 + numexitirregular51)
for any regular irregular: g perexitX101 = numexitX101 / (numexitregular101 + numexitirregular101)
for any regular irregular: g perexitX201 = numexitX201 / (numexitregular201 + numexitirregular201)
for any regular irregular: g perexitX2010 = numexitX2010 / (numexitregular2010 + numexitirregular2010)


for any regular irregular: g perexitXNC11 = numexitXNC11 / (numexitregularNC11 + numexitirregularNC11)
for any regular irregular: g perexitXNC51 = numexitXNC51 / (numexitregularNC51 + numexitirregularNC51)
for any regular irregular: g perexitXNC101 = numexitXNC101 / (numexitregularNC101 + numexitirregularNC101)
for any regular irregular: g perexitXNC201 = numexitXNC201 / (numexitregularNC201 + numexitirregularNC201)
for any regular irregular: g perexitXNC2010 = numexitXNC2010 / (numexitregularNC2010 + numexitirregularNC2010)

* gen normed vars
g npolity2 = (polity2 + 10) / 20 if polity2 >= -10
g npolity2dummy = (npolity2 > .5) if npolity2 != .

tsset
foreach X of var npolity2 npolity2dummy {
	
	g `X'11 = f.`X' - l.`X'
	g `X'101 = f10.`X' - l.`X'
	g `X'201 = f20.`X' - l.`X'
	
	for var `X'*1 : g absX = abs(X)
	
}
replace polity = . if polity < -10

g lautoc = 1 if l.polity2 <= 0 
replace lautoc = 0 if l.polity2 > 0 & l.polity2 != .
g ldemoc = 1-lautoc
for var success attempt wounded seriousattempt : g Xlautoc = X * lautoc
for var success attempt wounded seriousattempt : g Xldemoc = X * ldemoc

label var successlautoc "Success * Lag Autoc"
label var successldemoc  "Success * Lag Democ"
label var lautoc "Lag Autoc"
label var success "Success"



* Set up the fixed effects variables

* Quarter century dummies
g qtrcenturytemp = floor(year / 25)
replace qtrcenturytemp = 1975/25 if year >= 2000
tab qtrcenturytemp, g(qtrcentury)
*omit first category
drop qtrcentury1
drop qtrcenturytemp 

replace weapon1 = 9 if weapon1 == . /*since 9 is 'unknown'*/
tab weapon1, g(weapondum)
*omit first category
drop weapondum1

g regdumAfrica = (region == 1) if region != .
g regdumAsia = (region == 2 | region == 3 | region == 4 | region == 5) if region != .
g regdumMENA = (region == 6 | region == 11) if region != .
g regdumLatAm = (region == 7 | region == 8) if region != .
g regdumEEur = (region == 9) if region != .
*g regdumWEurUSA = (region == 10) if region != .


* dummy out num serious attempts
g numserdum2 = (num_seriousevents == 2)
g numserdum3 = (num_seriousevents== 3)
g numserdum4 = (num_seriousevents == 4)

* Set up war variables
g prioanywar = 1 if prioconflictextrastate == 1 | prioconflictinterstate == 1 | prioconflictinternal == 1 | prioconflictintinternal == 1
replace prioanywar = 2 if prioconflictextrastate == 2 | prioconflictinterstate == 2 | prioconflictinternal == 2 | prioconflictintinternal == 2
replace prioanywar = 0 if prioconflictextrastate == 0 & prioconflictinterstate == 0 & prioconflictinternal == 0 & prioconflictintinternal == 0

for var prioanywar : replace X = X / 2 /*so it's on a 0-1 scale*/

g zGledAnywar = (zGledCivil == 1 | zGledInter == 1 | zGledExt == 1) if zGledInter != . & zGledCivil != . & zGledExt != .

* Make a set of prio and gleditschvariables defined over the same years
g sampleP = 1
for var prioanywar zGledAnywar: replace sampleP = 0 if X == .
for var zGledAnywar prioanywar : g XP = X if sampleP == 1

* Tenure variables
g lclock = l.clock
g durgroup = 1 if lclock <= 10
replace durgroup = 2 if lclock > 10 & lclock != .

save country_year_data, replace




****************************
* Make table program that displays results
****************************
capture program drop maketablerank
program define maketablerank

	local maxrows = 500
	
	syntax using/, rhs(string) varcol(string) [rankpval(real 99)] [replace] [append] [noastr] [pval]
	
	/*inputs:
	 1 = dep variable name
	 2 = indep variable name
	 3 = variable number (this is the nth variable in the table)*/ 
	
	local lhs = e(depvar)
	local newb = _b[`rhs']
	local newse = _se[`rhs']
	qui test `rhs' = 0
	local newp = r(p)
	
	if "`replace'" != "replace" & "`append'" != "append" {
		display "Must specify either replace or append"
		exit	
	}
	
	/*initialize dataset*/
	preserve
	clear
	if "`replace'" == "replace" {
		qui set obs `maxrows'
		qui g col0 = ""
		qui g col1 = "`varcol'" in 1
		local whichcol = "col1"
	}
	else {
		capture insheet using `using'.out, names
		if _rc != 0 {
			display "File not found"
			exit
		}
		local whichcol = ""
		local howmanycols = 0
		foreach X of var col* {
			qui count if `X' == "`varcol'"
			if r(N) > 0 {
				local whichcol = "`X'"	
			}
			local howmanycols = `howmanycols' + 1
		}
			
		if "`whichcol'" == "" {
			/*make a new column*/
			qui g col`howmanycols' = "`varcol'" in 1
			local whichcol = "col`howmanycols'"
			}					
			
	}
	
	/*find which row to use*/
	local lastrow = 0
	local brow = 0
	foreach X of num 1/`maxrows' {
		qui count if col0 == "`lhs'" in `X'
		if r(N) == 1 & `brow' == 0 {
			local brow = `X'	
		}
		qui count if col0 != "" in `X'
		if r(N) == 1 {
			local lastrow = `X'	
		}	
	}
	if `brow' == 0 {
		local brow = `lastrow' + 2
		if "`pval'" == "pval" {
			local brow = `brow' + 1
		}
		
		if `rankpval' != 99 {
			local brow = `brow' + 1
		}
		
	}
	
	local browse = `brow' + 1
	qui replace col0 = "`lhs'" in `brow'
	qui replace `whichcol' = string(`newb',"%4.3f") in `brow'
	qui replace `whichcol' = "(" + string(`newse',"%4.3f") + ")" in `browse'
	
	if "`noastr'" != "noastr" {
			qui replace `whichcol' = `whichcol' + "*" in `brow' if `newp' <= 0.10
			qui replace `whichcol' = `whichcol' + "*" in `brow' if `newp' <= 0.05
			qui replace `whichcol' = `whichcol' + "*" in `brow' if `newp' <= 0.01
	}
	
	local browpval = `browse' 
	if "`pval'" == "pval" {
			local browpval = `browse' + 1
			qui replace `whichcol' = string(`newp',"%3.2f") in `browpval'
		
	}
	if `rankpval' != 99 {
			local browpval = `browpval' + 1
			qui replace `whichcol' = string(`rankpval',"%3.2f")  in `browpval'
		
	}
	
	outsheet using `using'.out, replace
	restore

end



********************************************
*** Figure 1
********************************************

use assassinations_data, clear

*** Condense to year level observations
bys year:  egen success_peryr = sum(success)
by year:  egen attempt_peryr = sum(attempt)
by year:  egen serious_peryr = sum(serious)
by year:  keep if _n==1
g rate = success_peryr / serious_peryr
reg rate year
sort year
save yearlevelattempts, replace

* Get data on number of countries per year
use country_year_data, clear
keep cowcode year
bys year:  g numcountries = _N
by year:  keep if _n==1
keep year numcountries
keep if year>=1875
sort year
merge year using yearlevelattempts
drop _merge
erase yearlevelattempts.dta

* Get number of attempts per country data
foreach var of varlist success attempt serious {
	g `var'_percountryyr = `var'_peryr / numcountries
}

* Get data on numbers by decade
g decade = year - mod(year,10)
sort decade
foreach var of varlist attempt success serious {
	by decade:  egen sum_`var' = sum(`var'_peryr)
	replace sum_`var' = sum_`var' / 10
	by decade:  egen sum_`var'c = sum(`var'_percountryyr)
	replace sum_`var'c = sum_`var'c / 10
	
}

*** Figure 1a
label var sum_attempt "Attempts per Year"
label var sum_success "Assassinations per Year"
twoway (line sum_attempt decade) (line sum_success decade, clp(-) yaxis(2)) if decade>=1880 & decade<=1990, xtitle("Decade") xlabel(1880(20)1980) ylabel(0(1)4, axis(1)) saving("linesum", replace)

*** Figure 1b
label var sum_attemptc "Attempts per Country-Year"
label var sum_successc "Assassinations per Country-Year"
twoway (line sum_attemptc decade) (line sum_successc decade, clp(-) yaxis(2)) if decade>=1880 & decade<=1990, xtitle("Decade") xlabel(1880(20)1980) ylabel(0(.01).05, axis(1)) ylabel(0(.002).01, axis(2)) legend(size(small)) saving("linesum_country", replace)

*****************************************
*** Figure 2
*****************************************

use country_year_data, clear

* Any attempts
twoway histogram polity2 if attempt==0, discrete || kdensity polity2 if attempt==0, ylabel(0(.05).2) legend(off) ytit("Frequency") xtitle("Polity2 Variable") title("Years Without Asssassination Attempt") saving("FigE1a", replace)
twoway histogram polity2 if attempt==1, discrete || kdensity polity2 if attempt==1, ylabel(0(.05).2) legend(off) ytit("Frequency") xtitle("Polity2 Variable") title("Years With Asssassination Attempt") saving("FigE1b", replace)
graph combine FigE1a.gph FigE1b.gph, saving("FigE1", replace)

* Serious attempts
*twoway histogram polity2 if seriousattempt==1, discrete || kdensity polity2 if seriousattempt==1, ylabel(0(.05).2) legend(off) ytit("Frequency") xtitle("Polity2 Variable") title("Years With Serious Attempts") saving("FigE1bS", replace)
*graph combine FigE1a.gph FigE1bS.gph, saving("FigE1S", replace)

**************************************
*** Table 1: List of assassinated leaders
**************************************

use assassinations_data, clear
sort country year
list country year leadername weapon1 weapon2 if success==1

**************************************
*** Table 2: Summary Stats
**************************************

use assassinations_data, clear

*** COUNTS ***
** Weapons (codebook: 1 "gun" 2 "knife" 3 "explosive device" 4 "poison" 8 "other" 9 "unknown")
g gun = 0 if weapon1!=.
replace gun = 1 if weapon1==1 | weapon2==1 | weapon3==1
g bomb = 0 if weapon1!=.
replace bomb = 1 if weapon1==3 | weapon2==3 | weapon3==3
g knife = 0 if weapon1!=.
replace knife = 1 if weapon1==2 | weapon2==2 | weapon3==2
g other = 0 if weapon1!=.
replace other = 1 if weapon1==8 | weapon2==8 | weapon3==8 | weapon1==4 | weapon2==4 | weapon3==4
g unknown = 0 if weapon1!=.
replace unknown = 1 if weapon1==9 | weapon2==9 | weapon3==9

tab gun
tab bomb
tab knife
tab other
tab unknown

* LOCATION
g abroad = 0
replace abroad = 1 if loccntry!=country & loccntry!="" & country!=""
g home = !abroad
tab abroad

* SOLO V GROUP
g group = !solo
tab solo

*** PROBABILITY LEADER KILLED ***
foreach var of varlist gun bomb knife other unknown abroad home solo group {
	disp " "
	disp "***********************"
	disp "`var'"
	disp "all attempts"
	tab success if `var'==1
	disp "serious attempts"
	tab success if `var'==1 & serious==1
}
tab success
tab success if serious==1

*** CASUALTIES ***

foreach var of varlist gun bomb knife other unknown abroad home solo group {
	table `var', contents(mean dead mean wound)
}
sum dead wound
sum dead wound if serious==1


**************************************
*** Table 4: Exogeniety tests 
*** (Note:  Table 3 code follows)
**************************************

use country_year_data, clear

tsset
g anywarl1l3 = l.zGledAnywar - l3.zGledAnywar
g npolity2l1l3 = l.npolity2 - l3.npolity2
g lnenergy_pc = ln(energy) - ln(tpop)
g lnpop = ln(tpop)

g pol2dum = 0 if polity2<=0 & polity2!=. 
replace pol2dum = 1 if polity2>0 & polity2!=. 
g lpol2dum = l.pol2dum
g pol2duml1l3 = l.pol2dum - l3.pol2dum
for var npolity2 zGledAnywar lnenergy_pc lnpop age : g lX = l.X

foreach X of var prioanywar prioanywarP zGledAnywar zGledAnywarP {
	g `X'11 = f.`X' - l.`X'
	
}
capture drop outinfo*  varname
local numvar = 1

g str40 varname = ""
g outinfo1 = .
g outinfo2 = .
g outinfo3 = .
g outinfo4 = .
local numvar = 1

preserve
		
capture drop ltenure
capture drop lage

tsset
g ltenure = l.clock
g lage = l.age

label var lpol2dum "Democracy dummy (L1)"
label var pol2duml1l3 "Change in democracy dummy (L1 - L3)"
label var lzGledAnywar "Any war dummy (L1)"
label var anywarl1l3 "Change in any war dummy (L1 - L3)"
label var llnenergy_pc "Log energy user per capita (L1)"
label var llnpop "Log population (L1)"
label var lage "Age of leader (L1)"
label var lclock "Tenure of leader (L1)"


* move the dataset to the format where each observation is an attempt, rather than a country-year
bys cowcode year: assert _n == 1
expand num_seriousevents

* make sure we only have 1 success per year
bys cowcode year: replace success = 0 if _n > 1

foreach X of var lpol2dum pol2duml1l3 numexitanylast5 lzGledAnywar anywarl1l3 llnenergy_pc llnpop lage lclock {
	
	local numstd = `numvar' + 1
	replace varname = "`X'" in `numvar'
	
	qui summ `X' if success == 1 & seriousattempt == 1
	replace outinfo1 = r(mean) in `numvar'
	replace outinfo1 = -1 * r(sd) / (r(N) ^ .5) in `numstd'
	
	qui summ `X' if success == 0 & seriousattempt == 1
	replace outinfo2 = r(mean) in `numvar'
	replace outinfo2 = -1 * r(sd) / (r(N) ^ .5) in `numstd'
	
	ttest `X' if seriousattempt == 1, by(success) unequal
	replace outinfo3 = outinfo1 - outinfo2 in `numvar'
	replace outinfo3 = -1 * r(se) in `numstd'
	
	replace outinfo4 = r(p) in `numvar'
	
	local numvar = `numvar' + 2
	}

qui count if success == 1 & seriousattempt == 1
replace outinfo1 = r(N) in `numvar'
qui count if success == 0 & seriousattempt == 1
replace outinfo2 = r(N) in `numvar'


outsheet varname outinfo* using table_4a.out in 1/`numvar', replace
	
	
* Parallel to below
dprobit success lpol2dum pol2duml1l3 lzGledAnywar anywarl1l3 llnenergy_pc llnpop lage lclock if seriousattempt == 1, cluster(cowcode)
testparm lpol2dum pol2duml1l3 numexitanylast5 lzGledAnywar anywarl1l3 llnenergy_pc llnpop lage lclock 
local pval = r(p)
testparm *
outreg using table_4b, 3aster coefastr se replace title("table 3 regs") bd(3) adds("P-val",`pval',"P-val of regression",r(p))

dprobit success lpol2dum pol2duml1l3 lzGledAnywar anywarl1l3 llnenergy_pc llnpop lage lclock weapondum* if seriousattempt == 1, cluster(cowcode)
testparm lpol2dum pol2duml1l3 numexitanylast5 lzGledAnywar anywarl1l3 llnenergy_pc llnpop lage lclock 
local pval = r(p)
testparm *
outreg using table_4b, 3aster coefastr se append bd(3) adds("P-val",`pval',"P-val of regression",r(p))


dprobit success lpol2dum pol2duml1l3 lzGledAnywar anywarl1l3 llnenergy_pc llnpop lage lclock regdumAfrica regdumAsia regdumMENA regdumLatAm regdumEEur   if seriousattempt == 1, cluster(cowcode)
testparm lpol2dum pol2duml1l3 numexitanylast5 lzGledAnywar anywarl1l3 llnenergy_pc llnpop lage lclock 
local pval = r(p)
testparm *
outreg using table_4b, 3aster coefastr se append bd(3) adds("P-val",`pval',"P-val of regression",r(p))

dprobit success lpol2dum pol2duml1l3 lzGledAnywar anywarl1l3 llnenergy_pc llnpop lage lclock regdumAfrica regdumAsia regdumMENA regdumLatAm regdumEEur weapondum*    if seriousattempt == 1, cluster(cowcode)
testparm lpol2dum pol2duml1l3 numexitanylast5 lzGledAnywar anywarl1l3 llnenergy_pc llnpop lage lclock 
local pval = r(p)
testparm *
outreg using table_4b, 3aster coefastr se append bd(3) adds("P-val",`pval',"P-val of regression",r(p))

restore

**************************************
*** Table 3: Summary Stats: Dependent Variables
**************************************

** Get average transition probabilities	
** Polity dummy

g trans = .
g toauttrans = .
g todemtrans = .
g toauttransD = .
g todemtransA = .

g period1 = 1 if year>=1875 & year<=1949
g period2 = 1 if year>=1950 & year<=2004
g period3 = 1 if year>=1875 & year<=2004

g perlab2 = "pre 1950" in 1
replace perlab2 = "post 1950" in 2
replace perlab2 = "All Years" in 3

* Polity data
foreach num of numlist 1/3 {

	qui tab absnpolity2dummy11 if period`num'==1, g(np2_p`num'_t)
	qui sum np2_p`num'_t2
	replace trans = `r(mean)' in `num'

	qui tab npolity2dummy11 if period`num'==1, g(np2_p`num'_)
	qui sum np2_p`num'_1
	replace toauttrans = `r(mean)' in `num'
	qui sum np2_p`num'_3
	replace todemtrans = `r(mean)' in `num'

	qui tab npolity2dummy11 if period`num'==1 & lautoc==1, g(np2_p`num'_a)
	qui sum np2_p`num'_a2
	replace todemtransA = `r(mean)' in `num'
	
	qui tab npolity2dummy11 if period`num'==1 & ldemoc==1, g(np2_p`num'_d)
	qui sum np2_p`num'_d1
	replace toauttransD = `r(mean)' in `num'

	drop np2_p*

}

* Panel A
list perlab2 trans toauttransD todemtransA in 1/3

* Archigos data
drop *trans*
g trans = .
g toauttrans = .
g todemtrans = .
g toauttransD = .
g todemtransA = .

foreach num of numlist 1/3 {

	disp "1"
	sum perexitregular201 if period`num'==1
	replace trans = `r(mean)' in `num'

	disp "2"
	qui sum perexitregular201 if period`num'==1 & lautoc==1
	replace todemtransA = `r(mean)' in `num'
	
	disp "3"
	qui sum perexitregular201 if period`num'==1 & ldemoc==1
	replace toauttransD = `r(mean)' in `num'

}

* Panel B
list perlab2 trans toauttransD todemtransA in 1/3


** War transitions
g prioanywar11B = 0 if prioanywar11==0
replace prioanywar11B = 1 if prioanywar11>0 & prioanywar11!=.
replace prioanywar11B = -1 if prioanywar11<0 & prioanywar11!=.
g lprioanywar = l.prioanywar

* COW data
foreach var of varlist zGledAnywar11 {
	
	g up`var'=.
	g down`var'=.

	g up`var'NW=.
	g down`var'W=.
	
	* by period	
	foreach num of numlist 1/3 {
			
		qui tab `var' if period`num'==1, g(`var'p`num'_)
		qui sum `var'p`num'_3
		replace up`var' = `r(mean)' in `num'
		qui sum `var'p`num'_1
		replace down`var' = `r(mean)' in `num'

		qui tab `var' if period`num'==1 & lzGledAnywar==0, g(`var'p`num'_NW)
		qui sum `var'p`num'_NW2
		replace up`var'NW = `r(mean)' in `num'

		qui tab `var' if period`num'==1 & lzGledAnywar==1, g(`var'p`num'_W)
		qui sum `var'p`num'_W1
		replace down`var'W = `r(mean)' in `num'

		drop `var'p`num'_*

	}
	
}

* PRIO data
foreach var of varlist prioanywar11B {
	
	g up`var'=.
	g down`var'=.

	g up`var'NW=.
	g down`var'W=.
	
	* by period	
	foreach num of numlist 2/3 {
		
		qui tab `var' if period`num'==1, g(`var'p`num'_)
		qui sum `var'p`num'_3
		qui replace up`var' = `r(mean)' in `num'
		qui sum `var'p`num'_1
		replace down`var' = `r(mean)' in `num'

		qui tab `var' if period`num'==1 & lprioanywar==0, g(`var'p`num'_NW)
		qui sum `var'p`num'_NW2
		replace up`var'NW = `r(mean)' in `num'

		qui tab `var' if period`num'==1 & lprioanywar==1, g(`var'p`num'_W)
		qui sum `var'p`num'_W1
		replace down`var'W = `r(mean)' in `num'

		drop `var'p`num'_*

	}

}

*** Prio transition probabilities conditional on moderate war
g upmprioanywar11 = .
g downmprioanywar11 = .
* post 1950 period only
foreach num of numlist 2/3 {
	tab prioanywar11B if period`num'==1 & lprioanywar==.5, g(mprioanywar11p`num'_)
	qui sum mprioanywar11p`num'_3
	replace upmprioanywar11 = `r(mean)' in `num'
	qui sum mprioanywar11p`num'_1
	replace downmprioanywar11 = `r(mean)' in `num'
}

* Panels C and D
list perlab2 upprioanywar11BNW downprioanywar11BW upzGledAnywar11NW downzGledAnywar11W in 1/3
list perlab2 upmprio downmprio in 1/3


**************************************
*** Table 5: Assassinations and Institutional Change
**************************************

reg absnpolity2dummy11  success `fixedeffectvars' if seriousattempt == 1  , cluster(cowcode)
testparm success
local pvalparm = r(p)
tab absnpolity2dummy11  success if seriousattempt == 1, exact chi2
local pvalnonparm = r(p_exact)
outreg using table_5a, 3aster coefastr se adds("Parm p",`pvalparm',"Nonparm p",`pvalnonparm') replace title("Table 5")

reg npolity2dummy11 success `fixedeffectvars' if seriousattempt == 1  , cluster(cowcode)
testparm success
local pvalparm = r(p)
tab npolity2dummy11 success if seriousattempt == 1, exact chi2
local pvalnonparm = r(p_exact)
outreg using table_5a, 3aster coefastr se adds("Parm p",`pvalparm',"Nonparm p",`pvalnonparm') append 

reg perexitregularNC201 success `fixedeffectvars' if seriousattempt == 1  , cluster(cowcode)
testparm success
local pvalparm = r(p)
ranksumben perexitregularNC201 if seriousattempt == 1, by(success)
local pvalnonparm = r(p)
outreg using table_5a, 3aster coefastr se adds("Parm p",`pvalparm',"Nonparm p",`pvalnonparm') append 


reg npolity2dummy11   successlautoc successldemoc lautoc `fixedeffectvars' if seriousattempt == 1  , cluster(cowcode)
testparm successlautoc 
local pvalparmA = r(p)
testparm successldemoc
local pvalparmD = r(p)

tab npolity2dummy11  success if lautoc == 1 & seriousattempt == 1, exact chi2
local pvalnonparmA = r(p_exact)
tab npolity2dummy11  success if ldemoc == 1 & seriousattempt == 1, exact chi2
local pvalnonparmD = r(p_exact)
outreg using table_5b, 3aster coefastr se adds("Autoc-Parm p",`pvalparmA',"Autoc-Nonparm p",`pvalnonparmA',"Democ-Parm p",`pvalparmD',"Democ-Nonparm p",`pvalnonparmD') replace title("Table 5b")


reg perexitregularNC201 successlautoc successldemoc lautoc `fixedeffectvars' if seriousattempt == 1  , cluster(cowcode)
testparm successlautoc 
local pvalparmA = r(p)
testparm successldemoc
local pvalparmD = r(p)

ranksumben perexitregularNC201 if lautoc == 1 & seriousattempt == 1, by(successlautoc)
local pvalnonparmA = r(p)
ranksumben perexitregularNC201 if ldemoc == 1 & seriousattempt == 1, by(successldemoc)
local pvalnonparmD = r(p)
outreg using table_5b, 3aster coefastr se adds("Autoc-Parm p",`pvalparmA',"Autoc-Nonparm p",`pvalnonparmA',"Democ-Parm p",`pvalparmD',"Democ-Nonparm p",`pvalnonparmD') append 


**************************************
*** Table 6: Tenure of Leader and Duration Effects
**************************************

* Column 1: All serious attempts
tab npolity2dummy11 success if seriousattempt == 1, exact chi2
local pvalnonparm = r(p_exact)
reg npolity2dummy11 success `fixedeffectvars' if seriousattempt == 1  , cluster(cowcode)
maketablerank using table_6, rhs(success) varcol(All) replace 	pval noastr rankpval(`pvalnonparm')

tab npolity2dummy101 success if seriousattempt == 1, exact chi2
local pvalnonparm = r(p_exact)
reg npolity2dummy101  success `fixedeffectvars' if seriousattempt == 1  , cluster(cowcode)
maketablerank using table_6, rhs(success) varcol(All) append pval noastr rankpval(`pvalnonparm')

tab npolity2dummy201  success if seriousattempt == 1, exact chi2
local pvalnonparm = r(p_exact)
reg npolity2dummy201  success `fixedeffectvars' if seriousattempt == 1  , cluster(cowcode)
maketablerank using table_6, rhs(success) varcol(All) append pval noastr rankpval(`pvalnonparm')

ranksumben perexitregularNC101 if seriousattempt == 1, by(success)
local pvalnonparm = r(p)
reg perexitregularNC101 success `fixedeffectvars' if seriousattempt == 1  , cluster(cowcode)
maketablerank using table_6, rhs(success) varcol(All) append pval noastr rankpval(`pvalnonparm')

ranksumben perexitregularNC201 if seriousattempt == 1, by(success)
local pvalnonparm = r(p)
reg perexitregularNC201 success `fixedeffectvars' if seriousattempt == 1  , cluster(cowcode)
maketablerank using table_6, rhs(success) varcol(All) append pval noastr rankpval(`pvalnonparm')

ranksumben perexitregularNC2010 if seriousattempt == 1, by(success)
local pvalnonparm = r(p)
reg perexitregularNC2010 success `fixedeffectvars' if seriousattempt == 1  , cluster(cowcode)
maketablerank using table_6, rhs(success) varcol(All) append pval noastr rankpval(`pvalnonparm')


* Column 2: Tenure <= 10
tab npolity2dummy11 success if seriousattempt == 1 & durgroup == 1, exact chi2
local pvalnonparm = r(p_exact)
reg npolity2dummy11 success `fixedeffectvars' if seriousattempt == 1 & durgroup == 1  , cluster(cowcode)
maketablerank using table_6, rhs(success) varcol(TL10) append 	pval noastr rankpval(`pvalnonparm')

tab npolity2dummy101 success if seriousattempt == 1 & durgroup == 1, exact chi2
local pvalnonparm = r(p_exact)
reg npolity2dummy101  success `fixedeffectvars' if seriousattempt == 1 & durgroup == 1  , cluster(cowcode)
maketablerank using table_6, rhs(success) varcol(TL10) append pval noastr rankpval(`pvalnonparm')

tab npolity2dummy201  success if seriousattempt == 1 & durgroup == 1, exact chi2
local pvalnonparm = r(p_exact)
reg npolity2dummy201  success `fixedeffectvars' if seriousattempt == 1 & durgroup == 1  , cluster(cowcode)
maketablerank using table_6, rhs(success) varcol(TL10) append pval noastr rankpval(`pvalnonparm')

ranksumben perexitregularNC101 if seriousattempt == 1 & durgroup == 1, by(success)
local pvalnonparm = r(p)
reg perexitregularNC101 success `fixedeffectvars' if seriousattempt == 1 & durgroup == 1  , cluster(cowcode)
maketablerank using table_6, rhs(success) varcol(TL10) append pval noastr rankpval(`pvalnonparm')

ranksumben perexitregularNC201 if seriousattempt == 1 & durgroup == 1, by(success)
local pvalnonparm = r(p)
reg perexitregularNC201 success `fixedeffectvars' if seriousattempt == 1 & durgroup == 1  , cluster(cowcode)
maketablerank using table_6, rhs(success) varcol(TL10) append pval noastr rankpval(`pvalnonparm')

ranksumben perexitregularNC2010 if seriousattempt == 1 & durgroup == 1, by(success)
local pvalnonparm = r(p)
reg perexitregularNC2010 success `fixedeffectvars' if seriousattempt == 1 & durgroup == 1  , cluster(cowcode)
maketablerank using table_6, rhs(success) varcol(TL10) append pval noastr rankpval(`pvalnonparm')


* Column 3: Tenure > 10
tab npolity2dummy11 success if seriousattempt == 1 & durgroup == 2, exact chi2
local pvalnonparm = r(p_exact)
reg npolity2dummy11 success `fixedeffectvars' if seriousattempt == 1 & durgroup == 2  , cluster(cowcode)
maketablerank using table_6, rhs(success) varcol(TG10) append 	pval noastr rankpval(`pvalnonparm')

tab npolity2dummy101 success if seriousattempt == 1 & durgroup == 2, exact chi2
local pvalnonparm = r(p_exact)
reg npolity2dummy101  success `fixedeffectvars' if seriousattempt == 1 & durgroup == 2  , cluster(cowcode)
maketablerank using table_6, rhs(success) varcol(TG10) append pval noastr rankpval(`pvalnonparm')

tab npolity2dummy201  success if seriousattempt == 1 & durgroup == 2, exact chi2
local pvalnonparm = r(p_exact)
reg npolity2dummy201  success `fixedeffectvars' if seriousattempt == 1 & durgroup == 2  , cluster(cowcode)
maketablerank using table_6, rhs(success) varcol(TG10) append pval noastr rankpval(`pvalnonparm')

ranksumben perexitregularNC101 if seriousattempt == 1 & durgroup == 2, by(success)
local pvalnonparm = r(p)
reg perexitregularNC101 success `fixedeffectvars' if seriousattempt == 1 & durgroup == 2  , cluster(cowcode)
maketablerank using table_6, rhs(success) varcol(TG10) append pval noastr rankpval(`pvalnonparm')

ranksumben perexitregularNC201 if seriousattempt == 1 & durgroup == 2, by(success)
local pvalnonparm = r(p)
reg perexitregularNC201 success `fixedeffectvars' if seriousattempt == 1 & durgroup == 2  , cluster(cowcode)
maketablerank using table_6, rhs(success) varcol(TG10) append pval noastr rankpval(`pvalnonparm')

ranksumben perexitregularNC2010 if seriousattempt == 1 & durgroup == 2, by(success)
local pvalnonparm = r(p)
reg perexitregularNC2010 success `fixedeffectvars' if seriousattempt == 1 & durgroup == 2  , cluster(cowcode)
maketablerank using table_6, rhs(success) varcol(TG10) append pval noastr rankpval(`pvalnonparm')


* Column 4: All serious attempts on autocrats

tab npolity2dummy11 success if seriousattempt == 1  & lautoc == 1, exact chi2
local pvalnonparm = r(p_exact)
reg npolity2dummy11 success `fixedeffectvars' if seriousattempt == 1 & lautoc == 1  , cluster(cowcode)
maketablerank using table_6, rhs(success) varcol(Autoc-All) append pval noastr rankpval(`pvalnonparm')

tab npolity2dummy101 success if seriousattempt == 1 & lautoc == 1, exact chi2
local pvalnonparm = r(p_exact)
reg npolity2dummy101  success `fixedeffectvars' if seriousattempt == 1 & lautoc == 1  , cluster(cowcode)
maketablerank using table_6, rhs(success) varcol(Autoc-All) append pval noastr rankpval(`pvalnonparm')

tab npolity2dummy201  success if seriousattempt == 1 & lautoc == 1, exact chi2
local pvalnonparm = r(p_exact)
reg npolity2dummy201  success `fixedeffectvars' if seriousattempt == 1 & lautoc == 1  , cluster(cowcode)
maketablerank using table_6, rhs(success) varcol(Autoc-All) append pval noastr rankpval(`pvalnonparm')

ranksumben perexitregularNC101 if seriousattempt == 1 & lautoc == 1, by(success)
local pvalnonparm = r(p)
reg perexitregularNC101 success `fixedeffectvars' if seriousattempt == 1 & lautoc == 1  , cluster(cowcode)
maketablerank using table_6, rhs(success) varcol(Autoc-All) append pval noastr rankpval(`pvalnonparm')

ranksumben perexitregularNC201 if seriousattempt == 1 & lautoc == 1, by(success)
local pvalnonparm = r(p)
reg perexitregularNC201 success `fixedeffectvars' if seriousattempt == 1 & lautoc == 1  , cluster(cowcode)
maketablerank using table_6, rhs(success) varcol(Autoc-All) append pval noastr rankpval(`pvalnonparm')

ranksumben perexitregularNC2010 if seriousattempt == 1 & lautoc == 1, by(success)
local pvalnonparm = r(p)
reg perexitregularNC2010 success `fixedeffectvars' if seriousattempt == 1 & lautoc == 1  , cluster(cowcode)
maketablerank using table_6, rhs(success) varcol(Autoc-All) append pval noastr rankpval(`pvalnonparm')


* Column 5: Autocrat with tenure <= 10
tab npolity2dummy11 success if seriousattempt == 1 & lautoc == 1 & durgroup == 1, exact chi2
local pvalnonparm = r(p_exact)
reg npolity2dummy11 success `fixedeffectvars' if seriousattempt == 1 & lautoc == 1 & durgroup == 1  , cluster(cowcode)
maketablerank using table_6, rhs(success) varcol(Autoc-TL10) append 	pval noastr rankpval(`pvalnonparm')

tab npolity2dummy101 success if seriousattempt == 1 & lautoc == 1 & durgroup == 1, exact chi2
local pvalnonparm = r(p_exact)
reg npolity2dummy101  success `fixedeffectvars' if seriousattempt == 1 & lautoc == 1 & durgroup == 1  , cluster(cowcode)
maketablerank using table_6, rhs(success) varcol(Autoc-TL10) append pval noastr rankpval(`pvalnonparm')

tab npolity2dummy201  success if seriousattempt == 1 & lautoc == 1 & durgroup == 1, exact chi2
local pvalnonparm = r(p_exact)
reg npolity2dummy201  success `fixedeffectvars' if seriousattempt == 1 & lautoc == 1 & durgroup == 1  , cluster(cowcode)
maketablerank using table_6, rhs(success) varcol(Autoc-TL10) append pval noastr rankpval(`pvalnonparm')

ranksumben perexitregularNC101 if seriousattempt == 1 & lautoc == 1 & durgroup == 1, by(success)
local pvalnonparm = r(p)
reg perexitregularNC101 success `fixedeffectvars' if seriousattempt == 1 & lautoc == 1 & durgroup == 1  , cluster(cowcode)
maketablerank using table_6, rhs(success) varcol(Autoc-TL10) append pval noastr rankpval(`pvalnonparm')

ranksumben perexitregularNC201 if seriousattempt == 1 & lautoc == 1 & durgroup == 1, by(success)
local pvalnonparm = r(p)
reg perexitregularNC201 success `fixedeffectvars' if seriousattempt == 1 & lautoc == 1 & durgroup == 1  , cluster(cowcode)
maketablerank using table_6, rhs(success) varcol(Autoc-TL10) append pval noastr rankpval(`pvalnonparm')

ranksumben perexitregularNC2010 if seriousattempt == 1 & lautoc == 1 & durgroup == 1, by(success)
local pvalnonparm = r(p)
reg perexitregularNC2010 success `fixedeffectvars' if seriousattempt == 1 & lautoc == 1 & durgroup == 1  , cluster(cowcode)
maketablerank using table_6, rhs(success) varcol(Autoc-TL10) append pval noastr rankpval(`pvalnonparm')


* Column 6: Autocrats with tenure > 10
tab npolity2dummy11 success if seriousattempt == 1 & lautoc == 1 & durgroup == 2, exact chi2
local pvalnonparm = r(p_exact)
reg npolity2dummy11 success `fixedeffectvars' if seriousattempt == 1 & lautoc == 1 & durgroup == 2  , cluster(cowcode)
maketablerank using table_6, rhs(success) varcol(Autoc-TG10) append 	pval noastr rankpval(`pvalnonparm')

tab npolity2dummy101 success if seriousattempt == 1 & lautoc == 1 & durgroup == 2, exact chi2
local pvalnonparm = r(p_exact)
reg npolity2dummy101  success `fixedeffectvars' if seriousattempt == 1 & lautoc == 1 & durgroup == 2  , cluster(cowcode)
maketablerank using table_6, rhs(success) varcol(Autoc-TG10) append pval noastr rankpval(`pvalnonparm')

tab npolity2dummy201  success if seriousattempt == 1 & lautoc == 1 & durgroup == 2, exact chi2
local pvalnonparm = r(p_exact)
reg npolity2dummy201  success `fixedeffectvars' if seriousattempt == 1 & lautoc == 1 & durgroup == 2  , cluster(cowcode)
maketablerank using table_6, rhs(success) varcol(Autoc-TG10) append pval noastr rankpval(`pvalnonparm')

ranksumben perexitregularNC101 if seriousattempt == 1 & lautoc == 1 & durgroup == 2, by(success)
local pvalnonparm = r(p)
reg perexitregularNC101 success `fixedeffectvars' if seriousattempt == 1 & lautoc == 1 & durgroup == 2  , cluster(cowcode)
maketablerank using table_6, rhs(success) varcol(Autoc-TG10) append pval noastr rankpval(`pvalnonparm')

ranksumben perexitregularNC201 if seriousattempt == 1 & lautoc == 1 & durgroup == 2, by(success)
local pvalnonparm = r(p)
reg perexitregularNC201 success `fixedeffectvars' if seriousattempt == 1 & lautoc == 1 & durgroup == 2  , cluster(cowcode)
maketablerank using table_6, rhs(success) varcol(Autoc-TG10) append pval noastr rankpval(`pvalnonparm')

ranksumben perexitregularNC2010 if seriousattempt == 1 & lautoc == 1 & durgroup == 2, by(success)
local pvalnonparm = r(p)
reg perexitregularNC2010 success `fixedeffectvars' if seriousattempt == 1 & lautoc == 1 & durgroup == 2  , cluster(cowcode)
maketablerank using table_6, rhs(success) varcol(Autoc-TG10) append pval noastr rankpval(`pvalnonparm')





**************************************
*** Table 7: Assassinations and Conflict: Change 1 Year After Attempt
**************************************

* Panel A
reg zGledAnywar11 success `fixedeffectvars' if seriousattempt == 1  , cluster(cowcode)
testparm success
local pvalparm = r(p)
tab zGledAnywar11 success if seriousattempt == 1, exact chi2
local pvalnonparm = r(p_exact)
outreg using table_7a, 3aster coefastr se adds("Parm p",`pvalparm',"Nonparm p",`pvalnonparm') replace title("Table 7a")

reg zGledAnywarP11 success `fixedeffectvars' if seriousattempt == 1  , cluster(cowcode)
testparm success
local pvalparm = r(p)
tab zGledAnywarP11 success if seriousattempt == 1, exact chi2
local pvalnonparm = r(p_exact)
outreg using table_7a, 3aster coefastr se adds("Parm p",`pvalparm',"Nonparm p",`pvalnonparm') append 

reg prioanywarP11 success `fixedeffectvars' if seriousattempt == 1  , cluster(cowcode)
testparm success
local pvalparm = r(p)
tab prioanywarP11 success if seriousattempt == 1, exact chi2
local pvalnonparm = r(p_exact)
outreg using table_7a, 3aster coefastr se adds("Parm p",`pvalparm',"Nonparm p",`pvalnonparm') append 

* Panel B
g l1anywar = l1.zGledAnywar
g l1nowar = (1-l1anywar)
g successl1anywar = success * l1anywar
g successl1nowar = success * l1nowar
label var l1anywar  "Lag any war"

reg zGledAnywar11 successl1anywar successl1nowar  l1anywar `fixedeffectvars' if seriousattempt == 1  , cluster(cowcode)
testparm successl1anywar 
local pvalparmA = r(p)
testparm successl1nowar  
local pvalparmN = r(p)
tab zGledAnywar11  success if l1anywar == 1 & seriousattempt == 1, exact chi2
local pvalnonparmA = r(p_exact)
tab zGledAnywar11  success if l1anywar == 0 & seriousattempt == 1, exact chi2
local pvalnonparmN = r(p_exact)
outreg using table_7b, 3aster coefastr se adds("Intense war-Parm p",`pvalparmA',"Intense war-Nonparm p",`pvalnonparmA',"No war-Parm p",`pvalparmN',"No war-Nonparm p",`pvalnonparmN') replace


reg zGledAnywarP11 successl1anywar successl1nowar  l1anywar `fixedeffectvars' if seriousattempt == 1 , cluster(cowcode)
testparm successl1anywar 
local pvalparmA = r(p)
testparm successl1nowar  
local pvalparmN = r(p)
tab zGledAnywarP11  success if l1anywar == 1 & seriousattempt == 1, exact chi2
local pvalnonparmA = r(p_exact)
tab zGledAnywarP11  success if l1anywar == 0 & seriousattempt == 1, exact chi2
local pvalnonparmN = r(p_exact)
outreg using table_7b, 3aster coefastr se adds("Intense war-Parm p",`pvalparmA',"Intense war-Nonparm p",`pvalnonparmA',"No war-Parm p",`pvalparmN',"No war-Nonparm p",`pvalnonparmN') append


drop successl1anywar successl1nowar l1anywar l1nowar
g l1fullwar = (l1.prioanywarP == 1) if l1.prioanywarP != .
g l1somewar = (l1.prioanywarP == .5) if l1.prioanywarP != .
g l1nowar = (l1.prioanywarP == 0) if l1.prioanywarP != .
g successl1fullwar = success * l1fullwar
g successl1somewar = success * l1somewar
g successl1nowar = success * l1nowar
label var l1fullwar  "Lag intense war"
label var l1somewar  "Lag moderate war"


reg prioanywarP11 successl1fullwar successl1somewar  successl1nowar l1fullwar l1somewar `fixedeffectvars' if seriousattempt == 1  , cluster(cowcode)
testparm successl1fullwar 
local pvalparmA = r(p)
testparm successl1somewar 
local pvalparmS = r(p)
testparm successl1nowar  
local pvalparmN = r(p)

tab prioanywarP11 success if l1fullwar == 1 & seriousattempt == 1, exact chi2
local pvalnonparmA = r(p_exact)
tab prioanywarP11 success if l1somewar == 1 & seriousattempt == 1, exact chi2
local pvalnonparmS = r(p_exact)
tab prioanywarP11 success if l1nowar == 1 & seriousattempt == 1, exact chi2
local pvalnonparmN = r(p_exact)
outreg using table_7b, 3aster coefastr se adds("Intense war-Parm p",`pvalparmA',"Intense war-Nonparm p",`pvalnonparmA',"Moderate war-Parm p",`pvalparmS',"Moderate war-Nonparm p",`pvalnonparmS',"No war-Parm p",`pvalparmN',"No war-Nonparm p",`pvalnonparmN') append 



**************************************
*** Table 8: Alternative specifications
*** Note: this table is transposed from the version in the text
**************************************
* Column 1: absnpolity2dummy11 
tab absnpolity2dummy11 success if seriousattempt == 1, exact chi2
local pvalnonparm = r(p_exact)
reg absnpolity2dummy11 success `fixedeffectvars' if seriousattempt == 1  , cluster(cowcode)
maketablerank using table_8, rhs(success) varcol(Baseline) replace 	pval noastr rankpval(`pvalnonparm')

tab absnpolity2dummy11 success if woundedbystander == 1, exact chi2
local pvalnonparm = r(p_exact)
reg absnpolity2dummy11 success `fixedeffectvars' if woundedbystander == 1  , cluster(cowcode)
maketablerank using table_8, rhs(success) varcol(WoundBystander) append 	pval noastr rankpval(`pvalnonparm')

tab absnpolity2dummy11 success if wounded == 1, exact chi2
local pvalnonparm = r(p_exact)
reg absnpolity2dummy11 success `fixedeffectvars' if wounded ==  1  , cluster(cowcode)
maketablerank using table_8, rhs(success) varcol(TargetWounded) append pval noastr rankpval(`pvalnonparm')

tab absnpolity2dummy11 success if attempt == 1, exact chi2
local pvalnonparm = r(p_exact)
reg absnpolity2dummy11 success `fixedeffectvars' if attempt == 1  , cluster(cowcode)
maketablerank using table_8, rhs(success) varcol(AnyAttempt) append 	pval noastr rankpval(`pvalnonparm')

tab absnpolity2dummy11 success if seriousattempt == 1 & solo == 1, exact chi2
local pvalnonparm = r(p_exact)
reg absnpolity2dummy11 success `fixedeffectvars' if seriousattempt == 1 & solo == 1, cluster(cowcode)
maketablerank using table_8, rhs(success) varcol(Solo) append pval noastr rankpval(`pvalnonparm')

tab absnpolity2dummy11 success if seriousattempt == 1 & firstattempt == 1, exact chi2
local pvalnonparm = r(p_exact)
reg absnpolity2dummy11 success `fixedeffectvars' if seriousattempt == 1 & firstattempt == 1, cluster(cowcode)
maketablerank using table_8, rhs(success) varcol(FirstAttempt) append pval noastr rankpval(`pvalnonparm')

tab absnpolity2dummy11 success if seriousattempt == 1, exact chi2
local pvalnonparm = r(p_exact)
reg absnpolity2dummy11 success `fixedeffectvars' qtrcentury* lpol2dum pol2duml1l3 lzGledAnywar anywarl1l3 llnenergy_pc llnpop lage lclock regdumAfrica regdumAsia regdumMENA regdumLatAm regdumEEur if seriousattempt == 1  , cluster(cowcode)
maketablerank using table_8, rhs(success) varcol(AddControls) append 	pval noastr rankpval(`pvalnonparm')


* Column 2: npolity2dummy11 
tab npolity2dummy11 success if seriousattempt == 1, exact chi2
local pvalnonparm = r(p_exact)
reg npolity2dummy11 success `fixedeffectvars' if seriousattempt == 1  , cluster(cowcode)
maketablerank using table_8, rhs(success) varcol(Baseline) append pval noastr rankpval(`pvalnonparm')

tab npolity2dummy11 success if woundedbystander == 1, exact chi2
local pvalnonparm = r(p_exact)
reg npolity2dummy11 success `fixedeffectvars' if woundedbystander == 1  , cluster(cowcode)
maketablerank using table_8, rhs(success) varcol(WoundBystander) append 	pval noastr rankpval(`pvalnonparm')

tab npolity2dummy11 success if wounded == 1, exact chi2
local pvalnonparm = r(p_exact)
reg npolity2dummy11 success `fixedeffectvars' if wounded ==  1  , cluster(cowcode)
maketablerank using table_8, rhs(success) varcol(TargetWounded) append pval noastr rankpval(`pvalnonparm')

tab npolity2dummy11 success if attempt == 1, exact chi2
local pvalnonparm = r(p_exact)
reg npolity2dummy11 success `fixedeffectvars' if attempt == 1  , cluster(cowcode)
maketablerank using table_8, rhs(success) varcol(AnyAttempt) append 	pval noastr rankpval(`pvalnonparm')

tab npolity2dummy11 success if seriousattempt == 1 & solo == 1, exact chi2
local pvalnonparm = r(p_exact)
reg npolity2dummy11 success `fixedeffectvars' if seriousattempt == 1 & solo == 1, cluster(cowcode)
maketablerank using table_8, rhs(success) varcol(Solo) append pval noastr rankpval(`pvalnonparm')

tab npolity2dummy11 success if seriousattempt == 1 & firstattempt == 1, exact chi2
local pvalnonparm = r(p_exact)
reg npolity2dummy11 success `fixedeffectvars' if seriousattempt == 1 & firstattempt == 1, cluster(cowcode)
maketablerank using table_8, rhs(success) varcol(FirstAttempt) append pval noastr rankpval(`pvalnonparm')

tab npolity2dummy11 success if seriousattempt == 1, exact chi2
local pvalnonparm = r(p_exact)
reg npolity2dummy11 success `fixedeffectvars' qtrcentury* lpol2dum pol2duml1l3 lzGledAnywar anywarl1l3 llnenergy_pc llnpop lage lclock regdumAfrica regdumAsia regdumMENA regdumLatAm regdumEEur if seriousattempt == 1  , cluster(cowcode)
maketablerank using table_8, rhs(success) varcol(AddControls) append 	pval noastr rankpval(`pvalnonparm')

* Column 3: npolity2dummy11  for autocrats
g npolity2dummy11A = npolity2dummy11  
tab npolity2dummy11A success if seriousattempt == 1 & lautoc == 1, exact chi2
local pvalnonparm = r(p_exact)
reg npolity2dummy11A successlautoc successldemoc lautoc `fixedeffectvars' if seriousattempt == 1   , cluster(cowcode)
maketablerank using table_8, rhs(successlautoc) varcol(Baseline) append 	pval noastr rankpval(`pvalnonparm')

tab npolity2dummy11A success if woundedbystander == 1 & lautoc == 1 , exact chi2
local pvalnonparm = r(p_exact)
reg npolity2dummy11A successlautoc successldemoc lautoc `fixedeffectvars' if woundedbystander == 1  , cluster(cowcode)
maketablerank using table_8, rhs(successlautoc) varcol(WoundBystander) append 	pval noastr rankpval(`pvalnonparm')

tab npolity2dummy11A success if wounded == 1 & lautoc == 1 , exact chi2
local pvalnonparm = r(p_exact)
reg npolity2dummy11A successlautoc successldemoc lautoc `fixedeffectvars' if wounded ==  1  , cluster(cowcode)
maketablerank using table_8, rhs(successlautoc) varcol(TargetWounded) append pval noastr rankpval(`pvalnonparm')

tab npolity2dummy11A success if attempt == 1 & lautoc == 1 , exact chi2
local pvalnonparm = r(p_exact)
reg npolity2dummy11A successlautoc successldemoc lautoc `fixedeffectvars' if attempt == 1 , cluster(cowcode)
maketablerank using table_8, rhs(successlautoc) varcol(AnyAttempt) append 	pval noastr rankpval(`pvalnonparm')

tab npolity2dummy11A success if seriousattempt == 1 & lautoc == 1 & solo == 1, exact chi2
local pvalnonparm = r(p_exact)
reg npolity2dummy11A successlautoc successldemoc lautoc `fixedeffectvars' if seriousattempt == 1 & solo == 1, cluster(cowcode)
maketablerank using table_8, rhs(successlautoc) varcol(Solo) append pval noastr rankpval(`pvalnonparm')

tab npolity2dummy11A success if seriousattempt == 1 & lautoc == 1 & firstattempt == 1, exact chi2
local pvalnonparm = r(p_exact)
reg npolity2dummy11A successlautoc successldemoc lautoc `fixedeffectvars' if seriousattempt == 1  & firstattempt == 1, cluster(cowcode)
maketablerank using table_8, rhs(successlautoc) varcol(FirstAttempt) append pval noastr rankpval(`pvalnonparm')

tab npolity2dummy11A success if seriousattempt == 1 & lautoc == 1, exact chi2
local pvalnonparm = r(p_exact)
reg npolity2dummy11A successlautoc successldemoc lautoc `fixedeffectvars' qtrcentury* lpol2dum pol2duml1l3 lzGledAnywar anywarl1l3 llnenergy_pc llnpop lage lclock regdumAfrica regdumAsia regdumMENA regdumLatAm regdumEEur if seriousattempt == 1  , cluster(cowcode)
maketablerank using table_8, rhs(successlautoc) varcol(AddControls) append 	pval noastr rankpval(`pvalnonparm')


* Column 4: perexitregularNC201
ranksumben perexitregularNC201 if seriousattempt == 1, by(success)
local pvalnonparm = r(p)
reg perexitregularNC201 success `fixedeffectvars' if seriousattempt == 1  , cluster(cowcode)
maketablerank using table_8, rhs(success) varcol(Baseline) append pval noastr rankpval(`pvalnonparm')

ranksumben perexitregularNC201  if woundedbystander == 1, by(success)
local pvalnonparm = r(p)
reg perexitregularNC201 success `fixedeffectvars' if woundedbystander == 1  , cluster(cowcode)
maketablerank using table_8, rhs(success) varcol(WoundBystander) append 	pval noastr rankpval(`pvalnonparm')

ranksumben perexitregularNC201  if wounded == 1, by(success)
local pvalnonparm = r(p)
reg perexitregularNC201 success `fixedeffectvars' if wounded ==  1  , cluster(cowcode)
maketablerank using table_8, rhs(success) varcol(TargetWounded) append pval noastr rankpval(`pvalnonparm')

ranksumben perexitregularNC201  if attempt == 1, by(success)
local pvalnonparm = r(p)
reg perexitregularNC201 success `fixedeffectvars' if attempt == 1  , cluster(cowcode)
maketablerank using table_8, rhs(success) varcol(AnyAttempt) append 	pval noastr rankpval(`pvalnonparm')

ranksumben perexitregularNC201  if seriousattempt == 1 & solo == 1, by(success)
local pvalnonparm = r(p)
reg perexitregularNC201 success `fixedeffectvars' if seriousattempt == 1 & solo == 1, cluster(cowcode)
maketablerank using table_8, rhs(success) varcol(Solo) append pval noastr rankpval(`pvalnonparm')

ranksumben perexitregularNC201  if seriousattempt == 1 & firstattempt == 1, by(success)
local pvalnonparm = r(p)
reg perexitregularNC201 success `fixedeffectvars' if seriousattempt == 1 & firstattempt == 1, cluster(cowcode)
maketablerank using table_8, rhs(success) varcol(FirstAttempt) append pval noastr rankpval(`pvalnonparm')

ranksumben perexitregularNC201  if seriousattempt == 1, by(success)
local pvalnonparm = r(p)
reg perexitregularNC201 success `fixedeffectvars' qtrcentury* lpol2dum pol2duml1l3 lzGledAnywar anywarl1l3 llnenergy_pc llnpop lage lclock regdumAfrica regdumAsia regdumMENA regdumLatAm regdumEEur if seriousattempt == 1  , cluster(cowcode)
maketablerank using table_8, rhs(success) varcol(AddControls) append 	pval noastr rankpval(`pvalnonparm')

* Column 5: perexitregularNC201 autocrats
g perexitregularNC201A = perexitregularNC201 

ranksumben perexitregularNC201A if seriousattempt == 1 & lautoc == 1, by(success)
local pvalnonparm = r(p)
reg perexitregularNC201A successlautoc successldemoc lautoc `fixedeffectvars' if seriousattempt == 1   , cluster(cowcode)
maketablerank using table_8, rhs(successlautoc) varcol(Baseline) append 	pval noastr rankpval(`pvalnonparm')

ranksumben perexitregularNC201A if woundedbystander == 1 & lautoc == 1 , by(success)
local pvalnonparm = r(p)
reg perexitregularNC201A successlautoc successldemoc lautoc `fixedeffectvars' if woundedbystander == 1  , cluster(cowcode)
maketablerank using table_8, rhs(successlautoc) varcol(WoundBystander) append 	pval noastr rankpval(`pvalnonparm')

ranksumben perexitregularNC201A if wounded == 1 & lautoc == 1 , by(success)
local pvalnonparm = r(p)
reg perexitregularNC201A successlautoc successldemoc lautoc `fixedeffectvars' if wounded ==  1  , cluster(cowcode)
maketablerank using table_8, rhs(successlautoc) varcol(TargetWounded) append pval noastr rankpval(`pvalnonparm')

ranksumben perexitregularNC201A if attempt == 1 & lautoc == 1 , by(success)
local pvalnonparm = r(p)
reg perexitregularNC201A successlautoc successldemoc lautoc `fixedeffectvars' if attempt == 1 , cluster(cowcode)
maketablerank using table_8, rhs(successlautoc) varcol(AnyAttempt) append 	pval noastr rankpval(`pvalnonparm')

ranksumben perexitregularNC201A if seriousattempt == 1 & lautoc == 1 & solo == 1, by(success)
local pvalnonparm = r(p)
reg perexitregularNC201A successlautoc successldemoc lautoc `fixedeffectvars' if seriousattempt == 1 & solo == 1, cluster(cowcode)
maketablerank using table_8, rhs(successlautoc) varcol(Solo) append pval noastr rankpval(`pvalnonparm')

ranksumben perexitregularNC201A if seriousattempt == 1 & lautoc == 1 & firstattempt == 1, by(success)
local pvalnonparm = r(p)
reg perexitregularNC201A successlautoc successldemoc lautoc `fixedeffectvars' if seriousattempt == 1  & firstattempt == 1, cluster(cowcode)
maketablerank using table_8, rhs(successlautoc) varcol(FirstAttempt) append pval noastr rankpval(`pvalnonparm')

ranksumben perexitregularNC201A if seriousattempt == 1 & lautoc == 1, by(success)
local pvalnonparm = r(p)
reg perexitregularNC201A successlautoc successldemoc lautoc `fixedeffectvars' qtrcentury* lpol2dum pol2duml1l3 lzGledAnywar anywarl1l3 llnenergy_pc llnpop lage lclock regdumAfrica regdumAsia regdumMENA regdumLatAm regdumEEur if seriousattempt == 1  , cluster(cowcode)
maketablerank using table_8, rhs(successlautoc) varcol(AddControls) append 	pval noastr rankpval(`pvalnonparm')


**************************************
*** Table 9: Propensity score
**************************************

g failure = (seriousattempt == 1 & success == 0) if seriousattempt != . & success != .
	
capture drop lanywar
g lanywar = l.zGledAnywar
g ltenure = l.clock
label var lnpolity2 "Polity score"
label var ltenure "Tenure"
label var lanywar "At war"
label var llnenergy_pc "Ln energy use p.c."
label var llnpop "Population"


dprobit seriousattempt lpol2dum , cluster(cowcode)
testparm *
outreg using table_9, 3aster coefastr se replace title("Predicting attempts") bd(3) adds("P-val",r(p))

dprobit seriousattempt pol2duml1l3 , cluster(cowcode)
testparm *
outreg using table_9, 3aster coefastr se append bd(3) adds("P-val",r(p))

dprobit seriousattempt lanywar , cluster(cowcode)
testparm *
outreg using table_9, 3aster coefastr se append bd(3) adds("P-val",r(p))

dprobit seriousattempt anywarl1l3 , cluster(cowcode)
testparm *
outreg using table_9, 3aster coefastr se append bd(3) adds("P-val",r(p))

dprobit seriousattempt llnenergy_pc , cluster(cowcode) 
testparm *
outreg using table_9, 3aster coefastr se append bd(3) adds("P-val",r(p))

dprobit seriousattempt llnpop, cluster(cowcode)
testparm *
outreg using table_9, 3aster coefastr se append bd(3) adds("P-val",r(p))

dprobit seriousattempt lage, cluster(cowcode)
testparm *
outreg using table_9, 3aster coefastr se append bd(3) adds("P-val",r(p))

dprobit seriousattempt ltenure, cluster(cowcode)
testparm *
outreg using table_9, 3aster coefastr se append bd(3) adds("P-val",r(p))

dprobit seriousattempt lpol2dum pol2duml1l3 lanywar anywarl1l3 llnenergy_pc llnpop lage ltenure , cluster(cowcode)
testparm *
outreg using table_9, 3aster coefastr se append bd(3) adds("P-val",r(p))


**************************************
*** Table 10: Success vs. failure, institutional change
**************************************
* Set up propensity score variables
for var lpol2dum pol2duml1l3 anywarl1l3 lanywar llnpop llnenergy_pc lage ltenure : g mis_X = (X == .) \ g nonmis_X = X \ replace nonmis_X  = 0 if X == .

pscore seriousattempt nonmis_* mis_* if obsid != "" , pscore(pscoreseriousattempt) blockid(blockseriousattempt)

g failurelautoc = failure*lautoc
g failureldemoc = failure*ldemoc

* Table 10A
reg absnpolity2dummy11 success failure  , cluster(cowcode)
testparm success
local pvalS = r(p)
testparm failure
local pvalF = r(p)
outreg using table_10a, 3aster coefastr se replace title ("Table 10a") adds("Parm p-Success",`pvalS',"Parm p-Failure",`pvalF')

xi: reg absnpolity2dummy11 success failure nonmis_* mis_*  regdum* qtrcentury*  , cluster(cowcode)
testparm success
local pvalS = r(p)
testparm failure
local pvalF = r(p)
outreg using table_10a, 3aster coefastr se append adds("Parm p-Success",`pvalS',"Parm p-Failure",`pvalF')

xi: reg absnpolity2dummy11 success failure nonmis_* mis_*  i.blockserious regdum* qtrcentury*  , cluster(cowcode)
testparm success
local pvalS = r(p)
testparm failure
local pvalF = r(p)
outreg using table_10a, 3aster coefastr se append adds("Parm p-Success",`pvalS',"Parm p-Failure",`pvalF')

reg npolity2dummy11 success failure  , cluster(cowcode)
testparm success
local pvalS = r(p)
testparm failure
local pvalF = r(p)
outreg using table_10a, 3aster coefastr se append adds("Parm p-Success",`pvalS',"Parm p-Failure",`pvalF')

xi: reg npolity2dummy11 success failure nonmis_* mis_*  regdum* qtrcentury*  , cluster(cowcode)
testparm success
local pvalS = r(p)
testparm failure
local pvalF = r(p)
outreg using table_10a, 3aster coefastr se append adds("Parm p-Success",`pvalS',"Parm p-Failure",`pvalF')

xi: reg npolity2dummy11 success failure nonmis_* mis_*  i.blockserious regdum* qtrcentury*  , cluster(cowcode)
testparm success
local pvalS = r(p)
testparm failure
local pvalF = r(p)
outreg using table_10a, 3aster coefastr se append adds("Parm p-Success",`pvalS',"Parm p-Failure",`pvalF')

reg perexitregularNC201 success failure  , cluster(cowcode)
testparm success
local pvalS = r(p)
testparm failure
local pvalF = r(p)
outreg using table_10a, 3aster coefastr se append adds("Parm p-Success",`pvalS',"Parm p-Failure",`pvalF')

xi: reg perexitregularNC201 success failure nonmis_* mis_*  regdum* qtrcentury*  , cluster(cowcode)
testparm success
local pvalS = r(p)
testparm failure
local pvalF = r(p)
outreg using table_10a, 3aster coefastr se append adds("Parm p-Success",`pvalS',"Parm p-Failure",`pvalF')

xi: reg perexitregularNC201 success failure nonmis_* mis_*  i.blockserious regdum* qtrcentury*  , cluster(cowcode)
testparm success
local pvalS = r(p)
testparm failure
local pvalF = r(p)
outreg using table_10a, 3aster coefastr se append adds("Parm p-Success",`pvalS',"Parm p-Failure",`pvalF')


* Table 10B
reg npolity2dummy11 successlautoc failurelautoc successldemoc failureldemoc lautoc  , cluster(cowcode)
testparm successlautoc 
local pvalparmAS = r(p)
testparm successldemoc 
local pvalparmDS = r(p)
testparm failurelautoc 
local pvalparmAF = r(p)
testparm failureldemoc 
local pvalparmDF = r(p)
outreg using table_10b, 3aster coefastr se replace title("Table 10B") adds("Autoc-Parm p-success",`pvalparmAS',"Autoc-Parm p-Failure",`pvalparmAF',"Democ-Parm p-success",`pvalparmDS',"Democ-Parm p-Failure",`pvalparmDF') 

xi: reg npolity2dummy11 successlautoc failurelautoc successldemoc failureldemoc lautoc nonmis_* mis_*  regdum* qtrcentury*  , cluster(cowcode)
testparm successlautoc 
local pvalparmAS = r(p)
testparm successldemoc 
local pvalparmDS = r(p)
testparm failurelautoc 
local pvalparmAF = r(p)
testparm failureldemoc 
local pvalparmDF = r(p)
outreg using table_10b, 3aster coefastr se append adds("Autoc-Parm p-success",`pvalparmAS',"Autoc-Parm p-Failure",`pvalparmAF',"Democ-Parm p-success",`pvalparmDS',"Democ-Parm p-Failure",`pvalparmDF') 

xi: reg npolity2dummy11 successlautoc failurelautoc successldemoc failureldemoc lautoc nonmis_* mis_*  i.blockserious regdum* qtrcentury*  , cluster(cowcode)
testparm successlautoc 
local pvalparmAS = r(p)
testparm successldemoc 
local pvalparmDS = r(p)
testparm failurelautoc 
local pvalparmAF = r(p)
testparm failureldemoc 
local pvalparmDF = r(p)
outreg using table_10b, 3aster coefastr se append adds("Autoc-Parm p-success",`pvalparmAS',"Autoc-Parm p-Failure",`pvalparmAF',"Democ-Parm p-success",`pvalparmDS',"Democ-Parm p-Failure",`pvalparmDF') 

reg perexitregularNC201 successlautoc failurelautoc successldemoc failureldemoc lautoc  , cluster(cowcode)
testparm successlautoc 
local pvalparmAS = r(p)
testparm successldemoc 
local pvalparmDS = r(p)
testparm failurelautoc 
local pvalparmAF = r(p)
testparm failureldemoc 
local pvalparmDF = r(p)
outreg using table_10b, 3aster coefastr se append adds("Autoc-Parm p-success",`pvalparmAS',"Autoc-Parm p-Failure",`pvalparmAF',"Democ-Parm p-success",`pvalparmDS',"Democ-Parm p-Failure",`pvalparmDF') 

xi: reg perexitregularNC201 successlautoc failurelautoc successldemoc failureldemoc lautoc nonmis_* mis_*  regdum* qtrcentury*  , cluster(cowcode)
testparm successlautoc 
local pvalparmAS = r(p)
testparm successldemoc 
local pvalparmDS = r(p)
testparm failurelautoc 
local pvalparmAF = r(p)
testparm failureldemoc 
local pvalparmDF = r(p)
outreg using table_10b, 3aster coefastr se append adds("Autoc-Parm p-success",`pvalparmAS',"Autoc-Parm p-Failure",`pvalparmAF',"Democ-Parm p-success",`pvalparmDS',"Democ-Parm p-Failure",`pvalparmDF') 

xi: reg perexitregularNC201 successlautoc failurelautoc successldemoc failureldemoc lautoc nonmis_* mis_*  i.blockserious regdum* qtrcentury*  , cluster(cowcode)
testparm successlautoc 
local pvalparmAS = r(p)
testparm successldemoc 
local pvalparmDS = r(p)
testparm failurelautoc 
local pvalparmAF = r(p)
testparm failureldemoc 
local pvalparmDF = r(p)
outreg using table_10b, 3aster coefastr se append adds("Autoc-Parm p-success",`pvalparmAS',"Autoc-Parm p-Failure",`pvalparmAF',"Democ-Parm p-success",`pvalparmDS',"Democ-Parm p-Failure",`pvalparmDF') 

**************************************
*** Table 11: Success vs. failure, war
**************************************

* Table 11a
reg zGledAnywar11 success failure  , cluster(cowcode)
testparm success
local pvalS = r(p)
testparm failure
local pvalF = r(p)
outreg using table_11a, 3aster coefastr se replace title ("Table 11") adds("Parm p-Success",`pvalS',"Parm p-Failure",`pvalF')

xi: reg zGledAnywar11 success failure nonmis_* mis_*  regdum* qtrcentury*  , cluster(cowcode)
testparm success
local pvalS = r(p)
testparm failure
local pvalF = r(p)
outreg using table_11a, 3aster coefastr se append adds("Parm p-Success",`pvalS',"Parm p-Failure",`pvalF')

xi: reg zGledAnywar11 success failure nonmis_* mis_*  i.blockserious regdum* qtrcentury*  , cluster(cowcode)
testparm success
local pvalS = r(p)
testparm failure
local pvalF = r(p)
outreg using table_11a, 3aster coefastr se append adds("Parm p-Success",`pvalS',"Parm p-Failure",`pvalF')

reg zGledAnywarP11 success failure  , cluster(cowcode)
testparm success
local pvalS = r(p)
testparm failure
local pvalF = r(p)
outreg using table_11a, 3aster coefastr se append adds("Parm p-Success",`pvalS',"Parm p-Failure",`pvalF')

xi: reg zGledAnywarP11 success failure nonmis_* mis_*  regdum* qtrcentury*  , cluster(cowcode)
testparm success
local pvalS = r(p)
testparm failure
local pvalF = r(p)
outreg using table_11a, 3aster coefastr se append adds("Parm p-Success",`pvalS',"Parm p-Failure",`pvalF')

xi: reg zGledAnywarP11 success failure nonmis_* mis_*  i.blockserious regdum* qtrcentury*  , cluster(cowcode)
testparm success
local pvalS = r(p)
testparm failure
local pvalF = r(p)
outreg using table_11a, 3aster coefastr se append adds("Parm p-Success",`pvalS',"Parm p-Failure",`pvalF')
				
reg prioanywarP11 success failure  , cluster(cowcode)
testparm success
local pvalS = r(p)
testparm failure
local pvalF = r(p)
outreg using table_11a, 3aster coefastr se append adds("Parm p-Success",`pvalS',"Parm p-Failure",`pvalF')

xi: reg prioanywarP11 success failure nonmis_* mis_*  regdum* qtrcentury*  , cluster(cowcode)
testparm success
local pvalS = r(p)
testparm failure
local pvalF = r(p)
outreg using table_11a, 3aster coefastr se append adds("Parm p-Success",`pvalS',"Parm p-Failure",`pvalF')

xi: reg prioanywarP11 success failure nonmis_* mis_*  i.blockserious regdum* qtrcentury*  , cluster(cowcode)
testparm success
local pvalS = r(p)
testparm failure
local pvalF = r(p)
outreg using table_11a, 3aster coefastr se append adds("Parm p-Success",`pvalS',"Parm p-Failure",`pvalF')
				
				
* Table 11B
capture drop l1anywar
capture drop l1nowar
capture drop *success*war
capture drop *failure*war
g l1anywar = l1.zGledAnywar
g l1nowar = (1-l1anywar)
for var success failure : g Xl1anywar = X * l1anywar
for var success failure : g Xl1nowar = X * l1nowar
label var l1anywar  "Lag any war"

reg zGledAnywar11 successl1anywar failurel1anywar successl1nowar failurel1nowar lanywar  , cluster(cowcode)
testparm successl1anywar 
local pvalparmAS = r(p)
testparm successl1nowar 
local pvalparmDS = r(p)
testparm failurel1anywar 
local pvalparmAF = r(p)
testparm failurel1nowar 
local pvalparmDF = r(p)
outreg using table_11b, 3aster coefastr se replace title("Table 11B") adds("anywar-Parm p-success",`pvalparmAS',"anywar-Parm p-Failure",`pvalparmAF',"nowar-Parm p-success",`pvalparmDS',"nowar-Parm p-Failure",`pvalparmDF') 

xi: reg zGledAnywar11 successl1anywar failurel1anywar successl1nowar failurel1nowar lanywar nonmis_* mis_*  regdum* qtrcentury*  , cluster(cowcode)
testparm successl1anywar 
local pvalparmAS = r(p)
testparm successl1nowar 
local pvalparmDS = r(p)
testparm failurel1anywar 
local pvalparmAF = r(p)
testparm failurel1nowar 
local pvalparmDF = r(p)
outreg using table_11b, 3aster coefastr se append adds("anywar-Parm p-success",`pvalparmAS',"anywar-Parm p-Failure",`pvalparmAF',"nowar-Parm p-success",`pvalparmDS',"nowar-Parm p-Failure",`pvalparmDF') 

xi: reg zGledAnywar11 successl1anywar failurel1anywar successl1nowar failurel1nowar lanywar nonmis_* mis_*  i.blockserious regdum* qtrcentury*  , cluster(cowcode)
testparm successl1anywar 
local pvalparmAS = r(p)
testparm successl1nowar 
local pvalparmDS = r(p)
testparm failurel1anywar 
local pvalparmAF = r(p)
testparm failurel1nowar 
local pvalparmDF = r(p)
outreg using table_11b, 3aster coefastr se append adds("anywar-Parm p-success",`pvalparmAS',"anywar-Parm p-Failure",`pvalparmAF',"nowar-Parm p-success",`pvalparmDS',"nowar-Parm p-Failure",`pvalparmDF') 

reg zGledAnywarP11 successl1anywar failurel1anywar successl1nowar failurel1nowar lanywar  , cluster(cowcode)
testparm successl1anywar 
local pvalparmAS = r(p)
testparm successl1nowar 
local pvalparmDS = r(p)
testparm failurel1anywar 
local pvalparmAF = r(p)
testparm failurel1nowar 
local pvalparmDF = r(p)
outreg using table_11b, 3aster coefastr se append adds("anywar-Parm p-success",`pvalparmAS',"anywar-Parm p-Failure",`pvalparmAF',"nowar-Parm p-success",`pvalparmDS',"nowar-Parm p-Failure",`pvalparmDF') 

xi: reg zGledAnywarP11 successl1anywar failurel1anywar successl1nowar failurel1nowar lanywar nonmis_* mis_*  regdum* qtrcentury*  , cluster(cowcode)
testparm successl1anywar 
local pvalparmAS = r(p)
testparm successl1nowar 
local pvalparmDS = r(p)
testparm failurel1anywar 
local pvalparmAF = r(p)
testparm failurel1nowar 
local pvalparmDF = r(p)
outreg using table_11b, 3aster coefastr se append adds("anywar-Parm p-success",`pvalparmAS',"anywar-Parm p-Failure",`pvalparmAF',"nowar-Parm p-success",`pvalparmDS',"nowar-Parm p-Failure",`pvalparmDF') 

xi: reg zGledAnywarP11 successl1anywar failurel1anywar successl1nowar failurel1nowar lanywar nonmis_* mis_*  i.blockserious regdum* qtrcentury*  , cluster(cowcode)
testparm successl1anywar 
local pvalparmAS = r(p)
testparm successl1nowar 
local pvalparmDS = r(p)
testparm failurel1anywar 
local pvalparmAF = r(p)
testparm failurel1nowar 
local pvalparmDF = r(p)
outreg using table_11b, 3aster coefastr se append adds("anywar-Parm p-success",`pvalparmAS',"anywar-Parm p-Failure",`pvalparmAF',"nowar-Parm p-success",`pvalparmDS',"nowar-Parm p-Failure",`pvalparmDF') 

capture drop l1anywar
capture drop l1nowar
capture drop l1somewar
capture drop l1fullwar
capture drop *success*war
capture drop *failure*war
g l1fullwar = (l1.prioanywar == 1) if l1.prioanywar != .
g l1somewar = (l1.prioanywar == .5) if l1.prioanywar != .
g l1nowar = (l1.prioanywar == 0) if l1.prioanywar != .
for var success failure: g Xl1fullwar = X * l1fullwar
for var success failure: g Xl1somewar = X * l1somewar
for var success failure: g Xl1nowar = X * l1nowar
label var l1fullwar  "Lag intense war"
label var l1somewar  "Lag moderate war"

reg prioanywarP11 successl1fullwar failurel1fullwar successl1somewar failurel1somewar successl1nowar failurel1nowar l1fullwar l1somewar, cluster(cowcode)
testparm successl1fullwar 
local pvalparmAS = r(p)
testparm successl1somewar 
local pvalparmSS = r(p)
testparm successl1nowar 
local pvalparmDS = r(p)
testparm failurel1fullwar 
local pvalparmAF = r(p)
testparm failurel1somewar 
local pvalparmSF = r(p)
testparm failurel1nowar 
local pvalparmDF = r(p)
outreg using table_11b, 3aster coefastr se append adds("fullwar-Parm p-success",`pvalparmAS',"fullwar-Parm p-Failure",`pvalparmAF',"somewar-Parm p-success",`pvalparmSS',"somewar-Parm p-Failure",`pvalparmSF',"nowar-Parm p-success",`pvalparmDS',"nowar-Parm p-Failure",`pvalparmDF') 

xi: reg prioanywarP11 successl1fullwar failurel1fullwar successl1somewar failurel1somewar successl1nowar failurel1nowar l1fullwar l1somewar nonmis_* mis_*  regdum* qtrcentury*  , cluster(cowcode)
testparm successl1fullwar 
local pvalparmAS = r(p)
testparm successl1somewar 
local pvalparmSS = r(p)
testparm successl1nowar 
local pvalparmDS = r(p)
testparm failurel1fullwar 
local pvalparmAF = r(p)
testparm failurel1somewar 
local pvalparmSF = r(p)
testparm failurel1nowar 
local pvalparmDF = r(p)
	outreg using table_11b, 3aster coefastr se append adds("fullwar-Parm p-success",`pvalparmAS',"fullwar-Parm p-Failure",`pvalparmAF',"somewar-Parm p-success",`pvalparmSS',"somewar-Parm p-Failure",`pvalparmSF',"nowar-Parm p-success",`pvalparmDS',"nowar-Parm p-Failure",`pvalparmDF') 

xi: reg prioanywarP11 successl1fullwar failurel1fullwar successl1somewar failurel1somewar successl1nowar failurel1nowar l1fullwar l1somewar nonmis_* mis_*  i.blockserious regdum* qtrcentury*  , cluster(cowcode)
testparm successl1fullwar 
local pvalparmAS = r(p)
testparm successl1somewar 
local pvalparmSS = r(p)
testparm successl1nowar 
local pvalparmDS = r(p)
testparm failurel1fullwar 
local pvalparmAF = r(p)
testparm failurel1somewar 
local pvalparmSF = r(p)
testparm failurel1nowar 
local pvalparmDF = r(p)
outreg using table_11b, 3aster coefastr se append adds("fullwar-Parm p-success",`pvalparmAS',"fullwar-Parm p-Failure",`pvalparmAF',"somewar-Parm p-success",`pvalparmSS',"somewar-Parm p-Failure",`pvalparmSF',"nowar-Parm p-success",`pvalparmDS',"nowar-Parm p-Failure",`pvalparmDF') 


