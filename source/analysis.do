/* analysis.do v0.00            damiancclarke              yyyy-mm-dd:2021-01-28
----|----1----|----2----|----3----|----4----|----5----|----6----|----7----|----8

This file generates descriptive figures, tables and graphical results presented
in the paper
  "Access to The Emergency Contraceptive Pill and Women's Reproductive Health:
                   Evidence from Public Reform in Chile”
by Damian Clarke and Viviana Salinas.

This file uses a number of external ados, which are installed automatically on
lines 53-58 (provided internet connectivity is available).  If internet connec-
tivity is not avaialble and these ados are not installed, the file will not re-
plicate until the ados have been installed, or the computer is connected to the
internet.  It also uses a slightly edited version of the did_multiplegt ado,
renamed did_multiplegt_dc.ado.  The original ado can be installed from the SSC
(ssc install did_multiplegt).  A small correction has been made to produce av-
erage effects, as well as some generalization of graphing schemes.

This file has been written and for Stata 15 on a Unix system with no GUI.  As
such, some graph exporting commands are unnecessarily complicated, as graphs m-
ust be exported to pdf directly, rather than with Stata's graph export command.
On a windows or Mac system with GUI available these commands (which begin with
translate @graph) could be simplified significantly.

To control this file, the global MAIN on line 45 should be set as the directory
on your computer where these replication materials are stored.  No other changes
should be necessary to replicate results, provided all data is available.

For optimal viewing, set indent width to 4.

Contact:
  damian.clarke@protonmail.com
  dclarke@fen.uchile.cl

*/

vers 11
clear all
set more off
cap log close

*-------------------------------------------------------------------------------
*--- (1) Setup and globals
*-------------------------------------------------------------------------------
global MAIN "~/investigacion/2018/pillHealth"

global DAT "$MAIN/data/comunaLevel"
global MAP "$MAIN/data/map"
global OUT "$MAIN/results"
global LOG "$MAIN/log"


cap which scheme-plotplainblind.scheme
if _rc!=0 ssc install blindschemes
foreach ado in estout spmap {
    cap which `ado'
    if _rc!=0 ssc install `ado'
}

set scheme plotplainblind, permanently
graph set eps fontface "Times New Roman"

cap mkdir "$OUT"
cap mkdir "$OUT/event"
cap mkdir "$OUT/descriptives"
cap mkdir "$LOG"

log using "$LOG/analysis.txt", text replace
*-------------------------------------------------------------------------------
*--- (2) Descriptives
*-------------------------------------------------------------------------------
use "$DAT/pillHealthEntregas"

gen regcode= floor(comuna/1000)

foreach nn in 15_49 15_19 20_24 25_29 30_34 35_39 40_44 45_49 _private _public {
    drop haemorrhage`nn'
    rename haemorrhage2`nn' haemorrhage`nn'
}

#delimit ;
local hvars abortion15_49 abortion15_19 haemorrhage15_49 haemorrhage15_19
            abortion20_24 abortion25_29 haemorrhage20_24 haemorrhage25_29
            abortion30_34 abortion35_39 haemorrhage30_34 haemorrhage35_39
            abortion40_44 abortion45_49 haemorrhage40_44 haemorrhage45_49
  haemorrhage_public haemorrhage_private abortion_public abortion_private;
#delimit cr

preserve
collapse (sum) birth* poblacion*, by(year)
foreach age in 15_49 15_19 20_24 25_29 30_34 35_39 40_44 45_49 {
    gen birth`age'Pop=birth`age'/poblacion`age'*1000
}
keep birth*Pop year
drop if year==2017
#delimit ;
graph twoway connected birth15_19Pop year
          || connected birth20_24Pop year
          || connected birth25_29Pop year
          || connected birth30_34Pop year
          || connected birth35_39Pop year
          || connected birth40_44Pop year,
ytitle("Births per 1,000 Women") xtitle("Year")
xlabel(2002(2)2016)
legend(order(1 "15-19" 2 "20-24" 3 "25-29" 4 "30-34" 5 "35-39" 6 "40-44"));
graph export "$OUT/fert2002_2016.eps", replace;
#delimit cr
restore

local contvars diu oral_comb oral_prog injectable condom_women condom_men
preserve
drop if year==2002
collapse `contvars', by(servicio year)
collapse (sum) `contvars', by(year)
foreach cvar of varlist `contvars' {
    format `cvar' %9.0fc
    #delimit ;
    twoway connected `cvar' year, lwidth(thick) lcolor(gs10) ms(S) mc(black)
    xlabel(2003(1)2017, angle(45)) scheme(LF3) 
    ytitle("Population Covered") xtitle("Year");
    graph export "$OUT/descriptives/`cvar'.eps", replace;
    #delimit cr
}
#delimit ;
twoway connected diu        year, ms(Oh) lcolor(gs12) ||
       connected oral_comb  year, ms(Th) lcolor(gs12) ||
       connected oral_prog  year, ms(Sh) lcolor(gs12) ||
       connected injectable year, ms(Dh) lcolor(gs12) ||
       connected condom_w   year, ms(O) lcolor(gs12) ||
       connected condom_m   year, ms(T) lcolor(gs12) 
xlabel(2003(1)2017, angle(45)) scheme(LF3) ylabel(,angle(25))
ytitle("Population Covered" "") xtitle("Year")
legend(order(1 "IUD" 2 "Pill (combined)" 3 "Pill (Progrestin only)"
             4 "Injectable "5 "Condom (women)" 6 "Condom (men)")
       cols(3));
graph export "$OUT/descriptives/all_contraceptives.eps", replace;
#delimit cr
restore

preserve
collapse (sum) `hvars' puerperium maleMorbidity poblacion15_49, by(year)

foreach var of varlist `hvars' puerperium maleMorbidity {
    dis "`var'"
    if regexm(`"`var'"',"abortion")==1    local c "Abortion Morbidity"
    if regexm(`"`var'"',"haemorrhage")==1 local c "Haemorrhage Early in Pregnancy"
    if `"`var'"'=="puerperium" local c "Complications related to the puerperium"
    if `"`var'"'=="maleMorbidity" local c "Male morbidity (15-49 years)"
    #delimit ;
    twoway line `var' year, xline(2008.5) lwidth(thick)
    ||     scatter `var' year, msymbol(Sh) legend(off)
    ytitle("Inpatient Cases: `c'") xlabel(2001(2)2017) xtitle("Year");
    graph export "$OUT/descriptives/trendAll_`var'.eps", replace;
    #delimit cr
}
restore

gen pillDides = pill
tab pill year
replace pill = 1 if pill==0 & pildorasEntregadas>0&pildorasEntregadas!=.
replace pill = 1 if pill==. & pildorasEntregadas>0&pildorasEntregadas!=.
replace pill = 0 if pill==. & pildorasEntregadas==0
tab pill year

bys comuna (year): gen pillYears = sum(pill)
gen p2009 = pillYears==1&pill==1&year==2009
gen p2010 = pillYears==1&pill==1&year==2010
gen p2011 = pillYears==1&pill==1&year==2011
gen p2012 = pillYears==1&pill==1&year==2012
bys comuna: egen pill2009 = max(p2009)
bys comuna: egen pill2010 = max(p2010)
bys comuna: egen pill2011 = max(p2011)
bys comuna: egen pill2012 = max(p2012)


replace pildorasEntregadas=0 if pildorasEntregadas==.&year!=2017
replace pildorasNoEntregadas=0 if pildorasNoEntregadas==.&year!=2017
replace pildorasNoEntregadas=. if year==2009
gen pillPop = pildorasEntregadas/poblacion15_49*1000
gen noPillPop = pildorasNoEntregadas/poblacion15_49*1000
gen pillRejectRate = pildorasNoEntregadas/pildorasEntregadas

gen pillPopCont   = ceil(pillPop)
gen noPillPopCont = ceil(noPillPop)
tab pillPopCont


*gen pp = pillPop if year>=2009
bys comuna: egen meanPill = mean(pillPop)
sum meanPill, d

gen q1 = meanPill<0.84
gen q2 = meanPill>=0.84&meanPil<1.95
gen q3 = meanPill>=1.95


replace party = "missing" if party==""
tab party, gen(_parties)
replace mujer=3 if mujer==.
tab mujer, gen(_sexo)
gen voteMiss = votop==.
replace votop=0.5 if votop==.

gen oralpill=oral_comb+oral_prog
gen condoms =condom_women+condom_men
gen totalModern = oralpill+diu+injectable
bys servicio year: egen populationDenom = total(poblacion15_49)
foreach var of varlist diu oralpill injectable condoms totalModern {
    gen `var'_rate=`var'/populationDenom
}
sum *_rate



*-------------------------------------------------------------------------------
*--- (2) Summary statistics
*-------------------------------------------------------------------------------
preserve
collapse pill (sum) pildorasEntregadas pildorasNoEntregadas, by(comuna year)
collapse (sum) pill pildorasEntregadas pildorasNoEntregadas, by(year)
keep if year!=2017
#delimit ;
twoway connected pildorasEntregadas year if year!=2017,
  lwidth(thick) lcolor(gs10) ms(S) mc(black) ||
       connected pill year, lpattern(dash) yaxis(2) scheme(LF3) ms(Th)
lwidth(thick) xlabel(2002(1)2016, angle(45)) xsize(5) xtitle("Year")
ytitle("Pills Disbursed", axis(1)) ytitle("Municipalities Prescribing", axis(2))
legend(order(1 "Pills Disbursed" 2 "Municipalities Prescribing"));
graph export "$OUT/descriptives/pillRollout.eps", replace;
#delimit cr
restore

generat pillENJUV = 1 if pillNCP==1&year>=2010
replace pillENJUV = 0 if pillNCP==1&year<2010|pillNCP==0


lab var abortion15_49 "Number of Cases of Abortion Related Morbidity (15-49)"
lab var haemorrhage15_49 "Number of Cases of Haemorrhage Early in Pregnancy (15-49)"
lab var maleMorbidity "Number of Cases of all Cause Male Morbidity (15-49)"
lab var puerperium "Number of Complications Related to the Puerperium (15-49)"
lab var birth15_49 "Number of Births (15-49)"
lab var poblacion15_49 "Population of Fertile-Aged Women (15-49)"
lab var poblacionMale15_49 "Population of Fertile-Aged Men (15-49)"
lab var year "Year"
lab var pill "Municipality Has Pill Availability"
lab var pildorasEntregadas "Number of Pills Disbursed"
lab var pildorasNoEntregadas "Number of Pills Refused"
lab var pillPop "Pills Disbursed per 1000 Women"
lab var noPillPop "Pill Requests Rejected per 1000 Women"
lab var pillNCP "Pill Reported in ENJUV surveys"

gen rateAbort = abortion15_49/poblacion15_49*1000
gen rateHaem  = haemorrhage15_49/poblacion15_49*1000
gen rateBirth = birth15_49/poblacion15_49*1000
gen rateMaleHealth = maleMorbidity/poblacionMale15_49*1000
gen ratePuer  = puerperium/poblacion15_49*1000

sum rateHaem if year<2009 & q3==1

lab var rateAbort "Abortion Morbidity per 1,000 Reproductive Age Women"
lab var rateHaem  "Rate of Haemorrhage per 1,000 Reproductive Age Women"
lab var rateBirth "Birth Rate per 1,000 Reproductive Age Women"
lab var rateMaleH "Rate of Male Morbidity per 1,000 15-49 year-old Men"
lab var ratePuer  "Rate of Complications Related to the Puerperium"


preserve
keep if year!=2017
#delimit ;
estpost sum abortion15_49 haemorrhage15_49 puerperium maleMorbidity
poblacion15_49 poblacionMale15_49 rateAbort rateHaem ratePuer rateMaleH
pill pildorasEntregadas pildorasNoEntregadas pillPop noPillPop;
estout using "$OUT/descriptives/SumStats.tex", replace label style(tex)
cells("count mean(fmt(2)) sd(fmt(2)) min(fmt(0)) max(fmt(0))")
collabels(, none) mlabels(, none);

estpost sum abortion15_49 haemorrhage15_49 puerperium maleMorbidity
poblacion15_49 poblacionMale15_49 rateAbort rateHaem ratePuer rateMaleH
pill pildorasEntregadas pildorasNoEntregadas pillPop noPillPop
[aw=poblacion15_49];
estout using "$OUT/descriptives/SumStats_weighted.tex", replace label style(tex)
cells("count mean(fmt(2)) sd(fmt(2)) min(fmt(0)) max(fmt(0))")
collabels(, none) mlabels(, none);
#delimit cr


replace pillPop = . if year<2009
gen pillYear = 2009 if pill2009==1
replace pillYear = 2010 if pill2010==1
replace pillYear = 2011 if pill2011==1
replace pillYear = 2012 if pill2012==1

collapse pillPop pillYear, by(comuna nombre_comuna)
replace nombre_comuna = trim(nombre_comuna)
replace nombre_comuna = subinstr(nombre_comuna, "  ", " ", .)
replace nombre_comuna = "Buin" if nombre_comuna=="Buín"
replace nombre_comuna = "Cochamí" if nombre_comuna=="Cochamó"
replace nombre_comuna = "Hijuelas" if nombre_comuna=="Hijuela"
replace nombre_comuna = "Independencia" if nombre_comuna=="Inpendencia"
replace nombre_comuna = "Calera" if nombre_comuna=="La Calera"
replace nombre_comuna = "Los Alamos" if nombre_comuna=="Los Álamos"
replace nombre_comuna = "Los Angeles" if nombre_comuna=="Los Ángeles"
replace nombre_comuna = "Marchigüe" if nombre_comuna=="Marchihue"
replace nombre_comuna = "Maullén" if nombre_comuna=="Maullín"
replace nombre_comuna = "O'Higgins" if nombre_comuna=="O´Higgins"
replace nombre_comuna = "Paihuano" if nombre_comuna=="Paiguano"
replace nombre_comuna = "Peñalolén" if nombre_comuna=="Peñalolen"
replace nombre_comuna = "Ranquil" if nombre_comuna=="Ránquil"

rename comuna COD_COMUNA
rename nombre_comuna nom_com
merge 1:1 nom_com using "$MAP/ChileLite"
drop if _merge!=3

gen adoptTime = pillYear - 2008

rename _id _ID
drop if _ID==242|_ID==336|_ID==346

#delimit ;
spmap adoptTime using "$MAP/ChileLiteCoords", id(_ID) legstyle(2)
legend(symy(*2.8) symx(*2.8) size(*3.5) rowgap(1) title("Rollout", size(*2.5))
       lab(5 "2012") lab(4 "2011")
       lab(3 "2010") lab(2 "2009") position(11))
clmethod(custom) clbreaks(0 1.1 2.1 3.1 4.1)
fcolor(Spectral);
graph export "$OUT/Rollout_Time.eps", replace;

gen RM = regexm(nom_reg , "Santiago");
spmap adoptTime if RM==1 using "$MAP/Chile_coords", id(_ID) legstyle(2)
legend(symy(*1) symx(*1) size(*1.2) rowgap(1) title("Rollout")
       lab(5 "2012") lab(4 "2011")
       lab(3 "2010") lab(2 "2009") position(11))
clmethod(custom)  clbreaks(0 1.1 2.1 3.1 4.1)
fcolor(Spectral);
graph export "$OUT/Rollout_Time_RM.eps", replace;

format pillPop %5.2f;
spmap pillPop using "$MAP/ChileLiteCoords", id(_ID) legstyle(2)
legend(symy(*2.8) symx(*2.8) size(*3.5) rowgap(1)
       title("Disbursements", size(*1.5)) position(11))
fcolor(Spectral);
graph export "$OUT/PillIntensity.eps", replace;

spmap pillPop if RM==1 using "$MAP/Chile_coords", id(_ID) legstyle(2)
legend(symy(*1) symx(*1) size(*1.2) rowgap(1)
       title("Pill Disbursements", size(*0.8)) position(11))
fcolor(Spectral);
graph export "$OUT/PillIntensity_RM.eps", replace;
#delimit cr
restore



*** Descriptive analysis of pill measures
gen atLeastOnePill = pillPop!=0 if pillPop!=.
gen NoPills        = pillPop==0 if pillPop!=.

eststo: reg pillPop            pillDides
eststo: reg atLeastOnePill     pillDides
eststo: reg pildorasEntregadas pillDides

eststo: reg pillPop            pillDides [aw=poblacion15_49]
eststo: reg atLeastOnePill     pillDides [aw=poblacion15_49]
eststo: reg pildorasEntregadas pillDides [aw=poblacion15_49]


lab var pillDides "EC Pill Available"
#delimit ;
esttab est1 est2 est3 est4 est5 est6 using "$OUT/pillCorrelates.tex",
b(%-9.3f) se(%-9.3f) noobs keep(pillDides _cons) nonotes nogaps
mlabels(, none) nonumbers style(tex) fragment replace noline label
starlevel ("*" 0.10 "**" 0.05 "***" 0.01)
stats(N, fmt(%9.0gc) label("\\ Observations"));
estimates clear;
#delimit cr
lab var pill "Municipality Has Pill Availability"


*-------------------------------------------------------------------------------
*--- (3) DiD multiplegt
*-------------------------------------------------------------------------------
gen pillReject = 1 if pildorasNoEntregadas>0&pildorasNoEntregadas!=.
replace pillReject = 0 if pildorasNoEntregadas==0

gen left  = party=="PC"|party=="PCCH"|party=="PCCh"|party=="PS"
gen right = party=="UDI"|party=="RN"
gen indep = party=="IND"
gen logPop = log(poblacion15_49)
gen MayorWom = mujer==1 if mujer!=3
gen vote10 = floor(votop*10)
tab vote10, gen(_votes)

local B = 25
local cs _parties* _sexo* votop voteMi totalModern_rate poblacion15_49

cap mkdir "$OUT/DIDM"
preserve
keep if year!=2017

local j = 1
foreach y in MMR MM abort haem maleMorbidity puerperium birth {
    foreach age in 15_49 15_19 20_24 25_29 30_34 35_39 40_44 45_49 {
        if `j'==1 {
            gen haem`age'Pop          = haemorrhage`age'/poblacion`age'*1000
            gen abort`age'Pop         = abortion`age'   /poblacion`age'*1000
            gen birth`age'Pop         = birth`age'   /poblacion`age'*1000
            gen MMR`age'Pop           = maternalDeath`age'/birth`age'*100000
            gen MM`age'Pop            = maternalDeath`age'/poblacion`age'*1000
            *replace MMR`age'Pop = 0 if birth`age'==.
            gen maleMorbidity`age'Pop = maleMorbidity/poblacionMale`age'*1000
            gen puerperium`age'Pop    = puerperium/poblacion`age'*1000
        }
        if `"`y'"'=="MMR"&`"`age'"'=="45_49" {
            dis "MMR 45-49 not estimated due to very low coverage"
        }
        else {            
            local atext1
            local atext2
            if `"`y'"'=="abort"&`"`age'"'=="15_49" {
                #delimit ;
                local atext1 text(-1.5 -1.5 "{&beta}=-0.837{superscript:**}"
                                  -1.8 -1.5 "(0.388)", size(large));
                local atext2 text(0.8 -1.5 "{&beta}=0.176{superscript:*}"
                                  0.69 -1.5 "(0.104)", size(large));
                #delimit cr
            }
            if `"`y'"'=="haem"&`"`age'"'=="15_49" {
                #delimit ;
                local atext1 text(-0.4 -1.5 "{&beta}=0.054"
                                  -0.55 -1.5 "(0.159)", size(large));
                local atext2 text(0.35 -1.5 "{&beta}=0.074"
                                  0.3 -1.5 "(0.058)", size(large));
                #delimit cr
            }
            if `"`y'"'=="abort"&`"`age'"'=="15_19" {
                #delimit ;
                local atext1 text(-2.8 -1.5 "{&beta}=-1.930{superscript:**}"
                                  -3.8 -1.5 "(0.907)", size(large));
                local atext2 text(0.8 -1.5 "{&beta}=0.077{superscript:*}"
                                  0.65 -1.5 "(0.155)", size(large));
                #delimit cr
            }
            if `"`y'"'=="haem"&`"`age'"'=="15_19" {
                #delimit ;
                local atext1 text(-0.65 -1.5 "{&beta}=0.136"
                                  -0.85 -1.5 "(0.282)", size(large));
                local atext2 text(0.55 -1.5 "{&beta}=0.053"
                                  0.47 -1.5 "(0.128)", size(large));
                #delimit cr
            }
        
            **Note: consistently set same seeds for weighted/unweighted models
            set seed 12011303
            ***BINARY [Weighted and unweighted]
            #delimit ;
            did_multiplegt_dc `y'`age'Pop comuna year pill, placebo(3) dynamic(2)
            covariances weight(poblacion`age') breps(`B') controls(`cs')
            average_effect(prop_number_switchers) cluster(comuna)
            trends_nonparam(regcode) `atext1'; 
            ereturn list;
            local betaBin = `=e(effect_average)';
            local seBin   = `=e(se_effect_average)';
            #delimit cr
            foreach num of numlist 1 2 3 {
                local p`num'Bin   = `=e(placebo_`num')'
                local sep`num'Bin = `=e(se_placebo_`num')'
            }
            local b`age' = `betaBin'
            local se`age' = `seBin'
            sum `y'`age'Pop
            local N`age' = r(N)
            local m`age' = r(mean)
            #delimit ;

            graph export "$OUT/DIDM/DIDM_`y'_`age'.eps", replace;

            set seed 12011303;
            did_multiplegt_dc `y'`age'Pop comuna year pill, placebo(3) dynamic(2)
            covariances breps(`B') cluster(comuna) trends_nonparam(regcode) 
            average_effect(prop_number_switchers) controls(`cs');
            ereturn list;
            local betaBin_u = `=e(effect_average)';
            local seBin_u   = `=e(se_effect_average)';
            #delimit cr
            foreach num of numlist 1 2 3 {
                local p`num'Bin_u   = `=e(placebo_`num')'
                local sep`num'Bin_u = `=e(se_placebo_`num')'
            }
            local u`age' = `betaBin_u'
            local ue`age' = `seBin_u'
            sum `y'`age'Pop
            local U`age' = r(N)
            local d`age' = r(mean)
            #delimit ;
            graph export "$OUT/DIDM/DIDM_`y'_unweighted_`age'.eps", replace;
        
            set seed 121327;
            dis "CONTINUOUS [Weighted and unweighted]";
            did_multiplegt_dc `y'`age'Pop comuna year pillPopCont, placebo(3)
            dynamic(1) covariances weight(poblacion`age') breps(`B') cluster(comuna)
            average_effect(prop_number_switchers) controls(`cs') trends_nonparam(regcode);
            ereturn list;
            local betaCon = `=e(effect_average)';
            local seCon   = `=e(se_effect_average)';
            #delimit cr
            foreach num of numlist 1 2 3 {
                local p`num'Con   = `=e(placebo_`num')'
                local sep`num'Con = `=e(se_placebo_`num')'
            }
            #delimit ;
            graph export "$OUT/DIDM/DIDM_continuous_`y'_`age'.eps", replace;
            
            set seed 121327;
            **unweighted;
            did_multiplegt_dc `y'`age'Pop comuna year pillPopCont, placebo(3) dynamic(1)
            covariances breps(`B') cluster(comuna)
            average_effect(prop_number_switchers) controls(`cs') trends_nonparam(regcode);
            ereturn list;
            local betaCon_u = `=e(effect_average)';
            local seCon_u   = `=e(se_effect_average)';
            #delimit cr
            foreach num of numlist 1 2 3 {
                local p`num'Con_u   = `=e(placebo_`num')'
                local sep`num'Con_u = `=e(se_placebo_`num')'
            }
            #delimit ;
            graph export "$OUT/DIDM/DIDM_continuous_`y'_unweighted_`age'.eps", replace;

            set seed 2007;
            dis "BINARY REJECT [Weighted and unweighted]";
            did_multiplegt_dc `y'`age'Pop comuna year pillReject, placebo(3) dynamic(2)
            covariances weight(poblacion`age') breps(`B') cluster(comuna) `atext2'
            average_effect(prop_number_switchers) controls(`cs') trends_nonparam(regcode);
            ereturn list;
            local betaBinc = `=e(effect_average)';
            local seBinc   = `=e(se_effect_average)';
            #delimit cr
            foreach num of numlist 1 2 3 {
                local p`num'Binc   = `=e(placebo_`num')'
                local sep`num'Binc = `=e(se_placebo_`num')'
            }
            #delimit ;
            graph export "$OUT/DIDM/DIDM_`y'_noPillBinary_`age'.eps", replace;

            set seed 2007;
            did_multiplegt_dc `y'`age'Pop comuna year pillReject, placebo(3) dynamic(2)
            covariances breps(`B')  cluster(comuna) trends_nonparam(regcode) 
            average_effect(prop_number_switchers) controls(`cs');
            ereturn list;
            local betaBinc_u = `=e(effect_average)';
            local seBinc_u   = `=e(se_effect_average)';
            #delimit cr
            foreach num of numlist 1 2 3 {
                local p`num'Binc_u   = `=e(placebo_`num')'
                local sep`num'Binc_u = `=e(se_placebo_`num')'
            }
            #delimit ;
            graph export "$OUT/DIDM/DIDM_`y'_noPillBinary_unweighted_`age'.eps", replace;
    
            set seed 1601;
            dis "CONTINUOUS REJECT [Weighted and unweighted]";
            did_multiplegt_dc `y'`age'Pop comuna year noPillPopCont, placebo(3) dynamic(3)
            covariances weight(poblacion`age') breps(`B')  cluster(comuna)
            average_effect(prop_number_switchers) controls(`cs') trends_nonparam(regcode);
            ereturn list;
            local betaConc = `=e(effect_average)';
            local seConc   = `=e(se_effect_average)';
            #delimit cr
            foreach num of numlist 1 2 3 {
                local p`num'Conc   = `=e(placebo_`num')'
                local sep`num'Conc = `=e(se_placebo_`num')'
            }
            #delimit ;
            graph export "$OUT/DIDM/DIDM_noPillContinuous_`y'_`age'.eps", replace;
    
            set seed 1601;
            did_multiplegt_dc `y'`age'Pop comuna year noPillPopCont, placebo(3) dynamic(3)
            covariances breps(`B') cluster(comuna) average_effect(prop_number_switchers)
            controls(`cs') trends_nonparam(regcode);
            ereturn list;
            local betaConc_u = `=e(effect_average)';
            local seConc_u   = `=e(se_effect_average)';
            #delimit cr
            foreach num of numlist 1 2 3 {
                local p`num'Conc_u   = `=e(placebo_`num')'
                local sep`num'Conc_u = `=e(se_placebo_`num')'
            }
            #delimit ;
            graph export "$OUT/DIDM/DIDM_noPillContinuous_`y'_unweighted_`age'.eps", replace;
            #delimit cr
            
            foreach xx in Bin Bin_u Con Con_u Binc Binc_u Conc Conc_u {
                local crit = 2*(1-normal(abs(`beta`xx''/`se`xx'')))
                if `crit'<=0.01 {
                    local X`xx' "***"
                }
                else if `crit'<=0.05 {
                    local X`xx' "**"
                }
                else if `crit'<=0.10 {
                    local X`xx' "*"
                }
                else {
                    local X`xx' ""
                }
    
                foreach num of numlist 1 2 3 {
                    local crit = 2*(1-normal(abs(`p`num'`xx''/`sep`num'`xx'')))
                    if `crit'<=0.01 {
                        local X`num'`xx' "***"
                    }
                    else if `crit'<=0.05 {
                        local X`num'`xx' "**"
                    }
                    else if `crit'<=0.10 {
                        local X`num'`xx' "*"
                    }
                    else {
                        local X`num'`xx' ""
                    }
                }   
            }
        }
        file open myfile using "$OUT/DIDM/DIDM_`y'`age'_weighted.tex", write replace
        file write myfile %45s  "Binary Classification (EC Pill)    &" %5.3f ///
        (`betaBin') %10s "`XBin'" %6s "& &" %5.3f (`betaBinc') %15s "`XBinc' & \\" _n
        
        file write myfile %4s "& (" %5.3f (`seBin') %7s ") & & (" %5.3f (`seBinc') ///
        %10s ")   & \\" _n
        file write myfile %65s "Continuous Classification (EC Pills per 1,000 Women)&&" ///
        %5.3f (`betaCon') ///
        %10s "`XCon'" %6s "& &" %5.3f (`betaConc') %12s "`XConc' \\" _n
        file write myfile %4s "&& (" %5.3f (`seCon') %7s ") & & (" %5.3f (`seConc') ///
        %10s ")  \\ \\" _n
        
        file write myfile %10s "Placebo 1&" %5.3f (`p1Bin') %6s "`X1Bin'" %1s "&" ///
        %5.3f (`p1Con') %6s "`X1Con'" %1s "&" %5.3f (`p1Binc') %6s "`X1Binc'" %1s "&" ///
        %5.3f (`p1Conc') %6s "`X1Conc'" %3s "\\" _n
        file write myfile %10s "& (" %5.3f (`sep1Bin') %3s ")&(" %5.3f (`sep1Con') ///
        %3s ")&(" %5.3f (`sep1Binc') %3s ")&(" %5.3f (`sep1Conc') %4s ") \\" _n

        file write myfile %10s "Placebo 2&" %5.3f (`p2Bin') %6s "`X2Bin'" %1s "&" ///
        %5.3f (`p2Con') %6s "`X2Con'" %1s "&" %5.3f (`p2Binc') %6s "`X2Binc'" %1s "&" ///
        %5.3f (`p2Conc') %6s "`X2Conc'" %3s "\\" _n
        file write myfile %10s "& (" %5.3f (`sep2Bin') %3s ")&(" %5.3f (`sep2Con') ///
        %3s ")&(" %5.3f (`sep2Binc') %3s ")&(" %5.3f (`sep2Conc') %4s ") \\" _n

        file write myfile %10s "Placebo 3&" %5.3f (`p3Bin') %6s "`X3Bin'" %1s "&" ///
        %5.3f (`p3Con') %6s "`X3Con'" %1s "&" %5.3f (`p3Binc') %6s "`X3Binc'" %1s "&" ///
        %5.3f (`p3Conc') %6s "`X3Conc'" %3s "\\" _n
        file write myfile %10s "& (" %5.3f (`sep3Bin') %3s ")&(" %5.3f (`sep3Con') ///
        %3s ")&(" %5.3f (`sep3Binc') %3s ")&(" %5.3f (`sep3Conc') %4s ") \\ \\" _n
        count if `y'`age'Pop !=.
        local Nobs = r(N)
        
        file write myfile "Observations & `Nobs' & `Nobs' & `Nobs' & `Nobs' \\"
        file close myfile

        
        file open myfile using "$OUT/DIDM/DIDM_`y'`age'_unweighted.tex", write replace
        file write myfile %45s  "Binary Classification (EC Pill) &" %5.3f ///
        (`betaBin_u') %10s "`XBin_u'" %6s "& &" %5.3f (`betaBinc_u') %15s "`XBinc_u' & \\" _n
        
        file write myfile %4s "& (" %5.3f (`seBin_u') %7s ") & & (" %5.3f (`seBinc_u') ///
        %10s ")   & \\" _n
        file write myfile %65s "Continuous Classification (EC Pills per 1,000 Women) &&" ///
        %5.3f (`betaCon_u') ///
        %10s "`XCon_u'" %6s "& &" %5.3f (`betaConc_u') %12s "`XConc_u' \\" _n
        file write myfile %4s "&& (" %5.3f (`seCon_u') %7s ") & & (" %5.3f (`seConc_u') ///
        %10s ")   \\" _n
        
        file write myfile %10s "Placebo 1&" %5.3f (`p1Bin_u') %6s "`X1Bin_u'" %1s "&" ///
        %5.3f (`p1Con_u') %6s "`X1Con_u'" %1s "&" %5.3f (`p1Binc_u') %6s "`X1Binc_u'" %1s "&" ///
        %5.3f (`p1Conc_u') %6s "`X1Conc_u'" %3s "\\" _n
        file write myfile %10s "& (" %5.3f (`sep1Bin_u') %3s ")&(" %5.3f (`sep1Con_u') ///
        %3s ")&(" %5.3f (`sep1Binc_u') %3s ")&(" %5.3f (`sep1Conc_u') %4s ") \\" _n
        
        file write myfile %10s "Placebo 2&" %5.3f (`p2Bin_u') %6s "`X2Bin_u'" %1s "&" ///
        %5.3f (`p2Con') %6s "`X2Con'" %1s "&" %5.3f (`p2Binc_u') %6s "`X2Binc_u'" %1s "&" ///
        %5.3f (`p2Conc_u') %6s "`X2Conc_u'" %3s "\\" _n
        file write myfile %10s "& (" %5.3f (`sep2Bin_u') %3s ")&(" %5.3f (`sep2Con_u') ///
        %3s ")&(" %5.3f (`sep2Binc_u') %3s ")&(" %5.3f (`sep2Conc_u') %4s ") \\" _n

        file write myfile %10s "Placebo 3&" %5.3f (`p3Bin_u') %6s "`X3Bin_u'" %1s "&" ///
        %5.3f (`p3Con_u') %6s "`X3Con_u'" %1s "&" %5.3f (`p3Binc_u') %6s "`X3Binc_u'" %1s "&" ///
        %5.3f (`p3Conc_u') %6s "`X3Conc_u'" %3s "\\" _n
        file write myfile %10s "& (" %5.3f (`sep3Bin_u') %3s ")&(" %5.3f (`sep3Con_u') ///
        %3s ")&(" %5.3f (`sep3Binc_u') %3s ")&(" %5.3f (`sep3Conc_u') %4s ") \\ \\" _n
        
        file write myfile "Observations & `Nobs' & `Nobs' & `Nobs' & `Nobs' \\"
        file close myfile
        dis "1"
        
        dis "Table for Outcome: `y', `age' (Weighted)"
        dis "Binary Classification     & `betaBin'`XBin' & & `betaBinc'`XBinc' & \\"
        dis "                          & (`seBin')       & & (`seBinc')        & \\"
        dis "Continuous Classification & `betaCon'`XCon' & & `betaConc'`XConc' & \\"
        dis "                          & (`seCon')       & & (`seConc')        & \\"
        dis "2"
        
        dis "Table for Outcome: `y' (Unweighted)"
        dis "Binary Classification     & `betaBin_u'`XBin_u' & & `betaBinc_u'`XBinc_u' & \\"
        dis "                          & (`seBin_u')         & & (`seBinc_u')          & \\"
        dis "Continuous Classification & `betaCon_u'`XCon_u' & & `betaConc_u'`XConc_u' & \\"
        dis "                          & (`seCon_u')       & & (`seConc_u')            & \\"
        dis "3"
    }
    foreach age in 15_49 15_19 20_24 25_29 30_34 35_39 40_44 45_49 {
        if `"`y'"'=="MMR"&`"`age'"'=="45_49" {
            local crit = 1
            local b`age' = "--"
            local se`age' = "--"
            local N`age' = "--"
            local m`age' = "--"
        }
        else {
            local crit = 2*(1-normal(abs(`b`age''/`se`age'')))
            local b`age'  = string(`b`age'', "%5.3f")
            local se`age' = string(`se`age'', "%5.3f")
            local m`age'  = string(`m`age'', "%5.3f")
        }
        if `crit'<=0.01 {
            local b`age' "`b`age''***"
        }
        else if `crit'<=0.05 {
            local b`age' "`b`age''**"
        }
        else if `crit'<=0.10 {
            local b`age' "`b`age''*"
        }
        local se`age' = "(`se`age'')"

        if `"`y'"'=="MMR"&`"`age'"'=="45_49" {
            local crit = 1
            local u`age' = "--"
            local ue`age' = "--"
            local U`age' = "--"
            local d`age' = "--"
        }
        else {
            local crit = 2*(1-normal(abs(`u`age''/`ue`age'')))
            local u`age'  = string(`u`age'', "%5.3f")
            local ue`age' = string(`ue`age'', "%5.3f")        
            local d`age'  = string(`d`age'', "%5.3f")
        }
        if `crit'<=0.01 {
            local u`age' "`u`age''***"
        }
        else if `crit'<=0.05 {
            local u`age' "`u`age''**"
        }
        else if `crit'<=0.10 {
            local u`age' "`u`age''*"
        }
        local ue`age' = "(`ue`age'')"
    }
    file open myfile using "$OUT/DIDM/DIDM_`y'.tex", write replace
    file write myfile "EC Pill Available & `b15_49' & `b15_19' & `b20_24'"
    file write myfile "&`b25_29' &`b30_34' &`b35_39' &`b40_44'&`b45_49' \\" _n

    file write myfile " & `se15_49' & `se15_19' & `se20_24' &`se25_29' &`se30_34' "
    file write myfile "&`se35_39' &`se40_44'&`se45_49' \\ \\" _n

    file write myfile "Observations & `N15_49' & `N15_19' & `N20_24' &`N25_29'"
    file write myfile " &`N30_34' &`N35_39' &`N40_44'&`N45_49' \\" _n

    file write myfile "Mean of Dependent Variable & `m15_49' & `m15_19' & `m20_24'"
    file write myfile " &`m25_29' &`m30_34' &`m35_39' &`m40_44'&`m45_49' \\" _n
    file close myfile

    file open myfile using "$OUT/DIDM/DIDM_`y'_unweighted.tex", write replace
    file write myfile "EC Pill Available & `u15_49' & `u15_19' & `u20_24'"
    file write myfile "&`u25_29' &`u30_34' &`u35_39' &`u40_44'&`u45_49' \\" _n

    file write myfile " & `ue15_49' & `ue15_19' & `ue20_24' &`ue25_29' &`ue30_34' "
    file write myfile "&`ue35_39' &`ue40_44'&`ue45_49' \\ \\" _n

    file write myfile "Observations & `U15_49' & `U15_19' & `U20_24' &`U25_29'"
    file write myfile " &`U30_34' &`U35_39' &`U40_44'&`U45_49' \\" _n

    file write myfile "Mean of Dependent Variable & `d15_49' & `d15_19' & `d20_24'"
    file write myfile " &`d25_29' &`d30_34' &`d35_39' &`d40_44'&`d45_49' \\" _n
    file close myfile
    local ++j
}
restore

*-------------------------------------------------------------------------------
*--- (4) Event study (Full)
*-------------------------------------------------------------------------------
gen birth15_49Pop=birth15_49/poblacion15_49*1000
gen haem15_49Pop =haemorrhage15_49/poblacion15_49*1000
gen abort15_49Pop=abortion15_49    /poblacion15_49*1000
gen MMR15_49Pop=maternalDeath15_49/birth15_49*100000
gen MM15_49Pop=maternalDeath15_49/poblacion15_49*1000
**placebos
gen maleMorbidityPop   = maleMorbidity/poblacionMale15_49*1000
gen puerperiumPop      = puerperium/poblacion15_49*1000
gen femaleMorbidityPop = femaleMorbidity/poblacion15_49*1000

gen pillyear     = 2009 if pill2009==1
replace pillyear = 2010 if pill2010==1
replace pillyear = 2011 if pill2011==1
replace pillyear = 2012 if pill2012==1

gen pillAdopt = year - pillyear
gen reverse   = year>pillyear & pill == 0


foreach y of numlist 0(1)10 {
    gen PillN`y'=pillAdopt==-`y'
    gen PillP`y'=pillAdopt==`y'
}
drop PillN0 PillN1 PillP9 PillP10

set matsize 1000
local X1 poblacion15_49
local X2 _parties* _sexo* votop voteMiss poblacion15_49
local X3 `X2' totalModern_rate
local fname
local gopts name(Graph) xsize(8.00) ysize(4.00) tmargin(3.50) lmargin(0.25)


foreach c of numlist 1 2 3 {
    dis "Running event studies with controls X`c'"
    local leads PillN9 PillN8 PillN7 PillN6 PillN5 PillN4 PillN3 PillN2
    local lags  PillP0 PillP1 PillP2 PillP3 PillP4 PillP5 PillP6 PillP7 PillP8 
    local se abs(comuna) cluster(comuna)
    local wts [aw=poblacion15_49]
    local avars 
    if `c'==2 local fname polcontrols
    if `c'==3 local fname contcontrols

        
    foreach var in haem abort birth {
        if `"`var'"'=="birth" {
            local leads PillN9 PillN8 PillN7 PillN6 PillN5 PillN4 PillN3 PillN2
            local lags  PillP0 PillP1 PillP2 PillP3 PillP4 PillP5 PillP6 PillP7
        }
        areg `var'15_49Pop i.year `leads' `lags' `X`c'' `wts', `se'
        test `leads'
        test `lags'

        if `"`var'"'=="birth" local class "Births"
        else if `"`var'"'=="MMR"    local class "Mortality"
        else local class "Morbidity"
        if `"`var'"'=="birth" local maxT=7
        else local maxT=8
        preserve
        gen PointEst = .
        gen UB       = .
        gen LB       = .
        gen UB2       = .
        gen LB2       = .
        gen time     = .
        local j = 1
        local t = -9
        foreach lead in `leads' {
            replace PointEst = _b[`lead'] in `j'
            replace time = `t' in `j'
            replace LB = _b[`lead']  + invttail(e(N),0.975)*_se[`lead'] in `j'
            replace UB = _b[`lead']  + invttail(e(N),0.025)*_se[`lead'] in `j'    
            replace LB2 = _b[`lead'] + invttail(e(N),0.950)*_se[`lead'] in `j'
            replace UB2 = _b[`lead'] + invttail(e(N),0.050)*_se[`lead'] in `j'    
            local ++j
            local ++t
        }
        replace time     = `t' in `j'
        replace PointEst = 0 in `j'
        replace LB       = 0 in `j'
        replace UB       = 0 in `j'
        replace LB2       = 0 in `j'
        replace UB2       = 0 in `j'
        local ++t
        local ++j
        foreach lag in `lags' {
            replace PointEst = _b[`lag'] in `j'
            replace time = `t' in `j'
            replace LB = _b[`lag']  + invttail(e(N),0.975)*_se[`lag'] in `j'
            replace UB = _b[`lag']  + invttail(e(N),0.025)*_se[`lag'] in `j'
            replace LB2 = _b[`lag'] + invttail(e(N),0.950)*_se[`lag'] in `j'
            replace UB2 = _b[`lag'] + invttail(e(N),0.050)*_se[`lag'] in `j'
            local ++j
            local ++t
        }
        if `c'==1&(`"`var'"'=="haem"|`"`var'"'=="abort") {
            foreach lag in PillP0 PillP1 PillP2 PillP3 PillP4 PillP5 PillP6 PillP7 {
                sum poblacion15_49 if `lag'==1&pildorasEntregadas!=.
                local apob = r(mean)*r(N)
                sum poblacion15_49 if `lag'==1
                local pob = r(mean)*r(N)
                sum pildorasEntregadas if `lag'==1
                local pil = r(mean)*r(N)
                dis "TABLE A3: Lag `lag' Pil: `pil', Pob `apob', Rate:" `pil'/`apob'*1000
            }
        }
        sum poblacion15_49 if PillP0==1|PillN2==1|PillN3==1&pildorasEntregadas!=.
        local apob = r(mean)*r(N)
        
        sum pildorasEntregadas if PillP7==1|PillN2==1|PillN3==1
        local pil = r(mean)*r(N)
        dis "TABLE A3: Lag `lag' 7+: `pil', Pob `apob', Rate:" `pil'/`apob'*1000
            
        
        #delimit ;
        twoway rarea LB UB  time, color(gs14%40) yline(0, lcolor(red))
          || rarea LB2 UB2 time, color(gs14%70) 
          || scatter PointEst time in 1/`j',
        xline(-1, lcolor(black) lpattern(solid))
        xlabel(-9(1)`maxT') legend(order(3 "Point Estimate" 1 "95% CI"  2 "90% CI"))
        ytitle("`class' per 1,000 Women") xsize(8)
        xtitle("Years to Arrival of Emergency Contraceptive Pill");
        #delimit cr
        translate @Graph $OUT/event/`var'15_49`fname'.pdf, `gopts'        
        restore
    }



    ***"PLACEBO" OUTCOMES
    local leads PillN9 PillN8 PillN7 PillN6 PillN5 PillN4 PillN3 PillN2
    local lags  PillP0 PillP1 PillP2 PillP3 PillP4 PillP5 PillP6 PillP7 PillP8 
    foreach var of varlist maleMorbidity femaleMorbidity puerperium {
        local wts [aw=poblacion15_49]
        if `"`var'"' == "maleMorbidity" local wts
        areg `var'Pop i.year `leads' `lags' `X`c'' `wts', `se'
        test `leads'
        test `lags'
        
        preserve
        gen PointEst = .
        gen UB       = .
        gen LB       = .
        gen UB2       = .
        gen LB2       = .
        gen time     = .
        local j = 1
        local t = -9
        foreach lead in `leads' {
            replace PointEst = _b[`lead'] in `j'
            replace time = `t' in `j'
            replace LB = _b[`lead']  + invttail(e(N),0.975)*_se[`lead'] in `j'
            replace UB = _b[`lead']  + invttail(e(N),0.025)*_se[`lead'] in `j'    
            replace LB2 = _b[`lead'] + invttail(e(N),0.950)*_se[`lead'] in `j'
            replace UB2 = _b[`lead'] + invttail(e(N),0.050)*_se[`lead'] in `j'    
            local ++j
            local ++t
        }
        replace time     = `t' in `j'
        replace PointEst = 0 in `j'
        replace LB       = 0 in `j'
        replace UB       = 0 in `j'
        replace LB2       = 0 in `j'
        replace UB2       = 0 in `j'
        local ++t
        local ++j
        foreach lag in `lags' {
            replace PointEst = _b[`lag'] in `j'
            replace time = `t' in `j'
            replace LB = _b[`lag']  + invttail(e(N),0.975)*_se[`lag'] in `j'
            replace UB = _b[`lag']  + invttail(e(N),0.025)*_se[`lag'] in `j'
            replace LB2 = _b[`lag'] + invttail(e(N),0.950)*_se[`lag'] in `j'
            replace UB2 = _b[`lag'] + invttail(e(N),0.050)*_se[`lag'] in `j'
            local ++j
            local ++t
        }
    
        #delimit ;
        twoway rarea LB UB  time, color(gs14%40) yline(0, lcolor(red))
           || rarea LB2 UB2 time, color(gs14%70) 
           || scatter PointEst time in 1/`j', xline(-1, lcolor(black) lpattern(solid))
        xlabel(-9(1)8) legend(order(3 "Point Estimate" 1 "95% CI"  2 "90% CI"))
        ytitle("Morbidity per 1,000") xsize(8)
        xtitle("Years to Arrival of Emergency Contraceptive Pill");
        #delimit cr
        translate @Graph $OUT/event/`var'`fname'.pdf, name(Graph)
        restore
    }

    ***Event studies based on terciles.  3 sets of lags and leads
    foreach y of numlist 0(1)10 {
        foreach num of numlist 1 2 3 {
            cap gen PillN`y'_q`num'=pillAdopt==-`y'&q`num'==1
            cap gen PillP`y'_q`num'=pillAdopt==`y' &q`num'==1
            #delimit ;
            local leads`num' PillN9_q`num' PillN8_q`num' PillN7_q`num' PillN6_q`num'
                             PillN5_q`num' PillN4_q`num' PillN3_q`num' PillN1_q`num';
            local lags`num'  PillP0_q`num' PillP1_q`num' PillP2_q`num' PillP3_q`num'
                             PillP4_q`num' PillP5_q`num' PillP6_q`num' PillP7_q`num'
                             PillP8_q`num';
            #delimit cr
        }
    }
    drop PillN0_* PillN2_* PillP9_* PillP10_*
    local leads `leads1' `leads2' `leads3'
    local lags  `lags1'  `lags2'  `lags3'

    local ivars haem15_49Pop abort15_49Pop maleMorbidityPop puerperiumPop 
    foreach var in `ivars' {
        local wts [aw=poblacion15_49]
        if `"`var'"' == "maleMorbidityPop" local wts 
        areg `var' i.year `leads' `lags' `X`c'' `wts', `se'

        if `"`var'"'=="MM15_49Pop" local class "Mortality"
        else local class "Morbidity"

        preserve
        local jit = -0.34
        foreach num of numlist 1(1)3 {
            local jit = `jit'+0.17
            gen time_`num' = .    
            gen PointEst_`num' = .
            gen UB_`num'       = .
            gen LB_`num'       = .
            local j = 1
            local t = -9
            foreach lead in `leads`num'' {
                replace PointEst_`num' = _b[`lead'] in `j'
                replace time_`num' = `t'+`jit' in `j'
                replace LB_`num' = _b[`lead']  + invttail(e(N),0.975)*_se[`lead'] in `j'
                replace UB_`num' = _b[`lead']  + invttail(e(N),0.025)*_se[`lead'] in `j'    
                local ++j
                local ++t
            }
            replace time_`num'     = `t' in `j'
            replace PointEst_`num' = 0 in `j'
            replace LB_`num'       = 0 in `j'
            replace UB_`num'       = 0 in `j'
            local ++t
            local ++j
            foreach lag in `lags`num'' {
                replace PointEst_`num' = _b[`lag'] in `j'
                replace time_`num' = `t'+`jit' in `j'
                replace LB_`num' = _b[`lag']  + invttail(e(N),0.975)*_se[`lag'] in `j'
                replace UB_`num' = _b[`lag']  + invttail(e(N),0.025)*_se[`lag'] in `j'
                local ++j
                local ++t
            }
        }
        #delimit ;
        twoway rcap LB_1 UB_1 time_1, lcolor(gs12) yline(0, lcolor(red))
          || connected PointEst_1 time_1 in 1/`j', lcolor(black) lpattern(dash) ms(Oh)
          || rcap LB_2 UB_2 time_2, lcolor(gs12) 
          || connected PointEst_2 time_2 in 1/`j', lcolor(black) lpattern("_-") ms(T)
          || rcap LB_3 UB_3 time_3, lcolor(gs12)
          || connected PointEst_3 time_3 in 1/`j', ms(sh) lpattern(solid) lwidth(thick)
        xline(-1, lcolor(black) lpattern(solid))
        legend(order(2 "Low EC Pill" 4 "Medium EC Pill" 6 "High EC Pill" 1 "95% CI"))
        xlabel(-9(1)8) ytitle("`class' per 1,000") xsize(7.6) scheme(s1mono)
        xtitle("Years to Arrival of Emergency Contraceptive Pill");
        graph export "$OUT/event_`var'_intensity`fname'.eps", replace;
        #delimit cr
        restore
    }
}


preserve
drop PillN* PillP*
***Birth Event studies based on terciles.  3 sets of lags and leads
foreach y of numlist 0(1)10 {
    foreach num of numlist 1 2 3 {
        gen PillN`y'_q`num'=pillAdopt==-`y'&q`num'==1
        gen PillP`y'_q`num'=pillAdopt==`y' &q`num'==1
        #delimit ;
        local leadsb`num' PillN9_q`num' PillN8_q`num' PillN7_q`num' PillN6_q`num'
                          PillN5_q`num' PillN4_q`num' PillN3_q`num' PillN2_q`num';
        local lagsb`num'  PillP0_q`num' PillP1_q`num' PillP2_q`num' PillP3_q`num'
                          PillP4_q`num' PillP5_q`num' PillP6_q`num' PillP7_q`num';
        #delimit cr
    }
}
drop PillN0_* PillN1_* PillP8_* PillP9_* PillP10_*
local leadsb `leadsb1' `leadsb2' `leadsb3'
local lagsb  `lagsb1'  `lagsb2'  `lagsb3'

local wts [aw=poblacion15_49]
areg birth15_49Pop i.year `leadsb' `lagsb' `X2' `X3' `wts', `se'

local jit = -0.34
foreach num of numlist 1(1)3 {
    local jit = `jit'+0.17
    gen time_`num' = .    
    gen PointEst_`num' = .
    gen UB_`num'       = .
    gen LB_`num'       = .
    local j = 1
    local t = -9
    foreach lead in `leadsb`num'' {
        replace PointEst_`num' = _b[`lead'] in `j'
        replace time_`num' = `t'+`jit' in `j'
        replace LB_`num' = _b[`lead']  + invttail(e(N),0.975)*_se[`lead'] in `j'
        replace UB_`num' = _b[`lead']  + invttail(e(N),0.025)*_se[`lead'] in `j'    
        local ++j
        local ++t
    }
    replace time_`num'     = `t' in `j'
    replace PointEst_`num' = 0 in `j'
    replace LB_`num'       = 0 in `j'
    replace UB_`num'       = 0 in `j'
    local ++t
    local ++j
    foreach lag in `lagsb`num'' {
        replace PointEst_`num' = _b[`lag'] in `j'
        replace time_`num' = `t'+`jit' in `j'
        replace LB_`num' = _b[`lag']  + invttail(e(N),0.975)*_se[`lag'] in `j'
        replace UB_`num' = _b[`lag']  + invttail(e(N),0.025)*_se[`lag'] in `j'
        local ++j
        local ++t
    }
}
#delimit ;
twoway rcap LB_1 UB_1 time_1, lcolor(gs12) yline(0, lcolor(red))
  || connected PointEst_1 time_1 in 1/`j', lcolor(black) lpattern(dash) ms(Oh)
  || rcap LB_2 UB_2 time_2, lcolor(gs12) 
  || connected PointEst_2 time_2 in 1/`j', lcolor(black) lpattern("_-") ms(T)
  || rcap LB_3 UB_3 time_3, lcolor(gs12)
  || connected PointEst_3 time_3 in 1/`j', ms(sh) lpattern(solid) lwidth(thick)
xline(-1, lcolor(black) lpattern(solid))
legend(order(2 "Low EC Pill" 4 "Medium EC Pill" 6 "High EC Pill" 1 "95% CI"))
xlabel(-9(1)7) ytitle("Births per 1,000 Women") xsize(7.6) scheme(s1mono)
xtitle("Years to Arrival of Emergency Contraceptive Pill");
graph export "$OUT/event_birth15_49Pop_intensity.eps", replace;
#delimit cr        
drop PointEst_* time_* LB_* UB_*
restore


*-------------------------------------------------------------------------------
*--- (5) Event study (unnweighted)
*-------------------------------------------------------------------------------
local leads PillN9 PillN8 PillN7 PillN6 PillN5 PillN4 PillN3 PillN2
local lags  PillP0 PillP1 PillP2 PillP3 PillP4 PillP5 PillP6 PillP7 PillP8 
local se abs(comuna) cluster(comuna)
local wts [aw=poblacion15_49]

foreach var in haem abort MMR MM {
    areg `var'15_49Pop i.year `leads' `lags' `X1', `se'
    test `leads'
    test `lags'

    if `"`var'"'=="MMR" local class "Mortality"
    else local class "Morbidity"
    preserve
    gen PointEst = .
    gen UB       = .
    gen LB       = .
    gen UB2       = .
    gen LB2       = .
    gen time     = .
    local j = 1
    local t = -9
    foreach lead in `leads' {
        replace PointEst = _b[`lead'] in `j'
        replace time = `t' in `j'
        replace LB = _b[`lead']  + invttail(e(N),0.975)*_se[`lead'] in `j'
        replace UB = _b[`lead']  + invttail(e(N),0.025)*_se[`lead'] in `j'    
        replace LB2 = _b[`lead'] + invttail(e(N),0.950)*_se[`lead'] in `j'
        replace UB2 = _b[`lead'] + invttail(e(N),0.050)*_se[`lead'] in `j'    
        local ++j
        local ++t
    }
    replace time     = `t' in `j'
    replace PointEst = 0 in `j'
    replace LB       = 0 in `j'
    replace UB       = 0 in `j'
    replace LB2       = 0 in `j'
    replace UB2       = 0 in `j'
    local ++t
    local ++j
    foreach lag in `lags' {
        replace PointEst = _b[`lag'] in `j'
        replace time = `t' in `j'
        replace LB = _b[`lag']  + invttail(e(N),0.975)*_se[`lag'] in `j'
        replace UB = _b[`lag']  + invttail(e(N),0.025)*_se[`lag'] in `j'
        replace LB2 = _b[`lag'] + invttail(e(N),0.950)*_se[`lag'] in `j'
        replace UB2 = _b[`lag'] + invttail(e(N),0.050)*_se[`lag'] in `j'
        local ++j
        local ++t
    }
    
    #delimit ;
    twoway rarea LB UB  time, color(gs14%40) yline(0, lcolor(red))
       || rarea LB2 UB2 time, color(gs14%70) 
       || scatter PointEst time in 1/`j',
    xline(-1, lcolor(black) lpattern(solid))
    xlabel(-9(1)8) legend(order(3 "Point Estimate" 1 "95% CI"  2 "90% CI"))
    ytitle("`class' per 1,000 Women") xsize(8)
    xtitle("Years to Arrival of Emergency Contraceptive Pill");
    #delimit cr
    translate @Graph $OUT/event/`var'15_49_unweight.pdf, `gopts'        
    restore
}

*-------------------------------------------------------------------------------
*--- (5) Clean up
*-------------------------------------------------------------------------------
log close
