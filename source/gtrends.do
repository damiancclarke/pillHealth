clear all
insheet using "../data/google.csv", comma names
gen year  = substr(mes,1,4)
gen month = substr(mes,6,7)
destring year, replace
destring month, replace
gen time = year+month/12

lab var time "Time"
graph set eps fontface "Times New Roman"
#delimit ;
twoway line antic time, lwidth(medthick) 
    || line M time, lwidth(medthick) lpattern(dash)
    || line pae time, lpattern(longdash) scheme(LF3);
#delimit cr
graph export ../results/searches.eps, replace
