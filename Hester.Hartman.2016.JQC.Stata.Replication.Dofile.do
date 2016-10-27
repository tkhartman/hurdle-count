***  Hester & Hartman 2016 JQC   ***
*** Note: You will need the following packages to run this dofile:
* findit fre  // 'fre' is better version of the 'tab' command
* findit fitstat  // 'fitstat' displays more fit statistics following regression
* findit spost13_ado // Post-estimation add-ons for Stata 13
* findit spost9_legacy  // Post-estimation add-ons not included in above package
* findit coefplot  // Flexible plotting command

* Begin a Log File
* log using "hester.hartman.smcl"

* Allow dofile to run continuously
set more off, perm

* Show Excluded Category When Running Models
set showbaselevels on, perm

*** Primary Dependent Variable(s)
** DV1: EXPECTED MIN SENTENCE IN MONTHS 
** Create Multiplier for Expected Minimum Sentence (.25, .33, .85, or 1)
gen multiplier = 999
#delimit ; 
replace multiplier = .25 if inlist(sgc_offcode,
4, 8, 9, 13, 19, 24, 25, 41, 44, 48, 49, 50, 51,
52, 56, 57, 65, 74, 76, 80, 87, 88, 91, 94,    
100, 101, 102, 107, 108, 112, 115, 118, 120, 
122, 124, 129, 130, 135, 137, 162, 163, 164,  
165, 171, 174, 175, 176, 177, 178, 179, 180, 
182, 183, 186, 187, 189, 190, 191, 202, 215,  
218, 222, 240, 254, 255, 256, 258, 267, 273,  
274, 297, 306, 315, 316, 323, 324, 326, 340,   
348, 378, 381, 383, 406, 416, 420, 421, 422, 
423, 426, 427, 428, 429, 430, 437, 446, 448,   
470, 478, 479, 488, 491, 492, 493, 509, 510,   
513, 514, 521, 522, 527, 529, 530, 531, 532,    
533, 538, 541, 553, 561, 562, 615, 635, 657,    
659, 671, 685, 701, 717, 731, 744, 754, 756,    
781, 788, 795, 800, 840, 878, 892, 956, 1113,   
1206, 1252, 1345, 1561, 1573, 2161, 2349, 2352, 
2362, 2364, 2366, 2367, 2369, 2370, 2390, 2396,
2397, 2403, 2404, 2408, 2413, 2414, 2415, 2427, 
2451, 2461, 2462, 2464, 2468, 2481, 2506, 2525, 
2526, 2527, 2528, 2644, 2557, 2570, 2587, 2597, 
2607, 2615, 2655, 2657, 2689, 2759, 2770, 2877, 
2878, 2920) ;

#delimit ; 
replace multiplier = .25 if inlist(sgc_offcode,
87, 174, 2570, 10, 24, 27, 29, 30, 
31, 38, 43, 75, 78, 92, 93, 111, 123, 144, 150, 
193, 196, 197, 341, 243, 244, 245, 247, 249,    
261, 263, 268, 293, 295, 296, 298, 300, 311,    
314, 331, 300, 480, 494, 495, 508, 512, 516,    
520, 528, 535, 539, 540, 552, 566, 572, 573,   
610, 612, 622, 623, 624, 640, 641, 658, 670,    
682, 690, 709, 724, 766, 733, 755, 764, 774,    
865, 819, 895, 955, 966, 1068, 1165, 1167,      
1185, 1186, 1187, 1207, 1209, 1223, 1247, 1251, 
1298, 1486, 1530, 1612, 1638, 1650, 1651, 1783,
1855, 2055, 2175, 2350, 2355, 2363, 2372, 2383, 
2392, 2393, 2401, 2402, 2405, 2416, 2418, 2419, 
2439, 2457, 2482, 2491, 2505, 2522, 2529, 2544, 
2553, 2558, 2598, 2605, 2656, 2659, 2571, 2672, 
2688, 2760, 2776, 2791, 2804, 2813) ;

#delimit ; 
replace multiplier = 1/3 if inlist(sgc_offcode,
86, 278, 450, 454, 2360, 2362) ;

#delimit ; 
replace multiplier = .85 if inlist(sgc_offcode,
14, 17, 26, 79, 90, 95, 113, 114, 139, 141, 160,
161, 184, 185, 217, 253, 257, 280, 283, 312, 313, 
368, 385, 386, 387, 388, 389, 392, 394, 395, 396,
397, 402, 451, 457, 768, 2356, 2359, 2361, 2398, 
2463, 2550, 2551, 2584, 2585, 2599, 2600, 2658, 2766) ;

#delimit ; 
replace multiplier = 1 if inlist(sgc_offcode,
116, 148, 188, 281, 284, 349, 370, 452, 456, 2369, 2417) ;

#delimit cr
tab multiplier

* Use Multiplier to Alter Real Sentence (in Months)
gen expmin = 999
replace expmin = sentence*multiplier

tab expmin
su expmin, det

gen dv1 = ceil(expmin)
su dv1, det

* Extreme Outliers (e.g., Life) Need to Be Top-Coded at ~99.65%
replace dv1 = 720 if dv1>720
su dv1, det

label var dv1 "Prison Term (in Months)"

* Diagnostics: Distribution of the DV
histogram dv1, discrete w(10) percent xlabel(0(120)720)


*** Independent Variables
** Case Characteristics
* SERIOUS: Offense Seriousness (5-category ordinal item; 1=Low, 5=High; major offenses collapsed into 5)
fre offser
recode offser 5/8 = 5, gen(serious)
tab serious
su serious, det

* COMMITMENT: Commitment Score, Alternative Measure of Offense Seriousness (12-category ordinal item; 1=Low, 12=High)
tab ccpts99
gen commitment = ccpts99
tab commitment
su commitment, det

* OFFENSE: Offense Type (4-category nominal item; 1=Violent (incl. drug trafficking), 2=Drug, 3=Property, 4=Other
tab offtypeLibHyp
tab offtypeLibHyp of_dstrb
gen offense = offtypeLibHyp
replace offense = 1 if of_dstrb==1
tab offense
tab offense of_dstr
su offense, det

* TRIAL: Found or Pled Guilty? (1=Found Guilty; 0=Guilty Plea)
tab trial
su trial, det

* MANMIN: Mandatory Minimum? (1=Yes; 0=No)
gen manmin = 0

replace manmin=1 if inlist(sgc_offcode,79,90,113,116,114,139,148,217,267,280,  ///
	281,283,284,312,349,368,370,383,387,388,389,392,395,402,452,454,456,457,2356, ///
	2359,2360,231,2362,2417)

tab manmin incarc


** Offender Characteristics
* HISTORY: Individual's Criminal History (5-category ordinal item; 1=None, 5=Major)
fre crimhist
gen history = crimhist
su history, det

* BLACK: Dummy Variable for Race (1=Black, 0=White)
tab black
su black, det

* MALE: Dummy Variable for Gender (1=Male, 0=Female)
tab male
su male, det

* AGE: Offender's Real Age in Years
tab age
su age, det

** Standard Error Correction: Clustering by Judge
fre judge

* Label variables
label var dv1 "Expected Minimum Sentence"
label var serious "Offense Seriousness"
label var commitment "Commitment Score"
label var offense "Offense Type"
label var trial "Trial"
label var manmin "Mandatory Minimum"
label var history "Criminal History"
label var black "Black"
label var male "Male"
label var age "Age"


*** RESULTS: Hurdle NBRM (with Bootstrapped Clustered Standard Errors)
* HRM Part 1: Binary (logit) Model Predicting 0s
bootstrap, reps(1000) cluster(judge): logit dv1 c.serious##i.black commitment i.offense i.trial i.manmin c.history##i.black c.age##c.age i.male
logit dv1 c.serious##i.black commitment i.offense i.trial i.manmin c.history##i.black c.age##c.age i.male, nolog vce(cluster judge)
* Use Joseph Hilbe's GLME3_software: http://works.bepress.com/joseph_hilbe/60/
abich
margins, dydx(*) noesample vce(unconditional) post
coefplot, horizontal xline(0) yscale(reverse) recast(scatter)  cismooth grid(none)  /// 
		order(1.manmin 1.trial serious commitment 4.offense 2.offense 3.offense  ///
		history  1.male 1.black  age )  ///
	    coeflabel(1.manmin="Mandatory Minimum" 1.trial="Trial" history="Criminal History"  ///
		serious="Offense Seriousness" 1.male="Male" 1.black="Black"  ///
		commitment="Commitment Score" age="Age" 4.offense="Other Crime"  ///
		2.offense="Drug Crime" 3.offense="Property Crime", wrap(20))  ///
		headings(1.manmin="{bf:Case Characteristics}" history="{bf:Offender Characteristics}")  ///
		xtitle("Average Marginal Effect: Pr(Prison)")

logit dv1 c.serious##i.black commitment i.offense i.trial i.manmin c.history##i.black c.age##c.age i.male, nolog vce(cluster judge)
margins, dydx(black) at(history = (1 (1) 5)) noesample vce(unconditional) post
coefplot, horizontal xline(0) yscale(reverse) recast(scatter)  cismooth grid(none)  /// 
	    coeflabel(1._at="None" 2._at="Minimal" 3._at="Moderate" 4._at="Considerable" 5._at="Extensive", wrap(20)) ///
        eqrename(1.black = "Criminal History") eqstrict ///
		xtitle("Discrete Change in Pr(Prison > 0) for Blacks")
fitstat


* HRM Part 2: Zero-Truncated Negative Binomial (ZTBN) Model Predicting Positive Counts
bootstrap, reps(1000) cluster(judge): tnbreg dv1 c.serious##i.black commitment i.offense i.trial i.manmin c.history##i.black c.age##c.age i.male if dv1>0
tnbreg dv1 c.serious##i.black commitment i.offense i.trial i.manmin c.history##i.black c.age##c.age i.male if dv1>0, vce(cluster judge)
* Use Joseph Hilbe's GLME3_software: http://works.bepress.com/joseph_hilbe/60/
abich
margins, dydx(*) noesample vce(unconditional) post
coefplot,  horizontal xline(0) yscale(reverse) recast(scatter)  cismooth grid(none)  /// 
		order(1.trial 1.manmin serious commitment 2.offense 3.offense 4.offense  ///
		1.male history age 1.black)  ///
	    headings(1.trial="{bf:Case Characteristics}" 1.male="{bf:Offender Characteristics}")  ///
		coeflabel(1.trial="Trial" 1.manmin="Mandatory Minimum" history="Criminal History" serious="Offense Seriousness"  ///
		1.male="Male" 1.black="Black" commitment="Commitment Score" age="Age" 4.offense="Other Crime"  ///
		2.offense="Drug Crime" 3.offense="Property Crime", wrap(20))  ///
		xtitle("Average Marginal Effect: Predicted Sentence")
		
tnbreg dv1 c.serious##i.black commitment i.offense i.trial i.manmin c.history##i.black c.age##c.age i.male if dv1>0, vce(cluster judge)
margins, dydx(black) at(serious = (1 (1) 5)) noesample vce(unconditional) post
coefplot, horizontal xline(0) yscale(reverse) recast(scatter)  cismooth grid(none)  /// 
	    coeflabel(1._at="Misdemeanor" 2._at="F Felony" 3._at="E Felony" 4._at="D Felony" 5._at="A/B/C Felony", wrap(20)) ///
        eqrename(1.black = "Offense Seriousness") eqstrict ///
		xtitle("Discrete Change in Predicted Prison Term (in Months) for Blacks")
fitstat


*** RESULTS for Raw (Unmodified) Sentence Instead of Expected Minimum Sentence
tab sentence
su sentence, det
gen dv2 = ceil(sentence)

* Extreme Outliers Top-Coded at ~ 99.5% (720 months, 60 years)
replace dv2 = 720 if dv2>720
su dv2, det

label var dv2 "Raw Prison Term (in Months)"
* HRM Part 1: Binary (logit) Model Predicting 0s
bootstrap, reps(1000) cluster(judge): logit dv2 c.serious##i.black commitment i.offense i.trial i.manmin c.history##i.black c.age##c.age i.male
logit dv2 c.serious##i.black commitment i.offense i.trial i.manmin c.history##i.black c.age##c.age i.male, nolog vce(cluster judge)
abich
margins, dydx(*) noesample vce(unconditional) post
logit dv2 c.serious##i.black commitment i.offense i.trial i.manmin c.history##i.black c.age##c.age i.male, nolog vce(cluster judge)
margins, dydx(black) at(history = (1 (1) 5)) noesample vce(unconditional)
marginsplot
fitstat

* HRM Part 2: Zero-Truncated Negative Binomial (ZTBN) Model Predicting Positive Counts
bootstrap, reps(1000) cluster(judge): tnbreg dv2 c.serious##i.black commitment i.offense i.trial i.manmin c.history##i.black c.age##c.age i.male if dv2>0
tnbreg dv2 c.serious##i.black commitment i.offense i.trial i.manmin c.history##i.black c.age##c.age i.male if dv2>0, vce(cluster judge)
* Use Joseph Hilbe's GLME3_software: http://works.bepress.com/joseph_hilbe/60/
abich
margins, dydx(*) noesample vce(unconditional) post
coefplot,  horizontal xline(0) yscale(reverse) recast(scatter)  cismooth grid(none)  /// 
		order(1.trial 1.manmin serious commitment 2.offense 3.offense 4.offense  ///
		1.male history age 1.black)  ///
	    headings(1.trial="{bf:Case Characteristics}" 1.male="{bf:Offender Characteristics}")  ///
		coeflabel(1.trial="Trial" 1.manmin="Mandatory Minimum" history="Criminal History" serious="Offense Seriousness"  ///
		1.male="Male" 1.black="Black" commitment="Commitment Score" age="Age" 4.offense="Other Crime"  ///
		2.offense="Drug Crime" 3.offense="Property Crime", wrap(20))  ///
		xtitle("Average Marginal Effect: Predicted Sentence")
		
tnbreg dv2 c.serious##i.black commitment i.offense i.trial i.manmin c.history##i.black c.age##c.age i.male if dv2>0, vce(cluster judge)
margins, dydx(black) at(serious = (1 (1) 5)) noesample vce(unconditional)
marginsplot
fitstat


*** COMPARISON OF MODELS 
tab dv1, missing
su dv1, det

poisson dv1 c.serious##i.black commitment i.offense i.trial i.manmin c.history##i.black c.age##c.age i.male, vce(cluster judge)
fitstat
mgen, stub(prm) pr(0/12) meanpred
label var prmpreq "Predicted: PRM"
label var prmobeq "Observed"

nbreg dv1 c.serious##i.black commitment i.offense i.trial i.manmin c.history##i.black c.age##c.age i.male, vce(cluster judge)
fitstat
mgen, stub(nbrm) pr(0/12) meanpred
label var nbrmpreq "Predicted: NBRM"

logit dv1 c.serious##i.black commitment i.offense i.trial i.manmin c.history##i.black c.age##c.age i.male, vce(cluster judge)
est store Hlogit

tnbreg dv1 c.serious##i.black commitment i.offense i.trial i.manmin c.history##i.black c.age##c.age i.male if dv1 > 0, vce(cluster judge)
est store Hztnb

estimates restore Hlogit
predict Hprobgt0
label var Hprobgt "Pr(y>0)"
gen Hprob0 = 1 - Hprobgt0
label var Hprob0 "Pr(y=0)"
su Hprob0 Hprobgt0

estimates restore Hztnb
predict Hcrate, cm
label var Hcrate "Conditonal rate"
gen Hrate = Hcrate*Hprobgt0
label var Hrate "Unconditional rate"
su Hcrate Hrate
forvalues icount = 1/12 {
	predict Hcprob`icount', cpr(`icount')
	label var Hcprob`icount' "Pr(y=`icount'|y>0"
	gen Hprob`icount' = Hcprob`icount' * Hprobgt0
	label var Hprob`icount' "Prob(y=`icount')"
	}
su Hprob*

gen x = _n - 1
gen ztnbpreq = .

forvalues icount = 0/12 {
	egen ztnbmean`icount' = mean(Hprob`icount')
	replace ztnbpreq = ztnbmean`icount' if x==`icount'
	}
su ztnbmean*
tab ztnbpreq

* create deviations
gen obs = prmobeq
gen dprm = obs - prmpreq
label var dprm "PRM"
gen dnbrm = obs - nbrmpreq
label var dnbrm "NBRM"
gen dztnb = obs - ztnbpreq
label var dztnb "HRM-NB"

su obs dprm dnbrm dztnb

* plot deviations
graph twoway connected dprm dnbrm dztnb prmval, ///
ytitle(Observed - Predicted Counts) ylabel(-.20(.10).40)      ///
xlabel(0(1)12) msymbol(oh Sh O S) legend(col(3))
graph display, scheme(sj)


* Formal Likelihood Ratio Test
poisson dv1 c.serious##i.black commitment i.offense i.trial i.manmin c.history##i.black c.age##c.age i.male, vce(cluster judge)
scalar llp = e(ll)
ereturn list
nbreg dv1 c.serious##i.black commitment i.offense i.trial i.manmin c.history##i.black c.age##c.age i.male, vce(cluster judge)
scalar llnb = e(ll)
scalar lr = -2*(llp-llnb)
scalar pvalue = chiprob(1,lr)/2
* IMPORTANT  --  Enter lnalpha from the NB model
scalar lnalpha = 1.398832 
if (lnalpha < -20) scalar pvalue = 1
di as text "Likelihood-ratio test comparing PRM to NBRM: " as res %8.3f lr as text " Prob>=" as res %5.3f pvalue

* Vuong Tests
quietly poisson dv1 c.serious##i.black commitment i.offense i.trial i.manmin c.history##i.black c.age##c.age i.male, vce(cluster judge)
forvalues icount = 0/720 {
	predict PRM`icount',pr(`icount')
	}

quietly nbreg dv1 c.serious##i.black commitment i.offense i.trial i.manmin c.history##i.black c.age##c.age i.male, vce(cluster judge)
forvalues icount = 0/720 {
	predict NBRM`icount',pr(`icount')
	}

quietly logit dv1 c.serious##i.black commitment i.offense i.trial i.manmin c.history##i.black c.age##c.age i.male, vce(cluster judge)
est store Hlogit2
quietly tnbreg dv1 c.serious##i.black commitment i.offense i.trial i.manmin c.history##i.black c.age##c.age i.male if dv1 > 0, vce(cluster judge)
est store Hztnb2

est restore Hlogit2
predict HRMnot0
label var HRMnot0 "HRM prob of non-zero count"
gen HRM0 = 1 - HRMnot0
est restore Hztnb2
forvalues icount = 1/720 {
	predict HRM`icount', cpr(`icount')
	quietly replace HRM`icount' = HRM`icount'*HRMnot0
	label var HRM`icount' "HRM unconditional prob(`icount')"
	}

gen PRMprobs = .
label var PRMprobs "PRM prob of count that was observed"
gen NBRMprobs = .
label var NBRMprobs "NBRM prob of count that was observed"
gen HRMprobs = .
label var HRMprobs "HRM prob of count that was observed"

forvalues icount = 0/720 {
	quietly replace PRMprobs = PRM`icount' if dv1==`icount'
	quietly replace NBRMprobs = NBRM`icount' if dv1==`icount'
	quietly replace HRMprobs = HRM`icount' if dv1==`icount'
	}

su PRMprobs NBRMprobs HRMprobs

gen mPRM_HRM = ln(PRMprobs/HRMprobs)
gen mNBRM_HRM = ln(NBRMprobs/HRMprobs)
su mPRM_HRM 
display (r(mean)*sqrt(r(N)))/r(sd)
su mNBRM_HRM
display (r(mean)*sqrt(r(N)))/r(sd)


* Ln(DV1) OLS MODEL
gen dvlog = dv1 if dv1>0
tab dvlog
replace dvlog = 720 if dvlog >720 & dvlog~=.
replace dvlog = ln(dvlog) if dvlog~=.
tab dvlog
su dvlog, det
histogram dvlog, percent normal
kdensity dvlog, normal

bootstrap, reps(1000) cluster(judge): reg dvlog c.serious##i.black commitment i.offense i.trial i.manmin c.history##i.black c.age##c.age i.male if dv1>0
reg dvlog c.serious##i.black commitment i.offense i.trial i.manmin c.history##i.black c.age##c.age i.male if dv1>0, vce(cluster judge)
margins, dydx(*) noesample vce(unconditional) post
reg dvlog c.serious##i.black commitment i.offense i.trial i.manmin c.history##i.black c.age##c.age i.male if dv1>0, vce(cluster judge)
margins, at(black = (0 1) serious = (1 (1) 5)) noesample vce(unconditional)
reg dvlog c.serious##i.black commitment i.offense i.trial i.manmin c.history##i.black c.age##c.age i.male if dv1>0, vce(cluster judge)
margins, dydx(black) at(serious = (1 (1) 5)) noesample vce(unconditional)
marginsplot
fitstat

* Percent increase from baseline
reg dvlog c.serious##i.black commitment i.offense i.trial i.manmin c.history##i.black c.age##c.age i.male if dv1>0, vce(cluster judge) noconstant eform(GM/Ratio) 

gen YY = ln(expmin)
reg YY c.serious##i.black commitment i.offense i.trial i.manmin c.history##i.black c.age##c.age i.male if expmin<721, vce(cluster judge)
ovfplot
rvfplot, yline(0)
tnbreg expmin c.serious##i.black commitment i.offense i.trial i.manmin c.history##i.black c.age##c.age i.male if dv1 > 0 & expmin<721, vce(cluster judge)
ovfplot


* Demonstration of Hetereoskedasticity in both the naive OLS model and Log-Linear OLS Model
reg dv1 c.serious##i.black commitment i.offense i.trial i.manmin c.history##i.black c.age##c.age i.male
rvfplot, yline(0)
estat imtest
hettest
whitetst

reg dv1 c.serious##i.black commitment i.offense i.trial i.manmin c.history##i.black c.age##c.age i.male if dv1>0
rvfplot, yline(0)
estat imtest
hettest
whitetst

gen Y = ln(dv1)
reg Y c.serious##i.black commitment i.offense i.trial i.manmin c.history##i.black c.age##c.age i.male if dv1>0
rvfplot, yline(0)
estat imtest
hettest
whitetst


***  Sandbox  ***
* Use a Gamma GLM
glm dv1 c.serious##i.black commitment i.offense i.trial i.manmin c.history##i.black c.age##c.age i.male if dv1 > 0, family(gamma) link(log) vce(cluster judge)
margins, dydx(black) at(serious = (1 (1) 5)) noesample vce(unconditional)

* Generalized NB Model
gnbreg dv1 c.serious##i.black commitment i.offense i.trial i.manmin c.history##i.black c.age##c.age i.male if dv1 > 0, lnalpha(serious commitment i.offense i.trial i.manmin) vce(cluster judge)
abich
linktest

* Zero-Inflated Negative Binomial Model (ZINB)
bootstrap, reps(1000) cluster(judge): zinb dv1 c.serious##i.black c.commitment i.offense i.trial i.manmin c.history##i.black c.age##c.age i.male, inflate(c.serious##i.black c.commitment i.offense i.trial i.manmin c.history##i.black c.age##c.age i.male) vce(cluster judge)
listcoef, help
fitstat
margins, dydx(black) at(history = (1 (1) 5)) vce(unconditional) predict(pr)
marginsplot
margins, dydx(black) at(serious = (1 (1) 5)) vce(unconditional)
marginsplot

