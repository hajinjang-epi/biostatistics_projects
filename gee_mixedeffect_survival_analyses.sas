
/* GEE / Mixed-Effect Model / Survival Analysis */
/* Writer: Hajin Jang */
/* Date: 9/11/2024 */
/* Dataset: SWAN */



libname a "C:\Users\HAJ90\OneDrive - University of Pittsburgh\SAS\longitudinal_cluster";

data swan; set a.swan; run;


* time-dependent: visit, age, hotfcat, bmi, trig, ldl, hdl, gluc, ins, status, sbpavg ;
* time-independent: id, race;

proc genmod data=swan descending;
	class swanid visit hotfcat race (ref='5') / param=ref;
	model hotfcat = bmi age race / dist=bin link=logit type3;
	repeated subject= swanid / type=un modelse withinsubject= visit corrw;
run;

proc genmod data=swan descending;
	class swanid visit hotfcat race (ref='5') / param=ref;
	model hotfcat = bmi age race / dist=bin link=logit type3;
	repeated subject= swanid / type=AR(1) modelse withinsubject= visit corrw;
run;

proc genmod data=swan descending;
	class swanid visit hotfcat race (ref='5') / param=ref;
	model hotfcat = bmi age race / dist=bin link=logit type3;
	repeated subject= swanid / type=IND modelse withinsubject= visit corrw;
run;


proc genmod data=swan descending;
    class swanid visit hotfcat race (ref='5') / param=ref;
    model hotfcat = bmi age race / dist=bin link=logit type3;
    repeated subject=swanid / type=AR(1) modelse withinsubject=visit corrw;
    
    estimate 'Log odds Ratio for BMI' bmi 1 / exp;
    
    estimate 'Log odds Ratio for Race 1 vs 5' race 1 0 0 0 / exp;
    estimate 'Log odds Ratio for Race 2 vs 5' race 0 1 0 0 / exp;
    estimate 'Log odds Ratio for Race 3 vs 5' race 0 0 1 0 / exp;
    estimate 'Log odds Ratio for Race 4 vs 5' race 0 0 0 1 / exp;
run;


* Testing joint effects of BMI and age (generalized Wald test;

proc genmod data=swan descending;
    class swanid visit hotfcat race (ref='5') / param=ref;
    model hotfcat = bmi age race / dist=bin link=logit type3;
    repeated subject=swanid / type=AR(1) modelse withinsubject=visit corrw;
    
    estimate 'Log odds Ratio for BMI' bmi 1 / exp;
    
    estimate 'Log odds Ratio for Race 1 vs 5' race 1 0 0 0 / exp;
    estimate 'Log odds Ratio for Race 2 vs 5' race 0 1 0 0 / exp;
    estimate 'Log odds Ratio for Race 3 vs 5' race 0 0 1 0 / exp;
    estimate 'Log odds Ratio for Race 4 vs 5' race 0 0 0 1 / exp;

	contrast 'Score test bmi and age' bmi 1, age 1/wald;
run;


/******/


*model 1;
proc mixed data=swan covtest;
class swanid status visit race;
model sbpavg = status bmi ldl hdl race /solution;
random intercept /subject=swanid g;
repeated visit /subject=swanid r;
run;

*model 2;
proc mixed data=swan covtest;
class swanid status visit race;
model sbpavg = status bmi ldl hdl race /solution;
repeated visit /type=cs subject=swanid r;
run;

*model 3;
proc mixed data=swan covtest;
class swanid status visit race;
model sbpavg = status bmi ldl hdl race /solution;
random intercept /subject=swanid g;
repeated visit /type=ar(1) subject=swanid r;
run;


/******/
/******/
/******/
/******/


/* Survival Analysis */
/* Cox regression and lifetable cumulative survival analyses */

data stan1; set a.stan1; run;
data swandiab; set a.swandiab; run;

*single record approach;

proc phreg data=stan1;
model surv1*dead(0) = plant ageaccpt surg;
if wait=. or trans=0 or surv1<=wait then plant=0;
else if surv1>wait then plant=1;
run;

proc phreg data=stan1;
model surv1*dead(0) = plant ageaccpt surg;
if wait=. or trans=0 or surv1<=wait then plant=0;
else plant=1;
run;

proc phreg data=stan1;
model surv1*dead(0) = plant ageaccpt surg;
if trans=0 or surv1<=wait then plant=0;
if surv1>wait and trans=1 then plant=1;
run;

*counting process approach;

proc print data=stan1; where wait=surv1; run;

data stan2; set stan1; 
plant=0;
set=0;
if trans=0 then do;
dead_re=dead;
end=surv1;
if end=0 then end=0.1;
output;
end;

else do;
end=wait;
if end=0 then end=0.1;
dead_re=0;
output;
plant=1;
set=wait;
if end=0.1 then set=0.1;
end=surv1;
dead_re=dead;
output;
end;
run;

proc phreg data=stan2;
model (set,end)*dead_re(0) = plant ageaccpt surg;
run;


PROC PRINT DATA=stan1; *no participant dead at the same day of transplant;
WHERE WAIT=SURV1;
RUN;
*Expected number of observations=34+69*2=172;

DATA stanlong;
SET hw8.stan;
PLANT=0;
START=0;
IF TRANS=0 THEN DO;
DEAD2=DEAD;
STOP=SURV1;
IF STOP=0 THEN STOP=0.1;*To avoid cases where start=stop;
OUTPUT;
END;
ELSE DO;
STOP=WAIT;
IF STOP=0 THEN STOP=0.1;*To avoid cases where start=stop;
DEAD2=0;
OUTPUT;
PLANT=1;
START=WAIT;
IF STOP=0.1 THEN START=0.1;*To avoid cases where start=stop;
STOP=SURV1;
DEAD2=DEAD;
OUTPUT;
END;
RUN;*172 as planned;


PROC PHREG DATA=stanlong;
MODEL (START, STOP)*DEAD2(0)= plant surg ageaccpt;
run;



*2b;
proc lifereg data=stan1;
model surv1*dead(0) = surg / distribution=exponential;
run;



*2c;
proc lifereg data=stan1;
model surv1*dead(0) = surg / distribution=weibull;
run;


*3b;
data swandiab2; set swandiab;
do visit=0 to diabvisit;
if visit=diabvisit and diabetes=1 then diab=1;
else diab=0;

if visit=0 then d0=1;
else d0=0;

if visit=1 then d1=1;
else d1=0;
if visit=2 then d2=1;
else d2=0;
if visit=3 then d3=1;
else d3=0;
if visit=4 then d4=1;
else d4=0;
if visit=5 then d5=1;
else d5=0;
if visit=6 then d6=1;
else d6=0;

output;
end;
run;

data swandiab2; set swandiab;
do visit=0 to diabvisit;
if visit=diabvisit and diabetes=1 then diabetes_re=1;
else diabetes_re=0;

if visit=0 then diab0=1; else diab0=0;
if visit=1 then diab1=1; else diab1=0;
if visit=2 then diab2=1; else diab2=0;
if visit=3 then diab3=1; else diab3=0;
if visit=4 then diab4=1; else diab4=0;
if visit=5 then diab5=1; else diab5=0;
if visit=6 then diab6=1; else diab6=0;

output;
end;
run;



*logit model;
proc logistic data=swandiab2 descending;
model diab = d0 d1 d2 d3 d4 d5 d6 white / noint;
run;


*clog-log model;
proc logistic data=swandiab2 descending;
model diab = d0 d1 d2 d3 d4 d5 d6 white / noint link=cloglog;
white: test white;
run;


*logit model;
proc logistic data=swandiab2 descending;
model diabetes_re = white diab0 diab1 diab2 diab3 diab4 diab5 diab6 / noint;
run;


*clog-log model;
proc logistic data=swandiab2 descending;
model diabetes_re = white diab0 diab1 diab2 diab3 diab4 diab5 diab6 / noint link=cloglog;
white: test white;
run;


/*******/
/*******/
/*******/


data bmt; set a.bmt; run;

proc contents data=bmt; run;

proc freq data=bmt; table status; run;

data bmt2;
  set hw9.bmt;
  logT=log(T);
run;


data relapse;
	set hw9.bmt;
	event=(status=1);
	type=1;
data death;
	set hw9.bmt;
	event=(status=2);
	type=2;
data combine;
	set relapse death;
proc lifetest data=combine plots=(LLS H);
time logT*event(0);
strata type;
run;



data relapse;
	set hw9.bmt;
	event=(status=1);
	type=1;
data death;
	set hw9.bmt;
	event=(status=2);
	type=2;
data combine;
	set relapse death;
proc lifetest data=combine plots=(LLS H);
time T*event(0);
strata type;
run;


*d;
%CIF(DATA=BMT, OUT=BMTn, TIME=logT, STATUS=STATUS, EVENT=1, CENSORED=0, GROUP=group);

%CIF(DATA=BMT, OUT=BMTn, TIME=logT, STATUS=STATUS, EVENT=2, CENSORED=0, GROUP=group);

%CIF(DATA=BMT, OUT=BMTn, TIME=T, STATUS=STATUS, EVENT=1, CENSORED=0, GROUP=group);

%CIF(DATA=BMT, OUT=BMTn, TIME=T, STATUS=STATUS, EVENT=2, CENSORED=0, GROUP=group);


*cumulative incidence for relapse;
PROC LIFETEST DATA=hw9.bmt 
PLOTS= cif(TEST);
TIME logT*status( 0 ) /eventcode=1;
STRATA group;
RUN;

*cumulative incidence for death;
PROC LIFETEST DATA=hw9.bmt 
PLOTS= cif(TEST);
TIME logT*status( 0 ) /eventcode=2;
STRATA group;
RUN;


