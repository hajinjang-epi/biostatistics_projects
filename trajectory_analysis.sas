
/* Trajectory Analysis */
/* Writer: Hajin Jang */
/* Date: 10/18/2024 */
/* Data: SWAN */


libname a 'C:\Users\HAJ90\OneDrive - University of Pittsburgh\SAS\trajectory';

data gbtm; set a.gbtm; run;
data media; set a.media; run;



/* final model code */

proc means data=gbtm mean min max; var bmi0-bmi6; run;
*max = 61.0136617;
*min = 14.3347866;

proc traj data=gbtm out=out_gbtm outstat=out_gbtm_stat outplot=out_gbtm_plot outest=out_gbtm_est ci95m;
title "BMI trajectory (1 2 1)";
var bmi0-bmi6;
indep t0-t6;
model cnorm;
id randomid;
min 14.3347866;
max 61.0136617;
ngroups 3;
order 1 2 1;
run;

%trajplotnew(out_gbtm_plot , out_gbtm_stat ,"BMI Trajectory order of 1 2 1");



/* GBTM trajectory group selection */

%macro gbtm(ngroups, order);
proc traj data=gbtm out=out_gbtm outstat=out_gbtm_stat outplot=out_gbtm_plot outest=out_gbtm_est ci95m;
title "BMI trajectory (1 2 1)";
var bmi0-bmi6;
indep t0-t6;
model cnorm;
id randomid;
min 14.3347866;
max 61.0136617;
ngroups &ngroups.;
order &order.;
run;
%trajplotnew(out_gbtm_plot , out_gbtm_stat ,"BMI Trajectory order of &order. ");
%mend gbtm;

%gbtm(1,3);
%gbtm(2,3 3);
%gbtm(3,3 3 3);
%gbtm(4,3 3 3 3);

%gbtm(3,4 4 4);
%gbtm(3,3 3 3);
%gbtm(3,2 2 2);

%gbtm(3,1 1 1);
proc sort data=out_gbtm; by group; run;
proc means data=out_gbtm; by group; var grp1prb grp2prb grp3prb; run;

%gbtm(3,1 2 1);
proc sort data=out_gbtm; by group; run;
proc means data=out_gbtm; by group; var grp1prb grp2prb grp3prb; run;


/* race/ethnicity and age at baseline predict group membership */

data gbtm; set gbtm;
race1=.; 
race2=.; 
race3=.; 
race4=.;
if ethnic="BLACK" then race1=1; else race1=0;
if ethnic="CAUCA" then race2=1; else race2=0;
if ethnic="CHINE" then race3=1; else race3=0;
if ethnic="HISPA" then race4=1; else race4=0;
run;

proc traj data=gbtm out=out_gbtm outstat=out_gbtm_stat outplot=out_gbtm_plot outest=out_gbtm_est ci95m;
title "BMI trajectory (1 2 1)";
var bmi0-bmi6;
indep t0-t6;
model cnorm;
id randomid;
min 14.3347866;
max 61.0136617;
ngroups 3;
order 1 2 1;
risk race1 race2 race3 race4 agecont0 ;
run;

%trajplotnew(out_gbtm_plot , out_gbtm_stat ,"BMI Trajectory order of 1-2-1");




/* part 2 */

proc causalmed data=media decomp;
class obese0 crp0 ethnic degreexs stat_xs / descending; 
model imt = obese0 | crp0;
mediator crp0 = obese0;
covar ethnic degreexs stat_xs;
run;

/* Q2, 1) List the main 4 steps to fit weighted MSM and include your SAS code. */

*1. weight for the exposure;

proc logistic data=media descending;
model obese0=;
output out=media predicted=pna0;
run;

proc logistic data=media descending;
class ethnic degreexs stat_xs;
model obese0=ethnic degreexs stat_xs;
output out=media predicted=pda0;
run;

proc logistic data=media descending;
class obese0 crp0;
model obese1=obese0 crp0;
output out=media predicted=pna1;
run;

proc logistic data=media descending;
class obese0 crp0 ethnic degreexs stat_xs;
model obese1=obese0 crp0 glucres0 ethnic degreexs stat_xs;
output out=media predicted=pda1;
run;

*2. weight for the mediator;

proc logistic data=media descending;
class obese0;
model crp0=obese0;
output out=media predicted=pnm0;
run;

proc logistic data=media descending;
class obese0 ethnic degreexs stat_xs;
model crp0=obese0 ethnic degreexs stat_xs;
output out=media predicted=pdm0;
run;

proc logistic data=media descending;
class obese0 obese1 crp0;
model crp1=obese0 obese1 crp0;
output out=media predicted=pnm1;
run;

proc logistic data=media descending;
class obese0 obese1 crp0 ethnic degreexs stat_xs;
model crp1=obese0 obese1 crp0 glucres0 ethnic degreexs stat_xs;
output out=media predicted=pdm1;
run;

*3. Overall weight for the each subject;

data media;set media;
if obese0=1 then wta0=pna0/pda0; else wta0=(1-pna0)/(1-pda0);
if obese1=1 then wta1=pna1/pda1; else wta1=(1-pna1)/(1-pda1);
if crp0=1 then wtm0=pnm0/pdm0; else wtm0=(1-pnm0)/(1-pdm0);
if crp1=1 then wtm1=pnm1/pdm1; else wtm1=(1-pnm1)/(1-pdm1);
wwt=wta0*wta1*wtm0*wtm1;
run;

*4: Weighted marginal structural model (MSM);

proc genmod data=media;
class randomid obese0(ref='0') obese1(ref='0') crp0(ref='0') crp1(ref='0');
model imt=obese0 obese1 crp0 crp1/error=normal link=identity;
weight wwt;
repeated subject=randomid/type=unstr;
run;


