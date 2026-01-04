
/* Weight change and reproductive health - Type 2 diabetes */
/* Writer: Hajin Jang */
/* Date: 1/21/2024 */


* Import data;
OPTIONS MPRINT MLOGIC;

%MACRO MYIMPORT(OUTFILE=, INFILE=, WKST=);
	PROC IMPORT OUT= &OUTFILE
				DATAFILE="C:\Users\HajinJang\OneDrive - University of Pittsburgh\PITT\Document\obesity_project\&INFILE"
				DBMS=EXCEL REPLACE;
		SHEET="'&WKST'";
		GETNAMES=YES;
		MIXED=NO;
		SCANTEXT=YES;
		USEDATE=YES;
		SCANTIME=YES;
	RUN;

	PROC SORT DATA= &OUTFILE;
		BY ID;
	RUN;

	PROC CONTENTS DATA=&OUTFILE VARNUM;
	RUN;
%MEND MYIMPORT;

%MYIMPORT(OUTFILE=PROJECT1, INFILE=PROJECT1.XLS, WKST=SHEET1);

%MYIMPORT(OUTFILE=PROJECT2, INFILE=PROJECT2.XLS, WKST=SHEET1);

%MYIMPORT(OUTFILE=PROJECT3_BP, INFILE=PROJECT3.XLS, WKST=BP);
%MYIMPORT(OUTFILE=PROJECT3_BIO, INFILE=PROJECT3.XLS, WKST=BIO);
%MYIMPORT(OUTFILE=PROJECT3_REPRO, INFILE=PROJECT3.XLS, WKST=REPRO);



* Combine datasets;
DATA PROJECT2;
	SET PROJECT2;

	SEX=GENDER;
	SMOKE=SMOKING;

	DROP GENDER SMOKING;
RUN;

DATA PROJECT12;
	SET PROJECT1 PROJECT2;
RUN;

DATA PROJECT3;
	MERGE PROJECT3_BP PROJECT3_BIO PROJECT3_REPRO;
	BY ID;
RUN;

DATA PROJECT;
	MERGE PROJECT12 PROJECT3;
	BY ID;

	IF SEX="F";

	IF FIRST.ID AND LAST.ID THEN DUP=0;
	ELSE DUP=1;

RUN;

PROC PRINT DATA=PROJECT;
	WHERE DUP=1;
RUN;



* Duplicates;
DATA PROJECT;
	SET PROJECT;

	IF ID=54 AND SMOKE="Not available" THEN DELETE;
	IF ID=88 AND SBP1=999 THEN DELETE;
	IF ID=95 AND WAIST=999 THEN DELETE;
	IF ID=362 AND SMOKE="NOT AVAILABLE" THEN DELETE;
	IF ID=402 AND SMOKE="Not available" THEN DELETE;
RUN;

PROC PRINT DATA=PROJECT;
	WHERE DUP=1;
RUN;



* Missingness;
PROC CONTENTS DATA=PROJECT VARNUM; RUN;

PROC FREQ DATA=PROJECT;
	TABLE BIRTHDAY BIRTHMONTH BIRTHYEAR VISITDAY VISITMONTH VISITYEAR SEX SMOKE BPMED FGLUC2 MNP PAROUS CHILDREN;
RUN;

PROC UNIVARIATE DATA=PROJECT;
	VAR WAIST HIP WEIGHT20 HEIGHT WEIGHT SBP1 SBP2 SBP3 DBP1 DBP2 DBP3 FGLUC1 HDL CHOL;
RUN;

DATA PROJECT;
	SET PROJECT;

	ARRAY A{2} BPMED CHILDREN;
	ARRAY B{10} WAIST HIP SBP1 SBP2 SBP3 DBP1 DBP2 DBP3 HDL CHOL;

	DO I=1 TO 2;
		IF A{I}=99 THEN A{I}=.;
	END;

	DO I=1 TO 10;
		IF B{I}>=888 OR B{I}<0 THEN B{I}=.;
	END;
RUN;

DATA PROJECT;
	SET PROJECT;

	ARRAY C{2} SMOKE FGLUC2;

	DO I=1 TO 2;
		C{I} = UPCASE(C{I});
		IF C{I}="NOT AVAILABLE" OR C{I}="UNAVAILABLE" THEN C{I}=" ";
	END;

	DROP I;
RUN;



* Preprocessing;
DATA PROJECT;
	SET PROJECT;

	BIRTHDT=MDY(BIRTHMONTH, BIRTHDAY, BIRTHYEAR);
	VISITDT=MDY(VISITMONTH, VISITDAY, VISITYEAR);
	AGE=(VISITDT - BIRTHDT)/365.25;

	IF WAIST NE . AND HIP NE . THEN DO;
	WHR= WAIST/HIP;
	END;

	IF WEIGHT NE . AND HEIGHT NE . THEN DO;
	BMI=WEIGHT/((HEIGHT/100)**2);
	END;

	IF WEIGHT NE . AND WEIGHT20 NE . THEN DO;
	WTPCT=((WEIGHT-WEIGHT20)/WEIGHT20)*100;
	END;

	IF CHOL NE . AND HDL NE . THEN DO;
	TCHDL=CHOL/HDL;
	END;

	SBP = ROUND(MEAN(SBP1, SBP2, SBP3));

	DBP = ROUND(MEAN(DBP1, DBP2, DBP3));

	IF SBP>=140 OR DBP>=90 OR BPMED=1 THEN HTN=1;
	ELSE IF .Z<SBP<140 AND .Z<DBP<90 AND BPMED=0 THEN HTN=0;

	IF PAROUS=0 THEN NKIDS=1;
	ELSE IF PAROUS=1 AND CHILDREN IN (1,2) THEN NKIDS=2;
	ELSE IF PAROUS=1 AND CHILDREN>=3 THEN NKIDS=3;

	NEWFGLUC2 = INPUT(FGLUC2, 8.);

	MEANFG=MEAN(FGLUC1, NEWFGLUC2);

	IF FGLUC1<126 OR (FGLUC1>=126 AND NEWFGLUC2<126) THEN DIABETES=0;
	ELSE IF FGLUC1>=126 AND NEWFGLUC2>=126 THEN DIABETES=1;

	IF .Z<BMI<25 THEN BMICAT=1;
	ELSE IF 25<=BMI<30 THEN BMICAT=2;
	ELSE IF BMI>=30 THEN BMICAT=3;
RUN;

PROC UNIVARIATE DATA=PROJECT;
	VAR AGE WHR BMI WTPCT TCHDL SBP DBP NEWFGLUC2 MEANFG;
RUN;

PROC FREQ DATA=PROJECT;
	TABLE HTN NKIDS DIABETES BMICAT;
RUN;

PROC MEANS DATA=PROJECT MIN MAX;
	CLASS BMICAT;
	VAR BMI;
RUN;



* Labels;
DATA PROJECT;
	SET PROJECT;
	LABEL	MNP = "Menopausal status"
			Parous = "Having given birth"
			Children = "Number of children"
			Nkids = "Number of children (categories)"
			WHR = "Waist to hip ratio"
			Weight20 = "Weight (kg) at age 20"
			Weight = "Current weight (kg)"
			WTPCT = "Percent weight change from age 20"
			Height = "Height (cm)"
			BMIcat = "BMI category"
			SMOKE = "Smoking status"
			MeanFG = "Mean fasting plasma glucose"
			TCHDL = "Total cholesterol to HDL ratio";
RUN;



* Format;
PROC FORMAT;
	VALUE	FYN	1="Yes"
				0="No";
	VALUE	FNKIDS	1="No children"
					2="One to two children"
					3="Three or more children";
	VALUE	FBMICAT	1="Underweight or normal weight"
					2="Overweight"
					3="Obese";
RUN;

DATA PROJECT;
	SET PROJECT;
	FORMAT BPMED FYN. MNP FYN. PAROUS FYN. DIABETES FYN. NKIDS FNKIDS. BMICAT FBMICAT.;
RUN;



* EDA;
PROC UNIVARIATE DATA=PROJECT NORMAL PLOT;
	VAR AGE WTPCT WHR BMI SBP DBP TCHDL MEANFG ;
	CLASS DIABETES;
RUN;


PROC FREQ DATA=PROJECT;
	TABLE (BMICAT HTN SMOKE PAROUS NKIDS MNP)*DIABETES /EXPECTED CHISQ EXACT;
RUN;


PROC TTEST DATA=PROJECT;
	CLASS DIABETES;
	VAR DBP;
RUN;

PROC MEANS DATA=PROJECT N MEDIAN P25 P75;
	CLASS DIABETES;
	VAR AGE WTPCT WHR BMI SBP TCHDL MEANFG;
RUN;

PROC NPAR1WAY DATA=PROJECT WILCOXON;
	CLASS DIABETES;
	VAR AGE WTPCT WHR BMI SBP TCHDL MEANFG;
RUN;



* Correlation: mean fasting glucose, age, body composition, bp, lipid concentrations;
PROC UNIVARIATE DATA=PROJECT NORMAL PLOT;
	VAR AGE WEIGHT WEIGHT20 WTPCT WHR BMI HDL CHOL TCHDL SBP DBP;
RUN;

PROC CORR DATA=PROJECT PEARSON SPEARMAN;	
	VAR AGE WEIGHT WEIGHT20 WTPCT WHR BMI HDL CHOL TCHDL SBP DBP;
	WITH MEANFG;
RUN;

PROC CORR DATA=PROJECT PEARSON SPEARMAN;	
	PARTIAL AGE;
	VAR WEIGHT WEIGHT20 WTPCT WHR BMI HDL CHOL TCHDL SBP DBP;
	WITH MEANFG;
RUN;



* Regression: mean difference in % weight change by BMI categories & by the number of children;
%MACRO GLM(CAT=, CON=);
PROC GLM DATA=PROJECT;
	CLASS &CAT;
	MODEL WTPCT = &CAT &CON / SOLUTION;
	MEANS &CAT;
	LSMEANS &CAT / STDERR PDIFF ADJUST=BON;
RUN;
%MEND GLM;

%GLM(CAT=BMICAT, CON=);
%GLM(CAT=BMICAT, CON=AGE);
%GLM(CAT=BMICAT MNP, CON=AGE);

%GLM(CAT=NKIDS, CON=);
%GLM(CAT=NKIDS, CON=AGE);
%GLM(CAT=NKIDS MNP, CON=AGE);



*Regression: correlation of mean fasting plasma glucose concentrations;
DATA PROJECT;
	SET PROJECT;

	ARRAY DBMI{2};
	ARRAY DKID{2};

	DO I=1 TO 2;
		IF BMICAT=I+1 THEN DBMI{I}=1;
			ELSE IF BMICAT~=. THEN DBMI{I}=0;

		IF NKIDS=I+1 THEN DKID{I}=1;
			ELSE IF NKIDS~=. THEN DKID{I}=0;
	END;
RUN;

PROC FREQ DATA=PROJECT;
	TABLES BMICAT*DBMI1*DBMI2 / LIST;
RUN;

PROC FREQ DATA=PROJECT;
	TABLES NKIDS*DKID1*DKID2 / LIST;
RUN;


%MACRO LINREG(VAR=);
PROC REG DATA=PROJECT;
	MODEL MEANFG = &VAR;
RUN;
%MEND LINREG;

%LINREG(VAR=BMI);
%LINREG(VAR=DBMI1 DBMI2);
%LINREG(VAR=WTPCT);
%LINREG(VAR=MNP);
%LINREG(VAR=DKID1 DKID2);


%LINREG(VAR=DKID1 DKID2 BMI WTPCT);

/* Interpretation:

When adjusted for continuous BMI and percent weight gain, the beta coefficient of having 1-2 children in fasting plasma glucose changed from -8.46 (7.12) to -13.96 (5.77).
The association became statistically significant with the p-value changing from 0.2362 to 0.0162.

On the other hand, when adjusted for continuous BMI and percent weight gain, the beta coefficient of having >=3 children in fasting plasma glucose changed in effect size and direction as well, from 29.75 (7.87) to -6.62 (7.10).
The statistical significance disappeared with the p-value changing from 0.0002 to 0.3523.
*/



* Risk factors associated with t2d status;
%MACRO LOG(VAR=);
PROC LOGISTIC DATA=PROJECT;
	MODEL DIABETES (event="Yes") = &VAR;
RUN;
%MEND LOG;

%LOG(VAR=AGE);
%LOG(VAR=WTPCT);
%LOG(VAR=BMI);
%LOG(VAR=TCHDL);
%LOG(VAR=HTN);
%LOG(VAR=MNP);
%LOG(VAR=PAROUS);


PROC LOGISTIC DATA=PROJECT;
	CLASS NKIDS (ref="No children") / PARAM=REF;
	MODEL DIABETES (event="Yes") = NKIDS;
RUN;

PROC LOGISTIC DATA=PROJECT;
	CLASS SMOKE (ref="NEVER") / PARAM=REF;
	MODEL DIABETES (event="Yes") = SMOKE;
RUN;


PROC LOGISTIC DATA=PROJECT;
	CLASS MNP (ref="No") SMOKE (ref="NEVER") NKIDS (ref="No children") / PARAM=REF;
	MODEL DIABETES (event="Yes") = WTPCT AGE BMI MNP SMOKE NKIDS;
RUN;


/* Interpretation:

For the main independent variable percent weight change, the odds of diabetes prevalence were statistically significantly associated with percent weight change, 
adjusting for age, BMI, menopause, smoking status, and the categorical variable for number of chilren (Type 3 analysis of effects p-value<0.0001).
Thd odds of diabetes prevalence are 1.2 times higher by 1 unit increase in percent weight change (OR=1.200, 95% CI=1.119-1.286).
*/


