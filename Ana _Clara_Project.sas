/*Project*/

PROC OPTIONS OPTION = MACRO;
RUN;

/*SETTING PERMANENT LIBRARY*/
LIBNAME PERM "D:\1_Metro College\Courses\Advanced SAS\Project\Data";

/*FEATURES.:

/*1.Acctno: account number.*/
/*2.Actdt: account activation date*/
/*3.Deactdt: account deactivation date*/
/*4.DeactReason: reason for deactivation.*/
/*5.GoodCredit: customer’s credit is good or not.*/
/*6.RatePlan: rate plan for the customer.*/
/*7.DealerType: dealer type.*/
/*8.Age: customer age.*/
/*9.Province: province.*/
/*10.Sales: the amount of sales to a customer.*/

/*FORMATS*/
PROC FORMAT;
	VALUE G_CREDIT_F
		0 = "N"
		1 = "Y"
	OTHER = "Missing"
		;

	VALUE SALES_F
		  0 -< 100 = "< $100"
		100 -< 500 = "$100 - $500"
		500 -< 800 = "$500 - $800"
		800 - HIGH = "> $800"
			. = "Missing"
		;

	VALUE AGE_F
		LOW - 20 = " <20 years"
		 21 - 40 = "21 - 40 years"
		 41 - 60 = "41 - 60 years"
		 60 - HIGH = "> 60 years"
		 	OTHER  = "Missing"
		 ;

	VALUE TENURE_F
		LOW - 30  = "30 days"
		 31 - 60  = "31 - 60 days"
		 61 - 365 = "61 - 365 days"
		366 - HIGH = "> 1 year" 
			OTHER  = "Missing"
		 ;		
RUN;
		
/*Data Input*/
DATA PERM.CRM;
INFILE "D:\1_Metro College\Courses\Advanced SAS\Project\New_Wireless_Fixed.txt" DSD DLM = " "  ;
INPUT @1 Acctno: $14. @15 Actdt:mmddyy10. @26 Deactdt: mmddyy10. @41 DeactReason: $4. @53 GoodCredit: 1. @62 RatePlan: 1. 
@65 DealerType: $2. @74 Age  @80 Province: $2. @85 Sales dollar6.2;*$8.;
FORMAT Actdt mmddyy10. Deactdt: mmddyy10. Sales dollar8.2 GoodCredit $g_credit_f.;
LABEL 
Acctno = A/c Number
Actdt = A/c Activation Date
Deactdt = A/c Deactivation Date
DeactReason = Deactivation Reason
GoodCredit = Good Credit?
RatePlan = Rate plan
DealerType = Dealer Type
Age = Age
Province = Province
Sales = Sales Amount;

/*GENERAL DESCRIPTION OF FILE AND DATASET*/

TITLE1 "Final Project SAS - CRM";
TITLE2 "METRO COLLEGE OF TECHNOLOGY - DATA SCIENCE AND APPLICATION";
TITLE3 "ANA CLARA TUPINAMBÁ FREITAS, mentored by Professor HAMID RAJAEE";
TITLE4 "Presented at 22nd of July, 2021"; 
DATA TITLE;
TITLE5 = "Today’s marketplace demands that businesses reduce customer turnover. 
This project analyses 2 years' worth of customers data of a telecommunications company with the goal of getting insights into customer’s behaviours and identify which features are key to design the best marketing strategy.
";
proc report data=title nowindows noheader nocenter
  style(column) = { background = NONE }; RUN;
TITLE1;
TITLE2;
TITLE3;
TITLE4;

PROC CONTENTS DATA=PERM.CRM; RUN;

TITLE1;
TITLE2;
TITLE3;
TITLE4;

TITLE " ";
TITLE2 "FIRST VIEW OF DATA SET";
TITLE3 " ";
TITLE4 "FIRST AND LAST OBSERVATIONS";
PROC PRINT DATA = PERM.CRM (OBS = 5);RUN;
TITLE1;
TITLE2;
TITLE3;
TITLE4;
PROC PRINT DATA = PERM.CRM (OBS = 102255 FIRSTOBS = 102251);RUN;


/*INCLUDING SEGMENTATION*/
/*Tenure will be considered with end date of:2Jan2001*/
PROC SQL;
	CREATE TABLE ACT_VAR AS
	SELECT 
		ACCTNO,
		ACTDT,
		DEACTDT,
		DEACTREASON,
		COALESCE(DEACTDT,MDY(1,21,2001)) AS TENURE_AUX,
		GOODCREDIT,
		RATEPLAN,
		DEALERTYPE,
		AGE,
		PROVINCE,
		SALES,
	CASE WHEN MISSING(DEACTDT) 
		THEN "Y"
		ELSE "N"
	END AS ACTIVE
	FROM PERM.CRM;
QUIT;

/*PROC PRINT DATA = ACT_VAR (OBS=10) ;RUN;*/
DATA SEG;
SET ACT_VAR;
SALES_SEG = SALES;
AGE_SEG = AGE;
TENURE = INTCK("DAY",ACTDT,TENURE_AUX);
TENURE_SEG = TENURE;
FORMAT SALES_SEG $SALES_F. AGE_SEG $AGE_F. TENURE_AUX mmddyy10. TENURE_SEG $TENURE_F.;
LABEL
TENURE = Tenure(Days);

TITLE1 "DATA AFTER INCLUDING SEGMENTS*";

PROC CONTENTS DATA=SEG; RUN;

/*GENERAL DESCRIPTION OF FILE AND DATASET*/
PROC PRINT DATA = SEG (OBS = 5);RUN;
TITLE1;
PROC PRINT DATA = SEG (OBS = 102255 FIRSTOBS = 102251);
FOOTNOTE "*FIRST AND LAST OBSERVATIONS";RUN;
FOOTNOTE;
RUN;

/*EXPLORATORY DATA ANALYSIS*/
*---------------------------------------------------------------------------------------------------------------;
/*Account Number*/

TITLE1 "EXPLORATORY DATA ANALYSIS(EDA)";

PROC SQL;
TITLE2  "Account Number";
TITLE3 "DUPLICATES?";
	SELECT 
		COUNT(Acctno) AS "TOTAL # ACCOUNTS"n,
		COUNT(DISTINCT Acctno) AS "TOTAL # DISTINCT ACCOUNTS"n,
		CASE 
			WHEN COUNT(Acctno) = COUNT(DISTINCT Acctno) 
			THEN "There's no duplicates in data set."
			ELSE "There are duplicates in data set."
		END AS "DUPLICATES?"n
	FROM SEG;
QUIT;
TITLE1;
TITLE2;
TITLE3;

*----------------------------------------------------------------------------------;
/*MACROS*/

/*Dates*/
%MACRO DATES(DATA,VAR);
TITLE1 " ANALYSIS OF &VAR";
/*MINIMUM AND MAXIMUM DATES*/
TITLE2 "MINIMUM AND MAXIMUM &VAR DATES";

/*# OBS, MIN, AND MAX*/
PROC SUMMARY DATA = &DATA;
VAR &VAR;
OUTPUT OUT = DATE_ACT N = N_OBS MIN=MIN MAX=MAX;

PROC PRINT;
VAR MIN MAX; RUN;

PROC FREQ DATA = &DATA;
FOOTNOTE "*10 FIRST OBSERVATIONS";
TABLES &VAR/ OUT = FREQ_DATE NOPRINT; RUN;

TITLE2;
FOOTNOTE;

PROC SQL;
CREATE TABLE FREQ_DATE_&VAR AS
	SELECT 
		&VAR, 
		COUNT
	FROM
		FREQ_DATE
	WHERE NOT MISSING(&VAR);
QUIT;
PROC SQL NOPRINT;
	SELECT 	MAX(COUNT) INTO: MAX1
	FROM FREQ_DATE
	WHERE NOT MISSING(&VAR);
QUIT; 

TITLE "FREQUENCY OF &VAR";
FOOTNOTE "FIRST 10 OBSERVATIONS";
PROC PRINT DATA = FREQ_DATE_&VAR (OBS=10);
RUN;

FOOTNOTE;

PROC SGPLOT DATA = FREQ_DATE_&VAR;
TITLE "FREQUENCY OF &VAR PER DATE";
SERIES X= &VAR Y= COUNT;
/*SERIES X= Actdt Y= COUNT / GROUP = DEACTIVATE ;*/*LOOK FOR BIVARIATE;
REFLINE &MAX1 / AXIS=Y 
LINEATTRS=(THICKNESS=2 COLOR=RED PATTERN=DASH)
LABEL=("MAX");
YAXIS GRID OFFSETMAX=0.05;
RUN;

TITLE "DAY WITH MOST #";
PROC SQL;
SELECT &VAR,COUNT
FROM FREQ_DATE_&VAR
HAVING MAX(COUNT)=COUNT;
QUIT;
TITLE;

%MEND DATES;

%MACRO NUMERICAL(DATA,VAR);
TITLE "THIS IS UNIVARIATE ANALYSIS FOR &VAR IN $DATA";
PROC MEANS DATA=&DATA N NMISS MEAN MEDIAN MODE MIN MAX STDDEV VAR RANGE QRANGE MAXDEC=2;
VAR &var;

 TITLE "THIS IS HISTOGRAM FOR &VAR";
 PROC SGPLOT DATA=&DATA;
  HISTOGRAM &VAR / fillattrs=(color = Plum)dataskin=pressed;
  DENSITY &VAR/ lineattrs= (color = black);
  DENSITY &VAR/type=kernel lineattrs= (color= darkBlue) ;
    STYLEATTRS 
    BACKCOLOR=snow
    WALLCOLOR= WhiteSmoke;
  KEYLEGEND / LOCATION=inside POSITION=topright;
 RUN;
 QUIT;
 TITLE "THIS IS HORIZONTAL BOXPLOT FOR &VAR";
 PROC SGPLOT DATA=&DATA;
  HBOX &VAR / fillattrs=(color = Plum) dataskin=matte MEANATTRS=(symbol=DiamondFilled color=Turquoise);
    STYLEATTRS 
    BACKCOLOR=snow
    WALLCOLOR=WhiteSmoke
	AXISEXTENT=DATA;
 RUN;
TITLE;
RUN;
%MEND NUMERICAL ;

%MACRO CATEGORICAL(DATA,VAR);
 TITLE "UNIVARIATE ANALYSIS OF &VAR FOR &DATA";
  PROC FREQ DATA=&DATA NLEVELS;
  TABLE &VAR/MISSING;
 RUN;

TITLE "BARCHART OF &VAR FOR &DATA";
PROC SGPLOT DATA = &DATA;
 VBAR &VAR / FILLATTRS=(COLOR = Plum)DATASKIN=pressed;
 	STYLEATTRS 
	BACKCOLOR=snow 
    WALLCOLOR=WhiteSmoke
	AXISEXTENT= DATA 
     ;
 RUN;

TITLE "PIECHART OF &VAR FOR &DATA";
PROC GCHART DATA=&DATA;
  PIE3D &VAR/discrete 
             value=INSIDE /*ARROW, OUTSIDE*/
             percent=OUTSIDE
             EXPLODE=ALL
			 SLICE=OUTSIDE
			 RADIUS=22		
;

RUN;
%MEND CATEGORICAL;

/*BIVARIATE ANALYSIS*/

*CATEGORICAL VS CATEGORICAL;
%MACRO BIVAR_CAT_CAT(DATA,VAR1,VAR2);
TITLE "BIVARIATE ANALYSIS OF &VAR1 AND &VAR2 FOR &DATA";
TITLE2 "Null hypothesis:  &VAR1 is independent of the &VAR2";
FOOTNOTE "If condition of chi-square are satisfied and p-value is less than significant level (5%),reject null hypothesis: 
There is a relationship between them at 5% significant level.";
PROC SORT DATA=&DATA;
BY &VAR1 &VAR2;
PROC FREQ DATA = &DATA /*ORDER = FREQ*/;
TABLES &VAR1*&VAR2/ MISSING PLOTS = FREQPLOT CHISQ;
RUN;
TITLE;
TITLE2;
FOOTNOTE1;
%MEND BIVAR_CAT_CAT;

/*CATEGORICAL VS CONTINUOUS*/
%MACRO BIVAR_CAT_CONT(DSN ,CLASS , VAR );
%LET N = %SYSFUNC(COUNTW(&VAR));
%DO I = 1 %TO &N;
	%LET X = %SCAN(&VAR,&I);
	PROC MEANS DATA = &DSN. N NMISS MIN Q1 MEDIAN MEAN Q3 MAX qrange cv clm maxdec=2 ;
	TITLE " RELATION BETWEEN &X. AND &CLASS.";
	CLASS &CLASS. ;
	VAR &X.;
	OUTPUT OUT= OUT_&CLASS._&X. MIN =   MEAN=  STD = MAX = /AUTONAME ;

	RUN;
%END;
TITLE; RUN;
PROC SGPLOT DATA=&DSN;
VBOX &VAR/ GROUP = &CLASS DATASKIN=pressed;
 	STYLEATTRS 
	BACKCOLOR=snow 
    WALLCOLOR=WhiteSmoke
	AXISEXTENT= DATA ;
RUN;

PROC SGPLOT DATA=&DSN;
HISTOGRAM &VAR /DATASKIN=PRESSED 
					FILLATTRS=(COLOR=PLUM);
RUN;
%MEND BIVAR_CAT_CONT;


*-------------------------------------------------------------------------------;
%MACRO BIVAR_CAT_CONT_ASSOC(DATA,CLASS,VAR);
PROC SORT DATA = &DATA;
BY &CLASS;RUN;

PROC UNIVARIATE DATA= &DATA NORMAL;
BY &CLASS;
var &VAR;
QQPLOT / NORMAL (mu=est sigma=est);
RUN;
%MEND BIVAR_CAT_CONT_ASSOC;

%MACRO MULTVAR_CAT_CONT(DATA,GROUP_VAR,CONT_VAR,SEG_CONT_VAR);
PROC MEANS DATA=&DATA NMISS MEAN STD STDERR CV LCLM UCLM MEDIAN MIN MAX QRANGE MAXDEC=2;
CLASS &GROUP_VAR;
VAR &CONT_VAR;RUN;

PROC FREQ DATA=&DATA ;
TABLES &GROUP_VAR*&SEG_CONT_VAR ;
RUN;

PROC SORT DATA=&DATA;
BY &GROUP_VAR ;

PROC SGPLOT DATA=&DATA;
VBOX &CONT_VAR/ GROUP = &GROUP_VAR DATASKIN=pressed;
 	STYLEATTRS 
	BACKCOLOR=snow 
    WALLCOLOR=WhiteSmoke
	AXISEXTENT= DATA ;
RUN;

PROC SGPANEL DATA=&DATA;
PANELBY &GROUP_VAR / UNISCALE=ROW skipemptycells  ;
HISTOGRAM &CONT_VAR /DATASKIN=PRESSED 
					FILLATTRS=(COLOR=PLUM);
RUN;
%MEND MULTVAR_CAT_CONT;

*----------------------------------------------------------------------------------------;
/*ANALYSIS OF DATES*/
%DATES(PERM.CRM,ACTDT);
%DATES(PERM.CRM,DEACTDT);

/*ACT VS DEACT*/
TITLE1 "ACTIVE VS DEACTIVE # ACCOUNTS PER DATE";

DATA FREQ_DATE;
SET FREQ_DATE_DEACTDT FREQ_DATE_ACTDT (KEEP=ACTDT COUNT);
RUN;

PROC SQL;
CREATE TABLE FREQ_DATE1 AS
	SELECT COALESCE(ACTDT,DEACTDT) AS DATE FORMAT mmddyy10.,
	CASE WHEN MISSING(ACTDT) 
		THEN "N"
		ELSE "Y"
	END AS ACTIVE,
	COUNT
	FROM FREQ_DATE;

QUIT;RUN;

DATA FREQ_DATE2;
SET FREQ_DATE1;
WHERE NOT MISSING(DATE);
RUN;
FOOTNOTE "*10 FIRST OBSERVATIONS";
PROC PRINT DATA= FREQ_DATE2(OBS=10);RUN;
FOOTNOTE;

PROC SGPLOT DATA = FREQ_DATE2;
SERIES X= DATE Y= COUNT / GROUP = ACTIVE;
YAXIS GRID OFFSETMAX=0.04;
RUN;
TITLE1;

/*UNIVARIATE ANALYSIS*/
TITLE1 "UNIVARIATE ANALYSIS";

DATA TITLE;
TITLE5 = "";
proc report data=title nowindows noheader nocenter
  style(column) = { background = NONE }; RUN;
TITLE1;

%CATEGORICAL(SEG,DeactReason);
%CATEGORICAL(SEG,GoodCredit);
%CATEGORICAL(SEG,RatePlan);
%CATEGORICAL(SEG,DealerType);
%CATEGORICAL(SEG,Province);

%CATEGORICAL(SEG,TENURE_SEG);
%CATEGORICAL(SEG,AGE_SEG);
%CATEGORICAL(SEG,SALES_SEG);

%NUMERICAL(SEG,TENURE);

%NUMERICAL(SEG,Sales);
%NUMERICAL(SEG,Age);


/*DROPPING OBSERVATIONS*/

TITLE1 "DROPPING OBSERVATIONS";

DATA TITLE;
TITLE5 = "We can see age as low as 0, since the goal of this analysis is to investigate customers' distribution and behaviours, 
I'll drop any observations with the age of 18, the usually  legal age. 

It was decided that since this is a behavioural study to trimm observations below the legal age and missing provinces and missing sales.
Keeping even then 75% of abservations.
 ";
proc report data=title nowindows noheader nocenter
  style(column) = { background = NONE }; RUN;
TITLE1;

PROC SQL;
CREATE TABLE Before AS
SELECT
"1" as id,
COUNT(*) AS Total_obs_BEFORE_dropping
FROM SEG ;
QUIT;
RUN;

DATA SEG;
SET SEG;
WHERE AGE > 18 AND NOT MISSING (PROVINCE) AND NOT MISSING (SALES);
RUN;

/*DATA TITLE;*/
/*TITLE = "TRIMMING OBSERVATIONS - Excluding obs with Age lower than 18, and missing values for Province and Sales";*/
/*proc report data=title nowindows noheader nocenter*/
/*  style(column) = { background = yellow };*/
/*run;*/

PROC SQL;
CREATE TABLE After AS
SELECT
"1" as id,
COUNT(*) AS Total_obs_AFTER_dropping
FROM SEG ;
QUIT;
RUN;

DATA COMP;
MERGE BEFORE AFTER;
BY ID;

DATA COMP;
SET COMP;
PERC=Total_obs_AFTER_dropping/Total_obs_BEFORE_dropping; 
FORMAT PERC PERCENT5.;
RUN;

TITLE "PERCENTAGE OF PRESERVED DATA";
PROC PRINT DATA=COMP; RUN;

/*UNIVARIATE ANALYSIS*/
TITLE1 "UNIVARIATE ANALYSIS - After Trimming observations";

DATA TITLE;
TITLE5 = "";
proc report data=title nowindows noheader nocenter
  style(column) = { background = NONE }; RUN;
TITLE1;

%CATEGORICAL(SEG,DeactReason);
%CATEGORICAL(SEG,GoodCredit);
%CATEGORICAL(SEG,RatePlan);
%CATEGORICAL(SEG,DealerType);
%CATEGORICAL(SEG,Province);

%NUMERICAL(SEG,Age);
%NUMERICAL(SEG,Sales);

%NUMERICAL(SEG,TENURE);

%CATEGORICAL(SEG,TENURE_SEG);
%CATEGORICAL(SEG,AGE_SEG);
%CATEGORICAL(SEG,SALES_SEG);

/*# OF DEACTIVATE ACCOUNTS PER MONTH/YEAR */
TITLE1 "# OF DEACTIVED ACCOUNTS PER MONTH/YEAR";
PROC SQL;
CREATE TABLE DEACTDT_YEAR_MONTH AS
SELECT DISTINCT
	CATX("/",YEAR(DEACTDT),MONTH(DEACTDT)) AS YEAR_MONTH,
	COUNT(DEACTDT) AS "N_ACCOUNTS"n
	FROM SEG
	WHERE NOT MISSING(DEACTDT)
GROUP BY CALCULATED YEAR_MONTH
ORDER BY CATX("/",YEAR(DEACTDT),MONTH(DEACTDT));
QUIT;
PROC PRINT DATA = DEACTDT_YEAR_MONTH ;

PROC SGPLOT DATA = DEACTDT_YEAR_MONTH;
VBAR YEAR_MONTH/FREQ=N_ACCOUNTS grouporder=ascending FILLATTRS=(COLOR = Plum)DATASKIN=pressed;
 	STYLEATTRS 
	BACKCOLOR=snow 
    WALLCOLOR=WhiteSmoke
	AXISEXTENT= DATA ;
RUN;
TITLE1;

/*BIVARIATE ANALYSIS*/
/*CATEGORICAL VS CATEGORICAL*/

TITLE1 "BIVARIATE ANALYSIS";
TITLE2 "CATEGORICAL VS CATEGORICAL";

DATA TITLE;
INPUT TEXT $150.;
DATALINES;
Chi-square test:
Null hypothesis:
Assumptions:
1. N, the total frequency, should be reasonably large (greater than 50)
2. The sample observations should be independent. No individual item should be included twice or more in the sample"
3. No expected frequencies should be small. Preferably each expected frequency should be larger than 10 but in any case not less than 5.
                                                 
If condition of chi-square are satisfied and p-value is less than significant level (5%), reject null hypothesis: 
 - There is a relationship between them at 5% significant level.
;
RUN;
proc report data=title nowindows noheader nocenter
  style(column) = { background = NONE }; RUN;
TITLE1;

%BIVAR_CAT_CAT(SEG,ACTIVE,PROVINCE);

DATA TITLE;
INPUT TEXT $230.;
DATALINES;
We can see that the assumptions for chi-square test are met, with p-value of 0.9353, we fail to reject the null hypothesis and can't say that there's a relationship between the features.
;
RUN;
proc report data=title nowindows noheader nocenter
  style(column) = { background = NONE }; RUN;
TITLE1;



DATA SEG1;
SET SEG;
WHERE=(ACTIVE="N");RUN;

%BIVAR_CAT_CAT(SEG1,ACTIVE,DEACTREASON);

DATA TITLE;
INPUT TEXT $90.;
DATALINES;
For Deactive reason, only a frequency table and distribution plot will be showed.
RUN;
proc report data=title nowindows noheader nocenter
  style(column) = { background = NONE }; RUN;
TITLE1;

PROC SQL;
DROP TABLE SEG1;

%BIVAR_CAT_CAT(SEG,ACTIVE,GOODCREDIT);
DATA TITLE;
INPUT TEXT $230.;
DATALINES;
We can see that the assumptions for chi-square test are met, with p-value of <0.0001, we can reject the null hypothesis and say that there's a relationship between the features.
;
RUN;
proc report data=title nowindows noheader nocenter
  style(column) = { background = NONE }; RUN;
TITLE1;

%BIVAR_CAT_CAT(SEG,ACTIVE,RATEPLAN);
DATA TITLE;
INPUT TEXT $230.;
DATALINES;
We can see that the assumptions for chi-square test are met, with p-value of <0.0001, we can reject the null hypothesis and say that there's a relationship between the features.
;
RUN;
proc report data=title nowindows noheader nocenter
  style(column) = { background = NONE }; RUN;
TITLE1;

%BIVAR_CAT_CAT(SEG,ACTIVE,DEALERTYPE);
DATA TITLE;
INPUT TEXT $230.;
DATALINES;
We can see that the assumptions for chi-square test are met, with p-value of <0.0001, we can reject the null hypothesis and say that there's a relationship between the features.
;
RUN;
proc report data=title nowindows noheader nocenter
  style(column) = { background = NONE }; RUN;
TITLE1;

%BIVAR_CAT_CAT(SEG,ACTIVE,AGE_SEG);
DATA TITLE;
INPUT TEXT $230.;
DATALINES;
We can see that the assumptions for chi-square test are met, with p-value of 0.6320, we fail to reject the null hypothesis and can't say that there's a relationship between the features.
;
RUN;
proc report data=title nowindows noheader nocenter
  style(column) = { background = NONE }; RUN;
TITLE1;

%BIVAR_CAT_CAT(SEG,ACTIVE,TENURE_SEG);
DATA TITLE;
INPUT TEXT $230.;
DATALINES;
We can see that the assumptions for chi-square test are met, with p-value of <0.0001, we can reject the null hypothesis and say that there's a relationship between the features.
;
RUN;
proc report data=title nowindows noheader nocenter
  style(column) = { background = NONE }; RUN;
TITLE1;

%BIVAR_CAT_CAT(SEG,ACTIVE,SALES_SEG);
%BIVAR_CAT_CAT(SEG,ACTIVE,AGE_SEG);
DATA TITLE;
INPUT TEXT $230.;
DATALINES;
We can see that the assumptions for chi-square test are met, with p-value of 0.6320, we fail to reject the null hypothesis and can't say that there's a relationship between the features.
;
RUN;
proc report data=title nowindows noheader nocenter
  style(column) = { background = NONE }; RUN;
TITLE1;

TITLE1 "CATEGORICAL VS CONTINUOUS";

DATA TITLE;
INPUT TEXT $125.;
DATALINES;
T-test:
Null hypothesis: There’s no difference in means
Assumptions:
1.Sample distribution must be normal:
Shapiro (null hypothesis: sample has a normal distribution)
CLT :
If looks normal each group must have more than 30 observations – no need for Shapiro’s test
If moderately skewed, each group must have more than 100 observations – no need for Shapiro’s test
2.Groups are independent of one another. 
3.There are no major outliers.
4.A check for unequal variances will help determine which version of an independent samples t-test is most appropriate: 
(Levene’s test, null hypothesis: equal variance) 
    a.If variances are equal, then a pooled t-test is appropriate
    b.If variances are unequal, then a Satterthwaite (also known as Welch’s) t-test is appropriate
;
RUN;
proc report data=title nowindows noheader nocenter
  style(column) = { background = NONE }; RUN;
TITLE1;

%BIVAR_CAT_CONT(SEG,ACTIVE,SALES);
DATA TITLE;
INPUT TEXT $150.;
DATALINES;
We can see a great number of major outliers, so as the data is, it's not possible to use t-test for sales and active features.
This test will be performed in due time, after trimming major outliers.
;
RUN;
proc report data=title nowindows noheader nocenter
  style(column) = { background = NONE }; RUN;

%BIVAR_CAT_CONT(SEG,ACTIVE,AGE);
DATA TITLE;
INPUT TEXT $150.;
DATALINES;
We can see that in each group between active and age features we have more than 100 observations, so there's no need to test for normality.
Let's test for homogeneity of variances:
;
RUN;
proc report data=title nowindows noheader nocenter
  style(column) = { background = NONE }; RUN;

PROC GLM data=SEG PLOTS=NONE;
class ACTIVE;
model AGE = ACTIVE;
means ACTIVE/ hovtest=levene(type=abs) welch;
run;
DATA TITLE;
INPUT TEXT $150.;
DATALINES;
We can see that Levene's test points to equal variances (pvalue of 0.2072), since we fail to reject null hypothesis at 5% significance level.
;
RUN;
proc report data=title nowindows noheader nocenter
  style(column) = { background = NONE }; RUN;


proc ttest data=seg;
var age;
class active;
run;
DATA TITLE;
INPUT TEXT $130.;
DATALINES;
We can see that with a pvalue of 0.9520, we failed to reject null hypothesis. Thus, the two groups are equal.
;
RUN;
proc report data=title nowindows noheader nocenter
  style(column) = { background = NONE }; RUN;

TITLE1 "Testing association - TENURE SEGMENT AND GOODCREDIT, RATEPLAN, AND DEALERTYPE"; 
DATA TITLE;
INPUT TEXT $150.;
DATALINES;
Chi-square test:
Assumptions:
1. N, the total frequency, should be reasonably large (greater than 50)
2. The sample observations should be independent. No individual item should be included twice or more in the sample"
3. No expected frequencies should be small. Preferably each expected frequency should be larger than 10 but in any case not less than 5.
                                                 
If condition of chi-square are satisfied and p-value is less than significant level (5%), reject null hypothesis: 
 - There is a relationship between them at 5% significant level.
;
RUN;
proc report data=title nowindows noheader nocenter
  style(column) = { background = NONE }; RUN;
TITLE1;

ods graphics on / width=6in  height=9in;
%BIVAR_CAT_CAT(SEG,TENURE_SEG,GOODCREDIT);
DATA TITLE;
INPUT TEXT $150.;
DATALINES;
The assumptions are met. And, as pvalue is <.0001, we can reject the null hypothesis at 5% significance level and say that there's 
an association between the features.
;
RUN;
proc report data=title nowindows noheader nocenter
  style(column) = { background = NONE }; RUN;
TITLE1;

%BIVAR_CAT_CAT(SEG,TENURE_SEG,RATEPLAN);
DATA TITLE;
INPUT TEXT $150.;
DATALINES;
The assumptions are met. And, as pvalue is <.0001, we can reject the null hypothesis at 5% significance level and say that there's 
an association between the features.
;
RUN;
proc report data=title nowindows noheader nocenter
  style(column) = { background = NONE }; RUN;
TITLE1;

%BIVAR_CAT_CAT(SEG,TENURE_SEG,DEALERTYPE);
DATA TITLE;
INPUT TEXT $150.;
DATALINES;
The assumptions are met. And, as pvalue is <.0001, we can reject the null hypothesis at 5% significance level and say that there's 
an association between the features.
;
RUN;
proc report data=title nowindows noheader nocenter
  style(column) = { background = NONE }; RUN;
TITLE1;

ods graphics on / reset=all;

TITLE1 "CATEGORICAL VS CONTINUOUS";
TITLE2 "Testing association - SALES AMOUNT AND ACCOUNT STATUS, GOOD CREDIT AND AGE SEGMENTS";

DATA TITLE;
INPUT TEXT $130.;
DATALINES;
T-test:
Null hypothesis: There’s no difference in means
Assumptions:
1.Sample distribution must be normal:
Shapiro (null hypothesis: sample has a normal distribution)
CLT :
If looks normal each group must have more than 30 observations – no need for Shapiro’s test
If moderately skewed, each group must have more than 100 observations – no need for Shapiro’s test
2.Groups are independent of one another. 
3.There are no major outliers.
4.A check for unequal variances will help determine which version of an independent samples t-test is most appropriate: 
(Levene’s test, null hypothesis: equal variance) 
    a.If variances are equal, then a pooled t-test is appropriate
    b.If variances are unequal, then a Satterthwaite (also known as Welch’s) t-test is appropriate
;
RUN;
proc report data=title nowindows noheader nocenter
  style(column) = { background = NONE }; RUN;
TITLE1;
TITLE2;

%BIVAR_CAT_CONT(SEG,ACTIVE,SALES);
DATA TITLE;
INPUT TEXT $230.;
DATALINES;
We can see a great number of major outliers, so as the data is, it's not possible to use t-test for sales and active features.
Sales greater than $390 (~Q3+3*IQR) will be dropped to perform the test.
;
RUN;
proc report data=title nowindows noheader nocenter
  style(column) = { background = NONE }; RUN;
TITLE1;

DATA SEG1;
SET SEG;
WHERE SALES <=390; RUN;

%BIVAR_CAT_CONT(SEG1,ACTIVE,SALES);
DATA TITLE;
INPUT TEXT $150.;
DATALINES;
We can see that in each group between active and age features we have more than 100 observations, so there's no need to test for normality.
Let's test for homogeneity of variances:
;
RUN;
proc report data=title nowindows noheader nocenter
  style(column) = { background = NONE }; RUN;
TITLE1;

PROC GLM data=SEG1 PLOTS=NONE;
class ACTIVE;
model SALES = ACTIVE;
means ACTIVE/ hovtest=levene(type=abs) welch;
run;
DATA TITLE;
INPUT TEXT $150.;
DATALINES;
We can see that Levene's test points to equal variances (pvalue of 0.8427), since we fail to reject null hypothesis at 5% significance level.
;
RUN;
proc report data=title nowindows noheader nocenter
  style(column) = { background = NONE }; RUN;
TITLE1;


proc ttest data=seg1;
var SALES;
class active;
run;
DATA TITLE;
INPUT TEXT $150.;
DATALINES;
We can see that with a pvalue of 0.4678, we failed to reject null hypothesis. Thus, the two groups are equal.
;
RUN;
proc report data=title nowindows noheader nocenter
  style(column) = { background = NONE }; RUN;
TITLE1;


%BIVAR_CAT_CONT(SEG,GOODCREDIT,SALES);
DATA TITLE;
INPUT TEXT $150.;
DATALINES;
We can see a great number of major outliers, so as the data is, it's not possible to use t-test for sales and active features.
Sales greater than $600 (~Q3+3*IQR) will be dropped to perform the test.
;
RUN;
proc report data=title nowindows noheader nocenter
  style(column) = { background = NONE }; RUN;
TITLE1;

DATA SEG1;
SET SEG;
WHERE SALES <=600; RUN;

%BIVAR_CAT_CONT(SEG1,GOODCREDIT,SALES);
DATA TITLE;
INPUT TEXT $150.;
DATALINES;
We can see that in each group between active and age features we have more than 100 observations, so there's no need to test for normality.
Let's test for homogeneity of variances:
;
RUN;
proc report data=title nowindows noheader nocenter
  style(column) = { background = NONE }; RUN;
TITLE1;

PROC GLM data=SEG1 PLOTS=NONE;
class GOODCREDIT;
model SALES = GOODCREDIT;
means GOODCREDIT/ hovtest=levene(type=abs) welch;
run;
DATA TITLE;
INPUT TEXT $150.;
DATALINES;
We can see that Levene's test points to equal variances (pvalue of 0.5345), since we fail to reject null hypothesis at 5% significance level.
;
RUN;
proc report data=title nowindows noheader nocenter
  style(column) = { background = NONE }; RUN;
TITLE1;

proc ttest data=seg1;
var SALES;
class GOODCREDIT;
run;
DATA TITLE;
INPUT TEXT $150.;
DATALINES;
We can see that with a pvalue of 0.6212, we failed to reject null hypothesis. Thus, the two groups are equal.
;
RUN;
proc report data=title nowindows noheader nocenter
  style(column) = { background = NONE }; RUN;
TITLE1;

%BIVAR_CAT_CONT(SEG,AGE_SEG,SALES);
DATA TITLE;
INPUT TEXT $150.;
DATALINES;
We can see a great number of major outliers, so as the data is, it's not possible to use t-test for sales and active features.
Sales greater than $590 (~Q3+3*IQR) will be dropped to perform the test.
;
RUN;
proc report data=title nowindows noheader nocenter
  style(column) = { background = NONE }; RUN;
TITLE1;

DATA SEG1;
SET SEG;
WHERE SALES <=590; RUN;

%BIVAR_CAT_CONT(SEG1,AGE_SEG,SALES);
DATA TITLE;
INPUT TEXT $150.;
DATALINES;
We can see that in each group between active and age features we have more than 100 observations, so there's no need to test for normality.
Let's test for homogeneity of variances:
;
RUN;
proc report data=title nowindows noheader nocenter
  style(column) = { background = NONE }; RUN;
TITLE1;

PROC GLM data=SEG1 PLOTS=NONE;
class AGE_SEG;
model SALES = AGE_SEG;
means AGE_SEG/ hovtest=levene(type=abs) welch;
run;
DATA TITLE;
INPUT TEXT $150.;
DATALINES;
We can see that Levene's test points to equal variances (pvalue of 0.0751), since we fail to reject null hypothesis at 5% significance level.
;
RUN;
proc report data=title nowindows noheader nocenter
  style(column) = { background = NONE }; RUN;
TITLE1;

PROC GLM data=SEG1 PLOTS=NONE;
class AGE_SEG;
model SALES = AGE_SEG ;
run;
DATA TITLE;
INPUT TEXT $150.;
DATALINES;
We can see that with a pvalue of 0.7907, we failed to reject null hypothesis. Thus, the two groups are equal.
;
RUN;
proc report data=title nowindows noheader nocenter
  style(column) = { background = NONE }; RUN;
TITLE1;

TITLE "CONCLUSIONS";
DATA TITLE;
INPUT TEXT $150.;
DATALINES;
We can see that from the gathered data, there may be a trend of increase in activations in the beginning of year and in the middle.
It seems to be a threshold of 60 days in tenure that either makes customer's leave or stay in a long term relationship.
The NEED reason is the one being most used.
                                 
Account Status it's impacted by:
Good credit
Rate Plan
Dealer Type
Tenure(Segmented)
                                 
Segmented Tenure it's impacted by:
Good credit
Rate Plan
Dealer Type
                                               
Sales amount it's not impacted by Account Status, Good credit, or even Age.
;
RUN;
proc report data=title nowindows noheader nocenter
  style(column) = { background = NONE }; RUN;
TITLE1;

TITLE "RECOMMENDATIONS";
DATA TITLE;
INPUT TEXT $150.;
DATALINES;
Observe the increasing of deactivations in the last 6 months of 2000 and beginning of 2001.
Investigate further the type NEED of deactivations reasons to look for a direct marketing strategy.
Investigate further to see the threshold between a finer adjustment the credit score checking would bring benefits.
Investigate further Rate Plan 1 for its success with the customers to replicate its features into the other plans.
Investigate further  Dealer Type A1 for its success with the customers to replicate its features with other dealers.
Investigate further the Tenure Segments.
                                                                                                                 
                                                                                                                 
Next steps: do multivariate analysis(when possible) with the features mentioned above to find other associations.
;
RUN;
proc report data=title nowindows noheader nocenter
  style(column) = { background = NONE }; RUN;
TITLE1;
RUN;




