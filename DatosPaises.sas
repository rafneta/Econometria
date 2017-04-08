/*******************************************************************************
Macro name: CochraneOrcutt
Written by: Piyas Bandyopadhyay bpiyas@yahoo.com .
Doctoral Student, Michigan State University
Creation date: 04/21/2010.

As of date: 04/21/2010.
SAS version: 9.2.
Purpose: This macro uses Cochrane - Orcutt Procedure to estimate the autocorrelation and then transform the model
to estimate the regression parameters. Durbin -Watson test is done to see if the autocorrelation has removed.

Assumptions: 1st order auto regressive

Parameters: The database, the dependent variable (Y) and the independent variables  (as in the model statement)

Format: List the components of the actual macro call. For example:
%CochraneOrcutt (database =
, Y =
, Xs =
)

Required
Parameters: 
database - Name of SAS data set.
Y - The independent variable.
Xs - All the X variables in the format A B C D

Sub-macros
called: SYMPUT
Data sets
created: co_regout co_final co_r.  All are dropped after execution

Limitations: 1st order only.
Can deal with only one independent variable, I will create a new version with many xs

Notes: References, background information or rationale of the macro.
Kutner, Nachtsheim and Neter (2009), Applied Linear Regression Models, chapter 12, page 492

History: List bug fixes, coding changes, etc.
Sample Macro
call: What the actual macro call would look like, e.g.
%print_obs (data =mydata
, Y = TheYVar
, Xs = Xvar1
)
You may also want to consider including sample data/output to
fully demonstrate how the macro should work.
*********************************************************************************/
%MACRO CochraneOrcutt(database=, Y=, Xs=, noint= );
%Local 	R_estimated;

*Title 'This is your data';
*PROC PRINT DATA = &database NOOBS;
*RUN;


title 'Now I will run a regression to original data';
proc reg data= &database;
	model  &Y=&Xs /dw dwprob spec &noint;
	output out=CO_regout p=yhat r=ei;
run;

*Title 'Regession output';
*PROC PRINT data=CO_regout NOOBS;
*RUN;

data co_regout;
set cO_regout;
	ei1=lag(ei);
	ei_ei1=ei*ei1;
	ei1_2=ei1*ei1;
	y1=lag(&Y);
	x1=lag(&Xs);
	casenum=_n_;
run;

proc sql;
create table co_r as
select a.casenum, sum(a.ei_ei1)/sum(a.ei1_2) as r_est 
from co_regOut a
	, co_regOut b
	where b.casenum=a.casenum;
quit;

data _NULL_;
set co_r;
	IF _N_ = 1 THEN
		CALL SYMPUT("R_estimated", r_est);
	ELSE STOP;
run;

data co_regout;
set co_regout;
merge co_r;
	by casenum;
run;

*proc print;
*run;

proc sql;
	drop table co_r;
quit;

data co_regout;
set  co_regout;
	yp = &Y-r_est*y1;
	xp = &Xs-r_est*x1;
run;

title 'Now I will run a regression to transform data';
proc reg data= co_regout;
	model yp =xp /dw dwprob spec &noint;
	output out = out_reg student = stdres;
run;


proc autoreg data = co_regout;
	model yp = xp / dw=1 dwprob &noint;
	output out=CO_Final;
run;

*proc print; 
*run;

data Co_Final;
set CO_Final;
	if Variable='Intercept' then do;
		OriginalEst = Estimate/(1- &R_estimated);
		OriginalStdErr = Stderr/(1- &R_estimated);
	end;
	else do;
		OriginalStdErr = Stderr;
		OriginalEst = Estimate;
	end;
	Correlation = &R_estimated;
	drop model;
run;


proc print data=Co_Final noobs;
Title "Final estimates based on Cochrane-Orcutt";
run;

proc sql;
	drop table Co_Final;
	drop table co_regout;
quit;

%MEND CochraneOrcutt;



/* Código generado (IMPORT) */
/* Archivo de origen: Datos1962.xlsx */
/* Ruta de origen: /folders/myfolders/Econometria */

/*LEEMOS LOS DATOS DEL ARCHIVO DE EXCEL*/
FILENAME REFFILE '/folders/myfolders/Econometria/Datos1962.xlsx';
PROC IMPORT DATAFILE=REFFILE
	DBMS=XLSX
	OUT=WORK.datospaises;
	GETNAMES=YES;
RUN;

%CochraneOrcutt(database=datospaises, Y=Natalidad, Xs=Ingreso, noint=)

ods graphics;

proc reg data=work.datospaises;
model Natalidad= Mortalidad /  spec; /* Durbin-Watson test and White test */
		 output out=outNM1 residual=r;
title 'Regresión lineal Natalidad con Mortalidad';
run;
quit;

ods graphics;

proc reg data=work.datospaises;
model Natalidad= Mortalidad /  noint spec; /* Durbin-Watson test and White test */
		 output out=outNM2 residual=r;
title 'Regresión lineal Natalidad con Mortalidads sin interceptor';
run;
quit;


ods graphics;

proc reg data=work.datospaises;
model Natalidad= Poblacion / spec; /* Durbin-Watson test and White test */
		 output out=outNP1 residual=r;
title 'Regresión lineal Natalidad con Población ';
run;
quit;

ods graphics;

proc reg data=work.datospaises;
model Natalidad= Poblacion / noint spec; /*  White test */
		 output out=outNP2 residual=r;
title 'Regresión lineal Natalidad con Población sin interceptor ';
run;
quit;

ods graphics;

proc reg data=work.datospaises;
model Natalidad= Ingreso /spec; /*  White test */
		 output out=outNI1 residual=r;
title 'Regresión lineal Natalidad e Ingreso ';
run;
quit;

ods graphics;

proc reg data=work.datospaises;
model Natalidad= Ingreso / noint spec; /*  White test */
		 output out=outNI2 residual=r;
title 'Regresión lineal Natalidad e Ingreso sin interceptor';
run;
quit;



ods graphics / imagemap=on;
proc corr data=work.datospaises nosimple plots= matrix(nvar=all histogram);
var Ingreso Poblacion Mortalidad;
title 'Correlación entre las varibales de entrada';
run; 



proc corr data=work.datospaises rank plots(only)=scatter(nvar=all ellipse=none);
	var  Ingreso Poblacion Mortalidad;
	with Natalidad;
	title 'Correlación de las variables de entrada con la salida';
run;





%let interval= Ingreso Poblacion Mortalidad;

options nolabel;

proc sgscatter data=work.datospaises;
plot Natalidad*(&interval) / reg;
title 'Relaciones entre Ingreso Poblacion y Mortalidad con la Natalidad';
run;

data tras;
val=55;
I = log2(val);
v2 = log(val) / log(2);
put b2= ;
put v2=;
run;


%let interval= I P M;

options nolabel;

proc sgscatter data=work.datospaises;
plot Natalidad*(&interval) / reg;
title 'Relaciones entre Ingreso Poblacion y Mortalidad con la Natalidad';
run;




/*DESPLEGAMOS LA INFORMACIÓN DE LA VARIABLE datospaises*/

PROC CONTENTS DATA=WORK.datospaises;
RUN;

/* SE IMPRIMEN LA VARIBALE datospaises PARA SABER SI ES CORRECTA LA LECTURA*/

TITLE1 'The Birth Rate and Economic Development: An Empirical Study.';
PROC PRINT DATA=WORK.datospaises split='*';
	LABEL Natalidad= 'Tasa media de*nacimientos 1953-1954'
		  Ingreso='Ingreso medio per*cápita 1953-1954'
		  Poblacion='Razón de población*en agricultura 1950'
		  Mortalidad='Tasa media de Mortalidad*infantil 1953-1954';

RUN;

TITLE;
FOOTNOTE;


PROC MEANS DATA=work.datospaises n mean median mode std stderr lclm uclm
	var q1 q3 max min range qrange cv kurtosis skewness printall maxdec=3;
    TITLE 'Estadistica para cada variable de datos'; 
RUN;

PROC UNIVARIATE DATA=WORK.datospaises;
	VAR Natalidad Ingreso Poblacion Mortalidad;
	HISTOGRAM Natalidad Ingreso Poblacion Mortalidad / NORMAL(MU=est SIGMA=est);
	PROBPLOT Natalidad Ingreso Poblacion Mortalidad / NORMAL(MU=est SIGMA=est);
	INSET skewness kurtosis;
	TITLE 'Estadistica Univariada';
RUN;

/*Graficas de caja de las variables*/

ods graphics / reset imagemap;
proc sgplot data=work.datospaises;
	vbox Natalidad / datalabel=Paises fillattrs=(color=CXCAD5E5) name='Box';
	xaxis fitpolicy=splitrotate;
	yaxis grid;
	TITLE 'Gráfica de caja de Natalidad';
run;

ods graphics / reset imagemap;
proc sgplot data=work.datospaises;
	vbox Ingreso / datalabel=Paises fillattrs=(color=CXCAD5E5) name='Box';
	xaxis fitpolicy=splitrotate;
	yaxis grid;
	TITLE 'Gráfica de caja de Ingreso';
run;

ods graphics / reset imagemap;
proc sgplot data=work.datospaises;
	vbox Poblacion / datalabel=Paises fillattrs=(color=CXCAD5E5) name='Box';
	xaxis fitpolicy=splitrotate;
	yaxis grid;
	TITLE 'Gráfica de caja de Población';
run;

ods graphics / reset imagemap;
proc sgplot data=work.datospaises;
	vbox Mortalidad / datalabel=Paises fillattrs=(color=CXCAD5E5) name='Box';
	xaxis fitpolicy=splitrotate;
	yaxis grid;
	TITLE 'Gráfica de caja de Mortalidad';
run;


PROC REG DATA =WORK.datospaises ; /* full model */
	model Natalidad = Ingreso Poblacion Mortalidad;
run;



%let interval= Ingreso Poblacion Mortalidad;
ods graphics on;
proc glmselect data=work.datospaises plots=all;
   model Natalidad=&interval / selection=stepwise
                   details=steps select=SL slstay=0.05 slentry=0.05;
   title "Modelo mediante la selección Stepwise  para Natalidad";
run;


%let interval= Ingreso Poblacion Mortalidad;
ods graphics on;
proc glmselect data=work.datospaises plots=all;
   model Natalidad=&interval / selection=forward details=steps select=SL slentry=0.05;;
   title "Modelo mediante la selección Forward  para Natalidad";
run;

%let interval= Ingreso Poblacion Mortalidad;
ods graphics on;
proc glmselect data=work.datospaises plots=all;
   model Natalidad=&interval / selection=backward details=steps select=SL slentry=0.05;;
   title "Modelo mediante la selección Backward para Natalidad";
run;


ods graphics / imagemap=on;
proc reg data=work.datospaises plots(only)=(cp);
   model Natalidad = Ingreso Poblacion Mortalidad / selection=cp rsquare adjrsq; 
   title 'Mejor modelo (Hocking) utilizando todos las opciones de regresión con interceptor';
run;
quit;
title;

ods graphics / imagemap=on;
proc reg data=work.datospaises plots(only)=(cp);
   model Natalidad = Ingreso Poblacion Mortalidad / noint selection=cp rsquare adjrsq; 
   title 'Mejor modelo (Hocking) utilizando todos las opciones de regresión sin interceptor';
run;
quit;
title;



proc reg data=work.datospaises;
   model Natalidad = Ingreso  Mortalidad; 
   title 'Best Models Using All-Regression Option';
run;
quit;
title;



proc reg data=work.datospaises;
   model Natalidad= Mortalidad/  dwprob spec vif;
   title 'Colinealidad y varianza constante: Modelo con interceptor con Mortalidad con VIF';
   output out=out residual=r;	
run;
quit;


proc univariate data=out normal; /* Anderson-Darling */
	var r;
	title 'Anderson-Darling ';
run;
quit;



proc reg data=work.datospaises;
   model Natalidad= Ingreso Mortalidad/ spec vif;
   title 'Colinealidad y varianza constante: Modelo con interceptor Ingreso y Mortalidad con VIF';
   output out=out residual=r;	
run;
quit;


proc univariate data=out normal; /* Anderson-Darling */
	var r;
	title 'Anderson-Darling ';
run;
quit;

proc reg data=work.datospaises;
   model Natalidad= Ingreso Poblacion Mortalidad/ spec vif;
   title 'Colinealidad y varianza constante: Modelo con interceptor completo con VIF';
   output out=out residual=r;	
run;
quit;


proc univariate data=out normal; /* Anderson-Darling */
	var r;
	title 'Anderson-Darling ';
run;
quit;





proc reg data=work.datospaises;
   model Natalidad= Ingreso Poblacion Mortalidad/  noint spec vif;
   title 'Colinealidad y varianza constante: Modelo completo sin interceptor, con VIF';
   output out=out residual=r;	
run;
quit;


proc univariate data=out normal; /* Anderson-Darling */
	var r;
	title 'Anderson-Darling ';
run;
quit;




proc reg data=work.datospaises;
   model Natalidad= Ingreso Mortalidad/ noint spec vif;
   title 'Colinealidad y varianza constante: Modelo con Ingreso y Mortalidad sin interceptor, con VIF';
   output out=out residual=r;	
run;
quit;


proc univariate data=out normal; /* Anderson-Darling */
	var r;
	title 'Anderson-Darling ';
run;
quit;


proc reg data=work.datospaises;
   model Natalidad=  Mortalidad/ noint spec vif;
   title 'Colinealidad y varianza constante: Modelo con Mortalidad sin interceptor, con VIF';
   output out=out residual=r;	
run;
quit;


proc univariate data=out normal; /* Anderson-Darling */
	var r;
	title 'Anderson-Darling ';
run;
quit;





