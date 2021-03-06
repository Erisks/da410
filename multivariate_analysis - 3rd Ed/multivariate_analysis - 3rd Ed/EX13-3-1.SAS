/*EXAMPLES 13.3.1, 13.3.2, and 13.5.2b(a)*/   
options ls=78;
DATA PERCEP;
  INPUT Y1 Y2 Y3 Y4 Y5;
CARDS;
  1 5 5 1 1
  8 9 7 9 8
  9 8 9 9 8
  9 9 9 9 9
  1 9 1 1 9
  9 7 7 9 9
  9 7 9 9 7
;
PROC FACTOR METHOD=PRIN SCREE EV PLOT rotate=varimax;
  VAR Y1-Y5;
  LABEL Y1='KIND' Y2='INTELLIGENT' Y3='HAPPY' Y4='LIKEABLE' Y5='JUST';
  TITLE 'Examples 13.3.1 and 13.5.2b(a)';run;
PROC FACTOR METHOD=PRIN PRIORS=MAX rotate=varimax;
  VAR Y1-Y5;
  LABEL Y1='KIND' Y2='INTELLIGENT' Y3='HAPPY' Y4='LIKEABLE' Y5='JUST';
  TITLE 'Example 13.3.2';run;


/*  Examples 13.3.3, 13.3.4, and 13.4(b)  */
FILENAME SEISHU 'e:/s611/seishu.dat';
DATA WINE;
  INFILE SEISHU;
  INPUT Y1 Y2 X1 X2 X3 X4 X5 X6 X7 X8;run;
PROC FACTOR METHOD=PRIN SCREE EV;
title 'EXAMPLE 13.4(b)';
PROC FACTOR METHOD=PRIN PRIORS=SMC;
PROC FACTOR METHOD=PRINIT PRIORS=SMC HEYWOOD ;
title 'EXAMPLE 13.3.3';
PROC FACTOR METHOD=ML N=4 HEYWOOD; 
title 'EXAMPLE 13.3.4';
run;


