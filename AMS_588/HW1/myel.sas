options ls=78 ps=60;
data myel;

input dur status trt renal;
cards;
8 1 1 1
180 1 2 0
632 1 2 0
852 0 1 0
52 1 1 1
2240 0 2 0
220 1 1 0
63 1 1 1
195 1 2 0
76 1 2 0
70 1 2 0
8 1 1 0
13 1 2 1
1990 0 2 0
1976 0 1 0
18 1 2 1
700 1 2 0
1296 0 1 0
1460 0 1 0
210 1 2 0
63 1 1 1
1328 0 1 0
1296 1 2 0
365 0 1 0
23 1 2 1
run;

proc lifetest data=myel;
time dur*status(0);
strata trt;
run;

proc export data=myel dbms=csv
outfile="C:\Users\Linna_hu\Desktop\588\HW1\myel.csv"
replace;
run;
