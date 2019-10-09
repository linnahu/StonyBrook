options ls=80 ps=200;

Data tempsurv;
  infile "tempsurv.dat" firstobs=2;
  input survtime status;
cards;
3 1
5 1
6 0
8 1
10 0
11 0
15 1
20 0
22 1
23 1
27 0
29 1
32 1
35 1
40 1
26 1
28 1
33 0
21 1
24 0
run;

proc lifereg data=tempsurv;
model survtime*status(0)= / dist=weibull;
run;

