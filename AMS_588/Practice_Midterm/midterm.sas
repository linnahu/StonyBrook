data midterm;
input x delta z1 z2;
datalines;
3 1 7 2
2 0 6 4
1 1 4 9
4 1 5 8
5 0 10 3
6 1 9 6
;
run;

proc phreg data=midterm;
model x*delta(0) = z1 z2;
run;

proc export data=midterm dbms=csv
outfile="C:\Users\Linna_hu\Desktop\588\Practice midterm\midterm.csv"
replace;
run;
