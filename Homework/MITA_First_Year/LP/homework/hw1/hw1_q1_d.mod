var x1>=0;
var x2>=0;

minimize z: x2-2*x1;

subject to

line1:3*x1+2*x2>=60;
line2:-1*x1+2*x2<=20;
line3:x1<=30;
line4:x1-x2<=30;

