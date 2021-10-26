var x1>=0;
var x2>=0;
var x3>=0;

minimize z: 3*x2-5*x3;

subject to

c1:2*x1+x2+x3<=15;
c2:-2*x1+-3*x2+4*x3<=-10;
c3:x1+-1*x2+x3<=9;


