var x1>=0;
var x2>=0;
var x3>=0;
#x1,x2,x3 are machines to manufacture men's shoes, women's shoes and children's shoes.

#revenue=
maximize z: 40*26/1.5*x1+20*56*x2+40*14*x3;

subject to
#each type of shoes require a ceratin type of machines.
machine:x1+x2+x3<=20; 
labor:2.5 * 40 / 1.5 *x1 + 20 * 6 * x2 + 40 * 1.5 * x3 <= 2000;
quantity:40/1.5*x1+40*x3<=20*x2;

#set 12.5 machine for women's shoes 7.5 for man's give maxmized revenues and we got 200 men's shoes and 250 women's shoe's
#which gives us maxmized weekly revenue of 19200$.
