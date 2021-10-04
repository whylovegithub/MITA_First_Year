reset;
set SHOES;
set SUB_SHOES;

var produce{SHOES} >= 0;
var sub_produce{SUB_SHOES} >= 0;
param timer = 0;
param z;
param MC;
param mt_avail;
param labor_avail;


param mt_req{SHOES};
param labor_req{SHOES};
param profit{SHOES};

maximize weekly_revenue: sum{p in SHOES} profit[p]*produce[p];

subject to
Machine_time: sum{p in SHOES} mt_req[p]*produce[p]<=mt_avail;
Labor: sum{p in SHOES} labor_req[p]*produce[p]<=labor_avail;
Quantity: sum{p in SHOES} produce[p] >= 2*sum{q in SUB_SHOES} sub_produce[q];