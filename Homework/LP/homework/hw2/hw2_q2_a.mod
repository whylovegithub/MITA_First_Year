reset;
set PRODS;

var produce{PRODS} >= 0;

param manhrs_avail;
param machhrs_avail;
param space_avail;

param manhrs_req{PRODS};
param machhrs_req{PRODS};
param space_req{PRODS};
param revenue{PRODS};

maximize total: sum{p in PRODS} revenue[p] * produce[p];

subject to 
Manhours:sum{p in PRODS} manhrs_req[p] * produce[p]<=manhrs_avail;
Machehours:sum{p in PRODS} machhrs_req[p] * produce[p]<=machhrs_avail;
Spaces:sum{p in PRODS} space_req[p] * produce[p]<=space_avail;