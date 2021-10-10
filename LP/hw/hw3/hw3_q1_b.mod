reset;

set PRODS;
set WORKT;
set T;
set T_0;
set T_0_3;
set T_4;


param cp{PRODS};
param km{WORKT};
param am{WORKT};
param dpt{PRODS,T};
param rp{PRODS};

var xpt{PRODS,T} >=0;
var spt{PRODS,T} >=0;
var hmt{WORKT,T} >=0;

minimize Objective:
    sum{m in WORKT, t in T} km[m]*hmt[m,t] 
    +sum{t in T, p in PRODS}cp[p]*spt[p,t]
;

subject to 
restrain_store_for_0{p in PRODS, t in T_0}:spt[p,t] == 0;
hours_available{m in WORKT,t in T}:hmt[m,t]<=am[m];
satisfy_demand{p in PRODS, t in T_0_3}:xpt[p,t]+spt[p,t]-spt[p,t+1]>=dpt[p,t];
satisfy_demand_in_time_4{p in PRODS,t in T_4}:xpt[p,t]+spt[p,t]>=dpt[p,t];
hours_required{t in T}:sum{p in PRODS}rp[p]*xpt[p,t]<=sum{m in WORKT}hmt[m,t];
