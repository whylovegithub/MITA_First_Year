reset;
set SIZE;
set PLANT;
##set sub_p;

param cs{SIZE};
param cp{PLANT};
param profits{SIZE};
param capp{PLANT};
param bfs{SIZE};

var xps{PLANT,SIZE}>=0;

maximize Objective:
    sum{s in SIZE, p in PLANT}profits[s]*xps[p,s]
;

subject to
##retain{b in sub_p}:((sum{s in SIZE}xps[b,s])*capp[b+1]) == ((sum{s in SIZE}xps[b+1,s])*capp[b] );
space{p in PLANT}:sum{s in SIZE}xps[p,s]<=cp[p];
capacity{p in PLANT}:sum{s in SIZE}xps[p,s]<=capp[p];
limit{s in SIZE}:sum{p in PLANT}xps[p,s]<=bfs[s];

##the code with # is for q2_b