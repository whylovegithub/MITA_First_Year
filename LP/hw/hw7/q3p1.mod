reset;
#############  SETS  ##################

# set of nodes
set NODES;

# set of demand nodes
set DEMAND within NODES;
# set of supply nodes
set SUPPLY within NODES;
# set of transshipment nodes
set TRANS within NODES;

# arcs of network
set LINKS within (NODES cross NODES);


############ PARAMETERS  ####################

# shipment costs
param cost{LINKS};

# demands
param demand{DEMAND};

# supplies
param supply{SUPPLY};

#trans limit
param limit{TRANS};


############  VARIABLES  ##################

# flow on each arc
var flow{LINKS} >= 0;

############# OBJECTIVE  ####################

minimize Totalcost: sum{(i,j) in LINKS} cost[i,j] * flow[i,j];

############## CONSTRAINTS  ###################

subject to

# demand is satisfied
DemandSatis{j in DEMAND}: sum{(i,j) in LINKS} flow[i,j] = demand[j];

# supply is exhausted
SupplyExhaust{i in SUPPLY}: sum{(i,j) in LINKS} flow[i,j] <= supply[i];

# transshipment nodes are balanced
Balance{i in TRANS}: sum{(i,j) in LINKS} flow[i,j] = sum{(j,i) in LINKS} flow[j,i];

#capasity satis

CapasitySatis2{j in TRANS}: sum{(i,j) in LINKS} flow[i,j] <= limit[j];

