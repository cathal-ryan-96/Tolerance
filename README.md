# Tolerance
This package is an add-on for my dissertation as part of my MSc Statistics.
This must be used after the tobit mixed effects model has been created in SAS to provide the use of the predicted values and the standard errors of each predicted values in creation of the tolerance interval. The dataset must be in a long format with it being grouped both by the ID of each cow and also the timepoint. The SAS code assumes that it is formatted such that it is grouped by ID first and then within each cow is grouped by the Timepoints
SAS code:

/* Importing the Dataset */
proc import datafile  =  '/folders/myfolders/Name_of_dataset '
out  =  anti
dbms  =  xlsx
replace;
run;
Data anti;
	set anti;
	level_log = log(Level);
run;

*non tobit model;
*To get the starting values for the tobit model;
proc mixed data = anti noclprint;
  class id;
  model level_log = Time/ solution outp = predict;
  random int / subject=id;
run;
* tobit model;
proc nlmixed data=anti XTOL=1E-12 method=GAUSS qpoints=100;
parms sigma2_u="Intercept Estimate" sigma2="Residual Estimate" beta0=beta0 beta1=beta1;
bounds sigma2_u sigma2 >= 0;
pi = constant('pi');
mu = beta0 + b_0j + beta1*Time;
if level_log > log(LOQ_value_specified) then ll = (1 / (sqrt(2*pi*sigma2))) * exp( -(level_log-mu)**2 / (2*sigma2) );
if level_log <= log(LOQ_value_specified) then ll =  probnorm( (level_log - mu) / sqrt(sigma2) );
L=log(ll);
model level_log ~ general(L);
random b_0j ~ normal(0, sigma2_u) subject=id;
predict  b_0j + beta0 + beta1*tTme out=pred_table;
run;
quit;
proc export data = work.pred_table
	dbms = xlsx
	outfile = '/folders/myfolders/pred_anti.xlsx'
	replace;
run;
