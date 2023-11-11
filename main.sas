data import_1;
 infile '/home/u63477506/sasuser.v94/SAMPLE_CHURN.csv' dsd dlm=';' firstobs=2;
 input churn $ subscriber_id $ msisdn $ sales_channel_description $ date_activation :DATETIME. churn_date :DATETIME. first_call_date :DATETIME. last_call_date :DATETIME. first_recharge_date :DATETIME. first_recharge_amount last_international_call_date :DATETIME. original_operator $ dat_attrib_kridi :DATETIME. dat_remb_kridi_net :DATETIME. ported_in_request_date :DATETIME. SelectionProb SamplingWeight;
run;

data import_2;
 infile '/home/u63477506/sasuser.v94/MONTHLY_AGGREGATION_SAMPLE.csv' dsd dlm=';' firstobs=2;
 input subscriber_id $ period_id :YYYYMMDD.  msisdn $ recharge_amount number_of_recharges number_of_sms_onnet number_of_sms_offnet number_of_sms_international mou_onnet mou_offnet_mobile mou_offnet_fix mou_international bonus_on_consumption_revenue bonus_on_refill_revenue bonus_on_refill_flash_revenue number_of_reactivations consumption_revenue arpu_out_without_bonus incoming_revenue data_trafic_volume total_voice_revenu_amount  total_data_revenu_amount  device_type $ flag_smartphone $1.;
run;

/* Statistical analysis */
title "Type d'appareil";
proc sgplot data=import_2;
vbar device_type / datalabel missing;
label device_type = "Appareil";
run;

title "Opérateurs";
proc sgplot data=import_1;
vbar original_operator / datalabel missing;
label original_operator = "opérateur";
run;

title "";
proc sgplot data=import_2;
vbar flag_smartphone / datalabel missing;
label flag_smartphone = "flag dist";
run;

proc freq data=work.import_1; 
 table churn;
run;

title "Churn contre Device_type";
proc sgplot data=import_1 pctlevel=group;
 vbar device_type / group=churn stat=percent missing;
run;

/* Merging 2 datasets by their subscriber_id*/
proc sort data= import_1;
 by subscriber_id;
run;
proc sort data= import_2;
 by subscriber_id period_id;
run;

data import;
 merge import_1(in=imp1)
       import_2(in=imp2);
 by subscriber_id;
 if imp1 and imp2;
run;

proc sql;
 create table imp as
 select distinct       subscriber_id,
                       churn, first_recharge_amount,
                       (sum(consumption_revenue)) as consumption_revenue,
                       (sum(recharge_amount)) as recharge_amount,
                       (sum(incoming_revenue)) as incoming_revenue,
                       sum(number_of_recharges) as number_of_recharges,
                       sum(number_of_reactivations) as number_of_reactivations,
                       sum(mou_onnet)+sum(mou_offnet_mobile)+sum(mou_offnet_fix)+sum(mou_international)+sum(mou_onnet)
                       +sum(mou_offnet_mobile)+sum(mou_offnet_fix)+sum(mou_international) as mou,
                       sum(bonus_on_consumption_revenue)+sum(bonus_on_refill_revenue)+sum(bonus_on_refill_flash_revenue) as bonus,
                       sum(total_data_revenu_amount)+sum(total_voice_revenu_amount)+sum(arpu_out_without_bonus) as revenu_amount,
                       sum(data_trafic_volume) as data_trafic,
                       max(period_id) as last_period,
                       min(period_id) as first_period,
                       device_type, flag_smartphone
 from import
 group by subscriber_id;
quit;

/*Replace missing categorical variables and apply log transformation*/
data sample;
 set imp;
 if device_type="" then device_type= "Phablet";
 if flag_smartphone ne "Y" and flag_smartphone ne "N"  then flag_smartphone= "Y";
 if churn= "Yes" then ch=1;
 else ch=0;
 bonus_revenue=log(bonus+consumption_revenue+1);
 mou=log(mou+1);
 recharge_amount=log(recharge_amount+1);
 revenu_amount=log(revenu_amount+1);
run;

title "Opérateurs";
proc sgplot data=sample;
vbar device_type / datalabel missing;
label device_type = "type";
run;

title "";
proc sgplot data=sample;
vbar flag_smartphone / datalabel missing;
label flag_smartphone = "flag dist";
run;

title "Churn contre Device_type";
proc sgplot data=sample pctlevel=group;
 vbar device_type / group=ch stat=percent missing;
run;
proc means data=sample N Nmiss mean std min P1 P5 P10 P25 P50 P75 P90 P95 P99 max;
run;

proc sort data = sample out = train_sorted;
 by ch;
run;
     
ods graphics on;
proc reg data=sample;
model ch= consumption_revenue incoming_revenue bonus mou / stb clb;
output out=stdres p= predict r = resid rstudent=r h=lev
cookd=cookd dffits=dffit;
run;
quit;
ods graphics off;
/* Print only those observations having absolute value of studentized
residual greater than 3*/

proc print data=stdres;
 title "Outlier by studentized residuals";
 var consumption_revenue incoming_revenue bonus mou;
 where abs(r)>=3;
run;

/* Cook's D - Check Outliers */
proc print data=stdres;
 title "Outliers by Cook's Distance";
 where cookd > (4/51);
 var  consumption_revenue incoming_revenue bonus mou;
run;
 /* Box-Tidwell test( testing whether the logit transform is a linear function of the predictor)*/
 /* I applied before doing the log transformation above
 proc logistic data=sample;
 title "Box-Tidwell test for logit linearity";
 model ch = consumption_revenue incoming_revenue bonus mou consumption_revenue*log(consumption_revenue) incoming_revenue*log(incoming_revenue) bonus*log(bonus) mou*log(mou);
run;*/

/*Multicollinearity test*/
 proc corr data=sample;
 var bonus_revenue incoming_revenue  mou recharge_amount first_recharge_amount data_trafic revenu_amount;
 title 'Correlation Matrix';
 run;

 proc reg data=sample;
 model ch= incoming_revenue  mou recharge_amount bonus_revenue first_recharge_amount data_trafic  revenu_amount/ vif
 tol collin;
 title 'VIF and Tol';
 run;

/* Test for AutoCorrelation - Durbin-Watson */
 proc reg data = sample;
 title "AutoCorrelation test Durbin-Watson";
 model ch= incoming_revenue  mou recharge_amount bonus_revenue first_recharge_amount data_trafic  revenu_amount / dw;
 run;
 proc univariate data=sample noprint all plot normal;
     var       bonus_revenue  ;
     histogram bonus_revenue  / normal ;
     probplot  bonus_revenue  ;
     qqplot    bonus_revenue  / normal(mu=est sigma=est color=red L=1);
     inset     mean std / cfill=blank format=5.2 ;
run;
 proc univariate data=sample noprint all plot normal;
     var       mou  ;
     histogram mou  / normal ;
     probplot  mou  ;
     qqplot    mou  / normal(mu=est sigma=est color=red L=1);
     inset     mean std / cfill=blank format=5.2 ;
run;
 data temp;
   set sample;
   n = ranuni(100);
run;

/*Sort the temporary dataset by the random values */
 proc sort data=temp;
   by n;
 run;

/* Split the data into 3 parts*/
data training valid testing;
   set temp nobs=nobs;
   if _n_ <= 0.6*nobs then output training;
   else if _n_ <= 0.8*nobs then output valid;
   else output testing;
run;

 proc freq data=training;
 table ch;
 run;

/*Replacing missing values*/
proc stdize data=training reponly method=median out=train outstat=med;
 var _numeric_;
run;
 proc stdize data=valid out=valid reponly method=mean;
 var _numeric_;
 run;
proc stdize data=testing out=test reponly method=mean;
 var _numeric_;
run;

ods graphics on;
title 'logistic model';
 proc logistic data=train descending plots=roc;
  class device_type flag_smartphone;
  model ch= number_of_recharges number_of_reactivations incoming_revenue  mou recharge_amount bonus_revenue first_recharge_amount data_trafic  revenu_amount device_type flag_smartphone /
  selection=stepwise expb stb lackfit;
  output out = tempo p=new;
  store churn_logistic;
run;
/* Testing the data*/
 proc plm source=churn_logistic;
  score data=test out=test_scored predicted=p / ilink;
 run;

proc plm source=churn_logistic;
score data=valid out=valid_scored predicted=p / ilink;
run;

/*Extracting the result*/
 data test_scored;
  set test_scored;
  if p > 0.5 then pch = 1;
  else pch = 0;
  keep subscriber_id ch pch;
 run;

/* Confusion Matrix*/
 title 'Matrice de Confusion';
 proc freq data=test_scored;
  tables ch*pch / norow nocol nopercent;
 run;
