
/*
### ### ### ### ### ### ### ### ### ### ### ### ### #
### ### ### ### ### ### ### ### ### ### ### ### ### #
###              SAS script 0_folk.sas            ###
###                Replication file               ###        
###               2023 by T. Haaga                ###
### ### ### ### ### ### ### ### ### ### ### ### ### #
### ### ### ### ### ### ### ### ### ### ### ### ### #

# Content: This script merges FOLK modules 'basic information', 
			'income', and 'family';

Inputs: folk_perus_2021_1 + folk_perus_sukup_1 + folk_tulo_2021_1 + folk_perhe_2021_1
Output folk

Libnames: */

libname fbasic "D:\ready-made\CONTINUOUS\FOLK_PERUS_C";
libname fincome "D:\ready-made\CONTINUOUS\FOLK_TULO_C";
libname ffamily "D:\ready-made\CONTINUOUS\FOLK_PERH_C";

/*###
###*/


/*### ### ### ### ### ### ### ### ### ### ### ### ### 
#### 1) Read and merge the datasets.  ####
### ### ### ### ### ### ### ### ### ### ### ### ###*/

options nolabel;


* Read the three datasets and keep only relevant variables + sort them by person id;

data basic;
	set fbasic.folk_perus_2021_1;
	keep shnro kunta31_12 kuntaryhm31_12 ika maka ututku_aste ptoim1;
run;

proc sort Data=basic;
	by shnro;
run;

data gender;
	set fbasic.folk_perus_sukup_1;
run;

proc sort Data=gender;
	by shnro;
run;

data income;
	set fincome.folk_tulo_2021_1;
	keep shnro kturaha toimtu;
run;

proc sort Data=income;
	by shnro;
run;

data family;
	set ffamily.folk_perhe_2021_1;
	keep shnro petu;
run;

proc sort Data=family;
	by shnro;
run;


* Next, merge all datasets datasets by person id;

data folk;
	merge basic gender income family;
	by shnro;
run;

* keep only those who are observed to have municipality;

data folk;
	set folk;
	where not missing(kunta31_12);
run;

* if family id is not observed, use person id;

proc sql;
	alter table folk
	modify petu char(16) format=$16.;
quit;

data folk;
	set folk;
	if petu=. then petu=shnro;
	*'00000000' = Person does not belong to the family population but;
	* does have a adopted/biological child;
	if petu='00000000' then petu=shnro; 
run;


/*### ### ### ### ### ### ### ### ### ### ### ### ### 
#### 2) Compute equivalized family disposable income.  ####
### ### ### ### ### ### ### ### ### ### ### ### ###*/

* Now, calculate for each family the sum of disposable income;
* Then, calculate the equivalized family disposable income;

proc sort data=folk;
	by petu;
run;

proc sql;
	create table family_sum as
	select petu, sum(kturaha) as kturaha_petu,
		sum(kturaha) / (1+sum(ika<14)*0.3+0.5*(count(ika)-1-sum(ika<14))) as kturaha_ekv,
		count(ika) as pekoko
	from folk
	group by petu;
quit;

data family_sum;
	set family_sum;
	keep petu kturaha_ekv;
run;

* Take only unique rows;

proc sort data=family_sum NODUPRECS;
	by _all_;
run;

* Merge;

data folk;
	merge folk family_sum;
	by petu;
run;

* There was 1 municipal merger in 2022;
* Replace the municipality number of a merged municipality by the number of the new municipality;

data folk;
	set folk;
	if kunta31_12='099' then kunta31_12='214'; *2021;
run;

proc sort data=folk;
	by shnro;
run;


/*### ### ### ### ### ### ### ### ### ### ### ### ### 
#### 3) Save.  ####
### ### ### ### ### ### ### ### ### ### ### ### ###*/

proc export data=folk
	outfile= "W:\DIGISOTE\data\interim\folk.csv"
	dbms=csv;
run;

* End;
