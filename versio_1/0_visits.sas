
/*
### ### ### ### ### ### ### ### ### ### ### ### ### #
### ### ### ### ### ### ### ### ### ### ### ### ### #
###            SAS script 0_visits.sas            ###
###                Replication file               ###        
###               2024 by T. Haaga                ###
### ### ### ### ### ### ### ### ### ### ### ### ### #
### ### ### ### ### ### ### ### ### ### ### ### ### #

Content: Extract data from primary care contacts from Avohilmo for 2020-2022.

Inputs: tutkpalv_u1418_ammatti tutkpalv_u1418_ammattioikeudet
		avohilmo_2021_Y_s where y in (1:2)
		
Output: visits_20xx where xx in (20:22)

Libnames: */

libname hilmo "D:\d66\external";
libname hilmot "D:\d66\external\THL21_laakemaar2122_toitu19";

/*###
###*/


/*### ### ### ### ### ### ### ### ### ### ### ### ### 
#### 1) Load classifications for doctors and nurses. ####
### ### ### ### ### ### ### ### ### ### ### ### ###*/

* TK ammattiluokitus 2001;
data tk_codes(drop=avo_raportointiryhma_nimi);
	set hilmo.tutkpalv_u1418_ammatti;
	where avo_raportointiryhma_koodi in ('10','30'); * Doctors and nurses;
	rename tarkin_taso_koodi = ammatti;
run;

proc sort data=tk_codes;
	by avo_raportointiryhma_koodi;
run;

* Valvira ammattioikeudet 2008;
data valv_codes(drop=avo_raportointiryhma_nimi);
	set hilmo.tutkpalv_u1418_ammattioikeudet;
	where avo_raportointiryhma_koodi in ('10','30'); * Doctors and nurses;
	rename ammattioikeus_koodi = kaynti_ammattioikeus;
run;

proc sort data=valv_codes;
	by avo_raportointiryhma_koodi;
run;

/*
TK ammattiluokitus 2001:
Doctors: '222','2221','22211','22212','22213'
Nurses: '323','3231','32311','32312','32313','32314','32315','3232'

Valvira ammattioikeudet 2008
Doctors: '000','001','002','031','032','034','701','702','717','718','720','810','811','900','901','724','910','812'
Nurses: '100','300','400','503','508','509','710','730','740','780','790','800','820','727','728','803'
*/


/*### ### ### ### ### ### ### ### ### ### ### ### ### 
#### 2) Read and mutate data from 2020-2022. ####
### ### ### ### ### ### ### ### ### ### ### ### ###*/

%MACRO read_data;
%DO i = 1 %TO 2;

data visits_&i.;
	set hilmot.avohilmo_2021_&i._s;

	/* Not all data are read:
		1) IDs must be observed.
		2) Time stamp for the start of the visit must be observed.
		3) The contact was not cancelled. 
		4) We only take outpatient care (T11) and curative occupational healthcare (T31).
		5) Of the contacts, we take those visits where the client physically
			visited professional or telemedicine (either in real time or no real-time contact) */
		
	where not missing(shnro) 
		and not missing(kaynti_alkoi) 
		and missing(peruutus_ajankohta) and missing(peruutus_syy)
		and kaynti_palvelumuoto in ('T11', 'T31')
		and kaynti_yhteystapa in ('R10', 'R50', 'R51', 'R52', 'R55', 'R56');
	
	/* Create variable profession such that:
		1 = doctors
		2 = nurses and public health nurses 
		-1 = other or missing */

	ammatti = put(kaynti_ammatti, 6. -L); 

	if ammatti in ('222','2221','22211','22212','22213') or 
		kaynti_ammattioikeus in ('000','001','002','031','032','034','701','702','717','718','720','810','811','900','901','724','910','812') 
		then ammattiluokka = 1;
	else if ammatti in ('323','3231','32311','32312','32313','32314','32315') or
		kaynti_ammattioikeus in ('100','300','400','503','508','509','710','730','740','780','790','800','820','727','728','803') 
		then ammattiluokka = 2;
	else if missing(ammatti) and missing(kaynti_ammattioikeus) then ammattiluokka = -1;
	else ammattiluokka = -1;
	
	* Create a variable for weekday, month and year of the visit;
	visits_date = input(substr(strip(kaynti_alkoi), 1, 10), DDMMYY10.);
	vuosi = year(visits_date);
	
	* Create a variable that has value 1 for curative care, 0 for preventive care;
	* and -1 or missing type;
	if kaynti_luonne = 'SH' then kaynti_luonne_sh = 1;
	else if kaynti_luonne = 'TH' then kaynti_luonne_sh = 0;
	else kaynti_luonne_sh = -1;

	* Keep only relevant variables;
	keep shnro palveluntuottaja palveluntuottaja_yksikko kaynti_alkoi kaynti_loppui ammattiluokka 
		skaynti_toteuttaja kaynti_palvelumuoto kaynti_yhteystapa kaynti_luonne_sh vuosi;

run;

%END;
%MEND read_data;

%read_data;


* Concatenate;

data visits;
	set visits_1 visits_2;
run;


%MACRO visits_main;

%DO i = 2020 %TO 2022;

* Subset the data;
data visits_&i;
	set visits;
	where vuosi = &i;
	drop vuosi;
run;

* Save to cvs;
proc export data=visits_&i
	outfile= "W:\DIGISOTE\data\interim\visits_&i..csv"
	dbms=csv;
run;

%END;
%MEND visits_main;

%visits_main;

* End;
