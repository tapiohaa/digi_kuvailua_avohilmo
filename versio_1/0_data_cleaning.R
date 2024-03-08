
### ### ### ### ### ### ### ### ### ### ### ### ### #
### ### ### ### ### ### ### ### ### ### ### ### ### #
###           r-script 0_data_cleaning.R          ###
###                Replication file               ###        
###               2023 by T. Haaga                ###
### ### ### ### ### ### ### ### ### ### ### ### ### #
### ### ### ### ### ### ### ### ### ### ### ### ### #

# Content: Link TOPI and SOTE to the data on PHC contacts, and clean the FOLK
# data and the PHC contacts.
rm(list=ls())

# Install and load the following packages:
library(here)             # Relative file paths.
library(data.table)       # Mutating and aggregating data.
library(readxl)           # Reading xlsx files.

# Inputs:
input_folk = here::here('data', 'interim', 'folk.csv')
input_topi = here::here('data', 'raw', 'topi_unit_register_2022_06.csv')
input_sote = here::here('data', 'raw', 'sote_2022_02.xlsx')
input_visits = here::here('data', 'interim', 'visits_20') # visits_20XX.csv, XX in 20:22

# Outputs:
output_folk = here::here('data', 'interim', 'folk_clean.rds')
output_visits = here::here('data', 'interim', 'visits_clean_20') # visits_clean_20XX.rds, XX in 20:22


###
###


### ### ### ### ### ### ### ### ### ### ### ### ### 
#### 1) Tidy FOLK data. ####
### ### ### ### ### ### ### ### ### ### ### ### ###

folk = data.table::fread(input_folk, encoding='UTF-8')
setnames(folk, old='petu', new='petu_old')

# Replace family IDs with a more memory efficient integer:
family.ids = unique(folk[, .(petu_old)])[order(petu_old)]
family.ids[, petu := 1:nrow(family.ids)]
folk = merge(folk, family.ids, by='petu_old', all.x = TRUE)
folk[, petu_old := NULL]

# An indicator for being female:
folk[, nainen := as.integer(sukup == 2)]
folk[, sukup := NULL]

# Social assistance should be non-negative:
folk[toimtu < 0, toimtu := NA]

# An indicator for someone having received social assistance in the family:
assistance.petu = folk[, .(toimtu_petu = sum(toimtu, na.rm = TRUE)), by='petu']
assistance.petu[, toimtu_petu := as.integer(toimtu_petu > 0)]
folk = merge(folk, assistance.petu, by='petu', all.x = TRUE)
folk[, ':=' (toimtu = NULL, kturaha = NULL, kuntaryhm31_12 = NULL)]

saveRDS(folk, file=output_folk)
rm(folk)


### ### ### ### ### ### ### ### ### ### ### ### ### 
#### 2) TOPI unit register and SOTE organization register. ####
### ### ### ### ### ### ### ### ### ### ### ### ###


# TOPI: read data on TOPI units

topi = data.table::fread(
  input_topi, encoding='UTF-8',
  select = c('topi_code', 'year', 'service_area_code')
)

topi = topi[year==2022]

# Service area codes for health centers (120, 121, and 122):
topi = 
  topi[, terveysasema := 
         as.integer(grepl('120', service_area_code, fixed = TRUE) |
                      grepl('121', service_area_code, fixed = TRUE) |
                      grepl('122', service_area_code, fixed = TRUE))]

# Service area codes for private practice (261 and 262):
topi = 
  topi[, laakariasema := 
         as.integer(grepl('261', service_area_code, fixed = TRUE) |
                      grepl('262', service_area_code, fixed = TRUE))]

# Service area codes for occupational healthcare (250):
topi = 
  topi[, tyoterveys := 
         as.integer(grepl('250', service_area_code, fixed = TRUE))]

# Group by TOPI code:
topi = topi[, .(terveysasema = as.integer(sum(terveysasema) > 0),
                laakariasema = as.integer(sum(laakariasema) > 0),
                tyoterveys = as.integer(sum(tyoterveys) > 0)),
            by = 'topi_code']


# SOTE: Read data on social and healthcare organizations:
sote = setDT(readxl::read_excel(input_sote))
sote = sote[, .(Tunniste, Sektori)]
sote[, julkinen := as.integer(Sektori %in% c('1 Julkinen', '1 julkinen'))]
sote[, Sektori := NULL]
setnames(sote, old='Tunniste', new='sote_code')


### ### ### ### ### ### ### ### ### ### ### ### ### 
#### 3) Primary care contacts. ####
### ### ### ### ### ### ### ### ### ### ### ### ###

years = c(20:22)


# First, prepare to replace professional IDs with memory efficient integers:

professional.ids = lapply(years, function(yr) {
  print(yr)
  
  # Read visits:
  source = paste(input_visits, yr, ".csv", sep="")
  df = data.table::fread(source, select='skaynti_toteuttaja')
  df = unique(df)
  
  return(df)
  
})

professional.ids = rbindlist(professional.ids)
professional.ids = unique(professional.ids)
professional.ids = professional.ids[nchar(trimws(skaynti_toteuttaja)) > 0]
professional.ids[, kaynti_toteuttaja := 1:nrow(professional.ids)]


# Read primary care contacts:

visits = lapply(years, function(yr) {
  print(yr)
  
  # Read visits:
  source = paste(input_visits, yr, ".csv", sep="")
  df = data.table::fread(source)
  setnames(df, tolower(names(df)))
  
  # Link topi, sote, and visits via topi_code and sote_code:
  df = merge(df, topi, by.x='palveluntuottaja', by.y='topi_code', all.x = TRUE)
  df = merge(df, sote, by.x='palveluntuottaja_yksikko', by.y='sote_code', 
             all.x = TRUE)
  
  # Link professional.ids and visits via skaynti_toteuttaja:
  df = merge(df, professional.ids, by='skaynti_toteuttaja', all.x = TRUE)
  
  df[, ':=' (palveluntuottaja = NULL, palveluntuottaja_yksikko = NULL,
             skaynti_toteuttaja = NULL)]
  
  # How well did the linking work?
  print(100 * colMeans(is.na(df)))
  
  # Create an indicator for telemedicine contacts:
  df[, telemedicine := 
       as.integer(kaynti_yhteystapa %in% c('R50', 'R51', 'R52', 'R55', 'R56'))]
  
  # Impute -1 for missing values:
  df[is.na(terveysasema), terveysasema := -1]
  df[is.na(laakariasema), laakariasema := -1]
  df[is.na(tyoterveys), tyoterveys := -1]
  df[is.na(julkinen), julkinen := -1]
  
  # Transform character datetimes to datetimes:
  setnames(df, old=c('kaynti_alkoi', 'kaynti_loppui'),
           new=c('kaynti_alkoi_old', 'kaynti_loppui_old'))
  
  df[, ':=' (kaynti_alkoi = as.POSIXct(as.character(kaynti_alkoi_old), 
                                       format = "%d.%m.%Y %H:%M"),
             kaynti_loppui = as.POSIXct(as.character(kaynti_loppui_old), 
                                        format = "%d.%m.%Y %H:%M"))]
  
  
  df[, ':=' (kaynti_alkoi_old = NULL, kaynti_loppui_old = NULL)]
  
  # Save:
  source = paste(output_visits, yr, ".rds", sep="")
  saveRDS(df, file=source)
  
})

# End.
