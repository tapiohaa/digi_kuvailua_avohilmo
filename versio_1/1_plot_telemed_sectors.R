
### ### ### ### ### ### ### ### ### ### ### ### ### #
### ### ### ### ### ### ### ### ### ### ### ### ### #
###       r-script 1_plot_telemed_sectors.R       ###
###                Replication file               ###        
###               2024 by T. Haaga                ###
### ### ### ### ### ### ### ### ### ### ### ### ### #
### ### ### ### ### ### ### ### ### ### ### ### ### #

# Content: Describe the users of telemedicine by different sectors and 
# describe professionals in different sectors.
rm(list=ls())

# Install and load the following packages:
library(here)             # Relative file paths.
library(data.table)       # Mutating and aggregating data.
library(ggplot2)          # Plotting data.
library(scales)           # Scale functions for visualizations.
library(patchwork)        # Print multiple plots into same figure.

# Inputs:
input_folk = here::here('data', 'interim', 'folk_clean.rds')
input_visits = here::here('data', 'interim', 'visits_clean_20') # visits_clean_20XX.rds, XX in 20:22

# Outputs:
output.prof.1 = "W:/DIGISOTE/ETAKAYNNIT/figures/ammattilaiset_etaosuus.pdf"
output.prof.2 = 
  "W:/DIGISOTE/ETAKAYNNIT/figures/ammattilaiset_kunnat_asiakkaat.pdf"
output.prov.1 = "W:/DIGISOTE/ETAKAYNNIT/figures/kaynnit_viikonpaivittain.pdf"
output.prov.2 = "W:/DIGISOTE/ETAKAYNNIT/figures/kaynnit_tunneittain.pdf"
output.prov.3 = "W:/DIGISOTE/ETAKAYNNIT/figures/aukioloajat_asiakkaat.pdf"
output.pat.1 = "W:/DIGISOTE/ETAKAYNNIT/figures/etaosuus_nainen_ptoim.pdf"
output.pat.2 = "W:/DIGISOTE/ETAKAYNNIT/figures/etaosuus_ika.pdf"
output.pat.3 = "W:/DIGISOTE/ETAKAYNNIT/figures/etaosuus_kturaha.pdf"
output.pat.4 = "W:/DIGISOTE/ETAKAYNNIT/figures/etaosuus_maka.pdf"
output.pat.5 = "W:/DIGISOTE/ETAKAYNNIT/figures/etaosuus_ututku.pdf"
output.pat.6 = "W:/DIGISOTE/ETAKAYNNIT/figures/asiakkaat_nainen_ptoim.pdf"
output.pat.7 = "W:/DIGISOTE/ETAKAYNNIT/figures/asiakkaat_ika.pdf"
output.pat.8 = "W:/DIGISOTE/ETAKAYNNIT/figures/asiakkaat_kturaha.pdf"
output.pat.9 = "W:/DIGISOTE/ETAKAYNNIT/figures/asiakkaat_maka.pdf"
output.pat.10 = "W:/DIGISOTE/ETAKAYNNIT/figures/asiakkaat_ututku.pdf"

###
###


### ### ### ### ### ### ### ### ### ### ### ### ### 
#### 1) Read and tidy datasets. ####
### ### ### ### ### ### ### ### ### ### ### ### ###


#### Data on primary health care. ####

# Read PHC data from 6/2021-5/2022:

years = c(21:22)

phc = lapply(years, function(yr) {
  print(yr)
  source = paste(input_visits, yr, ".rds", sep="")
  df = readRDS(source)
})
phc = rbindlist(phc)

phc[, date := as.Date(kaynti_alkoi)]
phc = phc[date >= as.Date('2021-06-01') & date < as.Date('2022-06-01')]

# Patients' municipality of residence and other characteristics:
folk = readRDS(input_folk)
phc = merge(phc, folk[, .(shnro, kunta31_12)], by='shnro', all.x = TRUE)


# Restrict to curative care.
# Curative / preventive indicator is rarely missing:
phc[, .(missing.kaynti_sh = 100 * mean(kaynti_luonne_sh==-1)), by='julkinen']
phc = phc[kaynti_luonne_sh == 1]


# Add time and hour to the data:
phc[, ':=' (hour = as.integer(format(kaynti_alkoi, '%H')),
            time = format(kaynti_alkoi, '%H:%M:%S'))]

# It may be that minutes are not always coded:
phc[, .N, by='time'][order(N)]
phc[, time := NULL]


# Define a function to map weekday names to numbers
weekday_to_number = function(weekday) {
  switch(weekday,
         'Monday' = 1,
         'Tuesday' = 2,
         'Wednesday' = 3,
         'Thursday' = 4,
         'Friday' = 5,
         'Saturday' = 6,
         'Sunday' = 7)
}

# Add a new column with weekday numbers:
phc[, weekday_no := sapply(weekdays(date), weekday_to_number)]


#### Profession (doctor or nurse). ####

# Some professional IDs are observed both as a nurse and a doctor.
# Missing professional IDs are mainly from public sector organizations:
phc[, .(missing.toteuttaja = 100*mean(is.na(kaynti_toteuttaja))), by='julkinen']

# For each professional, select whether the professional is doctor or nurse:

data = phc[!is.na(kaynti_toteuttaja), .(visits = .N), 
           by=c('kaynti_toteuttaja', 'ammattiluokka')]

# Most of the professionals have only one profession observed:
data[, .N, by = 'kaynti_toteuttaja'][, .N, by='N']

# Pivot wider:
data = dcast(data, kaynti_toteuttaja ~ paste0('prof_', ammattiluokka), 
             value.var = 'visits')

# Unique doctors:
data[is.na(`prof_-1`) & is.na(prof_2) & prof_1 > 0, profession := 1]

# Unique nurses:
data[is.na(`prof_-1`) & is.na(prof_1) & prof_2 > 0, profession := 2]

# Not nurses (thus doctors):
data[is.na(prof_2) & prof_1 > 0 & `prof_-1` > 0, profession := 1]

# Not doctors (thus nurses):
data[is.na(prof_1) & prof_2 > 0 & `prof_-1` > 0, profession := 2]

# Those who are observed both as a nurse and a doctor: the person is a doctor
# (nurse) if the number of observations as a doctor (nurse) is at least twice
# the number of observations as nurse (doctor).
data[prof_1 > 0 & prof_2 > 0 & prof_1 / prof_2 >= 2, profession := 1]
data[prof_1 > 0 & prof_2 > 0 & prof_2 / prof_1 >= 2, profession := 2]

# Missing:
data[`prof_-1` > 0 & is.na(prof_1) & is.na(prof_2), profession := NA_integer_]

# Merge into phc:
phc = merge(phc, data[, .(kaynti_toteuttaja, profession)], 
            by='kaynti_toteuttaja', all.x=TRUE)


#### Socioeconomic data. ####

# Tidy city-countryside classification:
dt = data.table(
  maka = c("", "K1", "K2", "K3", "M4", "M5", "M6", "M7"),
  maka_no = c(NA_integer_, 1:7)
)
folk = merge(folk, dt, by='maka', all.x = TRUE)
setnames(folk, old='maka', new='maka_label')

# Tidy age classification:
folk[ika %in% c(0:9), ':=' (ika_no = 1, ika_label = '0-9')]
folk[ika %in% c(10:19), ':=' (ika_no = 2, ika_label = '10-19')]
folk[ika %in% c(20:29), ':=' (ika_no = 3, ika_label = '20-29')]
folk[ika %in% c(30:39), ':=' (ika_no = 4, ika_label = '30-39')]
folk[ika %in% c(40:49), ':=' (ika_no = 5, ika_label = '40-49')]
folk[ika %in% c(50:59), ':=' (ika_no = 6, ika_label = '50-59')]
folk[ika %in% c(60:69), ':=' (ika_no = 7, ika_label = '60-69')]
folk[ika %in% c(70:79), ':=' (ika_no = 8, ika_label = '70-79')]
folk[ika %in% c(80:89), ':=' (ika_no = 9, ika_label = '80-89')]
folk[ika >= 90, ':=' (ika_no = 10, ika_label = '> 90')]

# Tidy gender:
folk[nainen==1, nainen_label := 'Naiset']
folk[nainen==0, nainen_label := 'Miehet']
setnames(folk, old='nainen', new='nainen_no')

# Add income decile:
folk = folk[order(kturaha_ekv)]
folk[, kturaha_no := 
       .bincode(kturaha_ekv,
                quantile(kturaha_ekv, probs = 0:10/10, na.rm=TRUE), 
                right = FALSE, include.lowest = TRUE)]
folk[, kturaha_label := kturaha_no]

folk[, ':=' (petu = NULL, toimtu_petu = NULL)]

# Tidy education level (only those aged 35 or more):
folk[is.na(ututku_aste), ututku_aste := 0]
folk[ututku_aste==0 & ika >= 35, 
     ':=' (ututku_label = 'Tieto puuttuu', ututku_no = 1)]
folk[ututku_aste==3 & ika >= 35, 
     ':=' (ututku_label = 'Toinen aste', ututku_no = 2)]
folk[ututku_aste==4 & ika >= 35, 
     ':=' (ututku_label = 'Erikoisammatti', ututku_no = 3)]
folk[ututku_aste==5 & ika >= 35, 
     ':=' (ututku_label = 'Alin korkea', ututku_no = 4)]
folk[ututku_aste==6 & ika >= 35, 
     ':=' (ututku_label = 'Alempi korkea', ututku_no = 5)]
folk[ututku_aste==7 & ika >= 35, 
     ':=' (ututku_label = 'Ylempi korkea', ututku_no = 6)]
folk[ututku_aste==8 & ika >= 35, 
     ':=' (ututku_label = 'Tutkija', ututku_no = 7)]


# Type of activity:
# - those aged 0-14, pupils, students, conscripts:
folk[ptoim1 %in% c(21, 22, 25), ':=' (ptoim_no = 1, ptoim_label = 'Nuoret')]
# - employed:
folk[ptoim1 %in% c(11), ':=' (ptoim_no = 2, ptoim_label = 'Työlliset')]
# - unemployed:
folk[ptoim1 %in% c(12, 29), ':=' (ptoim_no = 3, ptoim_label = 'Työttömät')]
# - pensioners:
folk[ptoim1 %in% c(24), ':=' (ptoim_no = 4, ptoim_label = 'Eläkeläiset')]


### ### ### ### ### ### ### ### ### ### ### ### ### 
#### 2) Professionals' perspective. ####
### ### ### ### ### ### ### ### ### ### ### ### ###


# We will plot the following specifications (three sectors; doctors and nurses):

specs = list(
  list(profession=1, julkinen=1, kaynti_palvelumuoto='T11', 
       sektori='Julkinen avosairaanhoito', ammatti='Lääkärit'),
  list(profession=1, julkinen=0, kaynti_palvelumuoto='T11',
       sektori='Yksityinen avosairaanhoito', ammatti='Lääkärit'),
  list(profession=1, julkinen=c(1,0,-1), kaynti_palvelumuoto='T31',
       sektori='Työterveys (sairaanhoito)', ammatti='Lääkärit'),
  list(profession=2, julkinen=1, kaynti_palvelumuoto='T11',
       sektori='Julkinen avosairaanhoito', ammatti='hoitajat'),
  list(profession=2, julkinen=0, kaynti_palvelumuoto='T11',
       sektori='Yksityinen avosairaanhoito', ammatti='hoitajat'),
  list(profession=2, julkinen=c(1,0,-1), kaynti_palvelumuoto='T31',
       sektori='Työterveys (sairaanhoito)', ammatti='hoitajat')
)


# This loop extracts and mutates the data and plots the figures.

plots = lapply(specs, function(x) {
  
  # Extract the data. The professional ID should be observed.
  data = phc[!is.na(kaynti_toteuttaja) & 
               profession %in% x$profession & 
               julkinen %in% x$julkinen &
               kaynti_palvelumuoto %in% x$kaynti_palvelumuoto]
  
  # Count the number of unique municipalities of residence of the patients:
  data.1 = data[, .(kunnat = length(unique(kunta31_12))), 
                by='kaynti_toteuttaja']
  
  # Count the number of visits and the share of telemedicine:
  data.2 = data[, .(visits = .N), by=c('kaynti_toteuttaja', 'telemedicine')]
  data.2 = dcast(data.2, kaynti_toteuttaja ~ paste0('telemed_', telemedicine), 
                 value.var = 'visits')
  data.2[is.na(telemed_1), telemed_1 := 0]
  data.2[is.na(telemed_0), telemed_0 := 0]
  data.2[, ':=' (eta_osuus = 100 * telemed_1 / (telemed_1 + telemed_0),
                 kaynnit = telemed_1 + telemed_0)]
  
  # Take only those professionals that have at least 10 visits:
  data.2 = data.2[kaynnit >= 10]
  
  # Merge:
  data = merge(data.2, data.1, by='kaynti_toteuttaja', all.x = TRUE)
  sample_size = nrow(data)
  
  # Create deciles for each outcome variable:
  
  data = data[order(kunnat)]
  data[, decile_kunnat := .bincode(kunnat,
                                   quantile(kunnat, probs = 0:10/10), 
                                   right = FALSE, include.lowest = TRUE)]
  
  data = data[order(kaynnit)]
  data[, decile_kaynnit := .bincode(kaynnit,
                                    quantile(kaynnit, probs = 0:10/10), 
                                    right = FALSE, include.lowest = TRUE)]
  
  data = data[order(eta_osuus)]
  data[, decile_eta := .bincode(eta_osuus,
                                quantile(eta_osuus, probs = 0:10/10), 
                                right = FALSE, include.lowest = TRUE)]
  
  # Compute decile-specific means:
  data.1 = data[, .(otc = mean(kunnat), N.decile = .N), by='decile_kunnat']
  data.2 = data[, .(otc = mean(kaynnit), N.decile = .N), by='decile_kaynnit']
  data.3 = data[, .(otc = mean(eta_osuus), N.decile = .N), by='decile_eta']
  
  # Problems occur if the breaks have non-unique values. Fix this manually.
  if(!(1 %in% data[, unique(decile_eta)])) {
    data.3 = rbind(data.3, data.table(decile_eta=1, otc=0), fill=TRUE) }
  if(!(2 %in% data[, unique(decile_eta)])) {
    data.3 = rbind(data.3, data.table(decile_eta=2, otc=0), fill=TRUE) }
  
  # Tidy and bind rows:
  setnames(data.1, old='decile_kunnat', new='decile')
  setnames(data.2, old='decile_kaynnit', new='decile')
  setnames(data.3, old='decile_eta', new='decile')
  data.1[, ':=' (outcome = 'kunnat', mean = data[, mean(kunnat)])]
  data.2[, ':=' (outcome = 'kaynnit', mean = data[, mean(kaynnit)])]
  data.3[, ':=' (outcome = 'eta_osuus', mean = data[, mean(eta_osuus)])]
  data = rbind(data.1, data.2, data.3)
  data[, N := sample_size]
  
  # The smallest group has the following number of observations:
  print(data[, min(N.decile, na.rm = TRUE)])
  
  # We have two outcomes:
  outcomes = list(
    list(otc='kunnat', ylab='Potilaita X eri kunnasta'),
    list(otc='eta_osuus', ylab='Etäkontaktien osuus (%)')
  )
  
  # Colour palette for different sectors:
  if(x$sektori=='Julkinen avosairaanhoito') {
    color.sektori = '#440154'
  } else if(x$sektori=='Yksityinen avosairaanhoito') {
    color.sektori = '#21918c'
  } else if (x$sektori=='Työterveys (sairaanhoito)') {
    color.sektori = '#fde725' }

  
  # Finally, plot:
  
  plots = lapply(outcomes, function(i) {
    
    p = ggplot(data=data[outcome==i$otc], aes(x=decile, y=otc)) +
      geom_bar(stat='identity', fill=color.sektori, color='black', alpha=0.8) +
      geom_text(aes(label = round(otc, digits=0)), vjust=-0.5, size=5) +
      geom_hline(yintercept = data[outcome==i$otc, unique(mean)]) +
      annotate(geom='text', x=1, y=data[outcome==i$otc, unique(mean)],
               label=paste('Ka.', round(data[outcome==i$otc, unique(mean)],
                                        digits=0)),
               size=5, vjust=-0.5) + 
      scale_x_continuous(breaks= c(1:10)) +
      labs(title = x$sektori, y = i$ylab, x = 'Desiili') +
      ylim(0, 1.1 * data[outcome==i$otc, max(otc)]) +
      annotate(geom='label', x=2, y= data[outcome==i$otc, max(otc)],
               label = paste('N:', data[outcome==i$otc, unique(sample_size)]),
               size = 5, vjust = -0.5) +
      theme(text = element_text(size=20),
            panel.background = element_rect(fill = 'white', colour = 'white'),
            panel.grid.major = element_line(size=0.25, linetype = 'solid',
                                            color = 'lightgrey'),
            panel.grid.minor = element_line(size=0.25, linetype = 'solid',
                                            color = 'lightgrey'),
            panel.border = element_rect(colour='black', fill=NA, size=0.5))
  })
  
})


# Save plots:

ggsave(
  filename = output.prof.1, width = 15, height = 12,
  plot = 
    wrap_elements(panel = plots[[1]][[2]] + plots[[2]][[2]] + plots[[3]][[2]]) + 
      ggtitle('A. Lääkärit') + 
      theme(plot.title = element_text(size=35, hjust=0.5)) +
    wrap_elements(panel = plots[[4]][[2]] + plots[[5]][[2]] + plots[[6]][[2]]) + 
      ggtitle('B. Sairaanhoitajat, terveydenhoitajat ym.') + 
      plot_layout(ncol=1) +
      theme(plot.title = element_text(size=35, hjust=0.5)))

ggsave(
  filename = output.prof.2, width = 15, height = 12,
  plot = 
    wrap_elements(panel = plots[[1]][[1]] + plots[[2]][[1]] + plots[[3]][[1]]) + 
      ggtitle('A. Lääkärit') + 
      theme(plot.title = element_text(size=35, hjust=0.5)) +
    wrap_elements(panel = plots[[4]][[1]] + plots[[5]][[1]] + plots[[6]][[1]]) + 
      ggtitle('B. Sairaanhoitajat, terveydenhoitajat ym.') + 
      plot_layout(ncol=1) +
      theme(plot.title = element_text(size=35, hjust=0.5)))


### ### ### ### ### ### ### ### ### ### ### ### ### 
#### 3) Providers' perspective. ####
### ### ### ### ### ### ### ### ### ### ### ### ###


# We will plot the following specifications (3 sectors; telemedicine or not):

specs = list(
  list(telemedicine=1, julkinen=1, kaynti_palvelumuoto='T11', 
       sektori='Julkinen avosairaanhoito', kayntityyppi='Etäasioinnit'),
  list(telemedicine=1, julkinen=0, kaynti_palvelumuoto='T11',
       sektori='Yksityinen avosairaanhoito', kayntityyppi='Etäasioinnit'),
  list(telemedicine=1, julkinen=c(1,0,-1), kaynti_palvelumuoto='T31',
       sektori='Työterveys (sairaanhoito)', kayntityyppi='Etäasioinnit'),
  list(telemedicine=0, julkinen=1, kaynti_palvelumuoto='T11',
       sektori='Julkinen avosairaanhoito', kayntityyppi='Vastaanottokäynnit'),
  list(telemedicine=0, julkinen=0, kaynti_palvelumuoto='T11',
       sektori='Yksityinen avosairaanhoito', kayntityyppi='Vastaanottokäynnit'),
  list(telemedicine=0, julkinen=c(1,0,-1), kaynti_palvelumuoto='T31',
       sektori='Työterveys (sairaanhoito)', kayntityyppi='Vastaanottokäynnit')
)


### Visits per weekday ###

plots = lapply(specs, function(x) {
  
  # Extract data:
  dt = phc[telemedicine %in% x$telemedicine &
             julkinen %in% x$julkinen &
             kaynti_palvelumuoto %in% x$kaynti_palvelumuoto]
  
  # Count the number of visits per weekday:
  dt.wd = dt[, .(visits = .N), by='weekday_no']
  
  # The shares of how many visits are on a given weekday:
  dt.wd[, visits.share := 100 * visits / nrow(dt)]
  
  # Divide visits by 1000:
  dt.wd[, visits := visits / 1000]
  
  # Sample size:
  sample_size = nrow(dt)
  
  # The smallest group has the following number of observations:
  print(dt.wd[, min(1000* visits)])
  
  # Colour palette for different sectors:
  if(x$sektori=='Julkinen avosairaanhoito') {
    color.sektori = '#440154'
  } else if(x$sektori=='Yksityinen avosairaanhoito') {
    color.sektori = '#21918c'
  } else if (x$sektori=='Työterveys (sairaanhoito)') {
    color.sektori = '#fde725' }

  
  # Plot:
  
  ggplot(data=dt.wd, aes(x=weekday_no, y=visits)) +
    geom_bar(stat='identity', fill=color.sektori, color='black', alpha=0.8) +
    geom_text(aes(label = paste(round(visits.share, digits=0), '%', sep='')), 
              vjust = -0.5, size = 5) +
    annotate(geom='label', x=4, y= dt.wd[, max(visits)],
             label = paste('N:', sample_size),
             size = 5, vjust = -0.5) +
    scale_x_continuous(breaks= c(1:7),
                       labels= c('Ma', 'Ti', 'Ke', 'To', 'Pe', 'La', 'Su')) +
    labs(title = x$sektori,
         y = 'Palvelutapahtumia (tuhansia)') +
    ylim(0, 1.1 * dt.wd[, max(visits)]) +
    theme(text = element_text(size=20),
          axis.title.x = element_blank(),
          panel.background = element_rect(fill = 'white', colour = 'white'),
          panel.grid.major = element_line(size=0.25, linetype = 'solid',
                                          color = 'lightgrey'),
          panel.grid.minor = element_line(size=0.25, linetype = 'solid',
                                          color = 'lightgrey'),
          panel.border = element_rect(colour='black', fill=NA, size=0.5))
  
})

# Save plots:

ggsave(
  filename = output.prov.1, width = 15, height = 12,
  plot = 
    wrap_elements(panel = plots[[1]] + plots[[2]] + plots[[3]]) + 
      ggtitle('A. Etäasioinnit') + 
      theme(plot.title = element_text(size=35, hjust=0.5)) +
    wrap_elements(panel = plots[[4]] + plots[[5]] + plots[[6]]) + 
      ggtitle('B. Vastaanottokäynnit') + plot_layout(ncol=1) +
      theme(plot.title = element_text(size=35, hjust=0.5))) 


### Visits per hour ###

plots = lapply(specs, function(x) {
  
  # Extract data:
  dt = phc[telemedicine %in% x$telemedicine &
             julkinen %in% x$julkinen &
             kaynti_palvelumuoto %in% x$kaynti_palvelumuoto]
  
  # Count the number of visits per hour:
  dt.hr = dt[, .(visits = .N), by='hour']
  
  # The shares of how many visits are on a given hour:
  dt.hr[, visits.share := 100 * visits / nrow(dt)]
  
  # Divide visits by 1000:
  dt.hr[, visits := visits / 1000]
  
  # Sample size:
  sample_size = nrow(dt)
  
  # The smallest group has the following number of observations:
  print(dt.hr[, min(1000* visits)])
  
  # Colour palette for different sectors:
  if(x$sektori=='Julkinen avosairaanhoito') {
    color.sektori = '#440154'
  } else if(x$sektori=='Yksityinen avosairaanhoito') {
    color.sektori = '#21918c'
  } else if (x$sektori=='Työterveys (sairaanhoito)') {
    color.sektori = '#fde725' }
  
  # Plot:
  
  ggplot(data=dt.hr, aes(x=hour, y=visits)) +
    geom_bar(stat='identity', fill=color.sektori, color='black', alpha=0.8) +
    geom_text(aes(label = paste(round(visits.share, digits=0), '%', sep='')), 
              vjust = -0.5, size = 3) +
    annotate(geom='label', x=1, y= dt.hr[, max(visits)],
             label = paste('N:', sample_size),
             size = 5, vjust = -0.5) +
    scale_x_continuous(breaks= seq(0,, 23, by=2)) +
    labs(title = x$kayntityyppi,
         y = 'Palvelutapahtumia (tuhansia)') +
    ylim(0, 1.1 * dt.hr[, max(visits)]) +
    theme(text = element_text(size=20),
          axis.title.x = element_blank(),
          panel.background = element_rect(fill = 'white', colour = 'white'),
          panel.grid.major = element_line(size=0.25, linetype = 'solid',
                                          color = 'lightgrey'),
          panel.grid.minor = element_line(size=0.25, linetype = 'solid',
                                          color = 'lightgrey'),
          panel.border = element_rect(colour='black', fill=NA, size=0.5))
  
})

# Save plots:

ggsave(
  filename = output.prov.2, width = 15, height = 18,
  plot = 
    wrap_elements(panel = plots[[1]] + plots[[4]]) + 
      ggtitle('A. Julkinen avosairaanhoito') + 
      theme(plot.title = element_text(size=35, hjust=0.5)) +
    wrap_elements(panel = plots[[2]] + plots[[5]]) + 
      ggtitle('B. Yksityinen avosairaanhoito') + plot_layout(ncol=1) +
      theme(plot.title = element_text(size=35, hjust=0.5)) +
    wrap_elements(panel = plots[[3]] + plots[[6]]) + 
      ggtitle('C. Työterveys (sairaanhoito)') + plot_layout(ncol=1) +
      theme(plot.title = element_text(size=35, hjust=0.5))) 


# We will plot the following specifications (3 sectors):

specs = list(
  list(julkinen=1, kaynti_palvelumuoto='T11', 
       sektori='Julkinen avosairaanhoito'),
  list(julkinen=0, kaynti_palvelumuoto='T11',
       sektori='Yksityinen avosairaanhoito'),
  list(julkinen=c(1,0,-1), kaynti_palvelumuoto='T31',
       sektori='Työterveys (sairaanhoito)')
)


### The characteristics of those who have telemedicine visits 
# on weekends or outside office hours. ###

dfs = lapply(specs, function(x) {
  
  # Extract data:
  dt = phc[telemedicine == 1 &
             julkinen %in% x$julkinen &
             kaynti_palvelumuoto %in% x$kaynti_palvelumuoto]
  
  dt[, office.hours := as.integer(weekday_no %in% c(1:5) & hour %in% c(8:16))]
  
  # Count the number of visits by person and time:
  dt.hr = dt[, .(visits = .N), by=c('shnro', 'office.hours')]
  dt.hr = dcast(dt.hr, shnro ~ paste0('office.hours.', office.hours), 
                value.var = 'visits')
  
  # Merge to FOLK data and impute zeroes if no visits are observed:
  data = merge(folk, dt.hr, by='shnro', all.x=TRUE)
  data[is.na(office.hours.1), office.hours.1 := 0]
  data[is.na(office.hours.0), office.hours.0 := 0]
  data[, visits.sum := office.hours.1 + office.hours.0]
  
  # Keep only those who have at least 1 telemedicine contact:
  data = data[visits.sum > 0]
  
  # Count the share of telemedicine visits on office hours:
  data[, share.office.hours := office.hours.1 / visits.sum]
  data[, only.office.hours := as.integer(share.office.hours == 1)]
  
  # The smallest group has the following number of persons:
  print(data[, .(N = .N), by='only.office.hours'][, min(N)])
  
  
  # Select and create variables for which odds ratios will be shown:
  data[, nainen := nainen_no]
  data[, ika.korkea := as.integer(ika >= 45)]
  data[, tulot.korkeat := as.integer(kturaha_no %in% c(6:10))]
  data[, koulutus.korkea := as.integer(ututku_aste %in% c(5:8))]
  data[, kaupunki := as.integer(maka_label %in% c('K1', 'K2', 'K3'))]
  
  # Loop over the following variables:
  variables = c('nainen', 'ika.korkea', 'tulot.korkeat', 'koulutus.korkea',
                'kaupunki')
  
  dfs = lapply(variables, function(var) {
    
    # Odds ratios:
    df = data[, .(ka = mean(get(var))), by='only.office.hours']
    df[, dimension := var]
    df = dcast(df, dimension ~ paste0('only.office.hrs.', only.office.hours), 
               value.var = 'ka')
    df[, or := only.office.hrs.0 / only.office.hrs.1]
    
  })
  
  dfs = rbindlist(dfs)
  
  # Labels:
  labels = c('Nainen', 'Ikä väh. 45', 'Tulojakauman ylin 50 %', 
             'Korkea-asteen koulutus', 'Asuu kaupungissa')
  dfs[, label := factor(labels, levels=rev(labels))]
  
  # Colour palette for different sectors:
  if(x$sektori=='Julkinen avosairaanhoito') {
    color.sektori = '#440154'
  } else if(x$sektori=='Yksityinen avosairaanhoito') {
    color.sektori = '#21918c'
  } else if (x$sektori=='Työterveys (sairaanhoito)') {
    color.sektori = '#fde725' }
  
  dfs[, ':=' (sektori = x$sektori,
              color.sektori = color.sektori)]
  return(dfs)
  
})
dfs = rbindlist(dfs)

# Order sectors:
dfs[, sektori := factor(sektori, levels = c('Julkinen avosairaanhoito',
                                            'Yksityinen avosairaanhoito',
                                            'Työterveys (sairaanhoito)'))]
# Their colors:
colors.fill = dfs[, .(colors = unique(color.sektori), by='sektori')][, colors]
colors.text = c('white', 'white', 'black')

# Plot:

plots = ggplot(dfs, aes(label=round(or, digits=3), x=or, y=label,
                        fill=color.sektori, color=color.sektori)) +
  geom_vline(xintercept = 1) + 
  geom_label() + 
  xlim(0.65, 1.35) +
  scale_y_discrete(labels = label_wrap(13)) + 
  scale_fill_manual(values = colors.fill) + 
  scale_color_manual(values = colors.text) + 
  facet_grid(cols = vars(sektori)) +
  guides(fill='none', color='none', alpha='none') +
  theme(text = element_text(size=20),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.background = element_rect(fill = 'white', colour = 'white'),
        panel.grid.major = element_line(size=0.25, linetype = 'solid',
                                        color = 'lightgrey'),
        panel.grid.minor = element_line(size=0.25, linetype = 'solid',
                                        color = 'lightgrey'),
        panel.border = element_rect(colour='black', fill=NA, size=0.5)) 

# Save:
ggsave(
  filename = output.prov.3, width = 12, height = 5,
  plot = plots)


### ### ### ### ### ### ### ### ### ### ### ### ### 
#### 4) Patients' perspective. ####
### ### ### ### ### ### ### ### ### ### ### ### ###


# We will plot the following specifications (3 sectors; telemedicine or not):

specs = list(
  list(julkinen=1, kaynti_palvelumuoto='T11', 
       sektori='Julkinen avosairaanhoito'),
  list(julkinen=0, kaynti_palvelumuoto='T11',
       sektori='Yksityinen avosairaanhoito'),
  list(julkinen=c(1,0,-1), kaynti_palvelumuoto='T31',
       sektori='Työterveys (sairaanhoito)')
)


### Share of telemedicine visits by group. ###

plots = lapply(specs, function(x) {
  
  # Extract data and count visits by person:
  dt = phc[julkinen %in% x$julkinen &
             kaynti_palvelumuoto %in% x$kaynti_palvelumuoto]
  dt = dt[, .(visits = .N), by=c('shnro', 'telemedicine')]
  dt = dcast(dt, shnro ~ paste0('evisits_', telemedicine), value.var = 'visits')
  
  # Merge to FOLK data and impute zeroes if no visits are observed:
  data = merge(folk, dt, by='shnro', all.x=TRUE)
  data[is.na(evisits_1), evisits_1 := 0]
  data[is.na(evisits_0), evisits_0 := 0]
  
  
  # Count the share of e-visits by group:
  
  variables = c('nainen', 'ika', 'kturaha', 'maka', 'ptoim', 'ututku')
  
  dfs = lapply(variables, function(var) {
    
    df = data[!is.na(get(paste(var, '_no', sep=''))), 
              .(share_evisits = 100*sum(evisits_1) / 
                  (sum(evisits_1) + sum(evisits_0)),
                sample_1000 = .N / 1000), 
              by=c(paste(var, '_no', sep=''), paste(var, '_label', sep=''))]
    
    colnames(df)[1:2] = c('no', 'label')
    df[, dimension := var]
    
  })
  
  
  # Collect all datasets and compute the mean:
  data = rbindlist(dfs)
  data[, share_evisits.mean := weighted.mean(share_evisits, w=sample_1000)]
  
  # We'll plot gender and main type of activity to the same figure (dimension):
  data[dimension=='nainen', no := no + 1]
  data[dimension=='ptoim', no := no + 2]
  data[dimension %in% c('nainen', 'ptoim'), 
       ':=' (dimension='nainen_ptoim',
             share_evisits.mean = data[dimension=='nainen', 
                                       unique(share_evisits.mean)])]
  
  # For occupational care, some groups (e.g., the young) are irrelevant:
  if(x$sektori=='Työterveys (sairaanhoito)') {
    data = data[!(label %in% c('Nuoret', 'Eläkeläiset', 
                               '0-9', '10-19', '70-79', '80-89', '> 90'))] 
    data[dimension=='ika', no := no - 2]
    data[dimension=='nainen_ptoim', no := c(1:4)]
  }
  
  # The smallest group has the following number of observations:
  print(data[, min(1000* sample_1000)])
  
  # Colour palette for different sectors:
  if(x$sektori=='Julkinen avosairaanhoito') {
    color.sektori = '#440154'
  } else if(x$sektori=='Yksityinen avosairaanhoito') {
    color.sektori = '#21918c'
  } else if (x$sektori=='Työterveys (sairaanhoito)') {
    color.sektori = '#fde725' }
  
  
  # We plot several plots over the following dimension:
  dimensions = c('nainen_ptoim', 'ika', 'kturaha', 'maka', 'ututku')
  
  plots = lapply(dimensions, function(dim) {
    
    # Extract data:
    dt.plt = data[dimension==dim]
    
    # Plot:
    p = ggplot(data=dt.plt, aes(x=no, y=share_evisits)) +
      geom_bar(stat='identity', fill=color.sektori, color='black', alpha=0.8) +
      geom_text(aes(label = round(share_evisits, digits=0)), 
                vjust = -0.5, size = 5) +
      annotate(geom='label', x=2, y= 1.05* dt.plt[, max(share_evisits)],
               label=paste('Ka. ', round(dt.plt[, unique(share_evisits.mean)], 
                                         digits=0), '%', sep = ' '),
               size = 5, vjust = -0.5) +
      scale_x_continuous(breaks= dt.plt$no, labels = dt.plt$label) +
      labs(title = x$sektori, y = 'Etäasiointien osuus (%)') +
      ylim(0, 1.15 * dt.plt[, max(share_evisits)]) +
      theme(text = element_text(size=20),
            axis.title.x = element_blank(),
            panel.background = element_rect(fill = 'white', colour = 'white'),
            panel.grid.major = element_line(size=0.25, linetype = 'solid',
                                            color = 'lightgrey'),
            panel.grid.minor = element_line(size=0.25, linetype = 'solid',
                                            color = 'lightgrey'),
            panel.border = element_rect(colour='black', fill=NA, size=0.5))
    
    if(dim %in% c('ika', 'nainen_ptoim', 'ututku')) {
      p = p + theme(axis.text.x = element_text(angle=45, hjust=0.5, vjust=0.5))} 
    
    return(p)
    
  })
  
})


# Save plots:

ggsave(
  filename = output.pat.1, width = 15, height = 6,
  plot = plots[[1]][[1]] + plots[[2]][[1]] + plots[[3]][[1]])  

ggsave(
  filename = output.pat.2, width = 15, height = 6,
  plot = plots[[1]][[2]] + plots[[2]][[2]] + plots[[3]][[2]])  

ggsave(
  filename = output.pat.3, width = 15, height = 6,
  plot = plots[[1]][[3]] + plots[[2]][[3]] + plots[[3]][[3]]) 

ggsave(
  filename = output.pat.4, width = 15, height = 6,
  plot = plots[[1]][[4]] + plots[[2]][[4]] + plots[[3]][[4]]) 

ggsave(
  filename = output.pat.5, width = 15, height = 6,
  plot = plots[[1]][[5]] + plots[[2]][[5]] + plots[[3]][[5]]) 



# We will plot the following specifications (3 sectors; telemedicine or not):

specs = list(
  list(telemedicine=1, julkinen=1, kaynti_palvelumuoto='T11', 
       sektori='Julkinen avosairaanhoito', kayntityyppi='Etäasioinnit'),
  list(telemedicine=1, julkinen=0, kaynti_palvelumuoto='T11',
       sektori='Yksityinen avosairaanhoito', kayntityyppi='Etäasioinnit'),
  list(telemedicine=1, julkinen=c(1,0,-1), kaynti_palvelumuoto='T31',
       sektori='Työterveys (sairaanhoito)', kayntityyppi='Etäasioinnit'),
  list(telemedicine=0, julkinen=1, kaynti_palvelumuoto='T11',
       sektori='Julkinen avosairaanhoito', kayntityyppi='Vastaanottokäynnit'),
  list(telemedicine=0, julkinen=0, kaynti_palvelumuoto='T11',
       sektori='Yksityinen avosairaanhoito', kayntityyppi='Vastaanottokäynnit'),
  list(telemedicine=0, julkinen=c(1,0,-1), kaynti_palvelumuoto='T31',
       sektori='Työterveys (sairaanhoito)', kayntityyppi='Vastaanottokäynnit')
)


### Share of patients by group. ###

plots = lapply(specs, function(x) {
  
  # Extract data and count visits by person:
  dt = phc[telemedicine %in% x$telemedicine &
             julkinen %in% x$julkinen &
             kaynti_palvelumuoto %in% x$kaynti_palvelumuoto]
  dt = dt[, .(visits = .N), by='shnro']
  
  # Merge to FOLK data and impute zeroes if no visits are observed:
  data = merge(folk, dt, by='shnro', all.x=TRUE)
  data[is.na(visits), visits := 0]
  data[, visited := as.integer(visits > 0)]
  
  
  # Count the share of users by group:
  
  variables = c('nainen', 'ika', 'kturaha', 'maka', 'ptoim', 'ututku')
  
  dfs = lapply(variables, function(var) {
    
    df = data[!is.na(get(paste(var, '_no', sep=''))), 
              .(users = 100*mean(visited),
                sample_1000 = .N / 1000), 
              by=c(paste(var, '_no', sep=''), paste(var, '_label', sep=''))]
    
    colnames(df)[1:2] = c('no', 'label')
    df[, dimension := var]
    
  })
  
  
  # Collect all datasets and compute the mean:
  data = rbindlist(dfs)
  data[, users.mean := weighted.mean(users, w=sample_1000)]
  
  # We'll plot gender and main type of activity to the same figure (dimension):
  data[dimension=='nainen', no := no + 1]
  data[dimension=='ptoim', no := no + 2]
  data[dimension %in% c('nainen', 'ptoim'), 
       ':=' (dimension='nainen_ptoim',
             users.mean = data[dimension=='nainen', unique(users.mean)])]
  
  # For occupational care, some groups (e.g., the young) are irrelevant:
  if(x$sektori=='Työterveys (sairaanhoito)') {
    data = data[!(label %in% c('Nuoret', 'Eläkeläiset', 
                               '0-9', '10-19', '70-79', '80-89', '> 90'))] 
    data[dimension=='ika', no := no - 2]
    data[dimension=='nainen_ptoim', no := c(1:4)]
  }
  
  # The smallest group has the following number of observations:
  print(data[, min(1000* sample_1000)])
  
  # Colour palette for different sectors:
  if(x$sektori=='Julkinen avosairaanhoito') {
    color.sektori = '#440154'
  } else if(x$sektori=='Yksityinen avosairaanhoito') {
    color.sektori = '#21918c'
  } else if (x$sektori=='Työterveys (sairaanhoito)') {
    color.sektori = '#fde725' }
  
  
  # We plot several plots over the following dimension:
  dimensions = c('nainen_ptoim', 'ika', 'kturaha', 'maka', 'ututku')

  plots = lapply(dimensions, function(dim) {
    
    # Extract data:
    dt.plt = data[dimension==dim]
    
    # Plot:
    p = ggplot(data=dt.plt, aes(x=no, y=users)) +
      geom_bar(stat='identity', fill=color.sektori, color='black', alpha=0.8) +
      geom_text(aes(label = round(users, digits=0)), 
                vjust = -0.5, size = 5) +
      annotate(geom='label', x=2, y= 1.05* dt.plt[, max(users)],
               label=paste('Ka. ', round(dt.plt[, unique(users.mean)], digits=),
                           '%', sep = ' '),
               size = 5, vjust = -0.5) +
      scale_x_continuous(breaks= dt.plt$no, labels = dt.plt$label) +
      labs(title = x$sektori, y = 'Asiakkaita väestöstä (%)') +
      ylim(0, 1.15 * dt.plt[, max(users)]) +
      theme(text = element_text(size=20),
            axis.title.x = element_blank(),
            panel.background = element_rect(fill = 'white', colour = 'white'),
            panel.grid.major = element_line(size=0.25, linetype = 'solid',
                                            color = 'lightgrey'),
            panel.grid.minor = element_line(size=0.25, linetype = 'solid',
                                            color = 'lightgrey'),
            panel.border = element_rect(colour='black', fill=NA, size=0.5))
    
    if(dim %in% c('ika', 'nainen_ptoim', 'ututku')) {
      p = p + theme(axis.text.x = element_text(angle=45, hjust=0.5, vjust=0.5))} 
    
    return(p)
    
  })
  
})


# Save plots:

ggsave(
  filename = output.pat.6, width = 15, height = 12,
  plot = 
    wrap_elements(panel = plots[[1]][[1]] + plots[[2]][[1]] + plots[[3]][[1]]) + 
      ggtitle('A. Etäasioinnit') + 
      theme(plot.title = element_text(size=35, hjust=0.5)) +
    wrap_elements(panel = plots[[4]][[1]] + plots[[5]][[1]] + plots[[6]][[1]]) + 
      ggtitle('B. Vastaanottokäynnit') + plot_layout(ncol=1) +
      theme(plot.title = element_text(size=35, hjust=0.5)))  

ggsave(
  filename = output.pat.7, width = 15, height = 12,
  plot = 
    wrap_elements(panel = plots[[1]][[2]] + plots[[2]][[2]] + plots[[3]][[2]]) + 
      ggtitle('A. Etäasioinnit') + 
      theme(plot.title = element_text(size=35, hjust=0.5)) +
    wrap_elements(panel = plots[[4]][[2]] + plots[[5]][[2]] + plots[[6]][[2]]) + 
      ggtitle('B. Vastaanottokäynnit') + plot_layout(ncol=1) +
      theme(plot.title = element_text(size=35, hjust=0.5)))  

ggsave(
  filename = output.pat.8, width = 15, height = 12,
  plot = 
    wrap_elements(panel = plots[[1]][[3]] + plots[[2]][[3]] + plots[[3]][[3]]) + 
      ggtitle('A. Etäasioinnit') + 
      theme(plot.title = element_text(size=35, hjust=0.5)) +
    wrap_elements(panel = plots[[4]][[3]] + plots[[5]][[3]] + plots[[6]][[3]]) + 
      ggtitle('B. Vastaanottokäynnit') + plot_layout(ncol=1) +
      theme(plot.title = element_text(size=35, hjust=0.5))) 

ggsave(
  filename = output.pat.9, width = 15, height = 12,
  plot = 
    wrap_elements(panel = plots[[1]][[4]] + plots[[2]][[4]] + plots[[3]][[4]]) + 
      ggtitle('A. Etäasioinnit') + 
      theme(plot.title = element_text(size=35, hjust=0.5)) +
    wrap_elements(panel = plots[[4]][[4]] + plots[[5]][[4]] + plots[[6]][[4]]) + 
      ggtitle('B. Vastaanottokäynnit') + plot_layout(ncol=1) +
      theme(plot.title = element_text(size=35, hjust=0.5))) 

ggsave(
  filename = output.pat.10, width = 15, height = 12,
  plot = 
    wrap_elements(panel = plots[[1]][[5]] + plots[[2]][[5]] + plots[[3]][[5]]) + 
    ggtitle('A. Etäasioinnit') + 
    theme(plot.title = element_text(size=35, hjust=0.5)) +
    wrap_elements(panel = plots[[4]][[5]] + plots[[5]][[5]] + plots[[6]][[5]]) + 
    ggtitle('B. Vastaanottokäynnit') + plot_layout(ncol=1) +
    theme(plot.title = element_text(size=35, hjust=0.5))) 

# End.
