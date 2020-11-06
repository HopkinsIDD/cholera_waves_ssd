## utility and helper functions for South Sudan analyses

reload_source <- function(){
    library(tidyverse)
    library(readxl)
    library(cowplot)
    library(ggsn)
    library(sf)
    library(lubridate)
        

    library(EpiEstim)
    library(incidence)
        
    library(ggridges)
    library(viridis)
    library(flextable)
    #library(officer)
        
        

    source("code/extra/utils.R")
}

fix_admin2Names <- function(names){
    recode(names,
           `Ikwoto` = "Ikotos",
           `Kajo Keji` = "Kajo-keji",
           `Kajo- Keji` = "Kajo-keji",
           `Lopa/Lafon` = "Lafon",
           `Bor` = "Bor South",
           `Canal Pigi` = "Canal/Pigi",
           `Raja` = "Raga" )
}

##loads and cleans 2014 linelist
load_and_clean_2014_data <- function(){

    ## mostly autmated cleaning although
    ## a few easy wins were dealt with manuscally in
    ## excel file: changed >=2015 dates to 2014 if they made sense
    ## got rid of other special characters (e.g., floating periods and extra slashes)

    m2014 <- read_xlsx("data/raw_data/master_2014.xlsx") %>%
    select(-`Case no`,-`Serial #`) %>%
    rename(age = `Age in Years`,
           sex = Sex,
           state = State,
           payam = Payam,
           county = County,
           village = Village,
           health_facility = `Reporting Health facility`,
           visit_date = `Date of visit at health facility        (DD-MM-YY)`,
           onset_date = `Date of onset    (DD-MM-YY)`,
           epi_week = `Epi- Week`,
           died = `Outcome _1 1:Alive 2:Health Facility death 3:Community death`
           ) %>%
    select(age,sex,state,county,payam, village,health_facility,visit_date,onset_date,epi_week,died)

    ## clean age and sex
    m2014 <- m2014 %>% mutate(age = str_extract(age,'[0-9\\.]+') %>% as.numeric,
                              age = ifelse(age > 120, NA, age),
                              sex = str_to_upper(sex),
                              sex = ifelse(sex == "NO INFO",NA,sex)) ## this gets one acceidental date

    ## outcome cleaning
    m2014 <- m2014 %>% mutate(died = recode(died,
                                            `1` = "0",
                                            `2` = "1",
                                            `3` = "1",
                                            `Alive` = "0",
                                            `Community death` = "1",
                                            `Health Facility death` = "1") %>% as.numeric)



    ## clean visit date (pass 1)
    m2014 <- m2014 %>% mutate(visit_date = ifelse(visit_date == "N/A",NA,visit_date),
                              visit_date = ifelse(grepl("/",visit_date),
                                                  as.Date(visit_date,format="%d/%m/%Y"),
                                           ifelse(grepl("^41+",visit_date),
                                                  as.Date(visit_date %>% as.numeric,origin="1899-12-30"),
                                                  NA)
                                           ),
                              visit_date = as.Date(visit_date,origin="1970-01-01"),
                              onset_date = as.Date(onset_date),
                              delay = visit_date - onset_date,
                              visit_date_imp = if_else(delay<0|is.na(visit_date),onset_date,visit_date),
                              onset_date_imp = if_else(delay<0|is.na(onset_date),visit_date,onset_date))

    ## delay_dist <- m2014 %>% mutate(delay_imp1 = visit_date_imp - onset_date_imp) %>% select(delay_imp1)
    ## table(delay_dist %>% unlist %>% as.numeric)/sum(table(delay_dist %>% unlist %>% as.numeric))

    m2014 <- m2014 %>% mutate(
                           visit_date_imp = if_else((!is.na(delay) && delay<0)|is.na(visit_date),onset_date,visit_date),
                           onset_date_imp = if_else((!is.na(delay) && delay<0)|is.na(onset_date),visit_date,onset_date))

    m2014 <- m2014 %>% mutate(admin2Name = county %>% str_to_title %>% fix_admin2Names)

    return(m2014)
}

load_and_clean_2015_data <- function(){

    m2015 <- read_xlsx("data/raw_data/master_2015.xlsx",sheet="Line list master ",range = "A5:AB1823") %>%
    rename(
        id = `Case no`,
        age = `Age in Years`,
        sex = Sex,
        state = State,
        payam = Payam,
        county = County,
        village = Village,
        health_facility = `Reporting Health facility`,
        visit_date = `Date of visit at health facility        (DD-MM-YY)`,
        onset_date = `Date of onset    (DD-MM-YY)`,
        epi_week = `Epi- Week`,
        died = `Outcome _1 1:Alive 2:Health Facility death 3:Community death`) %>%
    select(id,age,sex,state,county,payam, village,health_facility,visit_date,onset_date,epi_week,died)

    ## clean age and sex
    m2015 <- m2015 %>% mutate(age = str_extract(age,'[0-9\\.]+') %>% as.numeric,
                              age = ifelse(age > 120, NA, age),
                              sex = str_to_upper(sex),
                              sex = ifelse(sex == "NO INFO",NA,sex),
                              died = recode(died, `1` = 0,`2` = 1, `3` =1)) ## this gets one acceidental date


    m2015 <- m2015 %>% mutate(visit_date = ifelse(visit_date %in% c("no treatment","did not visit health facility"),NA,visit_date),
                              visit_date = as.Date(visit_date %>% as.numeric,origin="1899-12-30"),
                              onset_date = ifelse(onset_date == "ND",NA,onset_date),
                              onset_date =  as.Date(onset_date %>% as.numeric,origin="1899-12-30"),
                              delay = visit_date - onset_date,
                              visit_date_imp = if_else((!is.na(delay) && delay<0)|is.na(visit_date),onset_date,visit_date),
                              onset_date_imp = if_else((!is.na(delay) && delay<0)|is.na(onset_date),visit_date,onset_date))

    ## quick check of delay distribitino to justify imputation
    ## delay_dist <- m2015 %>% mutate(delay_imp1 = visit_date_imp - onset_date_imp) %>% select(delay_imp1)
    ## table(delay_dist %>% unlist %>% as.numeric)/sum(table(delay_dist %>% unlist %>% as.numeric))

    ## fix locations
    m2015 <- m2015 %>% mutate(admin2Name = county %>% str_to_title %>% fix_admin2Names)

    return(m2015)
}

load_and_clean_2017_data <- function(){
    ## these data were from EWARS so end up being a lot cleaner

    m2017 <- read_xlsx("data/raw_data/master_2016_2017.xlsx",sheet="d4b64d45-4531-4b0d-8a60-eed208e") %>%
    rename(
        id = case_no,
        payam = Payam,
        county = County,
        village = village,
        health_facility = health_facility,
        visit_date = date_visit_hf,
        onset_date = date_symp_onset ,
        iso_week = isoweek) %>%
    mutate(
        id = row_number(),
        visit_date = as.Date(visit_date),
        onset_date = as.Date(onset_date),
        died = ifelse(Alive_Died_ == "Died",1,0)) %>%
    select(id,age,sex,county,payam, village,health_facility,visit_date,onset_date,iso_week,died)

    ## clean sex (age is all good)
    m2017 <- m2017 %>% mutate(sex = str_to_upper(sex))

    ## now for the dates
    m2017 <- m2017 %>% mutate(delay = visit_date - onset_date)

    m2017 %>% filter(delay < 0 | delay > 30) %>% select(id,visit_date,onset_date) %>% data.frame %>% write_csv("data/raw_data/cases_2017_date_fix.csv")
    fixed_dates <- read_csv("data/raw_data/cases_2017_date_fix_manually_updated.csv") %>% mutate(
                                                     onset_date = as.Date(onset_date,format="%d/%m/%Y"),
                                                     onset_date_fixed = as.Date(onset_date_fixed,format="%d/%m/%Y"),
                                                     visit_date = as.Date(visit_date,format="%d/%m/%Y"),
                                                     visit_date_fixed = as.Date(visit_date_fixed,format="%d/%m/%Y"))

    ## now fix dates
    m2017 <- left_join(m2017,fixed_dates) %>% mutate(
                                                  onset_date = if_else(!is.na(onset_date_fixed),onset_date_fixed,onset_date),
                                                  visit_date = if_else(!is.na(visit_date_fixed),visit_date_fixed,visit_date),
                                                  delay = visit_date - onset_date,
                                                  visit_date_imp = if_else(is.na(visit_date),onset_date,visit_date),
                                                  onset_date_imp = if_else(is.na(onset_date),visit_date,onset_date)) %>%
    select(-visit_date_fixed,-onset_date_fixed)

    ## fix locations
    m2017 <- m2017 %>% mutate(admin2Name = county %>% str_to_title %>% fix_admin2Names)

    return(m2017)

}

#grabs legend from figures
g_legend<-function(a.gplot){
    tmp <- ggplot_gtable(ggplot_build(a.gplot))
    leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
    legend <- tmp$grobs[[leg]]
    return(legend)}



# calculate attack rate and CFR with confidence intervals
ARpoissonCI <- function(x){
    cases <- x[1]
    pop <-x[2]
    pt <- poisson.test(cases,T=pop,conf.level = 0.95)
    ci <-100*pt$conf.int[1:2]
    txt <- paste0(sprintf("%.1f",cases/pop*1000),
                  " (",paste(sprintf("%.1f",ci),collapse="-"),")")
    return(txt)
    
}

CFRexactCI <- function(y){
    cases<-y[1]
    deaths <- y[2]
    txt <- "-"
    if (cases>0){
        bt <- binom.test(deaths,cases,conf.level = 0.95)
        ci <-100*bt$conf.int[1:2]
        txt <- paste0(sprintf("%.1f",deaths/cases*100),
                      " (",paste(sprintf("%.1f",ci),collapse="-"),")"
        )
    }
    return(txt)
    
}

# calculate attack rate and CFR without confidence intervals
ARpoissonCIalt <- function(x){
    cases <- x[1]
    pop <-x[2]
    # pt <- poisson.test(cases,T=pop,conf.level = 0.95)
    # ci <-100*pt$conf.int[1:2]
    txt <- paste0(sprintf("%.1f",cases/pop*1000)#,
                  # " (",paste(sprintf("%.1f",ci),collapse="-"),")"
    )
    return(txt)
    
}

CFRexactCIalt <- function(y){
    cases<-y[1]
    deaths <- y[2]
    txt <- "-"
    if (cases>0){
        # bt <- binom.test(deaths,cases,conf.level = 0.95)
        # ci <-100*bt$conf.int[1:2]
        txt <- paste0(sprintf("%.1f",deaths/cases*100)#,
                      # " (",paste(sprintf("%.1f",ci),collapse="-"),")"
        )
    }
    return(txt)
    
}


#Make IOM data long

makeLong<- function(t,p,country,sOrigin,cOrigin,count){
    heatmapdf<- iomdata%>%
        mutate(
            countryOrigin = "South Sudan",
            type= t,
            period= p) %>%
        rename( stateAssessed = `State of assessment`,
                countyAssessed= `County of assessment`,
                admin2Pcod = `County P_code`,
                
                
                stateOrigin   =(!!as.symbol(sOrigin))   ,
                countyOrigin  =(!!as.symbol(cOrigin))  ,
                
                number        =(!!as.symbol(count))             
                
        )  %>% 
        select( type,period,
                stateAssessed, countyAssessed, admin2Pcod,
                countryOrigin,stateOrigin,countyOrigin,
                number) 
    if(t=="Returnee"){
        new <- iomdata %>%
            rename(countryOrigin = (!!as.symbol(country)))
        
        heatmapdf$countryOrigin <-  new$countryOrigin
    }
    
    return(heatmapdf)
    
}


