library("splines")
library(dlnm)
library(mgcv)

# Load data----------

#County map names
mapdf <- as.data.frame(read_sf("data/raw_data/ssd_admbnda_adm2_200k_ssnbs_20160114.shp")) %>%
        select(admin2Name,admin2Pcod)
#bring in rainfall data
chrps_rainfall_2010_2018 <- read_csv("data/raw_data/ssd_chrps_county/chrps_counties_dailyrainfall_2010_2018.csv") %>%
        rename(precip = avgDailyRainfall,
               Date = dates) %>%
        mutate(Date = as.Date(Date,format="%m/%d/%Y",origin="1970-1-1"))%>%
        left_join(mapdf,by="admin2Pcod")
#limit data to Juba and create aggregate precipitation 
rainfall_J1417 <- chrps_rainfall_2010_2018 %>% 
        filter(year(Date) %in% 2014:2017)%>%
        filter(admin2Name=="Juba") %>%
        mutate(
                ag7precip =      lag(precip,1) +
                        lag(precip,2) +
                        lag(precip,3) +
                        lag(precip,4) +
                        lag(precip,5) +
                        lag(precip,6) +
                        lag(precip,7),
                
                ag4precip =      lag(precip,1) +
                        lag(precip,2) +
                        lag(precip,3) +
                        lag(precip,4),
                ag10precip =      lag(precip,1) +
                        lag(precip,2) +
                        lag(precip,3) +
                        lag(precip,4) +
                        lag(precip,5) +
                        lag(precip,6) +
                        lag(precip,7) +
                        lag(precip,8) +
                        lag(precip,9) +
                        lag(precip,10)
                
        )
#bring in case data
master_dat <- read_rds("data/generated_data/master_linelist_final.rds")


# Define Serial interval ----------

# calculate the serial interval and instantaneous R(t)

#original
#SerI <- discr_si(1:10, 5, 8)/sum(discr_si(1:10, 5, 8))

#using phelps et al (mean= 3.7, 2.9-4.7)
#SerI <- discr_si(1:10, 3.7, 0.9/1.96)/sum(discr_si(1:10, 3.7, 0.9/1.96))

#playing around with various widths
Ser2015 <- discr_si(1:10, 3.95,5.80)/sum(discr_si(1:10, 3.95,5.80))
Ser2016 <- discr_si(1:10, 1.31,0.761)/sum(discr_si(1:10, 1.31,0.761))

mean_si<- 3.7
sd_si <-  (4.7-2.9)/(2*1.96)*sqrt(22)
SerPhelps <- discr_si(1:10, mean_si, sd_si)/sum(discr_si(1:10, mean_si, sd_si))


# Calculate reproductive number ---------
CalcRt<- function(SerI){
        #detection probability
        detectProb <- 0.10
        #set up cases and offset
        juba_cases <- master_dat %>%
                filter(admin2Name=="Juba")%>% 
                #calculate daily number of cases
                mutate(Date=as.Date(onset_date_imp)) %>%
                arrange(Date) %>%
                group_by(Date) %>%
                summarize(cases=n()) %>% 
                filter(!is.na(Date)) %>%
                mutate(yr= as.character(year(Date)))%>%#,
                #wave=ifelse(wave %in% c("2016","2017"),"2016/2017",wave))%>%
                #include zeroes for days in between cases
                group_by(yr)%>%
                complete(Date = seq.Date(min(Date), max(Date), by="day")) %>%
                mutate(cases=ifelse(is.na(cases),0,cases))%>%
                # ungroup()%>%
                # complete(Date = seq.Date(min(Date), max(Date), by="day")) %>%
                # arrange(wave,Date) %>%
                # group_by(wave)%>%
                #calculate force of infection
                mutate(wI= lag(cases,1)*SerI[1] +
                               lag(cases,2)*SerI[2] + 
                               lag(cases,3)*SerI[3] +
                               lag(cases,4)*SerI[4] +
                               lag(cases,5)*SerI[5] +
                               lag(cases,6)*SerI[6] +
                               lag(cases,7)*SerI[7] +
                               lag(cases,8)*SerI[8] +
                               lag(cases,9)*SerI[9] +
                               lag(cases,10)*SerI[10]) %>%
                ungroup()%>% mutate(S=1-cumsum(cases)/detectProb/514769.99)
        
        #create final dataset for regression
        final <- left_join(rainfall_J1417,juba_cases, by=c("Date")) %>%
                mutate(wI=ifelse(wI==0,NA,wI),
                       off=wI*S) %>%
                # #winsorize those values above 60
                mutate(
                        # ag7precip=ifelse(ag7precip>quantile(ag7precip,0.95,na.rm=TRUE),
                        #                  quantile(ag7precip,0.95,na.rm=TRUE),ag7precip),
                        # ag4precip=ifelse(ag4precip>quantile(ag4precip,0.95,na.rm=TRUE),
                        #                  quantile(ag4precip,0.95,na.rm=TRUE),ag4precip),
                        # ag10precip=ifelse(ag10precip>quantile(ag10precip,0.95,na.rm=TRUE),
                        #                  quantile(ag10precip,0.95,na.rm=TRUE),ag10precip),
                        dow=weekdays(Date))%>% 
                mutate(cases=round(cases/off*off),
                       log_off=log(off),
                       wave=ifelse(yr %in% c(2016,2017),"2016/2017",as.character(yr)))
        
        
}





