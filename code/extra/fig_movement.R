my_map <- read_sf("data/raw_data/ssd_admbnda_adm2_200k_ssnbs_20160114.shp") 
worldpop_dat <- read_csv("data/raw_data/SSD15adjv3_tif+SSD-HDX-Borders_adm2_zonal.csv") 
 master_dat <- read_rds("data/generated_data/master_linelist_final.rds")



#get attack rates
master_epi <- master_dat %>% 
        
        mutate(
                period= format(onset_date_imp, "%Y"),
                month = as.numeric(format(onset_date_imp, "%m"))#,
                # period = ifelse(year == 2017, 
                #                 ifelse(month<=6,
                #                        "2017JanJun","2017JulDec"),
                #                 as.character(year)
                # )
        )

incidence <- master_epi %>% 
        filter(!(is.na(period))) %>%
        group_by(#year,
                 period,
                 admin2Name) %>%
        summarize(cases = n()) %>% 
        left_join((worldpop_dat %>% dplyr::select(admin2Name,SUM))) %>%
        rename(Pop = SUM,
               origin = admin2Name) %>%
        mutate(AR=cases/Pop) %>% select(origin,period,AR)

### Connectivity

#get dataset ready
newiom <- read_csv("data/generated_data/IOM_longdata.csv") %>%
        mutate(number = ifelse(period=="2015/2016",number/2,number))

newiom2 <- newiom %>% 
        mutate(period = recode(period,
                               `2013/2014` = "2014",
                               `2015/2016` = "2015",
                               `2017JanJun`="2017",
                               `2017JulDec`="2017"
        )) %>% bind_rows(filter(newiom,period=="2015/2016")) %>%
        mutate(period = recode(period,
                               `2015/2016` = "2016")) %>%
        #filter(period != "2018JanApr") %>%
        mutate(
                origin=ifelse(countryOrigin=="South Sudan",countyOrigin,countryOrigin),
                origin= ifelse(origin =="Aweil Center","Aweil Centre",origin),
                origin= ifelse(origin =="Canal (Khorfulus)","Canal/Pigi",origin),
                origin= ifelse(origin =="Ikwoto","Ikotos",origin),
                origin= ifelse(origin =="Kajo-Keji","Kajo-keji",origin),
                origin= ifelse(origin =="Luakpiny (Nasir)","Luakpiny/Nasir",origin),
                origin= ifelse(origin =="Raja","Raga",origin),
                origin= ifelse(origin =="Terkeka","Terekeka",origin),
                
                assess= countyAssessed,
                assess= ifelse(assess =="Aweil Center","Aweil Centre",assess),
                assess= ifelse(assess =="Canal (Khorfulus)","Canal/Pigi",assess),
                assess= ifelse(assess =="Kajo-Keji","Kajo-keji",assess),
                
                count = number
        ) %>% 
        select(period,origin, assess,count,countryOrigin, admin2Pcod) %>% 
        group_by(period,origin, assess,countryOrigin,admin2Pcod) %>%
        summarize(count=sum(count))  %>% ungroup(period)%>%
        # mutate(period=ifelse(period
        #                      %in% c("2017JanJun","2017JulDec"),
        #                      "2017",period)) %>%
        filter(period!="2018JanApr" & count !=0)

#generate centroids for county
my_map <- my_map %>% mutate(center= st_centroid(geometry)) %>%
        mutate(assessed= admin2Pcod %in% unique(newiom$admin2Pcod))
centroids <- select(as.data.frame(my_map), admin2Name,center) %>%
        mutate(centerx =NA, centery = NA) 

for (i in seq_along(centroids$admin2Name)){
        centroids$centerx[i] <- centroids$center[[i]][[1]]
        centroids$centery[i] <- centroids$center[[i]][[2]]
}

centroids <- centroids %>% select(-center)

#add points from other countries
cent_country <- data.frame(
        admin2Name = c("Sudan", "Uganda", "Kenya", "Ethiopia", "DRC"),
        centery = c(10.83,3.00,3.54,8.5,3.72),
        centerx = c(28.88,32.61,35.91,34.71,27.86)
)

centroids <- rbind(centroids,cent_country)

#combine centroid with dataset
final_d <- newiom2 %>% 
        #group_by(origin,period,assess) %>%
        #summarize(count=sum(count)) %>%
        left_join(centroids, by= c("origin"="admin2Name")) %>%
        rename(x_origin=centerx ,
               y_origin=centery     ) %>%
        left_join(centroids, by= c("assess"="admin2Name")) %>%
        rename(x_assess = centerx ,
               y_assess = centery     )%>%
        filter(origin!=assess) %>%
        filter(origin != "Other (Specify)")
        #mutate(countryOrigin=ifelse(countryOrigin=="South Sudan","Between County","Immigration"))

#supplemental table



# ft %>% flextable() %>% print(preview = "docx")
#


# Movement Heatmap
# 

# 

# ccp <- paste(incidence$origin,incidence$period)


# final_d %>% filter(paste(origin,period) %in%ccp) %>%
#         group_by(period)%>%
#         summarize(sum(count))
# 
# final_d %>% filter(paste(origin,period) %in%ccp) %>%
#         filter(paste(assess,period) %in%ccp) %>%
#         group_by(period)%>%
#         summarize(sum(count))

