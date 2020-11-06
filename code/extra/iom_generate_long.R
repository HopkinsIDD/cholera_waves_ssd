# library(tidyverse)
# library(sf)
# library(gridExtra)
# library(grid)



# iomdata <- readxl::read_xlsx("data/20180720 IOM DTM MT R2 Final dataset_FJedited.xlsx")





idp201314<- makeLong(   t="IDP",       p = "2013/2014",
                       
                       #Country of Origin for returnees
                       country = "South Sudan",
                       #state of Origin
                       sOrigin="IDP Arrival 2013/2014: State of origin",
                       #county of Origin
                       cOrigin="IDP Arrival 2013/2014: County of origin",
                       #number of people
                       count="IDP Arrival 2013/2014:  (Ind.)"
                       
)

idp201516<- makeLong(   t="IDP",       p = "2015/2016",
                        
                        #Country of Origin for returnees
                        country = "South Sudan",
                        #state of Origin
                        sOrigin="IDP Arrival 2015/2016: State of origin",
                        #county of Origin
                        cOrigin="IDP Arrival 2015/2016: County of origin",
                        #number of people
                        count="IDP Arrival 2015/2016: (Ind.)"
                        
)

idp2017a<- makeLong(   t="IDP",       p = "2017JanJun",
                
                #Country of Origin for returnees
                country = "South Sudan",
                #state of Origin
                sOrigin="IDP Arrival Jan-June 2017: State of origin",
                #county of Origin
                cOrigin="IDP Arrival Jan-June 2017: County of origin",
                #number of people
                count="IDP Arrival Jan-June 2017 (Ind.)"
                
)

idp2017b<- makeLong(   t="IDP",       p = "2017JulDec",
                       
                       #Country of Origin for returnees
                       country = "South Sudan",
                       #state of Origin
                       sOrigin="IDP Arrival Jul-Dec 2017: State of origin",
                       #county of Origin
                       cOrigin="IDP Arrival Jul-Dec 2017: County of origin",
                       #number of people
                       count="IDP Arrival Jul-Dec 2017 (Ind.)"
                       
)

idp2018<- makeLong(   t="IDP",       p = "2018JanApr",
                       
                       #Country of Origin for returnees
                       country = "South Sudan",
                       #state of Origin
                       sOrigin="IDP Arrival 2018: State of origin",
                       #county of Origin
                       cOrigin="IDP Arrival 2018: County of origin",
                       #number of people
                       count="IDP Arrival 2018 (Ind.)"
                       
)

idp <- bind_rows(idp201314,idp201516,idp2017a,idp2017b,idp2018)

ret2015<- makeLong(   t="Returnee",       p = "2015",
                        
                        #Country of Origin for returnees
                        country = "Returnee Arrival 2015: Country of origin",
                        #state of Origin
                        sOrigin="Returnee Arrival 2015: State of origin",
                        #county of Origin
                        cOrigin="Returnee Arrival 2015: County of origin",
                        #number of people
                        count="Returnee Arrival 2015 (Ind.)"
)

ret2016<- makeLong(   t="Returnee",       p = "2016",
                      
                      #Country of Origin for returnees
                      country = "Returnee Arrival 2016: Country of origin",
                      #state of Origin
                      sOrigin="Returnee Arrival 2016: State of origin",
                      #county of Origin
                      cOrigin="Returnee Arrival 2016: County of origin",
                      #number of people
                      count="Returnee Arrival 2016 (Ind.)"
)

ret2017a<- makeLong(   t="Returnee",       p = "2017JanJun",
                      
                      #Country of Origin for returnees
                      country = "Returnee Arrival Jan-Jun 2017: Country of origin",
                      #state of Origin
                      sOrigin="Returnee Arrival Jan-Jun 2017:  State of origin",
                      #county of Origin
                      cOrigin="Returnee Arrival Jan-Jun 2017:  County of origin",
                      #number of people
                      count="Returnee Arrival Jan-Jun 2017 (Ind.)"
)

ret2017b<- makeLong(   t="Returnee",       p = "2017JulDec",
                       
                       #Country of Origin for returnees
                       country = "Returnee Arrival Jul-Dec 2017: Country of origin",
                       #state of Origin
                       sOrigin="Returnee Arrival Jul-Dec 2017:  State of origin",
                       #county of Origin
                       cOrigin="Returnee Arrival Jul-Dec 2017:  County of origin",
                       #number of people
                       count="Returnee Arrival Jul-Dec 2017 (Ind.)"
)

ret2018<- makeLong(   t="Returnee",       p = "2018JanApr",
                       
                       #Country of Origin for returnees
                       country = "Returnee Arrival 2018: Country of origin",
                       #state of Origin
                       sOrigin="Returnee Arrival 2018: State of origin",
                       #county of Origin
                       cOrigin="Returnee Arrival 2018: County of origin",
                       #number of people
                       count="Returnee Arrival 2018 (Ind.)"
)


ret <- bind_rows(ret2015,ret2016,ret2017a,ret2017b,ret2018)

newiom <- bind_rows(idp,ret)

write_csv(newiom, "data/generated_data/IOM_longdata.csv")




