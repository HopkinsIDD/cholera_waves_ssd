library(rstan)

# decide on the periods of exponential and not exponential growth
expgrowthJuba <- filter(master_dat,admin2Name=="Juba") %>%
        group_by(wave,onset_date_imp) %>%
        summarize(cases=n())%>%
        mutate(period= ifelse(onset_date_imp>=as.Date("2014-05-12")&
                                      onset_date_imp<=as.Date("2014-05-26"),
                              "Exp Growth","Not Exp Growth"),
               period= ifelse(onset_date_imp>=as.Date("2015-06-06")&
                                      onset_date_imp<=as.Date("2015-06-29"),
                              "Exp Growth",period),
               period= ifelse(onset_date_imp>=as.Date("2016-07-12")& 
                                      onset_date_imp<=as.Date("2016-07-28"),
                              "Exp Growth",period)
        )

# graph showing  exponential and not exponential growh
# expgrowthJuba %>% ggplot(aes(x=onset_date_imp,y=cases,fill=period))+
#         geom_col()+
#         facet_grid(.~wave,scales = "free")+
#         theme_bw()

#limit to periods of exponential growth
curve2014 <- filter(expgrowthJuba, wave=="2014" & period=="Exp Growth") %>%
        mutate(day=0:(nrow(.)-1))
curve2015 <- filter(expgrowthJuba, wave=="2015" & period=="Exp Growth")%>%
        mutate(day=0:(nrow(.)-1))
curve2016 <-filter(expgrowthJuba, wave=="2016/2017" & period=="Exp Growth")%>%
        mutate(day=0:(nrow(.)-1))

T14=nrow(curve2014)
T15=nrow(curve2015)
T16=nrow(curve2016)

## prep data for Stan model
stan_dat_2014 <- list(
        T_max=T14-1, # number of days to consider for estimatation
        N_mat =curve2014$cases[2:T14], ## cases per day
        N0=curve2014$cases[1], # number of cases to start
        shape_mean = 1,
        scale_mean = 1,
        shape_sd = 10,
        scale_sd = 10
)

stan_dat_2015 <- list(
        T_max=T15-1, # number of days to consider for estimatation
        N_mat =curve2015$cases[2:T15], ## cases per day
        N0=curve2015$cases[1], # number of cases to start
        shape_mean = 1,
        scale_mean = 1,
        shape_sd = 10,
        scale_sd = 10
)

stan_dat_2016 <- list(
        T_max=T16-1, # number of days to consider for estimatation
        N_mat =curve2016$cases[2:T16], ## cases per day
        N0=curve2016$cases[1], # number of cases to start
        shape_mean = 1,
        scale_mean = 1,
        shape_sd = 10,
        scale_sd = 10
)



#compile stan code
# fileName <- "code/extra/GT_estimation.stan"
# ret <- stanc(fileName) # Check Stan file
# ret_sm <- stan_model(stanc_ret = ret) # Compile Stan code
# save(fileName, ret, ret_sm, file="code/extra/GT_estimation_compiled.RData")

load(file="code/extra/GT_estimation_compiled.RData")
warmup <- 1000
iter <- 5000

fit2014 <- sampling(ret_sm, warmup=warmup, iter=iter,
                    seed=123, data=stan_dat_2014, chains=4,
                    control=list(adapt_delta=.95))
fit2015 <- sampling(ret_sm, warmup=warmup, iter=10000000,
                    seed=123, data=stan_dat_2015, chains=4,
                    control=list(adapt_delta=.95))
fit2016 <- sampling(ret_sm, warmup=warmup, iter=iter,
                    seed=123, data=stan_dat_2016, chains=4,
                    control=list(adapt_delta=.95))


# save the fits
# saveRDS(fit2014,"data/generated_data/fit2014.rds")
# saveRDS(fit2015,"data/generated_data/fit2015.rds")
# saveRDS(fit2016,"data/generated_data/fit2016.rds")

