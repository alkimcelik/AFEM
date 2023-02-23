#setwd("~/sciebo/UEE/TEACHING/Seminar_Advanced_Forecasting_in_Energy_Markets/R-code/MOSMIX/")

#region
# %%
pw<- "#q6a21I&OA5k"
library(conflicted)
library(tidyr)
library(RMariaDB)
library(dplyr)
library(dbx)
library(stringr)
library(lubridate)
library(readr)
library(shiny)
library(data.table)
library(zoo)
library(quantreg)
library(corrplot)
library(reshape2)
library(ggplot2)
library(FactoMineR)
library(xgboost)
library(tseries)
library(glmnet)
library(MASS)
library(Hmisc)


conflict_prefer("na.locf", "zoo")
conflict_prefer("filter", "dplyr")
conflict_prefer("select", "dplyr")
conflict_prefer("adf.test", "tseries")

data_preparation_for_prediction <- function(){
  last_forecast_horizons <- DATA %>% group_by(DateTime) %>% summarise(last_timestamp = max(forecast_origin))
  last_forecast_horizons_joined <- last_forecast_horizons%>% inner_join(DATA, by = c("DateTime" = "DateTime" , "last_timestamp"="forecast_origin"))%>% 
    filter(DateTime < ymd_hms("2023-01-01 00:00:00"))
  last_forecast_horizons_joined$last_timestamp <- NULL
  last_forecast_horizons_joined <- last_forecast_horizons_joined %>% mutate(x_lag_24 = dplyr::lag(AT_Load_Actual, n = 24), x_lag_168 = dplyr::lag(AT_Load_Actual, n=168))
  last_forecast_horizons_joined <- last_forecast_horizons_joined[c(169:nrow(last_forecast_horizons_joined)),]
  last_forecast_horizons_joined_train <- last_forecast_horizons_joined %>% filter(DateTime <= ymd_hms("2022-11-27 12:00:00"))
  last_forecast_horizons_joined_test <- last_forecast_horizons_joined %>% filter(DateTime > ymd_hms("2022-11-27 12:00:00"))
  last_forecast_horizons_joined_test$HU_Load_Actual <- ar_filling_regressors("HU_Load_Actual")
  last_forecast_horizons_joined_test$DE_Load_Actual <- ar_filling_regressors("DE_Load_Actual")
  last_forecast_horizons_joined_test$CZ_Load_Actual <- ar_filling_regressors("CZ_Load_Actual")
  last_forecast_horizons_joined_test$SK_Load_Actual <- ar_filling_regressors("SK_Load_Actual")
  last_forecast_horizons_joined_test$SI_Load_Actual <- ar_filling_regressors("SI_Load_Actual")
  last_forecast_horizons_joined <- rbind(last_forecast_horizons_joined_train, last_forecast_horizons_joined_test)
  last_forecast_horizons_joined
}

ar_filling_regressors <- function(ytarget){
  load_actual_data <- DATA %>% select(DateTime, AT_Load_Actual, HU_Load_Actual, DE_Load_Actual, CZ_Load_Actual, SK_Load_Actual, SI_Load_Actual) %>% distinct()
  load_actual_data_test <- load_actual_data %>% filter(DateTime > ymd_hms("2022-11-27 12:00:00"))
  load_actual_data_train <- load_actual_data %>% filter(DateTime <= ymd_hms("2022-11-27 12:00:00"))
  y <- unlist(load_actual_data_train[, ytarget])
  om <- 4*24*7
  mod <- ar(na.locf(y), order.max = om)
  predict(mod, n.ahead = nrow(load_actual_data_test))$pred
}

forecasting <- function(){
  last_forecast_horizons_joined <- data_preparation_for_prediction()
  mod <- rq(AT_Load_Actual ~ TTT  + FX1 + Neff + FF + Rad1h + HU_Load_Actual + DE_Load_Actual + CZ_Load_Actual + SK_Load_Actual + SI_Load_Actual + as.factor(HoD) + as.factor(DoW) + as.factor(is_holiday), data = last_forecast_horizons_joined)
  print(summary(mod))
  adf_result <- adf.test(mod$residuals)
  print(paste("p-value of residuals:", adf_result$p.value))
  print("Since p-value is less than the significant threshold, residuals are stationary.")
  for (time in seq(ymd("2022-11-27"), ymd("2023-01-01"), by = "day")){
    test <- last_forecast_horizons_joined %>% filter((ymd_hms(DateTime) >= ymd_hms(paste(as.Date(time),"00:00:00"))) & (ymd_hms(DateTime) < format(ymd_hms(paste(as.Date(time),"00:00:00")) + 86400, "%Y-%m-%d %H:%M:%S")))
    train <- last_forecast_horizons_joined %>% filter(ymd_hms(DateTime) < ymd_hms(paste(as.Date(time),"00:00:00")))
    remaining <- last_forecast_horizons_joined %>% filter( (ymd_hms(DateTime) >= format(ymd_hms(paste(as.Date(time),"00:00:00")) + 86400, "%Y-%m-%d %H:%M:%S")))
    if(sum(is.na(test$x_lag_24)) > 0){
      last_forecast_horizons_joined$x_lag_24 = dplyr::lag(last_forecast_horizons_joined$AT_Load_Actual, 24)
      last_forecast_horizons_joined$x_lag_168 = dplyr::lag(last_forecast_horizons_joined$AT_Load_Actual, 168)
      test <- last_forecast_horizons_joined %>% filter((ymd_hms(DateTime) >= ymd_hms(paste(as.Date(time),"00:00:00"))) & (ymd_hms(DateTime) < format(ymd_hms(paste(as.Date(time),"00:00:00")) + 86400, "%Y-%m-%d %H:%M:%S")))
      train <- last_forecast_horizons_joined %>% filter(ymd_hms(DateTime) < ymd_hms(paste(as.Date(time),"00:00:00")))
      remaining <- last_forecast_horizons_joined %>% filter( (ymd_hms(DateTime) >= format(ymd_hms(paste(as.Date(time),"00:00:00")) + 86400, "%Y-%m-%d %H:%M:%S")))
    }
    mod <- rq(AT_Load_Actual ~ TTT  + FX1 + Neff + x_lag_24 + x_lag_168 + HU_Load_Actual + DE_Load_Actual + CZ_Load_Actual + SK_Load_Actual + SI_Load_Actual + as.factor(HoD) + as.factor(DoW) + as.factor(is_holiday), data = train)
    test$AT_Load_Actual <- na.approx(predict(mod,  newdata = test)) #linear interpolation for missing values coming from prediction
    last_forecast_horizons_joined <- rbind(train, test, remaining)
  }
  last_forecast_horizons_joined$HU_Load_Actual <- na.locf(last_forecast_horizons_joined$HU_Load_Actual)#filling few null values
  last_forecast_horizons_joined
}

pca_importance <- function(){
  pca <- PCA(na.omit(DATA[,c(4:6,8:9,20:24)]), scale.unit = TRUE, ncp = 12, graph = FALSE)
  
  # extract the principal component scores
  scores <- pca$ind$coord
  
  # extract the variable importance measures
  var_importance <- pca$var$contrib
  
  # plot the variable importance measures
  var_df <- data.frame(var_names = rownames(var_importance), importance = var_importance[,1])
  ggplot(data = var_df, aes(x = reorder(var_names, importance), y = importance, fill = importance)) + 
    geom_bar(stat = "identity") + 
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
    labs(title = "Variable Importance by PCA", x = "Variable", y = "Importance")
}

correlation_map <- function(){
  correlation <- round(cor(DATA[,c(4:9,11,21:25)], use = "complete.obs"),2)
  correlation[upper.tri(correlation)] <- NA
  correlation <- na.omit(reshape2::melt(correlation))
  
  # Create ggplot without NA values and move y-ticks to the right side
  ggplot(data = correlation, aes(x = Var2, y = Var1, fill = value)) + 
    geom_tile() +
    geom_text(aes(label = sprintf("%1.2f", value)), size = 4) + # show correlation values with 2 decimal places
    scale_fill_gradient2(low = "red", high = "green", limit = c(-1, 1), name = "Correlation") +
    scale_x_discrete(expand = c(0,0)) + # remove gray areas in x-axis
    scale_y_discrete(expand = c(0,0)) + # remove gray areas in y-axis
    theme(axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          panel.background = element_blank())
}

log_midpipe <- function(x, ...) {
    force(x)
    cat(..., "\n")
    return(x)
}
# %%
#endregion
#anil
#region
# %% Get DWD Data
db <- dbxConnect(
    adapter = RMariaDB::MariaDB(),
    host = "132.252.60.112",
    port = 3306,
    dbname = "DWD_MOSMIX",
    user = "student", # Read only user
    password = pw,
    timezone_out = "UTC"
)

## locations in 'wider' EUROPE in DWD_MOSMIX
LOCATIONS <- tbl(db, "locations") %>%
    filter(
        longitude >= -30,
        longitude <= 60,
        latitude >= 30,
        latitude <= 80
    ) %>%
    collect()

ZONE <- c("AT", "HU", "DE","CZ", "SK","SI")
i.z <- 6 # later in loop
zone <- ZONE[i.z]

CITY <- read_csv("worldcities.csv")

citythr <- 3 ## for simplification take citythr largest cities

cityidsub <- CITY %>%
    filter(iso2 %in% ZONE) %>%
    group_by(iso2) %>%
    slice_max(order_by = population, n = 3) %>%
    split(.$iso2) # Order may change compared to ZONE

STAT <- purrr::map(cityidsub, .f = function(x) {
    stationid <- numeric(nrow(x))
    for (i.x in 1:nrow(x)) { ## to loop across cityidsub
        L2toCity <- (LOCATIONS$longitude - x$lng[i.x])^2 +
            (LOCATIONS$latitude - x$lat[i.x])^2
        stationid[i.x] <- LOCATIONS$stationid[which.min(L2toCity)]
    }
    return(stationid)
})

meteovar <- c("TTT", "Rad1h", "Neff", "RR1c", "FF", "FX1")
inith <- 8 ## hour of day to use forecast, 8 means take data from 8am morning

MET <- list()

for (zn in ZONE) {
    MET[[zn]] <- tbl(db, "mosmix_s_forecasts") %>%
        filter(
            stationid %in% !!STAT[[zn]]
        ) %>%
        select(
            stationid, horizon, timetoforecast_utc, all_of(meteovar)
        ) %>%
        log_midpipe("Start collecting") %>%
        collect() %>%
        log_midpipe("Collecting done") %>%
        mutate(forecast_origin = timetoforecast_utc - horizon * 3600) %>%
        filter(lubridate::hour(forecast_origin) == inith) %>%
        group_by(forecast_origin, horizon) %>%
        summarise_at(all_of(meteovar), mean, na.rm = TRUE) %>%
        arrange(forecast_origin, horizon) %>%
        mutate(
            timetoforecast_utc = forecast_origin + horizon * 3600, .before = 1
        ) #%>%
        # mutate(
        #     HoD = hour(timetoforecast_utc),
        #     DoW = wday(timetoforecast_utc, week_start = 1),
        #     DoY = yday(timetoforecast_utc),
        #     .after = 1
        # )
    cat(zn, "\n")
}
dbxDisconnect(db)
# %%
#endregion

#region get entsoe data
# %% Get ENTSOE Data
edb <- dbxConnect(
    adapter = RMariaDB::MariaDB(),
    host = "132.252.60.112",
    port = 3306,
    dbname = "ENTSOE",
    user = "student", # Read only user
    password = pw,
    timezone_out = "UTC"
)

EDAT<- list()
for (zn in ZONE) {

    # Say you want to get wind generation, actual and day-ahead for PT (portugal)
    # only where the year is 2022

    # Obtain specification table
    spec <- tbl(edb, "spec") %>% collect()

    # Get an overview
    glimpse(spec)

    unique(spec$Name) # We need "Generation" here...
    unique(spec$Type) # "DayAhead" and "Actual" ...
    unique(spec$ProductionType) # "Wind Onshore" and "Wind Offshore" ...
    unique(spec$MapCode) # We want "PT" here...
    unique(spec$MapTypeCode) # We take "BZN" here ...

    # Lets narrow down the spec table to get the targetTimeSeriesID's
    targets <- spec %>%
        filter(
            Name == "Load",
            Type %in% c("DayAhead", "Actual"),
            # ProductionType %in% c("Load"),
            MapCode == zn,
            MapTypeCode == "CTY"
        ) %>%
        # Remove empty columns
        select_if(function(x) !(all(is.na(x)) | all(x == "")))


    # Obtain (a connection to) the forecasts table
    values <- tbl(edb, "vals")

    glimpse(values)

    # Get the actual data
    EDAT[[zn]] <- values %>%
        filter(TimeSeriesID %in% !!targets$TimeSeriesID) %>%
        collect() %>%
        left_join(spec, by = "TimeSeriesID") %>%
        filter(
            lubridate::year(DateTime) >= 2015, # here keep only from 2021+
            lubridate::minute(DateTime) == 0
        )

# We may want to select and wrangle even further:
    EDAT[[zn]] <- EDAT[[zn]] %>%
        # Select the cols of interest
        select(DateTime, MapCode, Name, Type, Value) %>%
        arrange(DateTime) %>%
        pivot_wider(names_from = c(MapCode, Name, Type), values_from = Value)
names(EDAT[[zn]]) <- gsub(" ", "", names(EDAT[[zn]]))

}

dbxDisconnect(edb)

# %%
#endregion

#region holidays
#%% holidays
holidays<-read_csv("holidays/holidays_2000_2030.csv")
holidays$Name[] <- gsub(" ", "", holidays$Name)
#shrink to relevant space
# hld <- holidays %>% filter(CountryCode == "DE") 

get.HLD<- function(xtime, zone="DE", S=24, deg=3, bridgep = 0.5, k=0.25){
    # zone only supports full countries at the moment 
    # deg is, cubic spline degree
    # fraction of bridging effects, if 0.5 then half a day before and after the holiday potential effects can occur, values between 0 and 2 are reasonable
    # k determines the number of grid points for the splines, if k=1/S then each data point we have a new basis (if in addition deg=1, these are indicators), the smaller k the more basis functions.
    yrange<- range(lubridate::year(xtime))+c(-1,1) # safety margins [allows for predictions up to 1 year ahead]
    holidays <- holidays %>% filter(CountryCode == zone) %>%
        filter(lubridate::year(Date)>=yrange[1] & lubridate::year(Date)<=yrange[2] ) 
    # remove holidays which were not lauched yet.
    holidays$LaunchYear[is.na(holidays$LaunchYear)]<- 0
    holidays %>% filter(lubridate::year(Date)>= LaunchYear ) 
    #holidays %>% select(Date, Name) %>% mutate(
    #            DoW = lubridate::wday(Date, week_start = 1)
    #            )## TODO think about more information es. Global and counties
    #mutate_at(vars(x, y), factor)
    holidays$Name <- as.factor(holidays$Name)
    holnames<- levels(holidays$Name)
    #holnames <- gsub(" ", "",holnames)
    hldN<- length(holnames)

    xbas<- -(S*bridgep):((1+bridgep)*S)
    xbask<- seq(min(xbas), max(xbas), length=k*(length(xbas)-1)+1)
    library(splines)
    hldbas<- splineDesign(xbask, xbas, outer.ok=TRUE, ord=deg+1)
    hldbasc<- t(apply(hldbas,1,cumsum))
    hldbasx<- hldbas#cbind(hldbas,hldbasc)
    K<- dim(hldbasx)[2]

    DHL<- array(0, dim=c(length(xtime),K*hldN) ) ## add cumulative
    i.hld<-1
    for(i.hld in 1:hldN){
        idhld <- which<-which(xtime %in% as.POSIXct( holidays$Date[ holidays$Name==holnames[i.hld]], tz="UTC"))
        # idhld<- which(as.numeric(format(dtime, "%m")) == DATEhld[[i.hld]][1]	& as.numeric(format(dtime, "%d")) == DATEhld[[i.hld]][2] & as.numeric(format(dtime, "%H")) == 0)
        for(i.b in seq_along(idhld)){ ## add basis segments
                idb<- (idhld[i.b]+min(xbas)):(idhld[i.b]+max(xbas)) ## TODO does not work properly if hld is first or last day...
                idbact<- which(idb>0 & idb<= dim(DHL)[1])
                DHL[idb[idbact],(1:K)+K*(i.hld-1)]<- hldbasx[idbact,]
        }
    }

    ## holidays prepared but not used.
    dimnames(DHL)<- list(NULL,paste0(rep(holnames, rep(K,length(holnames))),"_",1:K))
    DHL 
}

#tmp<-get.HLD(DATA$DateTime, zone="AT")

#%%
#endregion

#region select a zone and create main data frame
#from here we work only with the first country/zone:
# %% combine meteorologic and entsoe data
for (tempindex in 1:6) {
  zone<- ZONE[[tempindex]]
  
  names(MET[[zone]])[1]<- "DateTime"
  DATA_temp <- dplyr::full_join(MET[[zone]], EDAT[[zone]], by = c("DateTime")) %>% arrange(DateTime, horizon) 
  names(DATA_temp) <- gsub(" ", "", names(DATA_temp))
  dnames <- names(DATA_temp)
  DATA_temp %>% arrange(DateTime, horizon)  ## this will be the input to all algorithms.
  ## DateTime is in UTC
  DATA_temp<- DATA_temp %>% mutate( DateTimeCET = as.POSIXlt(DateTime, tz="CET")            )
  
  
  SummerTime = lubridate::dst(DATA_temp$DateTimeCET)
  HoD = lubridate::hour(DATA_temp$DateTime)
  DoW = lubridate::wday(DATA_temp$DateTime, week_start = 1)
  DoY = lubridate::yday(DATA_temp$DateTime)
  HoDDST = lubridate::hour(DATA_temp$DateTimeCET)
  DoWDST = lubridate::wday(DATA_temp$DateTimeCET, week_start = 1)
  DoYDST = lubridate::yday(DATA_temp$DateTimeCET)
  DET<- cbind(SummerTime, HoD, DoW, DoY, HoDDST, DoWDST, DoYDST)
  
  DATA_temp<- cbind(DATA_temp, DET)
  assign(paste('DATA',zone,sep='_'),DATA_temp)
}

## illustration that HoDDST is better than HoD 

#DATA<- cbind(DATA, DateTimeDST)
#save.image("workspace.RData")

#load("workspace.RData")

# %%
#endregion

#region define shiftDST
# %%
# shift function that acts on local time if summer is provided otherwise simple shift
shiftDST<- function(x, clag=1, summer=NULL, hfreq=1){ 
    # clag == vector of lags
    # summer is index set of length/dim[1] of x indicating the summer time, e.g. comp by by "$isdst" argument of POSIXlt
    # hfreq=1 for hourly data, =4 for quarterly hourly etc. - indicated summer time shift, only relevant if summer is provided
    if(is.null(summer)){
        Xlst <- as.matrix(data.table(x)[, shift(.SD, clag, give.names=TRUE)])
    } else {
        SLAGS<- matrix(clag, ncol=length(clag),nrow=3, byrow=TRUE) + -1:1*hfreq
        Sxl <- as.matrix(data.table(x)[, data.table::shift(.SD, SLAGS, give.names=TRUE)])
        xd<- dim(Sxl)[2]/3
        Xlst <- as.matrix(Sxl[, 0:(xd-1)*3+2])
        dsummer<- c(0, diff(summer)) # TODO strictly minor error if first observation is at clock-change.
        marchid <- which(dsummer == 1) # 23 hours
        octid <- which( dsummer == -1)
        II<- diag(length(clag)) == 1
        for(i.l in seq_along(clag))for (i.r in seq_along(marchid)) {
            rid<- unique(pmax(pmin(marchid[i.r] + 0:(clag[i.l] - 1), dim(Xlst)[1]),1))
            Xlst[rid,i.l] <- Sxl[rid, (0:(xd-1)*3+1)[i.l]]
        }
        for(i.l in seq_along(clag))for (i.r in seq_along(octid)) {
            rid<- unique(pmax(pmin(octid[i.r] + 0:(clag[i.l] - 1), dim(Xlst)[1]),1))
            Xlst[rid,i.l] <- Sxl[rid, (0:(xd-1)*3+3)[i.l]]
        }
        }
    Xlst
}

clag<- c(24*1:4)
hfreq<- 1
x<- as.matrix(DATA_DE[,c(11)])
summer = SummerTime

xouts<- shiftDST(x, clag=c(168), SummerTime)
head(xouts)
xout<- shiftDST(x, clag=c(168))

idx<- 1:20000
summary(mods<-lm(unlist(DATA_DE[idx,c(11)]) ~ xouts[idx]))
summary(mod<-lm(unlist(DATA_DE[idx,c(11)]) ~ xout[idx]))

DATA <- merge(DATA_AT, DATA_HU[,c("DateTime","forecast_origin","HU_Load_Actual")], how = "left", on = c("DateTime","forecast_origin"))
DATA <- merge(DATA, DATA_DE[,c("DateTime","forecast_origin","DE_Load_Actual")], how = "left", on = c("DateTime","forecast_origin"))
DATA <- merge(DATA, DATA_CZ[,c("DateTime","forecast_origin","CZ_Load_Actual")], how = "left", on = c("DateTime","forecast_origin"))
DATA <- merge(DATA, DATA_SK[,c("DateTime","forecast_origin","SK_Load_Actual")], how = "left", on = c("DateTime","forecast_origin"))
DATA <- merge(DATA, DATA_SI[,c("DateTime","forecast_origin","SI_Load_Actual")], how = "left", on = c("DateTime","forecast_origin"))
DATA <- DATA %>% filter(DateTime < ymd_hms("2023-01-01 00:00:00"))

holidays_AT <- holidays %>% dplyr::filter(CountryCode == "AT")
DATA_copy <- DATA
DATA_copy$Date <- as.Date(DATA_copy$DateTime)
DATA_with_holidays <- merge(DATA_copy, holidays_AT %>% dplyr::select(1,3), by = "Date", all.x=TRUE)
DATA_with_holidays$Name[!is.na(DATA_with_holidays$Name)]<- 1
DATA_with_holidays$Name[is.na(DATA_with_holidays$Name)]<- 0
DATA_with_holidays$is_holiday <- as.factor(DATA_with_holidays$Name)
DATA_with_holidays$Date <- NULL
DATA_with_holidays$Name <- NULL
DATA_with_holidays <- DATA_with_holidays %>% arrange(DateTime, forecast_origin)
DATA <- DATA_with_holidays
# %%
#endregion

#region forecasting study part
H <- 240
horizonfull <- 1:H
last_time <- ymd_hms("2022-07-01 08:00:00") ## last known time
FSTUDYDAYS <- seq(last_time, max(DATA$DateTime) - 3600 * (H), by = 3600 * 24)
N <- length(FSTUDYDAYS)
zone <- c("AT")#,"HU")
IDTEST <- list()
for (i.hm in 1:N) {
  IDTEST[[i.hm]] <- which(DATA$DateTime >= FSTUDYDAYS[i.hm] + 1 * 3600 & DATA$DateTime <= FSTUDYDAYS[i.hm] + H * 3600 & FSTUDYDAYS[i.hm] == DATA$forecast_origin) # == FSTUDYDAYS[i.hm] == DATA$forecast_origin restricts to most recent known weather forecasts
}
model.names <- c("true","bench","GAM", "AR","GAM_new", "lm", "lasso", "lad","holt")
M <- length(model.names)
# for (i.m in model.names)
FORECASTS <- array(, dim = c(N, H, M))
dimnames(FORECASTS) <- list(format(FSTUDYDAYS, "%Y-%m-%d"), paste("h_", 1:H, sep = ""), model.names)

## reestimation ##
times <- list()
MAEs <- array(dim = c(2,M))
i.m <- 8
library(mgcv)
S <- 24
for (zones in zone){
  i = 1
  for (i.m in seq_along(model.names)) {
    ## forecasting horizon split [model dependent]
    mname <- model.names[i.m]
    start_time <- Sys.time()
    if (mname %in% c("GAM","GAM_new", "lm", "lasso", "lad")) {
      LAGS <- S * c(1:14, 21, 28)
      horizonc <- unique(c(0, findInterval(LAGS, 1:H)))
    } else { # AR
      horizonc <- c(0, H)
    }
    ## REESTIMATION [model dependent]
    reest <- 20 # could be chosen depending on model (!), usually fast models can be reestimated regularly.
    FSTUDYSEQid <- unique(c(seq(0, length(FSTUDYDAYS), by = reest), length(FSTUDYDAYS)))
    FSTUDYSEQ <- list()
    for (i.seq in 1:(length(FSTUDYSEQid) - 1)) FSTUDYSEQ[[i.seq]] <- FSTUDYDAYS[c((FSTUDYSEQid[i.seq] + 1), FSTUDYSEQid[i.seq + 1])]
    Nsplitlen <- length(FSTUDYSEQ) ## 
    
    #### model specific data preparation for the forecasting study [model dependent]: 
    if(mname == "GAM" || mname == "GAM_new" || mname == "lm"|| mname == "lasso" || mname == "lad"){
      library(data.table)
      vec<- as.integer(DATA$DateTime)
      subs<- match(unique(vec), vec)
      TMPDATA <- bind_cols(DateTime=DATA$DateTime[subs], shiftDST(DATA[subs,ytarget], summer=DATA$SummerTime[subs], clag=LAGS) )#bind_cols(EDAT[[zone]][, "DateTime"], as.data.table(shift(as.data.frame(EDAT[[zone]][, 1:2 + 1]), LAGS, give.names = TRUE))
      FDATA <- dplyr::full_join(DATA, TMPDATA, by = c("DateTime")) %>% arrange(DateTime, horizon) 
    } else {
      FDATA<- DATA 
    }
    DATATEST <- FDATA[unlist(IDTEST), ] # maximum test data,
    
    i.N <- 2
    for (i.N in seq_along(FSTUDYSEQ)) {
      seqid <- ((FSTUDYSEQid[i.N] + 1):FSTUDYSEQid[i.N + 1])
      ## model
      ### the horizonsplit is part of the model (!), esp. which lags etc. are used
      HORIZON <- list()
      for (i.hl in seq_along(horizonc)[-1]) HORIZON[[i.hl - 1]] <- (horizonc[i.hl - 1] + 1):horizonc[i.hl]
      
      i.hl <- 1
      for (i.hl in seq_along(HORIZON)) {
        
        hmin <- head(HORIZON[[i.hl]], 1)
        hmax <- tail(HORIZON[[i.hl]], 1)
        ## define in-sample/train and out-of-sample/test [caution both depend on FSTUDYSEQ and(!) horizon (as availability of weather data and lags varies)]
        idt <- FDATA$DateTime <= FSTUDYSEQ[[i.N]][1] & FDATA$horizon >= hmin & FDATA$horizon <= hmax & !is.na(FDATA$horizon)
        
        DATAtrain <- FDATA[idt, ]
        
        idtestl <- list()
        for (i.hm in seqid) {
          idtestl[[i.hm]] <- which(DATATEST$DateTime >= FSTUDYDAYS[i.hm] + hmin * 3600 & DATATEST$DateTime <= FSTUDYDAYS[i.hm] + hmax * 3600 & DATATEST$horizon >= hmin & DATATEST$horizon <= hmax & FSTUDYDAYS[i.hm] == DATATEST$forecast_origin)
        }
        idtest <- unlist(idtestl)
        DATAtest <- DATATEST[idtest, ] %>% arrange(horizon, DateTime)
        # head(DATAtrain[,1:7])
        
        ytarget <- paste(zones, "_Load_Actual", sep = "")
        if (mname == "GAM") {
          act_lags <- LAGS[LAGS >= hmax]
          formstr <- paste(ytarget, " ~ ti(HoD,k=18) + ti(DoW, k=7) + ti(DoW,HoD, k=c(6,12), bs='cs') + ti(DoY, bs='cs') + ti(TTT,k=6, bs='cs') + ", paste(paste("ti(", "x_lag_", act_lags, ",bs='cs',k=4)", sep = ""), collapse = "+"), sep = "") # + ti(TTT,k=6, bs='cs')
          form <- as.formula(formstr)
          mod <- bam(form, data = DATAtrain, select = TRUE, gamma = log(dim(DATA)[1]) / 2, discrete = TRUE)
          print(summary(mod))
          pred <- t(matrix(predict(mod, newdata = DATAtest), nrow = length(HORIZON[[i.hl]]), byrow = TRUE))
        } # GAM
        if (mname == "GAM_new") {
          act_lags <- LAGS[LAGS >= hmax]
          #formstr <- paste(ytarget, " ~ ti(HoD,k=18) + ti(DoW, k=7) + ti(DoW,HoD, k=c(6,12), bs='cs') + ti(DoY, bs='cs') + ti(TTT,k=6, bs='cs') + ti(FF,k=6, bs='cs') + ti(FX1,k=6, bs='cs') + ti(Neff,k=6, bs='cs') + ti(Rad1h,k=6, bs='cs') + s(Name, bs='fs') + ", paste(paste("ti(", zones, "_Load_Actual_lag_", act_lags, ",bs='cs',k=4)", sep = ""), collapse = "+"), sep = "") # + ti(TTT,k=6, bs='cs')
          formstr <- paste(ytarget, " ~ ti(HoD,k=18) + ti(DoW, k=7)  + ti(DoW,HoD, k=c(6,12), bs='cs') + ti(DoY, bs='cs') + ti(TTT,k=6, bs='cs') + 
                            ti(FX1,k=6, bs='cs') + ti(Neff,k=6, bs='cs')  + 
                           ti(HU_Load_Actual,k=4, bs='cs') + ti(DE_Load_Actual,k=4, bs='cs') + ti(CZ_Load_Actual,k=4, bs='cs') + ti(SK_Load_Actual,k=4, bs='cs') + ti(SI_Load_Actual,k=4, bs='cs') +
                           ", paste(paste("ti(", "x_lag_", act_lags, ",bs='cs',k=4)", sep = ""), collapse = "+"), sep = "") # + ti(TTT,k=6, bs='cs')
          form <- as.formula(formstr)
          mod <- bam(form, data = DATAtrain, select = TRUE, gamma = log(dim(DATA)[1]) / 2, discrete = TRUE)
          print(summary(mod))
          pred <- t(matrix(predict(mod, newdata = DATAtest), nrow = length(HORIZON[[i.hl]]), byrow = TRUE))
        } # GAM
        if (mname== "lasso"){
          DATAtrainDummy <- cbind(DATAtrain["AT_Load_Actual"], DATAtrain["TTT"], 
                                  DATAtrain["FX1"], DATAtrain["Neff"], 
                                  DATAtrain["x_lag_24"], DATAtrain["x_lag_168"], DATAtrain["HU_Load_Actual"],
                                  DATAtrain["DE_Load_Actual"], DATAtrain["CZ_Load_Actual"], 
                                  DATAtrain["SK_Load_Actual"], DATAtrain["SI_Load_Actual"],
                                  model.matrix(~ as.factor(is_holiday), data=DATAtrain)[,-1],
                                  model.matrix(~ as.factor(DoW) , data = DATAtrain)[,-1], 
                                  model.matrix(~ as.factor(HoD) , data = DATAtrain)[,-1])
          DATAtestDummy <- cbind(DATAtest["AT_Load_Actual"], DATAtest["TTT"], 
                                 DATAtest["FX1"], DATAtest["Neff"], 
                                 DATAtest["x_lag_24"], DATAtest["x_lag_168"], DATAtest["HU_Load_Actual"],
                                 DATAtest["DE_Load_Actual"], DATAtest["CZ_Load_Actual"], 
                                 DATAtest["SK_Load_Actual"], DATAtest["SI_Load_Actual"],
                                 model.matrix(~ as.factor(is_holiday), data=DATAtest)[,-1],
                                 model.matrix(~ as.factor(DoW) , data = DATAtest)[,-1],
                                  model.matrix(~ as.factor(HoD) , data = DATAtest)[,-1])
          if(ncol(DATAtrainDummy) != ncol(DATAtestDummy)){
            DATAtrainDummy <- cbind(DATAtrain["AT_Load_Actual"], DATAtrain["TTT"], 
                                    DATAtrain["FX1"], DATAtrain["Neff"], 
                                    DATAtrain["x_lag_24"], DATAtrain["x_lag_168"], DATAtrain["HU_Load_Actual"],
                                    DATAtrain["DE_Load_Actual"], DATAtrain["CZ_Load_Actual"], 
                                    DATAtrain["SK_Load_Actual"], DATAtrain["SI_Load_Actual"],
                                    model.matrix(~ as.factor(is_holiday), data=DATAtrain)[,-1],
                                    model.matrix(~ as.factor(HoD) , data = DATAtrain)[,-1])
            DATAtestDummy <- cbind(DATAtest["AT_Load_Actual"], DATAtest["TTT"], 
                                   DATAtest["FX1"], DATAtest["Neff"], 
                                   DATAtest["x_lag_24"], DATAtest["x_lag_168"], DATAtest["HU_Load_Actual"],
                                   DATAtest["DE_Load_Actual"], DATAtest["CZ_Load_Actual"], 
                                   DATAtest["SK_Load_Actual"], DATAtest["SI_Load_Actual"],
                                   model.matrix(~ as.factor(is_holiday), data=DATAtest)[,-1],
                                   model.matrix(~ as.factor(HoD) , data = DATAtest)[,-1])
            mod = glmnet(as.matrix(na.omit(DATAtrainDummy)[,2:ncol(DATAtrainDummy)]), as.matrix(na.omit(DATAtrainDummy)["AT_Load_Actual"]), alpha = 1, lambda = 2)
            print(summary(mod))
            pred <- t(matrix(predict(mod, s = 2, newx = as.matrix(DATAtestDummy[,2:ncol(DATAtestDummy)])), nrow = length(HORIZON[[i.hl]]), byrow = TRUE))
          }
          else {
            mod = glmnet(as.matrix(na.omit(DATAtrainDummy)[,2:ncol(DATAtrainDummy)]), as.matrix(na.omit(DATAtrainDummy)["AT_Load_Actual"]), alpha = 1, lambda = 2)
            print(summary(mod))
            pred <- t(matrix(predict(mod, s = 2, newx = as.matrix(DATAtestDummy[,2:ncol(DATAtestDummy)])), nrow = length(HORIZON[[i.hl]]), byrow = TRUE))
          }
        }
        if (mname== "lm"){
          mod <- lm(AT_Load_Actual ~ TTT  + FX1 + Neff  + x_lag_24 + x_lag_168 +  HU_Load_Actual + DE_Load_Actual + CZ_Load_Actual + SK_Load_Actual + SI_Load_Actual +  as.factor(HoD) + as.factor(DoW) + as.factor(is_holiday) , data = DATAtrain)
          print(summary(mod))
          pred <- t(matrix(predict(mod,  newdata = DATAtest), nrow = length(HORIZON[[i.hl]]), byrow = TRUE))
        }
        if (mname== "lad"){
          mod <- rq(AT_Load_Actual ~ TTT + FX1 + Neff + x_lag_24 + x_lag_168 + HU_Load_Actual + DE_Load_Actual + CZ_Load_Actual + SK_Load_Actual + SI_Load_Actual + as.factor(HoD) + as.factor(DoW) + as.factor(is_holiday), data = DATAtrain)
          print(summary(mod))
          pred <- t(matrix(predict(mod,  newdata = DATAtest), nrow = length(HORIZON[[i.hl]]), byrow = TRUE))
        }
        if (mname == "AR") {
          DATAtrainwow <- DATAtrain[DATAtrain$horizon <= S, ] # without weather 'doubling'
          DATAtestwow <- (DATAtest[DATAtest$horizon <= S, ] %>% arrange(DateTime))
          y <- unlist(DATAtrainwow[, ytarget])
          yn <- length(y)
          DATAwow <- c(y, unlist(DATAtestwow[, ytarget]))
          DATAwow <- na.locf(DATAwow)
          om <- 4*24*7
          mod <- ar(na.locf(y), order.max = om)
          pred <- matrix(, length(seqid), H)
          for (i.NN in 1:length(seqid)) pred[i.NN, ] <- predict(mod, newdata = DATAwow[yn + (-mod$order + 1):0 + (i.NN - 1) * S], n.ahead = H)$pred
        }
        if (mname == "holt"){
          DATAtrainwow <- DATAtrain[DATAtrain$horizon <= S, ] # without weather 'doubling'
          DATAtestwow <- (DATAtest[DATAtest$horizon <= S, ] %>% arrange(DateTime))
          y <- ts(unlist(zoo::na.locf(DATAtrainwow[, ytarget])),frequency = 24, start = DATAtrainwow[1,"DateTime"])
          yn <- length(y)
          DATAwow <- c(y, unlist(DATAtestwow[, ytarget]))
          DATAwow <- zoo::na.locf(DATAwow)
          mod <- HoltWinters(y)
          pred <- matrix(, length(seqid), H)
          for (i.NN in 1:length(seqid)) pred[i.NN, ] <- predict(mod, newdata = DATAwow[yn + (-mod$order + 1):0 + (i.NN - 1) * S], n.ahead = H)

        }
        if (mname == "bench") {
          ybench <- paste(zones, "_Load_DayAhead", sep = "")
          pred <- t(matrix(unlist(DATAtest[, ybench]), nrow = length(HORIZON[[i.hl]]), byrow = TRUE))
        } # entsoe day-ahead 'benchmark'
        if (mname == "true") {
          pred <- t(matrix(unlist(DATAtest[, ytarget]), nrow = length(HORIZON[[i.hl]]), byrow = TRUE))
        }
        # if (mname == "true_GAM_new") {
        #   pred <- t(matrix(unlist(DATAtest_with_holidays[, ytarget]), nrow = length(HORIZON[[i.hl]]), byrow = TRUE))
        # }
        # if (mname == "tree"){
        #   formula2 <- paste(ytarget, " ~ ", " TTT + as.factor(HoD) + as.factor(DoW) + as.factor(DoY)  + FF + FX1 + Neff + Rad1h + ", paste(paste(zones, "_Load_Actual_lag_", act_lags, sep = ""), collapse = "+"))
        #   formula2 <- as.formula(formula2)
        #   tree_3 <- rpart(formula2 , data = DATAtrain)
        #   pred <- t(matrix(predict(tree_3, DATAtest),nrow = length(HORIZON[[i.hl]]), byrow = TRUE))
        # }
        if (mname == "AR" || mname == "holt"){
          FORECASTS[seqid, HORIZON[[i.hl]], mname] <- pred
        }
        else {
          FORECASTS[seqid, HORIZON[[i.hl]], mname] <- pred[1:length(FORECASTS[seqid, HORIZON[[i.hl]], mname])]
        }
        cat(zones, "horizon:", hmin, "-", hmax, " done at split ", round(i.N / Nsplitlen * 100, 2), "% progress, mod:", mname, "\n")
      } # i.hl
    } # i.N
  end_time <- Sys.time()
  times[mname] <- end_time - start_time
  } # i.m
  FFT<- FORECASTS
  for(i.m in 1:M)FFT[,,i.m]<- FORECASTS[, , "true"]
  RES<- FORECASTS - FFT
  
  MAEh <- apply(abs(RES), c(2,3), mean, na.rm = TRUE) 
  MAE <- apply(abs(RES), c(3), mean, na.rm = TRUE) 
  MAEs[i,] <- MAE
  i <- i + 1
}

FFT<- FORECASTS
for(i.m in 1:M)FFT[,,i.m]<- FORECASTS[, , "true"]
RES<- FORECASTS - FFT

MAEh <- apply(abs(RES), c(2,3), mean, na.rm = TRUE) 
MAE <- apply(abs(RES), c(3), mean, na.rm = TRUE) 
MAE

ts.plot(MAEh, col = 1:8, ylab = "MAE")
legend("topleft", model.names[-1], col = 1:8, lwd = 1)
abline(v = 0:10 * S, col = "orange")
abline(v = 0:10 * S - 8, col = "steelblue")



forecasted_data <- forecasting()
last_forecast_horizons_joined_temp_train_AT <- forecasted_data %>% filter(DateTime < ymd_hms("2022-06-21 08:00:00"))
last_forecast_horizons_joined_temp_test_AT <- forecasted_data %>% filter(DateTime >= ymd_hms("2022-06-21 08:00:00"))
mod_for_plot <- rq(AT_Load_Actual ~ TTT  + FX1 + Neff  + HU_Load_Actual + DE_Load_Actual + CZ_Load_Actual + SK_Load_Actual + SI_Load_Actual + as.factor(HoD) + as.factor(DoW) + as.factor(is_holiday), data = last_forecast_horizons_joined_temp_train_AT)
last_forecast_horizons_joined_temp_test_AT$AT_Load_Predicted <- predict(mod_for_plot,  newdata = last_forecast_horizons_joined_temp_test_AT)
ggplot(last_forecast_horizons_joined_temp_test_AT %>% filter((DateTime > ymd_hms("2022-08-21 12:00:00")) & (DateTime < ymd_hms("2022-09-21 12:00:00"))), aes(x = DateTime)) +   
  geom_line(aes(y = AT_Load_Actual, color = "Actual"),  linetype = "solid") +
  geom_line(aes(y = AT_Load_Predicted, color = "Predicted"), linetype = "dashed") +
  theme_minimal() +
  scale_color_manual(values=c("green", "red")) +
  scale_x_datetime(date_breaks = "1 week", date_labels = "%b %d") +
  xlab("Dates") + ylab("AT Actual Load (MWh)") +
  labs(color = "Load Type", linetype = "Load Type", 
       title = "Actual vs. Predicted Electricity Load in Austria")
pca_importance()
forecasted_data <- forecasted_data %>% filter(DateTime > ymd_hms("2022-12-21 08:00:00"))







