library(loadflex)
library(openxlsx)
# Interpolation data: Packers Falls NO3 grab sample observations
dutch <- read.xlsx('Dutch_Hollow_WQ.xlsx',
                   sheet = 1,
                   startRow = 2,
                   detectDates = T)

  dutchpp <- dutch[,c(1,2,12)] # pull out PP data
    dutchpp <- na.omit(dutchpp) # only keep rows with samples of PP
    
  names(dutchpp) <- c('DATE', 'DISCHARGE', 'PP') # change column names
  
  dutchpp$DISCHARGE <- dutchpp$DISCHARGE*35.3146667 # convert m3/s to cfs
  
  #dutchpp$DATE <- as.POSIXct(dutchpp$DATE)
  
  dutchpp$PP <- dutchpp$PP/1000 # convert ug/l to mg/l
  
  dutchpp[,c(2,3)] <- round(dutchpp[,c(2,3)], digits = 4) # round to 4 digits

# Estimation data: Packers Falls discharge
  
  Q_dutch <- read.xlsx('Dutch_Q.xlsx',
                       sheet = 1, 
                       startRow = 2,
                       detectDates = T)
  
    names(Q_dutch) <- c('DATE', 'DISCHARGE')

    Q_dutch$DISCHARGE <- Q_dutch$DISCHARGE*35.3146667
    
    #Q_dutch$DATE <- as.POSIXct(Q_dutch$DATE)
    

meta <- metadata(constituent="PP", 
                 flow="DISCHARGE", 
                 dates="DATE", 
                 conc.units="mg L^-1", 
                 flow.units="cfs", 
                 load.units="kg", 
                 load.rate.units="kg d^-1", 
                 site.name="Dutch Hollow",
                 consti.name="Particulate Phos",
                 site.id='Dutch', 
                 lat=43.10259, lon=-70.95256)

no3_li <- loadInterp(interp.format="conc", 
                     interp.fun=rectangularInterpolation, 
                     data=dutchpp, 
                     metadata=meta)

no3_lm <- loadLm(formula=log(PP) ~ log(DISCHARGE), 
                 pred.format="conc", 
                 data=dutchpp, 
                 metadata=meta, 
                 retrans=exp)

library(rloadest)
no3_lr <- loadReg2(loadReg(PP ~ model(9), 
                           data=dutchpp,
                           flow="DISCHARGE", 
                           dates="DATE", 
                           time.step="day", 
                           flow.units="cfs", 
                           conc.units="mg/L", 
                           load.units="kg",
                           station='Dutch Hollow'))

no3_lc <- loadComp(reg.model=no3_lr, 
                   interp.format="conc", 
                   interp.data=dutchpp)

getMetadata(no3_li)
getFittingFunction(no3_lm)
getFittedModel(no3_lr)
getFittingData(no3_lc)

preds_li <- predictSolute(no3_li, 
                          "flux", 
                          Q_dutch, 
                          se.pred=TRUE, 
                          date=TRUE)
preds_lm <- predictSolute(no3_lm, 
                          "conc", 
                          Q_dutch, 
                          se.pred=TRUE, 
                          date=TRUE, 
                          lin.or.log="linear")

plot(preds_lm$date, preds_lm$fit*1000, type = 'l')

preds_lr <- predictSolute(no3_lr, 
                          "conc", 
                          Q_dutch, 
                          se.pred=TRUE, 
                          date=TRUE)
preds_lc <- predictSolute(no3_lc, 
                          "flux", 
                          Q_dutch, 
                          se.pred=TRUE, 
                          date=TRUE)

summary(getFittedModel(no3_lm))
ggplot2::qplot(x=Date, y=Resid, data=getResiduals(no3_li, newdata=intdat))
residDurbinWatson(no3_lr, "conc", newdata=regdat, irreg=TRUE)
residDurbinWatson(no3_lr, "conc", newdata=intdat, irreg=TRUE)
estimateRho(no3_lr, "conc", newdata=regdat, irreg=TRUE)$rho
estimateRho(no3_lr, "conc", newdata=intdat, irreg=TRUE)$rho
getCorrectionFraction(no3_lc, "flux", newdat=intdat)


aggs_li <- aggregateSolute(preds_li, meta, "flux rate", "month")
aggs_lm <- aggregateSolute(preds_lm, meta, "flux rate", "month")
aggs_lr <- aggregateSolute(preds_lr, meta, "flux rate", "month")
aggs_lc <- aggregateSolute(preds_lc, meta, "flux rate", "month")