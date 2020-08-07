library(loadflex)

# Interpolation data: Packers Falls NO3 grab sample observations
data(lamprey_nitrate)
intdat <- lamprey_nitrate[c("DATE","DISCHARGE","NO3")]

# Calibration data: Restrict to points separated by sufficient time
regdat <- subset(lamprey_nitrate, REGR)[c("DATE","DISCHARGE","NO3")]

# Estimation data: Packers Falls discharge
data(lamprey_discharge)
estdat <- subset(lamprey_discharge, DATE < as.POSIXct("2012-10-01 00:00:00", tz="EST5EDT"))
estdat <- estdat[seq(1, nrow(estdat), by=96/4),] # pare to 4 obs/day for speed

meta <- metadata(constituent="NO3", flow="DISCHARGE", 
                 dates="DATE", conc.units="mg L^-1", flow.units="cfs", load.units="kg", 
                 load.rate.units="kg d^-1", site.name="Lamprey River, NH",
                 consti.name="Nitrate", site.id='01073500', lat=43.10259, lon=-70.95256)

no3_li <- loadInterp(interp.format="conc", interp.fun=rectangularInterpolation, 
                     data=intdat, metadata=meta)
no3_lm <- loadLm(formula=log(NO3) ~ log(DISCHARGE), pred.format="conc", 
                 data=regdat, metadata=meta, retrans=exp)
library(rloadest)
no3_lr <- loadReg2(loadReg(NO3 ~ model(9), data=regdat,
                           flow="DISCHARGE", dates="DATE", time.step="instantaneous", 
                           flow.units="cfs", conc.units="mg/L", load.units="kg",
                           station='Lamprey River, NH'))
no3_lc <- loadComp(reg.model=no3_lr, interp.format="conc", 
                   interp.data=intdat)

getMetadata(no3_li)
getFittingFunction(no3_lm)
getFittedModel(no3_lr)
getFittingData(no3_lc)

preds_li <- predictSolute(no3_li, "flux", estdat, se.pred=TRUE, date=TRUE)
preds_lm <- predictSolute(no3_lm, "flux", estdat, se.pred=TRUE, date=TRUE, lin.or.log="linear")
preds_lr <- predictSolute(no3_lr, "flux", estdat, se.pred=TRUE, date=TRUE)
preds_lc <- predictSolute(no3_lc, "flux", estdat, se.pred=TRUE, date=TRUE)

head(preds_lr)

plot(preds_lm$date, preds_lm$fit, type = 'l')

plot(preds_lc$date, preds_lc$fit, type = 'l')

aggs_li <- aggregateSolute(preds_li, meta, "flux rate", "month")
aggs_lm <- aggregateSolute(preds_lm, meta, "flux rate", "month")
aggs_lr <- aggregateSolute(preds_lr, meta, "flux rate", "month")
aggs_lc <- aggregateSolute(preds_lc, meta, "flux rate", "month")