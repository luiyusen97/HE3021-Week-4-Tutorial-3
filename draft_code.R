library("tidyverse")
library("haven")
library("foreign")
library("ggplot2")
library("lmtest")
library("sandwich")
library("forecast")

filepath <- "C:\\Users\\Lui Yu Sen\\Google Drive\\NTU_study materials\\Economics\\HE3021 Intermediate Econometrics\\Week 3\\HE3021-Week-4-Tutorial-3\\rawdata\\VOLAT.dta"
dat <- read_dta(file = filepath)
dat <- as.data.frame(dat)
dat$date <- as.Date(sprintf("%.2f.01", dat$date), format = "%Y.%m.%d")

# dat$date <- as.Date(paste0(round(dat$date, 2), '.01'), '%Y.%m.%d')
# convertdatereadable <- function(datenumeric){
#     datenumeric <- trunc(datenumeric * 10000 + 1)
#     datenumeric <- as.character(datenumeric)
#     return(datenumeric)
# }
# dat[,1] <- apply(dat[, 1, drop = F], 2, convertdatereadable)
# 
# convertchardate <- function(datenumeric){
#     datenumeric <- as.Date.character(datenumeric, format = "%Y%m%d")
#     return(datenumeric)
# }
# for (n in 1:558){
#     dat[n, 1] <- as.Date.character(dat[n, 1], format = "%Y%m%d")
# }
# OKAY, so neither for nor apply work because for apply, dataframe is coerced into matrix class, and matrix
# cannot hold date classes, so it is coerced into numeric.
# dat[,1][[1]] <- lapply(dat[,1][[1]], convertchardate)
# as.Date.character(dat[[1]][[1]], format = "%Y%m%d")  this works lol

model <- tslm(rsp500~pcip+i3, ts(data = dat, frequency = 12, start = 1947))
summary(model)
# test for stationarity, fail. Null hypo is independence of each value from other time period values, from 1 to 25 period gaps.
box <- c()
for (n in 1:24){
    box_component <- Box.test(dat$rsp500, lag=n, type="Ljung-Box")$p.value
    box <- c(box, box_component)
    rm(box_component)
}
# heteroscedascity test
heterotest <- cbind(model$model, na.trim(model$residuals))
colnames(heterotest)[4] <- "residualsquared"
heterotest <- mutate(heterotest, residualsquared = residualsquared**2)
heterotest <- tslm(residualsquared ~ pcip + i3, ts(heterotest, frequency = 12))
