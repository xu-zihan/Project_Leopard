library(tidyverse)
library(lubridate)
library(car)
library(lawstat)
library(MuMIn)
library(rgdal)
library(raster)
library(sp)
require(fields)



# import data
x1 <- read.csv("BF8_Vectronix.csv")

# data process of x1
names(x1)[1] <- "date"
names(x1)[2] <- "time"

x1 <- x1 %>% rename(y = Latitude, x = Longitude, z = Elevation) %>% 
  unite("daytime", date:time, remove = FALSE) %>% dplyr::select(-date, -time) %>% 
  mutate(cat = "BF8", type = "dropoff_retrived") %>% drop_na(x)

x1$daytime <- gsub("_", " ", x1$daytime)
x1$daytime <- ymd_hms(x1$daytime)
class(x1$daytime)

# data process function for all data
process <- function(x, a="a", b="b", c="c"){
  names(x)[1] <- "date"
  names(x)[2] <- "time"
  x <- x %>% rename(y = Latitude, x = Longitude, z = Elevation) %>% 
    unite("daytime", date:time, remove = FALSE) %>% dplyr::select(-date, -time) %>% 
    drop_na(x) %>% mutate(cat = a, type = b, collar = c)
  x$daytime <- gsub("_", " ", x$daytime)
  x$daytime <- ymd_hms(x$daytime)
  return(x)
}

# data process 
x1 <- read.csv("BF8_Vectronix.csv")
x1 <- process(x1, "BF8", "dropoff_retrived", "Vec")
summary(is.na(x1))

x2 <- read.csv("BM4_Vectronix.csv")
x2 <- process(x2, "BM4", "dropoff_retrived", "Vec")
summary(is.na(x2))

x3 <- read.csv("BM5_Vectronix.csv")
x3 <- process(x3, "BM5", "stoptrans_notretrived", "Vec")
summary(is.na(x3))

x4 <- read.csv("BM7_Followit.csv")
x4 <- process(x4, "BM7", "mortality_retrived", "Fol")
summary(is.na(x4))
x4$z[is.na(x4$z)] <- mean(na.omit(x4$z))

x5 <- read.csv("BM12_Followit.csv")
x5 <- process(x5, "BM12", "dropoff_retrived", "Fol")
x5 <- x5 %>% dplyr::select(-X, -X.1, -X.2)
summary(is.na(x5))
x5$z[is.na(x5$z)] <- mean(na.omit(x5$z))

x6 <- read.csv("BM17_Followit.csv")
x6 <- process(x6, "BM17", "dropoff_retrived", "Fol")
x6 <- x6 %>% dplyr::select(-X, -X.1, -X.2)
summary(is.na(x6))
x6$z[is.na(x6$z)] <- mean(na.omit(x6$z))

x7 <- read.csv("BM18_Vectronix.csv")
x7 <- process(x7, "BM18", "stoptrans_notretrived", "Vec")
summary(is.na(x7))

x8 <- read.csv("BM21_Followit.csv")
x8 <- process(x8, "BM21", "faildrop_notretrived", "Fol")
x8 <- x8 %>% dplyr::select(-X, -X.1, -X.2, -X.3, -X.4)
summary(is.na(x8))
x8$z[is.na(x8$z)] <- mean(na.omit(x8$z))

x9 <- read.csv("BM22_Followit.csv")
x9 <- mutate(x9, Elevation=0)
x9 <- process(x9, "BM22", "stoptrans_notretrived", "Fol")

# combine all data
data <- rbind(x1,x2,x3,x4,x5,x6,x7,x8,x9)

# get mean coordinates
data1 <- data %>% group_by(cat) %>% mutate(mean_x=mean(x),
                                           mean_y=mean(y))
# add mean coordinates to cord
cord0 <- data1 %>% dplyr::select(x,y,z, mean_x, mean_y)

# transfer coordinates to utm structure
cord <- project(as.matrix(cord0[,c("x","y")]), "+proj=utm +zone=34 +south +ellps=WGS84 +towgs84=0,0,0,-0,-0,-0,0")
ggplot(data=cord0, aes(x=x, y=y))+geom_point(aes(color=cat))
mean_cord <- project(as.matrix(cord0[,c("mean_x","mean_y")]),
                     "+proj=utm +zone=34 +south +ellps=WGS84 +towgs84=0,0,0,-0,-0,-0,0")

# add mean coordinates to cord
cord <- data.frame(cord)
cat <- data.frame(data1$cat)
cord <- cord %>% mutate(cat=cat$data1.cat) %>% mutate(z=cord0$z)
mean_cord <- data.frame(mean_cord)
cord <- cord %>% mutate(mean_x=mean_cord$mean_x, mean_y=mean_cord$mean_y)

# create grid pixel 
summary(cord)
r <- raster(xmn=299123, ymn=6196484, xmx=361431, ymx=6379516, res=2100)
r[] <- 0
tab <- table(cellFromXY(r, cord))
tab
r[as.numeric(names(tab))] <- tab
r
# plot raster
plot(r, xlab="UTM Zone 34", ylab="UTM Zone 34", main="count in each pixel with centroid", 
     xaxs='i')
points(mean_cord$mean_x,mean_cord$mean_y, pch=21)

# create dataset for amount of point in each pixel
d <- data.frame(coordinates(r), count=r[])

# x1 
# pixel distance to central point
dist <- function(a,b,c,d) {
  a <- pointDistance(cbind(a, b), cbind(c, d), lonlat=F)
  return(a)
}

# calculate distance
# funcrion for all data
all_dist <- function(a="a") {
  cord <- filter(cord,cord$cat==a)
  r <- raster(xmn=299123, ymn=6196484, xmx=361431, ymx=6379516, res=2100)
  r[] <- 0
  tab <- table(cellFromXY(r, cord))
  r[as.numeric(names(tab))] <- tab
  d <- data.frame(coordinates(r), count=r[])
  b <- cord$mean_x[1]
  c <- cord$mean_y[1]
  d <- d %>% mutate(mean_x=b , mean_y=c) %>%mutate(cat=a) %>% data.frame()
  d <- d %>% mutate(distance=dist(d$x, d$y, d$mean_x, d$mean_y))
return(d)
}

d1 <- all_dist("BF8")
summary(cord$cat)
d2 <- all_dist("BM12")
d3 <- all_dist("BM17")
d4 <- all_dist("BM18")
d5 <- all_dist("BM21")
d6 <- all_dist("BM22")
d7 <- all_dist("BM4")
d8 <- all_dist("BM5")
d9 <- all_dist("BM7")

d_all <- rbind(d1,d2,d3,d4,d5,d6,d7,d8,d9)


# model for all data
plot(d_all$count, d_all$distance)

# Poisson Model 
glmFitAll <- glm(count ~ distance^2, 
               family=poisson, data=d_all)
# Quasipoisson Model
glmFitAll2 <- glm(count ~ distance^2, 
                  family=quasipoisson, data=d_all)
# Poisson Model 
glmFitAll3 <- glm(count ~ distance, 
                 family=poisson, data=d_all)
# Quasipoisson Model
glmFitAll4 <- glm(count ~ distance, 
                  family=quasipoisson, data=d_all)

# Check the model for glmFitAll2
car::Anova(glmFitAll2)
summary(glmFitAll2)
plot(glmFitAll2)

# NonLinearity 
residualPlots(glmFitAll2,
              type="pearson",
              terms=~.,
              quadratic=TRUE,
              smooth=list(smoother=gamLine, col="#377eb8"),
              fitted=FALSE,
              col.quad="#e41a1c",
              col="grey",
              pch=19,
              cex=0.3,
              ylim=c(-5, 5),
              xlim=c(0,30000),
              main="glmFitAll2")



# Residuals
runs.test(residuals(glmFitAll2))
plot(glmFitAll2)

# Check the model for glmFitAll4
car::Anova(glmFitAll4)
summary(glmFitAll4)
plot(glmFitAll4)


# NonLinearity 
residualPlots(glmFitAll4,
              type="pearson",
              terms=~.,
              quadratic=TRUE,
              smooth=list(smoother=gamLine, col="#377eb8"),
              fitted=FALSE,
              col.quad="#e41a1c",
              col="grey",
              pch=19,
              cex=0.3,
              ylim=c(-5, 5),
              xlim=c(0,30000),
              main="glmFitAll4")

# Residuals
runs.test(residuals(glmFitAll4))

# Elevation
# Import Elevation data
# Low resolution elevation data
elev90 <- raster("ZA_DEM_90m_UTM34.img")
plot(elev90)
elev90
new_elev <- crop(extend(elev90, r), r)
new_elev
r
# Plot elevation ratser 
plot(new_elev, xlab="UTM Zone 34", ylan="UTM Zone 34", main="elevation in pixel with centroid", xaxs='i')
points(mean_cord$mean_x,mean_cord$mean_y, pch=21)

# Landcover
landcov <- raster("Final_2014_WC_LC.tif")
plot(landcov)
landcov
new_land <- crop(extend(landcov, r), r)
new_land
r
# Plot landcover raster
plot(new_land,xlab="UTM Zone 34", ylab="UTM Zone 34", main="landcover in pixel with centroid", xaxs='i')
points(mean_cord$mean_x,mean_cord$mean_y, pch=21)

# Land cover data
load("coodcovs.Rdata")
d_all <- d_all %>% mutate(elev=coodelev) %>% mutate(landcov=coodlandcov)

# data plot
# Plot for parameters with count
plot(d_all$landcov, d_all$count, pch=20, xlab="landcover", ylab="count", 
     main="count vs landcover")
plot(d_all$elev, d_all$count, pch=20, xlab="elevation", ylab="count", 
     main="count vs elevation")
plot(d_all$distance, d_all$count, pch=20, xlab="distance", ylab="count", main="count vs distance")
(unique(d_all$cat))

# plot for each cat with distance
plt_all <- function(){
  list <- unique(d_all$cat)
  i <- 1
  while(i <=9) {
    xq <- list[i]
    data <- filter(d_all, cat==xq)
    plot(data$distance, data$count, main=xq, xlab="distance", ylab="count", pch=20)
    i <- i+1
  }
}

par(mfrow=c(3,3))
plt_all()

# Poisson Model with elevation
glmFitAll5 <- glm(count ~ distance^2 + elev, 
                 family=poisson, data=d_all)
# Quasipoisson Model with elevation
glmFitAll6 <- glm(count ~ distance^2 + elev, 
                  family=quasipoisson, data=d_all)

# Poisson Model with elevation
glmFitAll7 <- glm(count ~ distance + elev, 
                  family=poisson, data=d_all)
# Quasipoisson Model with elevation
glmFitAll8 <- glm(count ~ distance + elev, 
                  family=quasipoisson, data=d_all)

# Check the model for glmFitAll6
car::Anova(glmFitAll6)
summary(glmFitAll6)
plot(glmFitAll6)

# NonLinearity 
residualPlots(glmFitAll6,
              type="pearson",
              terms=~.,
              quadratic=TRUE,
              smooth=list(smoother=gamLine, col="#377eb8"),
              fitted=FALSE,
              col.quad="#e41a1c",
              col="grey",
              pch=19,
              cex=0.3,
              ylim=c(-5, 5))
residualPlots(glmFitAll6,
              type="pearson",
              terms=~distance,
              quadratic=TRUE,
              smooth=list(smoother=gamLine, col="#377eb8"),
              fitted=FALSE,
              col.quad="#e41a1c",
              col="grey",
              pch=19,
              cex=0.3,
              ylim=c(-5, 5),
              xlim=c(0, 30000))

# Residuals
runs.test(residuals(glmFitAll6))

# Check the model for glmFitAll8
car::Anova(glmFitAll8)
summary(glmFitAll8)
plot(glmFitAll8)

# NonLinearity 
residualPlots(glmFitAll8,
              type="pearson",
              terms=~.,
              quadratic=TRUE,
              smooth=list(smoother=gamLine, col="#377eb8"),
              fitted=FALSE,
              col.quad="#e41a1c",
              col="grey",
              pch=19,
              cex=0.3,
              ylim=c(-5, 5))
residualPlots(glmFitAll8,
              type="pearson",
              terms=~distance,
              quadratic=TRUE,
              smooth=list(smoother=gamLine, col="#377eb8"),
              fitted=FALSE,
              col.quad="#e41a1c",
              col="grey",
              pch=19,
              cex=0.3,
              ylim=c(-5, 5),
              xlim=c(0, 30000))

# Residuals
runs.test(residuals(glmFitAll8))

# Poisson Model with elevation and landcover
glmFitAll9 <- glm(count ~ distance^2 + elev + landcov, 
                 family=poisson, data=d_all)
# Quasipoisson Model with elevation and landcover
glmFitAll10 <- glm(count ~ distance^2 + elev + landcov, 
                  family=quasipoisson, data=d_all)

# Poisson Model with elevation and landcover
glmFitAll11 <- glm(count ~ distance + elev + landcov, 
                  family=poisson, data=d_all)
# Quasipoisson Model with elevation and landcover
glmFitAll12 <- glm(count ~ distance + elev + landcov, 
                   family=quasipoisson, data=d_all)

# Check the model 10
car::Anova(glmFitAll10)
summary(glmFitAll10)
plot(glmFitAll10)

# Collinearity
covariates <- c('distance', "elev", "landcov")
pairs(subset(d_all, select=covariates),
      upper.panel=NULL, pch=19, cex=0.3)

car::vif(glmFitAll10)


# NonLinearity 
residualPlots(glmFitAll10,
              type="pearson",
              terms=~.,
              quadratic=TRUE,
              smooth=list(smoother=gamLine, col="#377eb8"),
              fitted=FALSE,
              col.quad="#e41a1c",
              col="grey",
              pch=19,
              cex=0.3,
              ylim=c(-1000, 1000))
residualPlots(glmFitAll10,
              type="pearson",
              terms=~distance,
              quadratic=TRUE,
              smooth=list(smoother=gamLine, col="#377eb8"),
              fitted=FALSE,
              col.quad="#e41a1c",
              col="grey",
              pch=19,
              cex=0.3,
              ylim=c(-5, 5),
              xlim=c(0, 30000))

# Residuals
runs.test(residuals(glmFitAll10))

# Check the model 11
car::Anova(glmFitAll12)
summary(glmFitAll12)
plot(glmFitAll12)

# colinearity
car::vif(glmFitAll12)


# NonLinearity 
residualPlots(glmFitAll12,
              type="pearson",
              terms=~.,
              quadratic=TRUE,
              smooth=list(smoother=gamLine, col="#377eb8"),
              fitted=FALSE,
              col.quad="#e41a1c",
              col="grey",
              pch=19,
              cex=0.3,
              ylim=c(-5, 5))
residualPlots(glmFitAll12,
              type="pearson",
              terms=~distance,
              quadratic=TRUE,
              smooth=list(smoother=gamLine, col="#377eb8"),
              fitted=FALSE,
              col.quad="#e41a1c",
              col="grey",
              pch=19,
              cex=0.3,
              ylim=c(-5, 5),
              xlim=c(0, 30000))

# Residuals
runs.test(residuals(glmFitAll12))

# Model Selection
options(na.action="na.fail")
dredge1 <- head(dredge(glmFitAll9, rank="QAIC", chat=summary(glmFitAll10)$dispersion), n=10)


# Poisson Model with landcover
glmFitAll13 <- glm(count ~ distance^2 + landcov, 
                   family=poisson, data=d_all)
# Quasipoisson Model and landcover
glmFitAll14 <- glm(count ~ distance^2 + landcov, 
                   family=quasipoisson, data=d_all)
# # Check the model 10
car::Anova(glmFitAll14)
summary(glmFitAll14)
plot(glmFitAll14)


# Collinearity
car::vif(glmFitAll14)


# NonLinearity 
residualPlots(glmFitAll14,
              type="pearson",
              terms=~.,
              quadratic=TRUE,
              smooth=list(smoother=gamLine, col="#377eb8"),
              fitted=FALSE,
              col.quad="#e41a1c",
              col="grey",
              pch=19,
              cex=0.3,
              ylim=c(-5, 5))
residualPlots(glmFitAll14,
              type="pearson",
              terms=~distance,
              quadratic=TRUE,
              smooth=list(smoother=gamLine, col="#377eb8"),
              fitted=FALSE,
              col.quad="#e41a1c",
              col="grey",
              pch=19,
              cex=0.3,
              ylim=c(-5, 5),
              xlim=c(0, 30000))



# Residuals
runs.test(residuals(glmFitAll14))

# Model with interaction term
# Poisson Model with landcover and cat with distance
glmFitAll15 <- glm(count ~ distance^2*cat + landcov + elev, 
                   family=poisson, data=d_all)
# Quasipoisson Model with landcover and cat with distance
glmFitAll16 <- glm(count ~ distance^2*cat + landcov + elev, 
                   family=quasipoisson, data=d_all)
# Poisson Model with landcover and cat with distance
glmFitAll17 <- glm(count ~ distance*cat + landcov + elev, 
                   family=poisson, data=d_all)
# Quasipoisson Model with landcover and cat with distance
glmFitAll18 <- glm(count ~ distance*cat + landcov + elev, 
                   family=quasipoisson, data=d_all)

# Check the model 16
car::Anova(glmFitAll16)
summary(glmFitAll16)
plot(glmFitAll16)

# NonLinearity 
residualPlots(glmFitAll16,
              type="pearson",
              terms=~.,
              quadratic=TRUE,
              smooth=list(smoother=gamLine, col="#377eb8"),
              fitted=FALSE,
              col.quad="#e41a1c",
              col="grey",
              pch=19,
              cex=0.3,
              ylim=c(-10, 10))
residualPlots(glmFitAll16,
              type="pearson",
              terms=~distance,
              quadratic=TRUE,
              smooth=list(smoother=gamLine, col="#377eb8"),
              fitted=FALSE,
              col.quad="#e41a1c",
              col="grey",
              pch=19,
              cex=0.3,
              ylim=c(-5, 5),
              xlim=c(0, 100000))
# Residuals
runs.test(residuals(glmFitAll16))

# Check the model 18
car::Anova(glmFitAll18)
summary(glmFitAll18)
plot(glmFitAll18)



# NonLinearity 
residualPlots(glmFitAll18,
              type="pearson",
              terms=~.,
              quadratic=TRUE,
              smooth=list(smoother=gamLine, col="#377eb8"),
              fitted=FALSE,
              col.quad="#e41a1c",
              col="grey",
              pch=19,
              cex=0.3,
              ylim=c(-5, 5))
residualPlots(glmFitAll18,
              type="pearson",
              terms=~distance,
              quadratic=TRUE,
              smooth=list(smoother=gamLine, col="#377eb8"),
              fitted=FALSE,
              col.quad="#e41a1c",
              col="grey",
              pch=19,
              cex=0.3,
              ylim=c(-5, 5),
              xlim=c(0, 100000))

# Residuals
runs.test(residuals(glmFitAll18))
plot(pr, xlab="fitted", ylab="Pearson residual", main="pearson residual fitted value")


# Model Selection
options(na.action="na.fail")
dredge2 <- head(dredge(glmFitAll15, rank="QAIC", chat=summary(glmFitAll16)$dispersion), n=10)

# Predict
test <- dplyr::select(d_all, distance, elev, landcov, cat)
# Residuals
pr <- residuals(glmFitAll16, type="pearson")

predict <- predict(glmFitAll16, newdata=test, type = 'response')
summary(predict)
d_pred <- d_all %>% dplyr::select(cat, distance, x, y, elev, landcov, count) %>% 
  mutate(predict=predict) %>% mutate(residual=pr)

m_pred1 <- group_by(d_pred, x, y) %>% summarize(residual = mean(residual))
m_pred2 <- group_by(d_pred, x, y) %>% summarize(count = sum(count))
m_pred3 <- group_by(d_pred, x, y) %>% summarize(predict = sum(predict))

m_pred <- left_join(m_pred1, m_pred2, by=c("x", "y"))
m_pred <- left_join(m_pred, m_pred3, by=c("x", "y"))
m_cat <- d_pred %>% dplyr::filter(cat=="BF8") %>% dplyr::select(x,y,elev,landcov)
m_pred <- left_join(m_pred, m_cat, by=c("x", "y"))


# Plot comparison
# Distance
par(mfrow=c(1,2))
plot(d_pred$distance, d_pred$count, xlab="distance", ylab="count", main="data")
plot(d_pred$distance, d_pred$predict,xlab="distance", ylab="count", main="prediction")
# Elevation
plot(d_pred$elev, d_pred$count,xlab="elevation", ylab="count", main="data")
plot(d_pred$elev, d_pred$predict,xlab="elevation", ylab="count", main="prediction")
# Landcover
plot(d_pred$landcov, d_pred$count,xlab="landcover", ylab="count", main="data")
plot(d_pred$landcov, d_pred$predict,xlab="landcover", ylab="count", main="prediction")
m_pred <- m_pred %>% mutate(ce=count/(landcov+0.1))

# heatmap
par(mfrow=c(1,5))
quilt.plot(m_pred$x, m_pred$y, m_pred$count, nrow=30, ncol=87, col = rev(terrain.colors(100)),
           xlab="UTM Zone 34", ylab="UTM Zone 34", main="Count")
quilt.plot(m_pred$x, m_pred$y, m_pred$predict, nrow=30, ncol=87, col = rev(terrain.colors(100)),
           xlab="UTM Zone 34", ylab="UTM Zone 34", main="Prediction")
quilt.plot(m_pred$x, m_pred$y, m_pred$residual, nrow=30, ncol=87, col = rev(terrain.colors(100)),
           xlab="UTM Zone 34", ylab="UTM Zone 34", main="Residual")
quilt.plot(m_pred$x, m_pred$y, m_pred$elev, nrow=30, ncol=87, col = rev(terrain.colors(100)),
           xlab="UTM Zone 34", ylab="UTM Zone 34", main="Elevation")
quilt.plot(m_pred$x, m_pred$y, m_pred$landcov, nrow=30, ncol=87, col = rev(terrain.colors(100)),
           xlab="UTM Zone 34", ylab="UTM Zone 34", main="Landcover")

# Acuuracy
d_pred <- d_pred %>% mutate(accuracy=ifelse(abs(count-predict)<0.5, 1, 0))

sum(d_pred$accuracy==1)/23490
sum(d_pred$accuracy==0)/23490
isTRUE(0.1075351+0.8924649==1)

