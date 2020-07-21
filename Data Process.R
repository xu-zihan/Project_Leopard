library(tidyverse)
library(lubridate)

# import data
x1 <- read.csv("BF8_Vectronix.csv")
# data process of x1
names(x1)[1] <- "date"
names(x1)[2] <- "time"

x1 <- x1 %>% rename(y = Latitude, x = Longitude, z = Elevation) %>% 
  unite("daytime", date:time, remove = FALSE) %>% select(-date, -time) %>% 
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
x5 <- x5 %>% select(-X, -X.1, -X.2)

summary(is.na(x5))

x5$z[is.na(x5$z)] <- mean(na.omit(x5$z))

x6 <- read.csv("BM17_Followit.csv")
x6 <- process(x6, "BM17", "dropoff_retrived", "Fol")
x6 <- x6 %>% select(-X, -X.1, -X.2)
summary(is.na(x6))
x6$z[is.na(x6$z)] <- mean(na.omit(x6$z))

x7 <- read.csv("BM18_Vectronix.csv")
x7 <- process(x7, "BM18", "stoptrans_notretrived", "Vec")
summary(is.na(x7))

x8 <- read.csv("BM21_Followit.csv")
x8 <- process(x8, "BM21", "faildrop_notretrived", "Fol")
x8 <- x8 %>% select(-X, -X.1, -X.2, -X.3, -X.4)
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
library(rgdal)
cord <- project(as.matrix(cord0[,c("x","y")]), "+proj=utm +zone=34 ellps=WGS84")
plot(cord)

mean_cord <- project(as.matrix(cord0[,c("mean_x","mean_y")]),
                     "+proj=utm +zone=34 ellps=WGS84")

# add mean coordinates to cord
cord <- data.frame(cord)
cat <- data.frame(data1$cat)
cord <- cord %>% mutate(cat=cat$data1.cat) %>% mutate(z=cord0$z)
mean_cord <- data.frame(mean_cord)
cord <- cord %>% mutate(mean_x=mean_cord$mean_x, mean_y=mean_cord$mean_y)

# create grid pixel 
library(raster)
summary(cord)
r <- raster(xmn=299123, ymn=-3803516, xmx=361431, ymx=-3620484, res=2100)
r[] <- 0
tab <- table(cellFromXY(r, cord))
tab
r[as.numeric(names(tab))] <- tab
r
plot(r)
points(mean_cord$mean_x,mean_cord$mean_y, pch=20)
points(xy, pch=20)
# create dataset for amount of point in each pixel
d <- data.frame(coordinates(r), count=r[])

# x1 
# pixel distance to central point
data <- x1
#----run code on top again--------------------
mean_cord
d <- d %>% mutate(mean_x=303007.9 , mean_y=-3787445) %>% data.frame()
head(d)
pointDistance(c(300173, -3621534 ), c(303007.9, -3787445), lonlat=F)

dist <- function(a,b,c,d) {
  a <- pointDistance(cbind(a, b), cbind(c, d), lonlat=F)
  return(a)
}

# calculate distance
d <- d %>% mutate(distance=dist(d$x, d$y, d$mean_x, d$mean_y))

# funcrion for all data
all_dist <- function(a="a") {
  cord <- filter(cord,cord$cat==a)
  r <- raster(xmn=299123, ymn=-3803516, xmx=361431, ymx=-3620484, res=2100)
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
# GLM model for x1
summary(d1$count)
glmFit1 <- glm(count ~ distance^2, 
               family=poisson, data=d1)
summary(glmFit1)
glmFit11 <- glm(count ~ distance^2, 
               family=quasipoisson, data=d1)
summary(glmFit11)

library(car)
residualPlots(glmFit11,
              type="pearson",
              terms=~.-Phase,
              quadratic=TRUE,
              smooth=list(smoother=gamLine, col="#377eb8"),
              fitted=FALSE,
              col.quad="#e41a1c",
              col="grey",
              pch=19,
              cex=0.3,
              ylim=c(-5, 5))

acf(residuals(glmFit11, type="pearson"), main="glmFit11")

# model for x2
glmFit2 <- glm(count ~ distance^2, 
               family=poisson, data=d2)
summary(glmFit2)
glmFit22 <- glm(count ~ distance^2, 
               family=quasipoisson, data=d2)
summary(glmFit22)
residualPlots(glmFit2,
              type="pearson",
              terms=~.-Phase,
              quadratic=TRUE,
              smooth=list(smoother=gamLine, col="#377eb8"),
              fitted=FALSE,
              col.quad="#e41a1c",
              col="grey",
              pch=19,
              cex=0.3,
              ylim=c(-5, 5))

acf(residuals(glmFit2, type="pearson"), main="glmFit2")

# model for x3
glmFit3 <- glm(count ~ distance^2, 
               family=poisson, data=d3)
summary(glmFit3)
glmFit33 <- glm(count ~ distance^2, 
                family=quasipoisson, data=d3)
summary(glmFit33)
residualPlots(glmFit33,
              type="pearson",
              terms=~.-Phase,
              quadratic=TRUE,
              smooth=list(smoother=gamLine, col="#377eb8"),
              fitted=FALSE,
              col.quad="#e41a1c",
              col="grey",
              pch=19,
              cex=0.3,
              ylim=c(-5, 5))

acf(residuals(glmFit33, type="pearson"), main="glmFit33")

# model for x4
glmFit4 <- glm(count ~ distance^2, 
               family=poisson, data=d4)
summary(glmFit4)
glmFit44 <- glm(count ~ distance^2, 
                family=quasipoisson, data=d4)
summary(glmFit44)
residualPlots(glmFit44,
              type="pearson",
              terms=~.-Phase,
              quadratic=TRUE,
              smooth=list(smoother=gamLine, col="#377eb8"),
              fitted=FALSE,
              col.quad="#e41a1c",
              col="grey",
              pch=19,
              cex=0.3,
              ylim=c(-5, 5))

acf(residuals(glmFit44, type="pearson"), main="glmFit44")

# model for x5
glmFit5 <- glm(count ~ distance^2, 
               family=poisson, data=d5)
summary(glmFit5)
glmFit55 <- glm(count ~ distance^2, 
                family=quasipoisson, data=d5)
summary(glmFit55)
residualPlots(glmFit55,
              type="pearson",
              terms=~.-Phase,
              quadratic=TRUE,
              smooth=list(smoother=gamLine, col="#377eb8"),
              fitted=FALSE,
              col.quad="#e41a1c",
              col="grey",
              pch=19,
              cex=0.3,
              ylim=c(-5, 5))

acf(residuals(glmFit55, type="pearson"), main="glmFit55")

# model for x6
glmFit6 <- glm(count ~ distance^2, 
               family=poisson, data=d6)
summary(glmFit6)
glmFit66 <- glm(count ~ distance^2, 
                family=quasipoisson, data=d6)
summary(glmFit66)
residualPlots(glmFit6,
              type="pearson",
              terms=~.-Phase,
              quadratic=TRUE,
              smooth=list(smoother=gamLine, col="#377eb8"),
              fitted=FALSE,
              col.quad="#e41a1c",
              col="grey",
              pch=19,
              cex=0.3,
              ylim=c(-5, 5))

acf(residuals(glmFit6, type="pearson"), main="glmFit6")

# model for x7
glmFit7 <- glm(count ~ distance^2, 
               family=poisson, data=d7)
summary(glmFit7)
glmFit77 <- glm(count ~ distance^2, 
                family=quasipoisson, data=d7)
summary(glmFit77)
residualPlots(glmFit77,
              type="pearson",
              terms=~.-Phase,
              quadratic=TRUE,
              smooth=list(smoother=gamLine, col="#377eb8"),
              fitted=FALSE,
              col.quad="#e41a1c",
              col="grey",
              pch=19,
              cex=0.3,
              ylim=c(-5, 5))

acf(residuals(glmFit77, type="pearson"), main="glmFit77")

# model for x8
glmFit8 <- glm(count ~ distance^2, 
               family=poisson, data=d8)
summary(glmFit8)
glmFit88 <- glm(count ~ distance^2, 
                family=quasipoisson, data=d8)
summary(glmFit88)
residualPlots(glmFit88,
              type="pearson",
              terms=~.-Phase,
              quadratic=TRUE,
              smooth=list(smoother=gamLine, col="#377eb8"),
              fitted=FALSE,
              col.quad="#e41a1c",
              col="grey",
              pch=19,
              cex=0.3,
              ylim=c(-5, 5))

acf(residuals(glmFit88, type="pearson"), main="glmFit88")

# model for x9
glmFit9 <- glm(count ~ distance^2, 
               family=poisson, data=d9)
summary(glmFit9)
glmFit99 <- glm(count ~ distance^2, 
                family=quasipoisson, data=d9)
summary(glmFit99)
residualPlots(glmFit99,
              type="pearson",
              terms=~.-Phase,
              quadratic=TRUE,
              smooth=list(smoother=gamLine, col="#377eb8"),
              fitted=FALSE,
              col.quad="#e41a1c",
              col="grey",
              pch=19,
              cex=0.3,
              ylim=c(-5, 5))

acf(residuals(glmFit99, type="pearson"), main="glmFit88")

# model for all data

plot(d_all$count, d_all$distance)


glmFitAll <- glm(count ~ distance^2, 
               family=poisson, data=d_all)

glmFitAll2 <- glm(count ~ distance^2, 
                  family=quasipoisson, data=d_all)

plot(fitted(glmFitAll2))

summary(glmFitAll2)

residualPlots(glmFitAll2,
              type="pearson",
              terms=~.-Phase,
              quadratic=TRUE,
              smooth=list(smoother=gamLine, col="#377eb8"),
              fitted=FALSE,
              col.quad="#e41a1c",
              col="grey",
              pch=19,
              cex=0.3,
              ylim=c(-5, 5))

acf(residuals(glmFitAll2, type="pearson"), main="glmFit77")

#predict
pred <- fitted(glmFitAll2)
summary(pred)
d_pred <- d_all %>% dplyr::select(cat, distance) %>% mutate(predict=pred)
plot(d_pred$predict, d_pred$distance)


# elevation
d1 <- dplyr::select(d1, x,y)
coordinates(d1)<- ~ x + y
proj4string(d1) <- CRS("+proj=utm +zone=34 ellps=WGS84")
gridded(d1) = TRUE
x1 <- dplyr::select(cord, x,y,z)
coordinates(x1)<- ~ x + y
proj4string(x1) <- CRS("+proj=utm +zone=34 ellps=WGS84")

plot(d1)
plot(x1, add=T)

summary(x1)
summary(d1)
library(sp)
# over function get na result
o1 <- over(d1, x1)












