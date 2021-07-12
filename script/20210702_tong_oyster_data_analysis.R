### Oyster Tong Data
##
## B Ennis
## July 2, 2021
## Revised code based on J Moore 20201203
##
## SUMAARY:
## Data graphical comparisons between Pre/Post Season oyster tong data
##


#read in data
#tong <- read.csv("tong_data.csv", header = T)

tong<- read.csv("~/GitHub/oys_bb_subtidal_summary/data/tong_data_20210702.csv", header=TRUE)
#tong<- read.csv("~/GitHub/oys_bb_subtidal_summary/data/tong_data.csv", header=TRUE)

pre_season_2020 <- subset(tong, tong$sampling_period == 'Pre-Season')
post_season_2021 <- subset(tong, tong$sampling_period == 'Post-Season')

#vessels
length(unique(post_season_2021$vessel))
#5 levels

#stations
length(unique(post_season_2021$station))
#18

table(post_season_2021$vessel, post_season_2021$station)
#23 combinations

#list of the unique station names
stations <- unique(post_season_2021$station)

#create a plot showing cumulative counts per size class by lick
par(mfrow = c(1, 2))
for(i in stations){
  st <- i
  
  temp_pre <- subset(pre_season_2020, pre_season_2020$station == st)
  temp_s1 <- aggregate(size_class_1 ~ lick, data = temp_pre, FUN = 'sum')
  temp_s2 <- aggregate(size_class_2 ~ lick, data = temp_pre, FUN = 'sum')
  temp_s3 <- aggregate(size_class_3 ~ lick, data = temp_pre, FUN = 'sum')

  temp_post <- subset(post_season_2021, post_season_2021$station == st)
  temp_s4 <- aggregate(size_class_1 ~ lick, data = temp_post, FUN = 'sum')
  temp_s5 <- aggregate(size_class_2 ~ lick, data = temp_post, FUN = 'sum')
  temp_s6 <- aggregate(size_class_3 ~ lick, data = temp_post, FUN = 'sum')
  
  plot(temp_s1$lick, cumsum(temp_s1$size_class_1), type = "l", lty = 3, lwd = 2, col = 'black',
       ylim = c(0,max(cumsum(temp_pre$size_class_1),cumsum(temp_pre$size_class_2),cumsum(temp_pre$size_class_3))),
       xlab = "Lick", ylab = "Oyster Counts")
  lines(temp_s2$lick, cumsum(temp_s2$size_class_2), lty = 3, lwd = 2, col = 'red')
  lines(temp_s3$lick, cumsum(temp_s3$size_class_3), lty = 3, lwd = 2, col = 'purple')
  title(main = paste("Pre-Season Station:", i))
  legend("topleft", legend = c("Size Class 1: < 1 inch", "Size Class 2: 1-3 inches", "Size Class 3: > 3 inches"), cex = 0.85, lty = 3, col = c("black", "red", "purple"), bty="n")
  
  
  plot(temp_s4$lick, cumsum(temp_s4$size_class_1), type = "l", lty = 1, lwd = 2, col = 'black',
       ylim = c(0,max(cumsum(temp_post$size_class_1),cumsum(temp_post$size_class_2),cumsum(temp_post$size_class_3))),
       xlab = "Lick", ylab = "Oyster Counts")
  lines(temp_s5$lick, cumsum(temp_s5$size_class_2), lty = 1, lwd = 2, col = 'red')
  lines(temp_s6$lick, cumsum(temp_s6$size_class_3), lty = 1, lwd = 2, col = 'purple')
  title(main = paste("Post-Season Station:", i))
  legend("topleft", legend = c("Size Class 1: < 1 inch", "Size Class 2: 1-3 inches", "Size Class 3: > 3 inches"), cex = 0.85, lty = 1, col = c("black", "red", "purple"), bty="n")
}


#create a plot showing mean count across vessels per lick 
f <- function(x){
  c(m = mean(x),
    up = mean(x) + 1.96*sd(x),
    lo = mean(x) - 1.96*sd(x))
}

#par(mfrow = c(1, 2))


layout(matrix(c(1,1,2), nrow = 1, ncol = 3, byrow = FALSE))
for(i in stations){
  st <- i
  
  temp_pre <- subset(pre_season_2020, pre_season_2020$station == st)
  temp_s1 <- aggregate(size_class_1 ~ lick, data = temp_pre, FUN = f)
  temp_s2 <- aggregate(size_class_2 ~ lick, data = temp_pre, FUN = f)
  temp_s3 <- aggregate(size_class_3 ~ lick, data = temp_pre, FUN = f)
  
  temp_post <- subset(post_season_2021, post_season_2021$station == st)
  temp_s4 <- aggregate(size_class_1 ~ lick, data = temp_post, FUN = f)
  temp_s5 <- aggregate(size_class_2 ~ lick, data = temp_post, FUN = f)
  temp_s6 <- aggregate(size_class_3 ~ lick, data = temp_post, FUN = f)
  
  plot(temp_s1$lick, temp_s1$size_class_1[,1], type = "l", lty=3, lwd = 2, col = 'black',
       ylim = c(min(temp_s1$size_class_1[,1], temp_s2$size_class_2[,1], temp_s3$size_class_3[,1], temp_s4$size_class_1[,1], temp_s5$size_class_2[,1], temp_s6$size_class_3[,1], na.rm = T),
                max(temp_s1$size_class_1[,1],temp_s2$size_class_2[,1],temp_s3$size_class_3[,1], temp_s4$size_class_1[,1], temp_s5$size_class_2[,1], temp_s6$size_class_3[,1], na.rm = T)),
       xlim = c(0,30),
       xlab = "Lick",
       ylab = "Mean Oyster Counts")
  lines(temp_s2$lick, temp_s2$size_class_2[,1], lty=3,lwd = 2, col = 'red')
  lines(temp_s3$lick, temp_s3$size_class_3[,1], lty=3, lwd = 2, col = 'purple')
  
  lines(temp_s4$lick, temp_s4$size_class_1[,1], lty=1, lwd = 2, col = 'black')
  lines(temp_s5$lick, temp_s5$size_class_2[,1], lty=1,lwd = 2, col = 'red')
  lines(temp_s6$lick, temp_s6$size_class_3[,1], lty=1,lwd = 2, col = 'purple')
  title(main = paste("Station:", i))
  
  #legend("topright", legend = c("Pre-Season","Size Class 1: < 1 inch", "Size Class 2: 1-3 inches", "Size Class 3: >3 inches", "Post-Season","Size Class 1: < 1 inch", "Size Class 2: 1-3 inches", "Size Class 3: >3 inches"),
  #       lty = c(0,3,3,3,0,1,1,1),
  #       col = c("black", "black", "red", "purple"), bty="n",
  #       )
  
  plot(0,0, type="l", bty = "n", xaxt = "n", yaxt = "n", ann=FALSE )
  legend("topleft",
         legend = c("Pre-Season","Size Class 1: < 1 inch", "Size Class 2: 1-3 inches", "Size Class 3: >3 inches", "",
                    "Post-Season","Size Class 1: < 1 inch", "Size Class 2: 1-3 inches", "Size Class 3: >3 inches"),
         lty = c(0,3,3,3,0,0,1,1,1),
         col = c("black", "black", "red", "purple","black","black", "black", "red", "purple"),
         xpd = TRUE,
         bty = "y")
}

