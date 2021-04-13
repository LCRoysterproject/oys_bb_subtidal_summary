### Oyster Tong Data
## Jennifer Moore
## December 3, 2020



#read in data
#tong <- read.csv("tong_data.csv", header = T)

tong<- read.csv("~/GitHub/oys_bb_subtidal_summary/data/tong_data2.csv", header=TRUE)
#tong<- read.csv("~/GitHub/oys_bb_subtidal_summary/data/tong_data.csv", header=TRUE)


#vessels
length(unique(tong$vessel))
#5 levels

#stations
length(unique(tong$station))
#18

table(tong$vessel, tong$station)
#23 combinations

#list of the unique station names
stations <- unique(tong$station)

#create a plot showing cumulative counts per size class by lick
#want one set of plots for each station with separate subplots for each vessel
par(mfrow = c(3, 3))
for(i in stations){
  st <- i
  
  temp <- subset(tong, tong$station == st)
  temp_s1 <- aggregate(size_class_1 ~ lick, data = temp, FUN = 'sum')
  temp_s2 <- aggregate(size_class_2 ~ lick, data = temp, FUN = 'sum')
  temp_s3 <- aggregate(size_class_3 ~ lick, data = temp, FUN = 'sum')
  
 
  plot(temp_s1$lick, cumsum(temp_s1$size_class_1), type = "l", lwd = 2, col = 'black', ylim = c(0,max(cumsum(temp$size_class_1),cumsum(temp$size_class_2),cumsum(temp$size_class_3))), xlab = "Lick", ylab = "Oyster Counts")
  lines(temp_s2$lick, cumsum(temp_s2$size_class_2), lwd = 2, col = 'red')
  lines(temp_s3$lick, cumsum(temp_s3$size_class_3), lwd = 2, col = 'purple')
  legend("topleft", legend = c("Size Class 1: < 1 inch", "Size Class 2: 1-3 inches", "Size Class 3: >3 inches"), lty = 1, col = c("black", "red", "purple"), bty="n")
  title(main = paste("Station:", i))
 
}



#create a plot showing mean count across vessels per lick 


par(mfrow = c(3, 3))
for(i in stations){
  st <- i
  
  temp <- subset(tong, tong$station == st)
  f <- function(x){
    c(m = mean(x),
    up = mean(x) + 1.96*sd(x),
    lo = mean(x) - 1.96*sd(x))
  }
  temp_s1 <- aggregate(size_class_1 ~ lick, data = temp, FUN = f)
  temp_s2 <- aggregate(size_class_2 ~ lick, data = temp, FUN = f)
  temp_s3 <- aggregate(size_class_3 ~ lick, data = temp, FUN = f)
  
  plot(temp_s1$lick, temp_s1$size_class_1[,1], type = "l", lwd = 2, col = 'black', ylim = c(min(temp_s1$size_class_1[,1], temp_s2$size_class_2[,1], temp_s3$size_class_3[,1], na.rm = T),max(temp_s1$size_class_1[,1],temp_s2$size_class_2[,1],temp_s3$size_class_3[,1], na.rm = T)), xlab = "Lick", ylab = "Oyster Counts")
  #lines(temp_s1$lick, temp_s1$size_class_1[,2], type = "l", lwd = 2, col = 'black', lty = 2)
  #lines(temp_s1$lick, temp_s1$size_class_1[,3], type = "l", lwd = 2, col = 'black', lty = 2)
  
  lines(temp_s2$lick, temp_s2$size_class_2[,1], lwd = 2, col = 'red')
  #lines(temp_s2$lick, temp_s2$size_class_2[,2], lwd = 2, col = 'red', lty = 2)
  #lines(temp_s2$lick, temp_s2$size_class_2[,3], lwd = 2, col = 'red', lty = 2)
  
  lines(temp_s3$lick, temp_s3$size_class_3[,1], lwd = 2, col = 'purple')
  #lines(temp_s3$lick, temp_s3$size_class_3[,2], lwd = 2, col = 'purple', lty = 2)
  #lines(temp_s3$lick, temp_s3$size_class_3[,3], lwd = 2, col = 'purple', lty = 2)
  
  #legend("topleft", legend = c("Size Class 1: < 1 inch", "Size Class 2: 1-3 inches", "Size Class 3: >3 inches"), lty = 1, col = c("black", "red", "purple"), bty="n")
  title(main = paste("Station:", i))
  
}

