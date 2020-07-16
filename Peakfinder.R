# Script Name: leg-peakfind.R
# Purpose: This script reads load values of micro-indentation measurements, 
#         then find multiple peaks and valleys.
# Authors: Yoona Yang
# License: Creative Commons Attribution-ShareAlike 4.0 International License.
##########
# Latest Changelog Entires:
# v0.02 - leg-peakfind.R - Yoona Yang started this R script

# Set path

pathdata <- "./data/CSV/"

# Read metadata file
# meta.df <- read.csv(paste0(pathdata,"metadata.csv"))


library(dplyr)

# Function to calculate slope using two points
slope <- function(a, b) {
  return ((b[2] - a[2])/(b[1] - a[1]));
}
# Function to calculate x-intercept using point and slope
xIntercept <- function(a, m) {
  return (a[1] - a[2]/m);
} 



# Find peaks and valleys in a curve
# Use lag and lead to find the previous or next values in a load vector with the number of moved positions, N.mv 
N.mv <- 100

# for (k in 1:length(meta.df$rkey)) {
for (k in 1:1) {
  
  # Read a curve
  # data.df <- read.csv(paste0(pathdata, meta.df$rkey[k],"_", meta.df$mthd[k],".csv"))
  data.df <- read.csv(paste0(pathdata, "LU_1_19_5.csv"))
  
  # Find local maxima and local minima. This will give the region for local maxima and local minima, not exact position. 
  # idxmax or idxmin shows the where local maxima or local minima is.
  temp.df <- mutate(data.df, local.maxima = if_else(lag(load, n = N.mv) < load & lead(load, n = N.mv) < load, TRUE, FALSE))
  idxmax <- which(temp.df$local.maxima)
  temp.df <- mutate(temp.df, local.minima = if_else(lag(load, n = N.mv) > load & lead(load, n = N.mv) > load, TRUE, FALSE))
  idxmin <- which(temp.df$local.minima)
  
  # Here, you can plot all local maxima and local minima, but there are a number of local maxima and local minima, 
  # so it takes very long to show all the maxima and minima. They are needed to be filtered. 
#   plot(data.df$load, type = 'l')
#  for (i in 1:length(temp.df$local.maxima)) {
#    points(idxmax[i],data.df$load[idxmax[i]])
#  }
#  for (i in 1:length(temp.df$local.minima)) {
#    points(idxmin[i],data.df$load[idxmin[i]])
#  }
  

  # Let's find where the local minima or local maxima region starts from
  # There is a big difference in index number between peaks or valleys 
  idxidxmin <- which(diff(idxmin)>200)
  xmin.start <- idxmin[idxidxmin]
  xmin.start[length(xmin.start)+1] <- idxmin[length(idxmin)] # include point for last peak
  idxmin2 <- NULL    # Initialize idxmin2
  for (i in 1:length(xmin.start)) {
    # If the starting point is smaller than N.mv, set starting point to be 1
    if ((xmin.start[i]-N.mv) < 1) {
      tempmin <- which(data.df$load == min(data.df$load[1:(xmin.start[i]+N.mv)]))
    }
    # else, find minimum in the region from (starting point-N.mv) to (starting point+N.mv)
    else tempmin <- which(data.df$load == min(data.df$load[(xmin.start[i]-N.mv):(xmin.start[i]+N.mv)]))
        if (length(tempmin) > 1) {idxmin2 <- cbind(idxmin2, tempmin[which(abs(tempmin-xmin.start[i])==min(abs(tempmin-xmin.start[i])))])}
    else idxmin2 <- cbind(idxmin2,tempmin)
  }
  
  # Find where a big difference in index number (Find points near local maxima)
  idxidxmax <- which(diff(idxmax)>200)
  xmax.start <- idxmax[idxidxmax]   
  xmax.start[length(xmax.start)+1] <- idxmax[length(idxmax)] # include point for last peak
  idxmax2 <- NULL    # Initialize idxmax2
  for (i in 1:length(xmax.start)) {
    # If the starting point is smaller than N.mv, set starting point to be 1
    if ((xmax.start[i]-N.mv) < 1) {
      tempmax <- which(data.df$load == max(data.df$load[1:(xmax.start[i]+N.mv)]))
    }
    # else, find maximum in the region from (starting point-N.mv) to (starting point+N.mv)
    else tempmax <- which(data.df$load == max(data.df$load[(xmax.start[i]-N.mv):(xmax.start[i]+N.mv)]))
    if (length(tempmax) > 1) {idxmax2 <- cbind(idxmax2, tempmax[which(abs(tempmax-xmax.start[i])==min(abs(tempmax-xmax.start[i])))])}
    else idxmax2 <- cbind(idxmax2,tempmax)
  }
  
  # Save local maxima and minima in terms of load and depth using index numbers
  local.min <- NULL
  for (i in 1:length(idxmin2)) {
    local.min <- cbind(local.min, c(data.df$depth[idxmin2[i]],data.df$load[idxmin2[i]]))
  }
  local.min <- cbind(local.min, c(data.df$depth[length(data.df$depth)-100],data.df$load[length(data.df$load)-100]))
  idxmin2 <- cbind(idxmin2,length(data.df$depth)-100)
  
  local.max <- NULL
  for (i in 1:length(idxmax2)) {
    local.max <- cbind(local.max, c(data.df$depth[idxmax2[i]],data.df$load[idxmax2[i]]))
  }
  
  # Plot a load curve with local maxima and local minima
  #plot(data.df$load, type = 'l')
  #for (i in 1:length(idxmin2)) {
  #  points(idxmin2[i],data.df$load[idxmin2[i]], col="green")
  #}
  #for (i in 1:length(idxmax2)) {
  #  points(idxmax2[i],data.df$load[idxmax2[i]], col="red")
  #}
  
  # Plot a depth vs. load curve with local maxima and local minima
  #plot(data.df$depth, data.df$load, type = 'l')
  #for (i in 1:length(local.min[1,])) {
  #  points(local.min[1,i],local.min[2,i], col="green")
  #}
  #for (i in 1:length(local.max[1,])) {
  #  points(local.max[1,i],local.max[2,i], col="red")
  #}
  
  
  
  ## Get rid of wrong local maxima and minima
  # To avoid error for unmatched dimension, define minimum and maximum length of idxmax2 and idxmin2
  l.min <- min(c(length(idxmax2),length(idxmin2)))  
  l.max <- max(c(length(idxmax2),length(idxmin2)))
  df.l <- l.max - l.min
  
  # Remove wrong local maxima and minima until the dimensions of idxmax2 and idxmin2 match (df.l = 0)
  while (df.l != 0) {
    # If there are more number of local maxima
    if (length(idxmax2) > length(idxmin2)) {
      if (length(as.matrix(idxmin2)[,1]) > 2) {idxmin2 <- t(as.matrix(idxmin2)[,1])} # To avoid error due to transposed vector
      ext.idxmin2 <- cbind(matrix(0,1,l.max-l.min),idxmin2)     # Duplicate idxmin2 of same length of idxmax2
      # For a maximum/minimum pair, maximum should be appeared first (idxmax2 > idxmin2)
      # Find a wrong pair
      ix <- which(idxmax2-ext.idxmin2 > 0)      
      # If there are multiple wrong local maxima
      if (length(ix)>1) {
        for (i in 1:(length(ix)-1)) {
          # If multiple wrong local maxima are in series
          if ((ix[i+1] -ix[i]) == 1) {
            bb <- max(data.df$load[idxmax2[ix[i]]],data.df$load[idxmax2[ix[i+1]]])  # Find real maximum
            ix.notmax <- which(bb != data.df$load[idxmax2[ix[i]:ix[i+1]]]) # Index of wrong maximum
            # Remove wrong local maximum
            idxmax2 <- idxmax2[-ix.notmax] 
            local.max <- local.max[,-ix.notmax]
          }
        }
      } 
      # If there is a wrong local maximum
      if (length(ix) == 1) {
        bb <- max(data.df$load[idxmax2[ix]],data.df$load[idxmax2[ix+1]])   # Find real maximum
        ix.notmax <- which(bb != data.df$load[idxmax2[ix:(ix+1)]]) # Index of wrong maximum
        # Remove wrong local maximum
        idxmax2 <- idxmax2[-ix.notmax]
        local.max <- local.max[,-ix.notmax]
      }
    }
    
    # If first minimum appears before first maximum, remove the first minimum
    if (idxmax2[1]>idxmin2[1]) {
      idxmin2 <- idxmin2[,-1]
      local.min <- local.min[,-1]
    }
    
    # Recalculate l.min, l.max and df.l for next loop
    l.min <- min(c(length(idxmax2),length(idxmin2)))
    l.max <- max(c(length(idxmax2),length(idxmin2)))
    df.l <- l.max - l.min
  }
  
  # Plot a load curve with local maxima and local minima
  plot(data.df$load, type = 'l')
  for (i in 1:length(idxmin2)) {
    points(idxmin2[i],data.df$load[idxmin2[i]], col="green")
  }
  for (i in 1:length(idxmax2)) {
    points(idxmax2[i],data.df$load[idxmax2[i]], col="red")
  }
  
  
  halfpeak <- (data.df$load[idxmax2[1]]-data.df$load[1])/2
  idx.half <- which.min(abs(data.df$load[1:idxmax2[1]] - halfpeak))
  w.half <- 50   # Set (the number of data points)/2 that you want to extract.
  
  temp50 <- as.data.frame(data.df$load[(idx.half-w.half):(idx.half+w.half)])
  temp50[,2] <- data.df$depth[(idx.half-w.half):(idx.half+w.half)]
  colnames(temp50) <- c(paste0("X",k),paste0("Y",k))
  if (k == 1) {
    list50 <- as.data.frame(temp50)
  } else {
    list50 <- cbind(list50, temp50)
  }
  
  # Save local maxima and local minima
  if (k == 1) {
    max.list <- as.data.frame(local.max)
    min.list <- as.data.frame(local.min)
    }
  if (k > 1) {
    max.list <- dplyr::full_join(max.list,as.data.frame(local.max))
    min.list <- dplyr::full_join(min.list,as.data.frame(local.min))
    }
  
  # Estimate x-intercept (as h_i) by calculating load()
  h <- NULL
  
  afterpeak.depth <- NULL
  afterpeak.load <- NULL
  
  # Set the number of points in unloading part
  Npoint <- 100
  # Calculate slopes and x-intercepts and 50 points after peak
  for (i in 1:length(local.min[1,])) {
    if (i == 1) {
      afterpeak.depth <- as.data.frame(data.df$depth[idxmax2[i]:(idxmax2[i]+Npoint)])
      afterpeak.load <- as.data.frame(data.df$load[idxmax2[i]:(idxmax2[i]+Npoint)])
    } else {
      afterpeak.depth <- cbind(afterpeak.depth, data.df$depth[idxmax2[i]:(idxmax2[i]+Npoint)])
      afterpeak.load <- cbind(afterpeak.load, data.df$load[idxmax2[i]:(idxmax2[i]+Npoint)])
    }
    colnames(afterpeak.depth)[i] <- paste0('depth_',i)
    colnames(afterpeak.load)[i] <- paste0('load_',i)
    slp <- slope(local.max[,i],local.min[,i])
    h <- cbind(h, xIntercept(local.max[,i],slp))
  }
  
  # Save x-intercepts in h.list
  if (k == 1) {h.list <- as.data.frame(h)}
  if (k > 1) {h.list <- dplyr::full_join(h.list,as.data.frame(h))}
    #rbind(h.list,h)
  # Plot slopes and x-intercepts
  plot(data.df$depth, data.df$load, type = 'l')
  for (i in 1:length(h)) {
    lines(c(h[i],local.max[1,i]),c(0,local.max[2,i]),col="red",lty=2)
    lines(afterpeak.depth[,i],afterpeak.load[,i],col="blue", lwd=3)
  }
  lines(list50[,(k*2)],list50[,(k*2-1)],col="yellow", lwd=3)
  
  
}


write.csv(temp50, file = "EM.csv")
write.csv(afterpeak.depth, file = "depth.csv")
write.csv(afterpeak.load, file = "load.csv")

write.csv(max.list,file="peaks.csv")
write.csv(h.list,file="intercept.csv")





