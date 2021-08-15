main <- "D:/downscaling_after_talk/aoa/"

aoa_1 <- readRDS(paste0(main, "aoa_rf_rand_1.RDS"))
aoa_2 <- readRDS(paste0(main, "aoa_rf_rand_2.RDS"))
aoa_3 <- readRDS(paste0(main, "aoa_rf_rand_3.RDS"))

alist <- list(aoa_1, aoa_2, aoa_3)
nam <- c("spatial", "temporal", "spatiotemporal")
nam <- paste0(nam, " validation")

lapply(seq(alist), function(i){
  png(paste0(main, "distribution_plots/", i, ".png"), 
      width = 20, height=20, units = "cm", res = 300)
  par(mfrow=c(3,1))
  hist(alist[[i]]$DI, breaks=5000, main=paste0("DI ", nam[i]))
  boxplot(alist[[i]]$DI, horizontal = T, main=paste0("DI ", nam[i]))
  plot(alist[[i]]$DI, alist[[i]]$AOA, main =paste0("DI (x) and AOA (y)", nam[i]),
       xlab="DI", ylab="AOA")
  dev.off()
})

tvpath <- "D:/downscaling_after_talk/clean_data/train_valid/"
val1 <- read.csv2(paste0(tvpath, "validation_1_rand_150000.csv"))
val2 <- read.csv2(paste0(tvpath, "validation_2_rand_150000.csv"))
val3 <- read.csv2(paste0(tvpath, "validation_3_rand_150000.csv"))

val1$DI <- aoa_1$DI
val2$DI <- aoa_2$DI
val3$DI <- aoa_3$DI

train <- read.csv2(paste0(tvpath, "train_LHS_150000.csv"))
train$TeAq <- as.factor(substring(train$Mscene,1,3))
train$TeAqNum <- as.numeric(train$TeAq)

# per row: check, whether values in predictors are within Box of training data

pr <- c("Modis", "dem", "landcoverres", "soilraster",
  "ia", "slope", "aspect", "TeAqNum")


boxes <- lapply(seq(pr), function(i){
  print(i)
  v <- train[pr[i]]
  quantile(v[,1], probs = c(0.05,0.95))
})

names(boxes) <- pr

vallist <- list(val1, val2, val3)

vl <- lapply(seq(vallist), function(j){ # per validation set
    inbox_perrow <- lapply(seq(nrow(vallist[[j]])), function(i){ # per row in validation set
      
      vallist[[j]]$TeAq <- as.factor(substring(vallist[[j]]$Mscene,1,3))
      vallist[[j]]$TeAqNum <- as.numeric(vallist[[j]]$TeAq)
    
      vlj <- vallist[[j]][c("Modis", "dem", "landcoverres", "soilraster",
           "ia", "slope", "aspect", "TeAqNum")]
      dfboxes <- do.call("rbind",boxes)
      x <- vlj[i,] 
      intrainbox <- lapply(seq(ncol(x)), function(v){ # per predictor in validation set
        x[,v] > dfboxes[v,1] & x[,v] < dfboxes[v,2]
      })
      
      itb <- unlist(intrainbox)
      names(itb) <- c("Modis", "dem", "landcoverres", "soilraster",
                      "ia", "slope", "aspect", "TeAqNum")
      itb <- data.frame(t(data.frame(itb)))
      itb$DI <- alist[[j]]$DI[i]
      itb$AOA <- alist[[j]]$AOA[i]
      rownames(itb) <- rownames(vallist[[j]])[i]
      return(itb)
  })
  inbox <- do.call("rbind",inbox_perrow)
  write.csv2(inbox, paste0(main, "in_5_to_95_training_val_",j,".csv"))
})


