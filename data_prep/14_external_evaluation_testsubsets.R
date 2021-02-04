
modelpath <-"C:/Users/mleza/OneDrive/Documents/PhD/work_packages/auto_downscaling_30m/final_150000/"

############ get and organize test data ###################

# in 10a testsubsets from all months are gathered 
test <- read.csv2(paste0(modelpath, "test_complete.csv"))
train <- read.csv2(paste0(modelpath, "training_complete_150000_clhs.csv"))

testmonths <- c("2019-09","2018-10", "2016-01", "2015-02", "2014-11", "2013-12")

# test samples graphic 
traintestcolors <- c("#383D3D", "#899C9C")
testmonthstrue <- unique(test$ymo) %in% testmonths
cobarp <- ifelse(testmonthstrue, traintestcolors[1], traintestcolors[2])
barplot(table(test$ymo), las=2, main="test subsamples per month",
        col=cobarp)

##############################################################
#################    #1     ##################################
#########    test areas of all 30 months     #################
#### introducing unknown areas for known and unknown months ##
##############################################################

test1 <- test

nrow(test1)
write.csv(test1, paste0(modelpath, "test1.csv"))


##############################################################
#################    #2     ##################################
#####    training areas of the 6 testing months     ##########
#####   introducing known area, but unknown time step  ######
##############################################################

# take DI samples from the testmonths
f <- list.files(modelpath, pattern="train_DI", full.names=T)

# checking, whether everything's there
trainarchivesymo <- substring(basename(f), 10,16)
all(trainarchivesymo %in% testmonths)
all(testmonths %in% trainarchivesymo )

alltest2 <- lapply(seq(f), function(i){
  read.csv2(f[i])
})

test2 <- do.call(rbind, alltest2)

unique(test2$ymo)

nrow(test2)
write.csv(test2, paste0(modelpath, "test2.csv"))

##############################################################
#################    #3     ##################################
#####    test areas of the 6 testing months     ##############
#########    introducing unknown areas for unknown months ####
##############################################################
test3 <- test[test$ymo %in% testmonths,]

nrow(test3)
write.csv(test3, paste0(modelpath, "test3.csv"))

