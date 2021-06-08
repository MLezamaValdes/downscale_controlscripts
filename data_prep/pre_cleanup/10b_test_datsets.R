### 10b make test datasets #####

#########    model saw trainign areas of 24 months     #######

testpath <- "/scratch/tmp/llezamav/satstacks/extraction_result_new/"
testpath <- "C:/Users/mleza/OneDrive/Desktop/testsubsets/"
testmonths <- c("2019-09","2018-10", "2016-01", "2015-02", "2014-11", "2013-12")



test <- read.csv2(paste0(testpath, "test_complete.csv"))

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




##############################################################
#################    #2     ##################################
#####    training areas of the 6 testing months     ##########
#####   introducing known area, but unknown time step  ######
##############################################################

# look at 10a




##############################################################
#################    #3     ##################################
#####    test areas of the 6 testing months     ##############
#########    introducing unknown areas for unknown months ####
##############################################################

test3 <- test[test$ymo %in% testmonths,]
nrow(test3)
