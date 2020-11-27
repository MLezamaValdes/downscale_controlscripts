### 10b make test datasets #####

#########    model saw trainign areas of 24 months     #######

testpath <- "/scratch/tmp/llezamav/satstacks/extraction_result_new/"
testpath <- "C:/Users/mleza/OneDrive/Desktop/testsubsets/"

test <- read.csv2(paste0(testpath, "test_complete.csv"))


##############################################################
#################    #1     ##################################
#########    test areas of all 30 months     #################
#### introducing unknown areas for known and unknown months ##
##############################################################







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

