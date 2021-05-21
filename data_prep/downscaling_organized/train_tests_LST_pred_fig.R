
DIdir <- paste0(cddir, "train_test_DI/")

testp <- list.files(DIdir,full.names=T, pattern="test")

test <- read.csv2(testp)
