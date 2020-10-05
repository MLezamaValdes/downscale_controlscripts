
# check extraction result

datpath <- "/scratch/tmp/llezamav/satstacks/"

time_range <- readRDS("/scratch/tmp/llezamav/time_range.rds")

year <- c(2019:2013)
month <- c("01","02","03","04", "09", "10","11", "12")

fs <- lapply(seq(year), function(y){
  lapply(seq(month), function(m){
    
    ym <- substring(time_range[[y]][[m]][[1]][[1]], 1, 7)
    print(ym)
    
    # f <- paste0(datpath, "pott3_",ym, ".csv")
    # print(f)
    # if(exists(f)){
        ##################### get all generated products ################
        af <- paste0(datpath, "aux_df_swir_x_y_swir_", ym, ".csv")
        print(af)
        aux_swir <- file.size(af)
        fe <- paste0(datpath, "extraction_result/tempdyn_", ym,"_df.csv")
        print(fe)
        full_extraction <-  file.size(fe)
        check_extraction <-  file.size(paste0(datpath, "extraction_result/tempdyndf_check_", ym, ".csv"))
        check_aux <-  file.size(paste0(datpath, "extraction_result/auxdf_check_", ym, ".csv"))
        
        check_extr_cmpcss <-  file.size(paste0(datpath, "extraction_result/extr_comp_check_",ym, ".csv"))
        extr_cmpcss <-  file.size(paste0(datpath, "extraction_result/extr_complete_cases_",ym, ".csv"))
        
        test <- file.size(paste0(datpath, "extraction_result/testsubset_ds_", ym, ".csv")) #150000 samples from within test sites
        pott3 <- file.size(paste0(datpath, "extraction_result/pott3_",ym, ".csv"))
    # 
    # } else {
    #   aux_swir <- NA
    #   full_extraction <-  NA
    #   check_extraction <-  NA
    #   check_aux <-  NA
    #   
    #   check_extr_cmpcss <-  NA
    #   extr_cmpcss <-  NA
    #   
    #   test <- NA
    #   pott3 <- NA
    # }
    # 
    df <- data.frame(ym, aux_swir, full_extraction, check_extraction, check_aux, check_extr_cmpcss, extr_cmpcss, 
                     test, pott3)
    print(df)
    return(df)
  })
})

print(fs[[1]])

fs1 <- lapply(seq(year), function(y){
  do.call("rbind", fs[[y]])
})

print(fs1[[1]])

fsdf <- do.call("rbind", fs1)

fsdf <- fsdf[complete.cases(fsdf),]
fsdf_mb <- fsdf
fsdf_gb <- fsdf

fsdf_mb[,3:ncol(fsdf_mb)] <- fsdf_mb[,3:ncol(fsdf_mb)] / 1e-6
fsdf_gb[,3:ncol(fsdf_gb)] <- fsdf_gb[,3:ncol(fsdf_gb)] / 1e-9

write.csv2(fsdf,"/home/l/llezamav/check_file_sizes.csv")
write.csv2(fsdf_mb,"/home/l/llezamav/check_file_sizes_mb.csv")
write.csv2(fsdf_gb,"/home/l/llezamav/check_file_sizes_gb.csv")
