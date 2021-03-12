#2_2a_1_check_for_differences.R 


ri <- read.table( paste0(L8datpath, "ri_txt/all_swir_ids.txt"))

dirs <- grep(".csv$", list.files("E:/new_downscaling/data_download_preprocessing/L8/", full.names = T), 
     value = TRUE, invert = TRUE)

full_path_old_scenes <- unique(list.files(paste0(dirs, "/LST/"), full.names=T, pattern="bt_band10.tif"))


filenames_old_scenes <- unique(list.files(paste0(dirs, "/LST/"), full.names=F, pattern="bt_band10.tif"))
filenames_old_scenes <- substring(filenames_old_scenes, 1, 40)


filenames_old_date <- as.Date(substring(filenames_old_scenes, 18,25), format="%Y%m%d")
ri_date <- as.Date(substring(ri$V1, 18,25), format="%Y%m%d")

df <- data.frame(old_files=filenames_old_date, in_new = filenames_old_scenes %in% ri$V1)
oldnewplot <- ggplot(df, aes(x=old_files, y=in_new))+geom_point()+theme_minimal()+
  ggtitle("old files in new files?", 
          subtitle = paste0("n old = ", length(filenames_old_date), " n new = ", length(ri$V1)))+
  ylab("")+xlab("")



not_selected_now <- filenames_old_scenes[filenames_old_scenes %in% ri$V1==FALSE]

### get all momentary timediff files ##########################################################################
# tdfiles <- list.files(L8datpath, pattern = "timediff", recursive=T, full.names = T)
# 
# td <- lapply(seq(tdfiles), function(j){
#   read.csv2(tdfiles[j])
# })
# 
# tdcomp <- do.call("rbind", td)
# tdcomp$l8date <- as.POSIXct(tdcomp$l8date)
# 
# tdcomp$hour <- hour(tdcomp$l8date)
# tdcomp$month <- factor(month(tdcomp$l8date), levels=c("9", "10", "11", "12", "1", "2", "3"))
# tdcomp$monthyear <- paste0(tdcomp$month, "_", year(tdcomp$l8date))
# tdcomp$allminutesdiff <- tdcomp$timediff*60
# tdcomp$l8date_1 <- as.POSIXct(tdcomp$l8date)
# 
# tdcomp_closematch <- tdcomp[tdcomp$allminutesdiff < 15,]
# 
# tdcomp_closematch$MODMYD
# write.csv2(tdcomp_closematch, paste0(tdpath, "timediffs_below_15.csv")) 
# write.csv2(tdcomp, paste0(tdpath, "all_timediffs.csv")) # = all_comb_scenes.csv in old version



tdcomp <- read.csv2(paste0(tdpath, "all_timediffs.csv")) 


tdcomp <- tdcomp[tdcomp$found==TRUE,]
tdcomp <- tdcomp[complete.cases(tdcomp),]


head(tdcomp)
p <- ggplot(data=tdcomp, aes(x=as.Date(l8date_1), y=allminutesdiff, colour=as.factor(month)))+
  geom_point(size=2)+
  scale_color_manual(values=c("lightsteelblue2","coral3","hotpink4",
                              "darkgoldenrod2", "olivedrab3", "lightcyan4"))+
  theme_bw() +
  labs(color = "month")+
  theme(panel.grid.major = element_blank(), 
    plot.title = element_text(lineheight=.8, face="bold", size = 16),
    plot.subtitle = element_text(size = 14),
    legend.text=element_text(size=14, hjust = 0),
    legend.title = element_text(size=16, hjust=0),
    axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size = 16),
    panel.grid.minor = element_blank(),
    axis.text.y = element_text(size = 11),
    axis.text.x = element_text(size=11, angle=90,face="bold"),
    strip.background = element_blank(),
    strip.text = element_text(size=14))+
  labs(title="time difference (min) in Landsat / Modis scenes over time",
       subtitle = paste0("n scenes = ", nrow(tdcomp), "; n unique months = ", length(unique(tdcomp$monthyear))))+
  xlab("date")+ylab("min")

timediff_scene_plot <- p + scale_x_date(date_labels = "%Y %m",date_breaks = "6 month")



######### are there any images that


# old timediff
allcombold <- read.csv2("E:/new_downscaling/all_comb_scenes.csv")

head(allcombold$L8name)
head(tdcomp$L8scene)

inboth <- allcombold$L8name %in% substring(tdcomp$L8scene,1, 25)
inboth[inboth==FALSE] <- "old"
inboth[inboth==TRUE] <- "new"

allcombold$inboth <- as.factor(inboth)
allcombold$MODMYD <- as.factor(allcombold$MODMYD)
levels(allcombold$MODMYD) <- c("Terra (MOD)", "Aqua (MYD)")
allcombold$L8date <- as.POSIXct(allcombold$L8date)


nrow(allcombold[allcombold$inboth=="old",])
nrow(allcombold[allcombold$inboth=="new",])

bp <- ggplot(allcombold)+xlab("")

timediffplot_old_new_query <- bp + geom_boxplot(aes(y=timediff, x=inboth, fill=inboth))+
  ggtitle("time difference (min)")+theme(legend.position = "none")+xlab("")+ 
  geom_jitter(aes(y=timediff, x=inboth), width = 0.05)+
  scale_fill_manual(values=c("#69b3a2", "grey"))+
  theme(legend.position = "none")

ATplot <- bp + geom_bar(aes(x=MODMYD, fill=inboth), stat="count",position="dodge")+
  ggtitle("Aqua / Terra", subtitle="n old = 303, n new = 119")+
  scale_fill_manual(values=c("#69b3a2", "grey"))+ylab("n scenes")


# ############# get all qualitychecks 
# qcf <- list.files("E:/new_downscaling/data_download_preprocessing/L8/", pattern="qualitycheck", recursive = T, full.names=T)
# 
# qc <- lapply(seq(qcf), function(j){
#   read.csv(qcf[[j]])
# })
# 
# qmf <- list.files("E:/new_downscaling/data_download_preprocessing/L8/", pattern="L8querymatched.rds", recursive = T, full.names=T)
# l8querymatchedold <- lapply(seq(qmf), function(j){
#   x <- readRDS(qmf[j])
#   x
#   
#   
# })
# 
# 
# qmfnew <- list.files(L8datpath, pattern="L8querymatched.rds", recursive = T, full.names=T)
# l8querymatchednew <- lapply(seq(qmfnew), function(j){
#   readRDS(qmfnew[j])
# })


datefiles <- list.files("E:/new_downscaling/data_download_preprocessing/L8/", pattern="L8_date", full.names = T)
df <- lapply(seq(datefiles), function(j){
  read.csv(datefiles[j])
})


df <- do.call("rbind",df)
nrow(df)


df$selected_in_new <- df$fname %in% ri$V1 
df$selected_in_new[df$selected_in_new==FALSE] <- "old"
df$selected_in_new[df$selected_in_new==TRUE] <- "new"
df$selected_in_new <- as.factor(df$selected_in_new)

df$lcc[df$selected_in_new == "new"]

levels(df$selected_in_new)

plcc <- ggplot(df)+xlab("")+ geom_boxplot(aes(y=lcc, x=selected_in_new, fill=selected_in_new))+
  ggtitle("land cloud cover")+theme(legend.position = "none")+xlab("")+ 
  geom_jitter(aes(y=lcc, x=selected_in_new),width = 0.025)+
  scale_fill_manual(values=c("#69b3a2", "grey"))+ylab("lcc in %")+
  theme(legend.position = "none")

# any scene not tier 2? 
any(grepl("T1", df$fname))
