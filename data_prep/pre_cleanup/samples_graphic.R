# samples graphic

library(tidyr)
library(ggplot2)
library(Hmisc)
library(plyr)
library(RColorBrewer)
library(reshape2)




train <- readRDS(paste0(maindir, "extraction/train_DI__2019-01.rds"))
test <- readRDS(paste0(maindir, "extraction/test_ds_2019-01.rds"))

ts <- sample(nrow(test), 150000)
testsubset <- test[ts,]

test <- testsubset

names(train)
names(test)

train$tt <- "train"
test$tt <- "test"

comp <- rbind(train[,c(1:19,21)], test)
comp$tt <- as.factor(comp$tt)

complstm <- melt(comp, id.vars = c("tt"), 
                 measure.vars = c("Modis", "Landsat"), 
                 variable.name = "sensor", 
                 value.name = "LST")

complstm <- complstm[complstm$LST > -40,]

complstm$tt <- factor(complstm$tt , levels = c("train", "test"))
complstm$sensor <- factor(complstm$sensor , levels = c("Landsat", "Modis"))


lb <- function(x) mean(x) - sd(x)
ub <- function(x) mean(x) + sd(x)

library(plyr)
sumld<- ddply(complstm, ~Sensor, summarise, 
              mean = mean(LST), 
              median = median(LST), 
              lower = lb(LST), 
              upper = ub(LST))

head(sumld)



g <- ggplot(data = complstm, aes(y = LST, x = sensor, fill = tt)) +
  geom_split_violin(alpha=0.8)+
  geom_boxplot(width = .17, outlier.shape = NA, alpha = 0.8,
               position=position_dodge(0.5), color="black",lwd=0.75) +
  scale_fill_manual(values=c("gray29","gray87"))+
  theme_bw() +
  custom_theme+
  labs(title="Land surface temperature distribution January 2019", fill=" ", 
       subtitle = paste0("n training samples=", nrow(comp[comp$tt=="train",]), 
                         "  n test samples = ", nrow(comp[comp$tt=="test",])))
g

ggsave(filename = "D:/new_downscaling/figures/train_test_sample_distribution.png", 
       width = 10, height = 8, units = "in", dpi=300, type = "cairo")

