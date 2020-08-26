# samples graphic

library(tidyr)
library(ggplot2)
library(Hmisc)
library(plyr)
library(RColorBrewer)
library(reshape2)

GeomSplitViolin <- ggproto("GeomSplitViolin", GeomViolin, 
                           draw_group = function(self, data, ..., draw_quantiles = NULL) {
                             data <- transform(data, xminv = x - violinwidth * (x - xmin), xmaxv = x + violinwidth * (xmax - x))
                             grp <- data[1, "group"]
                             newdata <- plyr::arrange(transform(data, x = if (grp %% 2 == 1) xminv else xmaxv), if (grp %% 2 == 1) y else -y)
                             newdata <- rbind(newdata[1, ], newdata, newdata[nrow(newdata), ], newdata[1, ])
                             newdata[c(1, nrow(newdata) - 1, nrow(newdata)), "x"] <- round(newdata[1, "x"])
                             
                             if (length(draw_quantiles) > 0 & !scales::zero_range(range(data$y))) {
                               stopifnot(all(draw_quantiles >= 0), all(draw_quantiles <=
                                                                         1))
                               quantiles <- ggplot2:::create_quantile_segment_frame(data, draw_quantiles)
                               aesthetics <- data[rep(1, nrow(quantiles)), setdiff(names(data), c("x", "y")), drop = FALSE]
                               aesthetics$alpha <- rep(1, nrow(quantiles))
                               both <- cbind(quantiles, aesthetics)
                               quantile_grob <- GeomPath$draw_panel(both, ...)
                               ggplot2:::ggname("geom_split_violin", grid::grobTree(GeomPolygon$draw_panel(newdata, ...), quantile_grob))
                             }
                             else {
                               ggplot2:::ggname("geom_split_violin", GeomPolygon$draw_panel(newdata, ...))
                             }
                           })

geom_split_violin <- function(mapping = NULL, data = NULL, stat = "ydensity", position = "identity", ..., 
                              draw_quantiles = NULL, trim = TRUE, scale = "area", na.rm = FALSE, 
                              show.legend = NA, inherit.aes = TRUE) {
  layer(data = data, mapping = mapping, stat = stat, geom = GeomSplitViolin, 
        position = position, show.legend = show.legend, inherit.aes = inherit.aes, 
        params = list(trim = trim, scale = scale, draw_quantiles = draw_quantiles, na.rm = na.rm, ...))
}



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

custom_theme = theme(
  text = element_text(size = 10),
  axis.title.x = element_text(size = 16),
  axis.title.y = element_text(size = 16),
  axis.text = element_text(size = 14),
  axis.text.x = element_text(vjust = 0.5),
  legend.title=element_text(size=16),
  legend.text=element_text(size=16, hjust = 0),
  legend.position = c(0.8,0.95),
  legend.direction = "horizontal",
  plot.title = element_text(lineheight=.8, face="bold", size = 16),
  plot.subtitle = element_text(size = 12, face="bold"),
  panel.border = element_blank(),
  panel.grid.minor = element_blank(),
  panel.grid.major = element_blank(),
  legend.key.size = unit(1.5, "cm"),
  legend.key.width = unit(0.75,"cm"),
  axis.line.x = element_blank(),
  axis.ticks.x = element_blank(),
  axis.line.y = element_line(colour = 'black', size=0.3, linetype='solid'))


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

