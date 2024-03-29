### colors 

# stuff I like
cols <- c("hotpink4", "gray15",
          "chartreuse3", 
          "lightcyan4","yellowgreen",
          "turquoise4","lightsteelblue2",
          "darkgoldenrod2", "coral3","cadetblue4",
          "darkolivegreen3", "darkmagenta", "olivedrab3", 
          "#F5F5F5")

barplot(seq(1:length(cols)), col=cols)


# colsmonths
cm <- c()
cols <- c("hotpink4","coral3",
          "darkgoldenrod2", "olivedrab3",
          "cadetblue4","lightsteelblue2", "lightcyan4")
levels(all$month)

cols <- c("cadetblue4", "lightsteelblue2","hotpink4","coral3",
          "darkgoldenrod2", "olivedrab3", "lightcyan4")
barplot(seq(1:length(cols)), col=cols)


# this paper
cols2 <- c("hotpink4", "chartreuse3")
barplot(seq(1:length(cols2)), col=cols2)

grays <- c("gray87", "gray29")
barplot(seq(1:length(grays)), col=grays)
