outaoi <- round(seq(12000, 1, length.out = 200))

x <- 3.5

choose <- log(outaoi)**x

data.frame(outaoi, choose)

plot(outaoi, choose, main="n most dissimilar samples to choose from potential samples \n depending on how many samples are still outside of aoi",
     sub=paste0("reduction log(outaoi)^", x), ylab="n most dissimilar samples", xlab="samples outside of area of applicability")
