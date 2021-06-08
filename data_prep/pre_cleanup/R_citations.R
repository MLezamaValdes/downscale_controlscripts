library(bibtex)

path <- "C:/Users/mleza/OneDrive/Documents/PhD/work_packages/auto_downscaling_30m/paper/paper_draft/"


ip <- installed.packages()

write.bib(entry=ip[,1],file = paste0(path, "Rpackages.bib"))
