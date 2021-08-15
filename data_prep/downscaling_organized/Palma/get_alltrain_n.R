outdir <- "/scratch/tmp/llezamav/extraction/"

f <- list.files(outdir, pattern="extr_complete_cases",full.names=T)
print(f)

n15m <- lapply(seq(f), function(i){
  x <- read.csv2(f[i])
  nrow(x)
})

print(n15m)
df <- data.frame(f, unlist(n15m))

print("full n equals: ")
print(sum(unlist(n15m)))

write.csv2(df, paste0(outdir, "n_15_months.csv2"))
