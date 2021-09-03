library(stringr)

l <- c('a', 'b', 'c', 'd')

old <- 'c'
new <- 'y'

l <- replace(l, which(l == old), new)


for (i in l){
  message(i)
}

cnames <- colnames(data_03_m)
for (cname in cnames){
  if (startsWith(cname, "Temp")){
    temp_name <- cname
  } else if (startsWith(cname, "High Range")){
    cond_name <- cname
  } else if (startsWith(cname, "Date Time")){
    datetime_name <- cname
  }
}
cnames <- replace(cnames, which(cnames == cond_name), "cond_raw")


