# get Koshigaya & Sanda data from 2010 through 2014

# make this as automatic as possible for my case of
# 2010 to 2014, Sanda and Koshigaya

# set year as XXXX
# set month as MM

sanda_base <- "http://www.data.jma.go.jp/obd/stats/etrn/view/daily_a1.php?prec_no=63&block_no=0969&year=XXXX&month=MM&day=&view="
koshigaya_base <- "http://www.data.jma.go.jp/obd/stats/etrn/view/daily_a1.php?prec_no=43&block_no=0364&year=XXXX&month=MM&day=1&view="

koshigaya <- vector(mode ="character", length(5))

for (i in 1:5) {
  year <- 2009 + i
  address <- gsub("XXXX", year, koshigaya_base)
  koshigaya[[i]] <- address
}

results <- data.frame()

for(j in (1:12)) {
    address <- gsub("MM", j, koshigaya)
    newline <- cbind(j, address)
    results <- rbind(results, newline)
  }

year <- rep(2010:2014, times = 12)

results$address <- as.character(results$address)
results$month <- as.numeric(as.character(results$j))
results$year <- year

koshigaya5years <- data.frame()

for (i in 1:60) {
  koshigayaTables <- readHTMLTable(results[[i, 2]])
  weather.data <- koshigayaTables[[2]]
  weather.data$month <- results[[i, 1]]
  weather.data$year <- results[[i, 4]]
  koshigaya5years <- rbind(koshigaya5years, weather.data)
}

# clean the data
full.koshigaya <- as.data.frame(koshigaya5years[, c(1, 5, 6, 7, 17, 18)])
colnames(full.koshigaya) <- c("day", "mean.temp", "high", "low", "month", "year")
full.koshigaya$day <- as.numeric(as.character(full.koshigaya$day), as.is = TRUE)
full.koshigaya$mean.temp <- as.numeric(as.character(full.koshigaya$mean.temp), as.is = TRUE)
full.koshigaya$low <- as.numeric(as.character(full.koshigaya$low), as.is = TRUE)
full.koshigaya$high <- as.numeric(as.character(full.koshigaya$high), as.is = TRUE)
full.koshigaya$month <- as.numeric(as.character(full.koshigaya$month))

# remove the not necessary rows
full.koshigaya <- subset(full.koshigaya, mean.temp > -279)

# paste date to 1 variable
full.koshigaya$date <- ymd(paste(full.koshigaya$year, full.koshigaya$month,
                             full.koshigaya$day, sep = "-"))

p <- ggplot(data = full.koshigaya, aes(x = date, y = low))
p + geom_point()
