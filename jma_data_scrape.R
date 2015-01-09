# get daily data from the JMA site

source("r/utilities.R")

# make this as automatic as I can for various cases

# start with the page for a weather station, with the year and month modified
# set year as XXXX
# set month as MM

sanda_base <- "http://www.data.jma.go.jp/obd/stats/etrn/view/daily_a1.php?prec_no=63&block_no=0969&year=XXXX&month=MM&day=&view="
koshigaya_base <- "http://www.data.jma.go.jp/obd/stats/etrn/view/daily_a1.php?prec_no=43&block_no=0364&year=XXXX&month=MM&day=1&view="
tokyo_base <- "http://www.data.jma.go.jp/obd/stats/etrn/view/daily_s1.php?prec_no=44&block_no=47662&year=XXXX&month=MM&day=&view="

startYear <- 1949
endYear <- 2014
duration <- endYear - startYear + 1

tokyo <- vector(mode ="character", length(duration))

for (i in 1:duration) {
  year <- startYear - 1 + i
  address <- gsub("XXXX", year, tokyo_base)
  tokyo[[i]] <- address
}

results <- data.frame()

for(j in (1:12)) {
    address <- gsub("MM", j, tokyo)
    newline <- cbind(j, address)
    results <- rbind(results, newline)
  }

year <- rep(startYear:endYear, times = 12)

results$address <- as.character(results$address)
results$month <- as.numeric(as.character(results$j))
results$year <- year

tokyoDuration <- data.frame()

for (i in 1:(duration * 12)) {
   allTables <- readHTMLTable(results[[i, 2]])
   # pulling the proper table is tricky because
   # sometimes it is 2, or 3, or 4
   weather.data <- allTables[[4]]
   weather.data$month <- results[[i, 1]]
   weather.data$year <- results[[i, 4]]
   tokyoDuration <- rbind(tokyoDuration, weather.data)
}

# clean the data
full.tokyo <- as.data.frame(tokyoDuration[, c(1, 7, 8, 9, 22, 23)])
colnames(full.tokyo) <- c("day", "mean.temp", "high", "low", "month", "year")
full.tokyo$day <- as.numeric(as.character(full.tokyo$day), as.is = TRUE)
full.tokyo$mean.temp <- as.numeric(as.character(full.tokyo$mean.temp), as.is = TRUE)
full.tokyo$low <- as.numeric(as.character(full.tokyo$low), as.is = TRUE)
full.tokyo$high <- as.numeric(as.character(full.tokyo$high), as.is = TRUE)
full.tokyo$month <- as.numeric(as.character(full.tokyo$month))

# remove the not necessary rows
full.tokyo <- subset(full.tokyo, mean.temp > -279)

# paste date to 1 variable
full.tokyo$date <- ymd(paste(full.tokyo$year, full.tokyo$month,
                             full.tokyo$day, sep = "-"))

# write the data to a file so it doesn't have to be
# downloaded again
# write.table(full.tokyo, "data/full.tokyo.csv", sep = ",", row.names = FALSE)
