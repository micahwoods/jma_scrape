# make charts and counts of the Tokyo data

tokyo <- read.csv("data/full.tokyo.csv",
                  sep = ",", header = TRUE)

tokyo$date <- ymd(as.character(tokyo$date))

low21 <- filter(tokyo, low >= 21)

count21 <- low21 %>%
  group_by(year) %>%
  summarise(length(low))

colnames(count21) <- c("year", "countLow")

p <- ggplot(data = count21, aes(x = year, y = countLow))
p + geom_point() +
  scale_y_continuous(limits = c(0, 100)) +
  scale_x_continuous(limits = c(1945, 2020)) +
  geom_smooth(method = 'lm', se = FALSE) +
  labs(x = "Year",
       y = "Number of days in the year with low >= 21°C") +
  theme_bw(base_size = 18)


# plot this as a dot plot

# count21$order <- reorder(meanLow23$city, meanLow23$average)

# working here with lows of 21 at moment
p <- ggplot(data = count21, aes(x = year, y = countLow, label = countLow))
p + geom_point(size = 4) +
  scale_y_continuous(limits = c(0, 100)) +
  scale_x_continuous(breaks = seq(1945, 2020, 5),
                     limits = c(1945, 2020)) +
  geom_smooth(method = 'lm', se = FALSE) +
  #coord_flip() + 
  geom_text(size = 4, hjust=-.4, vjust=0) +
  labs(y =  "Number of days each year with low temperature >= 21°C, Tokyo",
       x = "Year") +
  theme_minimal(base_size = 20)

lm(countLow ~ year, data = count21)

reg <- lm(countLow ~ year, data = count21)
summary(reg)

# get mean temps by year, then plot

annualMeans <- tokyo %>%
  group_by(year) %>%
  summarise(mean(low),
            mean(mean.temp),
            mean(high))

colnames(annualMeans) <- c("year", "low", "mean", "high")
library(reshape2)
library(RColorBrewer)
ann2 <- melt(annualMeans, id = "year")

p <- ggplot(data = annualMeans, aes(x = year, y = low))
p + geom_point() +
  geom_line() +
  geom_point(data = annualMeans, aes(x = year, y = mean)) +
  geom_line(data = annualMeans, aes(x = year, y = mean)) +
  geom_point(data = annualMeans, aes(x = year, y = high)) +
  geom_line(data = annualMeans, aes(x = year, y = high))

# TOKYO 1876 kara
# http://www.data.jma.go.jp/obd/stats/etrn/view/annually_s.php?prec_no=44&block_no=47662&year=&month=&day=&view=

allTokyo <- readHTMLTable("http://www.data.jma.go.jp/obd/stats/etrn/view/annually_s.php?prec_no=44&block_no=47662&year=&month=&day=&view=")

tokyoAll <- allTokyo[[2]]

tokyoMeans <- as.data.frame(tokyoAll[, c(1, 8, 9, 10)])
colnames(tokyoMeans) <- c("year", "mean.temp", "high", "low")
tokyoMeans$year <- as.numeric(as.character(tokyoMeans$year))
tokyoMeans$mean.temp <- as.numeric(as.character(tokyoMeans$mean.temp))
tokyoMeans$high <- as.numeric(as.character(tokyoMeans$high))
tokyoMeans$low <- as.numeric(as.character(tokyoMeans$low))

tokyoMeans$highF <- "High"
tokyoMeans$meanF <- "Mean"
tokyoMeans$lowF <- "Low"

# plot tokyo means since 1876
p <- ggplot(data = tokyoMeans, aes(x = year, y = high))
p + geom_point(aes(colour = highF)) +
  geom_line(size = 0.2, aes(colour = highF)) +
  geom_point(data = tokyoMeans, aes(x = year, y = mean.temp,
                                    colour = meanF)) +
  geom_line(data = tokyoMeans, size = 0.2, aes(x = year, y = mean.temp,
                                   colour = meanF)) +
  geom_point(data = tokyoMeans, aes(x = year, y = low,
                                    colour = lowF)) +
  geom_line(data = tokyoMeans, size = 0.2, aes(x = year, y = low,
                                   colour = lowF)) +
  scale_y_continuous(limits = c(0, 22)) +
  scale_x_continuous(limits = c(1875, 2015),
                     breaks = seq(1870, 2020, 10)) +
  labs(x = "Year",
       y = "Temperature (°C)",
       title = "Tokyo Mean Annual Temperatures\n1876 to 2014") +
  scale_colour_brewer(palette = "Set1", name="") +
  theme_minimal(base_size = 22)

# look at just mean August temperature since 1949

august <- filter(tokyo, month == 8)

augMean <- august %>%
  group_by(year) %>%
  summarise(mean(high),
            mean(mean.temp),
            mean(low))

colnames(augMean) <- c("year", "high", "mean.temp", "low")

augMean$highF <- "High"
augMean$meanF <- "Mean"
augMean$lowF <- "Low"

# plot tokyo means since 1876
p <- ggplot(data = augMean, aes(x = year, y = high))
p + geom_point(aes(colour = highF)) +
  geom_line(size = 0.2, aes(colour = highF)) +
  geom_smooth(method = lm, se = FALSE) +
  geom_point(data = augMean, aes(x = year, y = mean.temp,
                                    colour = meanF)) +
  geom_line(data = augMean, size = 0.2, aes(x = year, y = mean.temp,
                                               colour = meanF)) +
  geom_smooth(data = augMean, aes(x = year, y = mean.temp), method = "lm",
              se = FALSE) +
  
  geom_point(data = augMean, aes(x = year, y = low,
                                    colour = lowF)) +
  geom_line(data = augMean, size = 0.2, aes(x = year, y = low,
                                               colour = lowF)) +
  geom_smooth(data = augMean, aes(x = year, y = low), method = "lm",
              se = FALSE) +
  scale_y_continuous(breaks = c(20:35)) +
  scale_x_continuous(limits = c(1949, 2015),
                     breaks = seq(1945, 2015, 5)) +
  labs(x = "Year",
       y = "Temperature (°C)",
       title = "Tokyo Mean August Temperatures\n1949 to 2014") +
  scale_colour_brewer(palette = "Set1", name="") +
  theme_minimal(base_size = 22)

augPer <- filter(tokyo, month == 8 &
                   low >= 21)

augPerSum <- augPer %>%
  group_by(year) %>%
  summarise(length(low))

colnames(augPerSum) <- c("year", "count")
augPerSum$percent <- augPerSum$count / 31 * 100

p <- ggplot(data = augPerSum, aes(x = year, y = percent))
p + geom_point(size = 3) +
  scale_y_continuous(limits = c(0, 100)) +
  scale_x_continuous(limits = c(1948, 2015), 
                     breaks = seq(1945, 2015, 5)) +
  labs(x = "Year",
       y = "Percentage of days in August with low >= 21°C") +
  theme_minimal(base_size = 20)
