# for a chart of long term Knoxville data
# these data are from http://www.ncdc.noaa.gov/cdo-web/datasets/GHCND/locations/CITY:US470010/detail
# I downloaded them and then read them in here

tys <- read.csv("data/455487.csv",
                sep = ",", header = TRUE)

tys <- tys[, c(3, 6, 7)]

colnames(tys) <- c("date", "high", "low")

tys$date <- ymd(tys$date)
tys$high <- tys$high / 10
tys$low <- tys$low / 10

low21 <- filter(tys, low > 21)

count21 <- low21 %>%
  group_by(year(date)) %>%
  summarise(length(low))

colnames(count21) <- c("year", "count")

# plot number of days with low > 21 per year
p <- ggplot(data = count21, aes(x = year, y = count))
p + geom_point() +
  scale_y_continuous(limits = c(0, 65)) +
  scale_x_continuous(limits = c(1910, 2015), breaks = seq(1910, 2015, 5)) +
  geom_smooth(se = FALSE) +
  labs(x = "Year",
       y = "Number of days in the year with low >= 21°C (70°F)",
       title = "Knoxville, Tennessee, 1910 to 2014") +
  theme_bw(base_size = 18)

tys$avg <- (tys$high + tys$low) / 2

# what is the hottest month?
monthly <- tys %>%
  group_by(month(date)) %>%
  summarise(mean(avg))

# July is hottest
# look at low trend for July

july <- filter(tys, month(date) == 7)

julyLow <- july %>%
  group_by(year(date)) %>%
  summarise(mean(low),
            mean(high))

colnames(julyLow) <- c("year", "low", "high")
julyLow$hf <- as.factor("high")
julyLow$lf <- as.factor("low")

# plot July low temp average

p <- ggplot(data = julyLow, aes(x = year, y = low))
p + geom_point(aes(colour = lf)) +
  geom_line(size = 0.2, aes(colour = lf)) +
  geom_point(data = julyLow, aes(x = year, y = high,
                                 colour = hf)) +
  geom_line(data = julyLow, size = 0.2, aes(x = year, y = high,
                                            colour = hf)) +
  scale_y_continuous(limits = c(0, 36), breaks = seq(0, 36, 3)) +
  scale_x_continuous(limits = c(1910, 2015),
                     breaks = seq(1910, 2015, 5)) +
  labs(x = "Year",
       y = "Temperature (°C)",
       title = "Knoxville Mean July Temperatures\n1910 to 2014") +
  scale_colour_brewer(palette = "Set1", name="") +
  theme_minimal(base_size = 18)

# plot annual high, low

annual <- tys %>%
  group_by(year(date)) %>%
  summarise(mean(high),
            mean(low))

colnames(annual) <- c("year", "high", "low")
annual$hf <- as.factor("high")
annual$lf <- as.factor("low")

# plot annual mean high and low
p <- ggplot(data = annual, aes(x = year, y = low))
p + geom_point(aes(colour = lf)) +
  geom_line(size = 0.2, aes(colour = lf)) +
  geom_point(data = annual, aes(x = year, y = high,
                                 colour = hf)) +
  geom_line(data = annual, size = 0.2, aes(x = year, y = high,
                                            colour = hf)) +
  scale_y_continuous(limits = c(0, 24), breaks = seq(0, 24, 2)) +
  scale_x_continuous(limits = c(1910, 2015),
                     breaks = seq(1910, 2015, 5)) +
  labs(x = "Year",
       y = "Temperature (°C)",
       title = "Knoxville Mean Annual Temperatures\n1910 to 2014") +
  scale_colour_brewer(palette = "Set1", name="") +
  theme_minimal(base_size = 18)
