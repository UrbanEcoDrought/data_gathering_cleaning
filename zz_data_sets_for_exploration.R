# SPI general data cleaning----
# https://www.ncei.noaa.gov/access/monitoring/nadm/indices/spi/div

# subsetting SPI data to IL Division 2
# US-DIV01102  41.80  -88.80 IL NORTHEAST

# have monthly data here.

us.spi.dat <- read.table("input_data/monthly_SPI_US.txt", header=F, na.strings = "-99.99")
head(us.spi.dat)

names(us.spi.dat) <- c("division", "element", "year", "jan", "feb", "march", "april", "may",
                       "jun", "jul", "aug", "sep", "oct", "nov", "dec")

head(us.spi.dat)

# subsetting to division that aligns with Morton arb

mort.spi.dat <- us.spi.dat[us.spi.dat$division=="US-DIV01102",]

# Going to stack to make things a bit more manageable

mort.dat.stack <- stack(mort.spi.dat[,!names(mort.spi.dat) %in% c("division", "element", "year")])
head(mort.dat.stack)
names(mort.dat.stack) <- c("spi.value", "month")

# putting other variables back in
mort.dat.stack$year <- mort.spi.dat$year
mort.dat.stack$division <- mort.spi.dat$division
mort.dat.stack$element <- mort.spi.dat$element

# making a month.year variable
mort.dat.stack$date <- paste(mort.dat.stack$month,"01", mort.dat.stack$year, sep=".")
head(mort.dat.stack)
mort.dat.stack$date <- as.Date(mort.dat.stack$date, tryFormats = "%b.%d.%Y")

write.csv(mort.dat.stack, "processed_data/spi_IL_div02.csv", row.names=F)

##############################################
# Can calc spi and spei data from the SPEI package by NCAR
# needs a water balance dataset for the point or grid (precipitation - potential evapotranspiration)
# the package has functions for calculatin PET, but we'd need to give it the raw data inputs.
library(SPEI)

