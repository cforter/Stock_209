sp500.path <- "C:\\Users\\carson.GROUP5\\Dropbox\\Stock Research\\sp500\\all.txt"
tickers.path <- "C:\\Users\\carson.GROUP5\\Dropbox\\Stock Research\\sp500\\ticker-symbols.csv"
cik.path <- "C:\\Users\\carson.GROUP5\\Dropbox\\Stock Research\\cik_ticker_join.csv"

sp500 <- read.csv(sp500.path, header=FALSE, stringsAsFactors=FALSE)
colnames(sp500) <- c("date","min","open","hi","low","close","volume")
ticker.symbols <- read.csv(tickers.path)
cik <- read.csv(cid.path,stringsAsFactors=FALSE)

# Create vector with previous day's closing price and day-over-day difference
sp500$previous.close <- c(NA,sp500$close[1:(length(sp500$close)-1)])
sp500$close.diff <- sp500$close - sp500$previous.close

# Data cleaning - find breaks and add tickers

# Create a column with the previous day's date. Records are in order here.
sp500$prev.date <- c(NA,sp500$date[1:(length(sp500$close.diff)-1)])

# Logical vector indicating if the date of the record comes after the date of the previous record
sp500$date.breaks <- ifelse((sp500$date - sp500$prev.date) < 0, 1, 0)

# Find just the records where the date does not come after
# the previous record, i.e. beginning of a new company
breaks <- rownames(sp500[sp500$date.breaks == 1,])
breaks <- as.numeric(breaks)

# Replace the leading NA with a 1
breaks <- c(1, breaks[2:length(breaks)])


# Load tickers, only provides one for each company
tickers <- ticker.symbols$ticker

# For each ticker, replciate equal to the number of records for that comany 
ticker.vector <- c()
for(i in 1:499) {
  this.ticker <- rep(as.character(tickers[i]),(breaks[i + 1] - breaks[i]))
  ticker.vector <- c(ticker.vector,this.ticker)
} 

# Add the last ticker, since the breaks vector doesn't do this
sp500$ticker <- c(ticker.vector, rep("zmh", 3020))

# Add CIKs
cik$ticker <- tolower(cid$ticker)
sp.joined <- merge(sp500, cik, by = "ticker")

# Write to csv in wd
write.csv(sp.joined, file = "sp500_cik.csv", row.names = F)

# Wide version of data
sp.compact <- sp500[,c(1,6,18)]
sp.wide <- reshape(sp.compact, 
                   timevar = "ticker",
                   idvar = "date",
                   direction = "wide")

#Correlations between companies
sp.cor <- cor(sp.wide[,-1],use="pairwise.complete.obs")


