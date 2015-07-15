### Packages ###
library(reshape2)
library(data.table)

### Paths ###
path.to.data <- "C:/Users/carson.GROUP5/Dropbox/Stock Research/209 data/sp500_cik.csv"
path.to.num <- "C:/Users/carson.GROUP5/Dropbox/Stock Research/2009q4/num.txt"
path.to.sub <- "C:/Users/carson.GROUP5/Dropbox/Stock Research/2009q4/sub.txt"
path.to.pre <- "C:/Users/carson.GROUP5/Dropbox/Stock Research/2009q4/pre.txt"

### Read Data and Joins ###
data <- read.csv(path.to.data, stringsAsFactors=FALSE)
num.2009 <- read.delim(path.to.num, sep = "\t")
sub.2009 <- read.delim(path.to.sub, sep = "\t")
pre.2009 <- read.delim(path.to.pre, sep = "\t")
joined.data <- merge(data, subset(sub.2009, select = c(cik,adsh)), by = "cik")

### Add Years For Joining ###
joined.data$year <- sapply(joined.data$date, function(x) substring(x,1,4))
sub.2009$year <- rep("2009", nrow(sub.2009))
pre.2009$year <- rep("2009", nrow(pre.2009))

##joined.pre <- merge(joined.data, pre.2009,by= c("adsh", "year"))## definitely not efficient

### Collapse 2009 data by company ###
data.2009 <- subset(joined.data, joined.data$year == "2009")
dt.2009 <- data.table(data.2009)
collapsed.2009 <- dt.2009[,list(year = year, cik = cik, 
                                adsh = adsh, open.mean = mean(open)),
                          by = ticker]
collapsed.2009 <- data.frame(collapsed.2009)
collapsed.2009 <- unique(collapsed.2009)
joined.pre <- merge(collapsed.2009, pre.2009, by = "adsh")

### Test Analysis ###
# Compute tag-wise mean
# Test one company - all means should be the same
test <- aggregate(open.mean ~ tag, FUN = mean, data = subset(joined.pre, joined.pre$ticker == "aep"))
# Test with random subset, for use with larger data sets
test <- aggregate(open ~ tag, FUN = mean, data = joined.pre[sample(nrow(joined.pre),10000),])
# Explore tags
tags.count <- as.data.frame(table(joined.pre$tag))
counts.count <- table(tags.count$Freq)

### Real Analysis ###
agg.2009 <- aggregate(open.mean ~ tag, FUN = mean, data = joined.pre)
# Merge tag count with tag-wise mean 
agg.2009 <- as.data.frame(agg.2009)
colnames(tags.count) <- c("tag", "Freq")
agg.merged <- merge(agg.2009, tags.count, by ="tag")

### Plots ###
agg.subset <- subset(agg.merged, agg.merged$Freq > 30)
barplot(sort(agg.subset$open.mean))

agg.top <- subset(agg.subset, agg.subset$open.mean > 43 | 
                    agg.subset$open.mean < 27 )

agg.values <- agg.top$open.mean
#names(agg.values) <- agg.top$tag
barplot(sort(agg.values), border = NA, ylab = "Share Price",

legend(legend = agg.top$tag, x = "topleft", text.width = 2,
       fill = palette(rainbow(45, alpha = .8, s = .4, v = .8)), cex = .7, , bty = "n")
