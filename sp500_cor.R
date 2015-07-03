stocks = read.csv('sp500_cik.csv');

# Wide version of data
sp.compact <- stocks[,c("ticker", "date", "close")]

sp.wide <- reshape(sp.compact, timevar = "ticker", idvar = "date", direction = "wide")

#Correlations between companies
sp.cor <- cor(sp.wide[,-1],use="pairwise.complete.obs")

min_cor = arrayInd(which.min(sp.cor), dim(sp.cor))

rownames(sp.cor)[min_cor[1]]
rownames(sp.cor)[min_cor[2]]
