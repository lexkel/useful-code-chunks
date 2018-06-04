From flowing data:
https://flowingdata.com/2016/09/08/beeswarm-plot-in-r-to-show-distributions/

library("beeswarm")

# Load data
workers <- read.csv("data/income-sample-2014.tsv", sep="\t", stringsAsFactors=FALSE)

# Beeswarm
beeswarm(workers$INCTOT)

# Beeswarm by occupation
beeswarm(INCTOT ~ main_occ, data=workers, method="swarm")

# Beeswarm options
beeswarm(INCTOT ~ main_occ, data=workers, col=sample(colors(), 27), pch=19, method="swarm", cex=0.5)

# More parameters
par(las=1)
beeswarm(INCTOT ~ main_occ, data=workers, col=sample(colors(), 27), pch=19, method="swarm", cex=0.5, horizontal=TRUE, 
xlab="Annual Income, Dollars", ylab="Occupation Category", main="Distribution of Income, by Occupation Caetgory", 
labels=c(LETTERS, "AA"), bty="n")
