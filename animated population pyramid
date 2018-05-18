From flowing data:
https://flowingdata.com/2015/11/03/animated-pyramid-chart-in-r/

library(animation)

# Load the data. Source: US Census Bureau
population <- read.csv("data/us-pop-age-sex.csv", stringsAsFactors=FALSE)

# Pyramid chart for one year, using plotrix package
library(plotrix)

# Subset on age and sex
ages <- unique(population$age)[-1]  # Won't chart the total
pop2015 <- subset(population, year==2015 & age %in% ages)
female <- pop2015$female_pct
male <- pop2015$male_pct

# Draw the plot
pyramid.plot(male, female, labels=ages, main="US Population 2015")

# Simplified
par(cex=0.85)
par(mar=pyramid.plot(male, female, labels=ages, main="Population Percentages by Age and Sex, 2015", lxcol="#A097CC", rxcol="#EDBFBE", unit="", xlim=c(10,10), gap=0))

# Plot for every year.
par(mfrow=c(4,4))
for (y in unique(population$year)) {
  pop_year <- subset(population, year==y & age %in% ages)
  female <- pop_year$female_pct
  male <- pop_year$male_pct  
  par(cex=0.6)
  par(mar=pyramid.plot(male, female, top.labels=c("Male", "", "Female"), labels=ages, main=y, lxcol="#A097CC", rxcol="#EDBFBE", xlim=c(10,10), gap=0, unit=""))
}

# Animate
ani.options(outdir = paste(getwd(), "/images", sep=""))

saveGIF({
  for (y in unique(population$year)) {
    pop_year <- subset(population, year==y & age %in% ages)
    female <- pop_year$female_pct
    male <- pop_year$male_pct    
    par(cex=0.8)
    par(mar=pyramid.plot(male, female, top.labels=c("Male", "", "Female"), labels=ages, 
      main=y, lxcol="#A097CC", rxcol="#EDBFBE", xlim=c(10,10), gap=0, unit=""))
  }  
}, movie.name = "population-pyramid-animated.gif", interval=0.15, nmax=100, ani.width=650, ani.height=400)

saveHTML({  
  for (y in unique(population$year)) {
    pop_year <- subset(population, year==y & age %in% ages)
    female <- pop_year$female_pct
    male <- pop_year$male_pct    
    par(cex=0.8)
    par(mar=pyramid.plot(male, female, top.labels=c("Male", "", "Female"), labels=ages, 
      main=y, lxcol="#A097CC", rxcol="#EDBFBE", xlim=c(10,10), gap=0, unit=""))    
  }
  
}, htmlfile = "population-pyramid-animated.html", interval=0.15, nmax=100, ani.width=650, ani.height=400)



