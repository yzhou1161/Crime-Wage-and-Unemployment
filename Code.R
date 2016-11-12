require(ggplot2, data.table, plyr)


c <- read.csv("CrimeStatebyState.csv")
#unemployment rate by state: from http://www.icip.iastate.edu/tables/employment/unemployment-states
u <- read.csv("unemploymentbystate.csv")
#clean up the data
names(u) <- gsub("X","",names(u))
tu <- as.data.frame(t(u))
tu[52:54] <- list(NULL)
colnames(tu) <- as.character(unlist(tu[1,]))
tu <- tu[-1,]
write.csv(tu, "tu.csv")
u <- read.csv("tu.csv")
#merge it with crime rate dataset
d <- read.csv("d.csv")



#Wage data: http://www.bls.gov/cew/datatoc.htm
#CSV By Area, Annual Averages 1990-2015
wage <- dir("~/Desktop/R/CU/Wage", recursive=TRUE, full.names=TRUE, pattern="Statewide.csv$")
w <- lapply(wage, function(x) fread(x, 
            nrows = 1,
            select = c("year","area_title","avg_annual_pay",
                       "annual_avg_wkly_wage")))
w <- do.call("rbind",w)
write.csv(w, "wage.csv")

w <- read.csv("wage.csv")
w$State <- gsub("\\ --.*","",w$State)
w$X <- NULL
write.csv(w,"wage.csv")
w <- w[with(w, order(w$State)),]
rownames(w) <- NULL
write.csv(w,"wage.csv")
#merge wage with the big data file
total <- merge(d,w,by=c("Year","State"))


#Wage2: CSVs by industry, 1980 - 1989
#CSV by Area data from 1980 to 1989 not directly available
wage2 <- dir("~/Desktop/R/CU/Wage/wage2", recursive=TRUE, full.names=TRUE, pattern="all industries.csv$")
w2 <- lapply(wage2, function(x) fread(x, 
                                   
                                    select = c("year","area_title","avg_annual_pay",
                                               "annual_avg_wkly_wage","own_title")))
w2 <- do.call("rbind",w2)
w2 <- subset(w2, w2$own_title == "Total Covered")
w2 <- w2[grep("Statewide", w2$area_title), ]
w2 <- w2[!grepl("Multicounty",w2$area_title),]
library(plyr)
w2 <- rename(w2, State = area_title, Year = year)
w2$State <- gsub("\\ --.*","",w2$State)
w2 <- w2[with(w2, order(w2$State)),]
rownames(w2) <- NULL
w2$own_title <- NULL
write.csv(w2, "wage2.csv")
w2 <- read.csv("wage2.csv")
w2$X <- NULL
w2$State <- as.character(w2$State)


w3 <- rbind(w2,w)
w3 <- w3[with(w3, order(w3$State)),]
total <- merge(d,w3,by=c("Year","State"))
rownames(total) <- NULL
write.csv(total, "d2.csv")


d <- read.csv("d2.csv")
head(d)
library(ggplot2)
p <- ggplot(d,
            aes(x=Year, y=Unemployment.Rate))
p + geom_line(aes(group=State,
                  color=(Violent.Crime.rate>400))) 


p2 <- ggplot(data=d,
            aes(x=avg_annual_pay,
                y=Violent.Crime.rate))
p2 + geom_point(aes(color=State)) +
  geom_smooth(method="loess") + 
  scale_x_log10()


p3 <- ggplot(d, aes(x=avg_annual_pay, y=State, color=Murder.and.nonnegligent.manslaughter.rate))
p3 + geom_point(size=3) + labs(x="Average annual pay",
                               y="State",
                               color="Murder and nonnegligent manslaughter rate") + ggtitle("1980 - 2015")


p <- ggplot(d, aes(x=Unemployment.Rate, y=reorder(State,Unemployment.Rate), color=Violent.Crime.rate))
p + geom_point(size=3)
            

stat <- as.character(state.region)
names(stat) <- as.character(state.name)
stat[c("Illinois","Michigan","Indiana","Ohio","Wisconsin",
       "Iowa","Kansas","Minnesota","Missouri","Nebraska",
       "North Dakota","South Dakota")] <- "Midwest"
d$region <- stat[as.character(d$State)]


p <- ggplot(d[d$Year == 2011,],
            aes(x=Unemployment.Rate, y=Violent.Crime.rate))
p + geom_point() + geom_smooth(method = "loess")




subd <- d[d$annual_avg_wkly_wage != 0,]

p2 <- ggplot(d,
             aes(x=Unemployment.Rate, y=Murder.and.nonnegligent.manslaughter.rate))
p2 + geom_point() + geom_smooth(method = "loess")


p4 <- ggplot(subd,
             aes(x=avg_annual_pay, y=Murder.and.nonnegligent.manslaughter.rate))
p4 + geom_point() + geom_smooth(method = "loess")


p3 <- ggplot(subd,
             aes(x=avg_annual_pay, y=Murder.and.nonnegligent.manslaughter.rate))
p3 + geom_point(aes(color = region)) + geom_smooth(method = "loess")





require(png)
region_list <- unique(d.long$region)

# create for loop to produce ggplot2 graphs 
for (i in seq_along(region_list)) { 
  # create plot for each county in df 
  png(paste0(region_list[i],".png"))
  ggplot(subset(d.long, d.long$region==region_list[i]),
                 aes(Year, value, group = crime, color = crime)) + 
    
    geom_line(size=1) +
    
    scale_y_continuous("Crime rate") +
    scale_x_continuous("Year") +
    
    ggtitle(paste(region_list[i], " Crime Trend",
                  sep=''))
  dev.off()
  #rasterGrob(readPNG(region_list[i],".png", native = FALSE),
             #interpolate = FALSE)
  
}

pdf("teh.pdf")
do.call(grid.arrange, c(thePlots, ncol = 3))
dev.off()









drop <- c("Population","avg_annual_pay","Unemployment.Rate","annual_avg_wkly_wage","State")
d2 <- d[,!names(d)%in%drop]

# melt data to long format 
d.long <- melt(d2, id.vars=c("region", "Year"), variable.name = "crime")


results <- "/Users/Mago/Desktop/R/CU"  






region_list <- unique(d.long$region)
  
  # create for loop to produce ggplot2 graphs 
  for (i in seq_along(region_list)) { 
    # create plot for each county in df 
    plot <- ggplot(subset(d.long, d.long$region==region_list[i]),
             aes(Year, value, group = crime, color = crime)) + 
      
      geom_line(size=1) +
      
  
      scale_y_continuous("Crime rate") +
      scale_x_continuous("Year") +
      
      ggtitle(paste(region_list[i], " Crime Trend",
                    sep=''))
    
  }
  
pdf("plots.pdf",onefile = T)
for (i in seq(length(region_list))) {
  do.call("grid.arrange", region_list[i])  
}
dev.off()
  
