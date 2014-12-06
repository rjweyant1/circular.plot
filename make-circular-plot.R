library("circlize")
library("plyr")

##  Read in table
setwd('/media/roberto/Main Storage/Documents/bertplot/code/R/Circlize')
dat<-read.table('import-export-trim.csv',sep=',',header=T,stringsAsFactors = T)

# Put data in matrix
m<-matrix(0,13,13)
m[1,2:13]<-dat[,2]
m[2:13,1]<-dat[,3]
rownames(m)<-colnames(m)<-c('USA',as.character(dat[,1]))
m<-m/max(m)

# Data.frame for details on each region
df1<-data.frame(order=1:13,region=rownames(m))

##  Sort order of data.frame
##  and matrix for plotting in circos
df1<-arrange(df1, order)
df1$region <- factor(df1$region, levels=df1$region)
m<-m[levels(df1$region),levels(df1$region)]

##  Define ranges of circos sections
df1$xmin <- 0
df1$xmax <- rowSums(m)+colSums(m)
n<-nrow(df1)

##  Define Colors for links and sections
df1$rcol<-c('#3333FF80',paste('#',dat$col,sep=''))
df1$lcol<-c('#3333FF80',paste('#',dat$col,sep=''))

##  Start Making Plot
par(mar=c(0,0,0,0))
png('test.png',height=1000,width=1000)

circos.clear()
# Set up basic circos par
circos.par(cell.padding=c(0,0,0,0),
           track.margin=c(0,0.05),
           start.degree = 90,
           gap.degree =4)

# Initialize Circos
circos.initialize(factors = df1$region, xlim = cbind(df1$xmin, df1$xmax))

# Plot Outer track and labels
circos.trackPlotRegion(ylim = c(0, 1.05),
                       # How wide the outer track is
                       track.height=0.1,
                       #panel.fun for each section
                       panel.fun = function(x, y) {
                         #select details of current sector
                         name = get.cell.meta.data("sector.index")
                         i = get.cell.meta.data("sector.numeric.index")
                         xlim = get.cell.meta.data("xlim")
                         ylim = get.cell.meta.data("ylim")

                         # Create Custom labels for each country
                         if(df1$xmax[i]>0.6) {
                           labels<-seq(0,df1$xmax[i],0.5)
                         } else if(df1$xmax[i]>0.3) {
                           labels<-seq(0,df1$xmax[i],0.25)
                         } else {
                           labels<-0.05*floor(df1$xmax[i]/.05)
                         }

                         # Add Country Text Labels
                         circos.text(x=mean(xlim), y=2.1, labels=name, facing = "bending", cex=1.7)

                         # Color Outer track for each country
                         circos.rect(xleft=xlim[1], ybottom=ylim[1], xright=xlim[2], ytop=ylim[2],
                                     col = df1$rcol[i], border=df1$rcol[i])

                         # USA Imports
                         circos.rect(xleft=xlim[1], ybottom=ylim[1], xright=xlim[2]-rowSums(m)[i], ytop=ylim[1]+0.3,
                                     col = df1$rcol[1], border = "white")

                         # Black border between main outline and segments
                         circos.rect(xleft=xlim[1], ybottom=0.3, xright=xlim[2], ytop=0.35, col = "black", border = "black")

                         # Axis Labels and tick marks
                         circos.axis(labels.cex=2,major.tick.percentage = .25,labels.niceFacing = F,lwd = 3,
                                     direction = "outside",
                                     #major.at=seq(0,df1$xmax[i],ifelse(df1$xmax[i]>.6,.5,.25)),
                                     major.at=labels,
                                     minor=0,
                                     labels.away.percentage = .1)
                       })

# Plot inner flows
# add sum values to df1, marking the x-position of the
# first links out (sum1) and in (sum2).
df1$sum1 <- colSums(m)
df1$sum2 <- numeric(n)

# Create a data.frame of the flow matrix sorted
# by flow size, to allow largest flow plotted first
df2<-cbind(as.data.frame(m),orig=rownames(m),  stringsAsFactors=FALSE)
df2<-reshape(df2, idvar="orig",
             varying=list(1:n),
             direction="long",
             timevar="dest",
             time=rownames(m),
             v.names = "m")

for(k in 1:nrow(df2)){
  #i,j reference of flow matrix
  i<-match(df2$orig[k],df1$region)
  j<-match(df2$dest[k],df1$region)

  # Plot link
  circos.link(sector.index1=df1$region[i], point1=c(df1$sum1[i], df1$sum1[i] + abs(m[i, j])),
              sector.index2=df1$region[j], point2=c(df1$sum2[j], df1$sum2[j] + abs(m[i, j])),
              col = df1$lcol[i])

  #update sum1 and sum2 for use when plotting the next link
  df1$sum1[i] = df1$sum1[i] + abs(m[i, j])
  df1$sum2[j] = df1$sum2[j] + abs(m[i, j])
}

dev.off()