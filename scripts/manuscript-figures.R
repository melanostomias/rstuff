library(jsonlite)
library(plotly)
## Font color and size
f <- list(
        size = 18,
        color = "black"
)

##Legend
l <- list(font=f,x = .7, y = .9)
## Margin
m <- list(b=100,l=80)



##Figure 3
## Counts of records
df <- read.csv("https://raw.githubusercontent.com/melanostomias/rstuff/master/data/summary/data/asih-by-records.csv",stringsAsFactors = F)
df <- df[order(df$freq,decreasing = T),]


x <- list(title = "ASIH Code",
          tickfont = f,
          titlefont = f,
          type = "category",
          categoryorder = "array",
          categoryarray = sort(df$freq,decreasing = T))
y <- list(title = "Number of Records (thousands)",
          tickfont = f,
          titlefont = f,
          showexponent = "none")
p <- plot_ly(x=df$ASIHCode,y=df$freq,
             marker = list(color = rep(c("black","grey"), times=length(df$ASIHCode), each=1)))%>%
        layout(xaxis = x, yaxis = y,margin=m)
p
export(p,file ="../data/summary/figures/fig3.png",vheight = 1080 )


##Fig4
## Number of individuals
sAC <- read.csv("https://raw.githubusercontent.com/melanostomias/rstuff/master/data/summary/data/asih-by-specimens1.csv")
sAC <- sAC[order(sAC$bluegill.individualCount,decreasing = T),]
x <- list(title = "ASIH Code",
          type = "category",
          tickfont = f,
          titlefont = f,
          categoryorder = "array",
          categoryarray = sort(sAC$bluegill.individualCount,decreasing = T))
y <- list(title = "Number of Specimens (millions)",
          tickfont = f,
          titlefont = f,
          showexponent = "none")

pl <- plot_ly(
        x = sAC$ASIHCode,
        y = sAC$bluegill.individualCount,
        name = "Specimens",
        type = "bar",
        marker = list(color = rep(c("black","grey"), times=length(sAC$ASIHCode), each=1)))%>%
        layout(xaxis = x, yaxis = y,margin=m)
pl
export(pl,file ="../data/summary/figures/fig4.png",vheight = 1080 )







##Fig5
## Unique families
agrDF <- read.csv("https://raw.githubusercontent.com/melanostomias/rstuff/master/data/summary/data/asih-by-families-PLOTLY.csv",stringsAsFactors = F)
x <- list(title = "ASIH Code",
          tickfont = f,
          titlefont = f,
          type = "category",
          categoryorder = "array",
          categoryarray = sort(agrDF$FamilyCount,decreasing = T))
y <- list(title = "Number of Unique Family Name Values",
          tickfont = f,
          titlefont = f,
          showexponent = "none")

pl <- plot_ly(
        x = agrDF$ASIHCode,
        y = agrDF$FamilyCount,
        name = "Unique Families by ASIH Code",
        type = "bar",
        marker = list(color = rep(c("black","grey"), times=length(agrDF$ASIHCode), each=1))) %>%
        layout(xaxis = x, yaxis = y,margin=m)
pl
export(pl,file ="../data/summary/figures/fig5.png",vheight = 1080 )  


##Fig6
## type records
tpDF <- read.csv("https://raw.githubusercontent.com/melanostomias/rstuff/master/data/summary/data/asih-types-summary.csv",stringsAsFactors = F)
x <- list(title = "ASIH Code",
          tickfont = f,
          titlefont = f,
          type = "category",
          categoryorder = "array",
          categoryarray = sort(tpDF$Holotype,decreasing = T))
y <- list(title = "Number of Type Records (thousands)",
          tickfont = f,
          titlefont = f,
          showexponent = "none")

pl <- plot_ly(
        data = tpDF,
        x = ~ASIHCode,
        y = ~Holotype,
        name = "Primary Type",
        type = "bar",
        marker=list(color = "grey")) %>%
        add_trace(y = ~Paratype, name="Secondary Type",marker=list(color = "black")) %>%
        layout(xaxis = x, yaxis = y,barmode="stack",margin=m,legend=l)# %>%
        #add_annotations(x = tpDF[tpDF$Holotype<50,]$ASIHCode,
        #                y = tpDF[tpDF$Holotype<50,]$Holotype,
        #                text = tpDF[tpDF$Holotype<50,]$Holotype,
        #                xref = "x",
        #                yref = "y",
        #                showarrow = TRUE,
        #                arrowhead = 4,
        #                arrowsize = .5
        #                #ax = 20,
        #                #ay = -40
        #                )
pl
export(pl,file ="../data/summary/figures/fig6.png",vheight = 1080 )


##fig7
##geopoints
##https://raw.githubusercontent.com/melanostomias/rstuff/master/data/summary/data/asih-geopoint-summary.csv

gpDF <- read.csv(file ="https://raw.githubusercontent.com/melanostomias/rstuff/master/data/summary/data/asih-geopoint-summary.csv",stringsAsFactors = F)
x <- list(title = "ASIH Code",
          type = "category",
          categoryorder = "array",
          tickfont = f,
          titlefont = f,
          categoryarray = sort(gpDF$gpPCT,decreasing = T))
y <- list(title = "Percent of Records with a Geopoint",
          tickfont = f,
          titlefont = f,
          showexponent = "none")
pl <- plot_ly(
        data = gpDF,
        x = ~ASIHCode,
        y = ~gpPCT,
        name = "Percentage of Records that a geoPoint can be Created",
        type = "bar",
        marker = list(color = rep(c("black","grey"), times=length(gpDF$ASIHCode), each=1))) %>%
        layout(xaxis = x, yaxis = y,margin=m)
pl
export(pl,file ="../data/summary/figures/fig7.png",vheight = 1080 )



##fig8
## comp records
## https://raw.githubusercontent.com/melanostomias/rstuff/master/data/summary/data/asih-by-records-1995.csv

recs <- read.csv("https://raw.githubusercontent.com/melanostomias/rstuff/master/data/summary/data/asih-by-records.csv",stringsAsFactors = FALSE)
recs95 <- read.csv("https://raw.githubusercontent.com/melanostomias/rstuff/master/data/summary/data/asih-by-records-1995.csv",stringsAsFactors = F,header = F)
names(recs95) <- c("ASIHCode","freq95")
recComb <- base::merge(recs,recs95,all.x=T)
recComb <- recComb[order(recComb$freq,decreasing = T),]
recComb <- recComb[!is.na(recComb$freq95),]
x <- list(title = "ASIH Code",
          type = "category",
          categoryorder = "array",
          tickfont = f,
          titlefont = f,
          categoryarray = sort(recComb$freq,decreasing = T)
          )
y <- list(title = "Number of Records (thousands)",
          autotick = FALSE,
          ticks = "outside",
          tick0 = 0,
          dtick = 25000,
          tickfont = f,
          titlefont = f,
          showexponent="none")
pl <- plot_ly(
        x = recComb$ASIHCode,
        y = recComb$freq,
        name = "Present Study",
        type = "bar",
        marker=list(color="black")) %>%
        add_trace(y=recComb$freq95,name="Poss and Collette 1995",marker=list(color="grey")) %>%
        layout(xaxis = x, yaxis = y, barmode="group",margin=m, legend=l)
pl
export(pl,file ="../data/summary/figures/fig8.png",vheight = 1080 )





##fig9
##comp individ
## https://raw.githubusercontent.com/melanostomias/rstuff/master/data/summary/data/asih-by-specimens1-1995.csv
## https://raw.githubusercontent.com/melanostomias/rstuff/master/data/summary/data/asih-by-specimens1.csv

sAC <- read.csv("https://raw.githubusercontent.com/melanostomias/rstuff/master/data/summary/data/asih-by-specimens1.csv")
sAC95 <- read.csv("https://raw.githubusercontent.com/melanostomias/rstuff/master/data/summary/data/asih-by-specimens1-1995.csv",stringsAsFactors = F,header = F)
names(sAC95) <- c("ASIHCode","freq95")
sACcomb <- base::merge(sAC,sAC95,all.x=T)
sACcomb <- sACcomb[order(sACcomb$bluegill.individualCount,decreasing = T),]
sACcomb <- sACcomb[!is.na(sACcomb$freq95),]
x <- list(title = "ASIH Code",
          type = "category",
          tickfont = f,
          titlefont = f,
          categoryorder = "array",
          categoryarray = sort(sACcomb$bluegill.individualCount,decreasing = T))
y <- list(title = "Number of Specimens (millions)",
          tickfont = f,
          titlefont = f,
          showexponent="none")

pl <- plot_ly(
        x = sACcomb$ASIHCode,
        y = sACcomb$bluegill.individualCount,
        name = "Present Study",
        type = "bar",
        marker=list(color="black")) %>%
        add_trace(y=sACcomb$freq95,name="Poss and Collette 1995",marker=list(color="grey"))%>%
        layout(xaxis = x, yaxis = y,barmode="group",margin=m, legend=l)
pl
export(pl,file ="../data/summary/figures/fig9.png",vheight = 1080 )



##fig10
##comp species
## https://raw.githubusercontent.com/melanostomias/rstuff/master/data/summary/data/asih-by-species-1995.csv
## https://raw.githubusercontent.com/melanostomias/rstuff/master/data/summary/data/asih-by-species.csv

sp <- read.csv("https://raw.githubusercontent.com/melanostomias/rstuff/master/data/summary/data/asih-by-species.csv",stringsAsFactors = FALSE)
sp95 <- read.csv("https://raw.githubusercontent.com/melanostomias/rstuff/master/data/summary/data/asih-by-species-1995.csv",stringsAsFactors = F,header = F)
names(sp95) <- c("ASIHCode","freq95")
spComb <- base::merge(sp,sp95,all.x=T)
aggSpecsDF <- spComb
aggSpecsDF <- aggSpecsDF[order(aggSpecsDF$freq,decreasing = T),]
x <- list(title = "ASIH Code",
          type = "category",
          tickfont = f,
          titlefont = f,
          categoryorder = "array",
          categoryarray = sort(aggSpecsDF$freq,decreasing = T))
y <- list(title = "Number of Unique Values in 'dwc.scientificName' (thousands)",
          tickfont = f,
          titlefont = f,
          showexponent = "none")

pl <- plot_ly(
        x = aggSpecsDF$ASIHCode,
        y = aggSpecsDF$freq,
        name = "Present Study",
        type = "bar",
        marker=list(color="black")) %>%
        add_trace(y= aggSpecsDF$freq95,name="Poss and Collette 1995",marker=list(color="grey"))%>%
        layout(xaxis = x, yaxis = y,barmode="group",margin=m, legend=l)
pl
export(pl,file ="../data/summary/figures/fig10.png",vheight = 1080 )  





##fig11
##comp type
## https://raw.githubusercontent.com/melanostomias/rstuff/master/data/summary/data/asih-types-summary-1995.csv
## https://raw.githubusercontent.com/melanostomias/rstuff/master/data/summary/data/asih-types-summary.csv

tpDF <- read.csv("https://raw.githubusercontent.com/melanostomias/rstuff/master/data/summary/data/asih-types-summary.csv",stringsAsFactors = F)
tpDF95 <- read.csv("https://raw.githubusercontent.com/melanostomias/rstuff/master/data/summary/data/asih-types-summary-1995.csv",stringsAsFactors = F)

names(tpDF95) <- c("ASIHCode","Holotype95","Paratype95")

tpDF <- base::merge(tpDF,tpDF95,all.x=T)
tpDF <- tpDF[order(tpDF$Holotype,decreasing = T),]
tpDF <- tpDF[!is.na(tpDF$Holotype95),]
##Scale YAXIS now
tpDF$Holotype <- tpDF$Holotype/1000
tpDF$Holotype95 <- tpDF$Holotype95/1000

x <- list(title = "ASIH Code",
          type = "category",
          tickfont = f,
          titlefont = f,
          categoryorder = "array",
          categoryarray = sort(tpDF$Holotype,decreasing = T))
y <- list(title = "Number of Primary Type Records (thousands)",
          tickfont = f,
          titlefont = f
          )
pl <- plot_ly(
        data = tpDF,
        x = ~ASIHCode,
        y = ~Holotype,
        name = "Present Study",
        type = "bar",
        marker=list(color="black")) %>%
        add_trace(y = ~Holotype95, name="Poss and Collette 1995",marker=list(color="grey")) %>%
        layout(xaxis = x, yaxis = y,barmode="group",margin=m, legend=l)
pl
export(pl,file ="../data/summary/figures/fig11.png",vheight = 1080 )




##fig12
## need description

x <- list(title = "ASIH Code",
          type = "category",
          tickfont = f,
          titlefont = f,
          categoryorder = "array",
          categoryarray = sort(tpDF$Paratype,decreasing = T))
y <- list(title = "Number of Secondary Type Records (thousands)",
          tickfont = f,
          titlefont = f,
          showexponent = "none")
pl <- plot_ly(
        data = tpDF,
        x = ~ASIHCode,
        y = ~Paratype,
        name = "Present Study",
        type = "bar",
        marker=list(color="black")) %>%
        add_trace(y = ~Paratype95, name="Poss and Collette 1995",marker=list(color="grey")) %>%
        layout(xaxis = x, yaxis = y,barmode="group",margin=m, legend=l)
pl
export(pl,file ="../data/summary/figures/fig12.png",vheight = 1080 )






preps <- read.csv("https://raw.githubusercontent.com/melanostomias/rstuff/master/data/summary/data/preps-distribution.csv",stringsAsFactors = F)

##fig13
## c&s
## https://raw.githubusercontent.com/melanostomias/rstuff/master/data/summary/data/preps-distribution.csv
cs <- preps[c(1,3)]
cs <- cs[complete.cases(cs),]
cs <- cs[order(cs$clearAndStain,decreasing = T),]
cs$clearAndStain <- cs$clearAndStain/1000
x <- list(title = "ASIH Code",
          type = "category",
          tickangle = -90,
          tickfont = f,
          titlefont = f,
          categoryorder = "array",
          categoryarray = sort(cs$clearAndStain,decreasing = T))
y <- list(title = "Number of Specimen Records with Cleared and Stained Specimen Preparations (thousands)",
          tickfont = f,
          titlefont = f)
p1 <- plot_ly(
        data = cs,
        x = ~ASIHCode,
        y = ~clearAndStain,
        type = "bar",
        marker = list(color = rep(c("black","grey"), times=length(cs$ASIHCode), each=1)))%>%
        layout(xaxis = x,yaxis=y,margin=m)
export(p1,file ="../data/summary/figures/fig13.png",vheight = 1080 )



##fig14
##skel
skel <- preps[c(1,8)]
skel <- skel[complete.cases(skel),]
skel <- skel[order(skel$skeleton,decreasing = T),]
x <- list(title = "ASIH Code",
          tickfont = f,
          titlefont = f,
          type = "category",
          categoryorder = "array",
          categoryarray = sort(skel$skeleton,decreasing = T))
y <- list(title = "Number of Specimen Records with Skeletal Specimen Preparations (thousands)",
          tickfont = f,
          titlefont = f,
          showexponent="none")
p2 <- plot_ly(
        data = skel,
        x = ~ASIHCode,
        y = ~skeleton,
        type = "bar",
        marker = list(color = rep(c("black","grey"), times=length(skel$ASIHCode), each=1)))%>%
        layout(xaxis = x,yaxis=y,margin=m)

export(p2,file ="../data/summary/figures/fig14.png",vheight = 1080 )

##fig15
##tissue
tiss <- preps[c(1,9)]
tiss <- tiss[complete.cases(tiss),]
tiss <- tiss[order(tiss$tissue,decreasing = T),]
x <- list(title = "ASIH Code",
          tickfont = f,
          titlefont = f,
          tickangle = -90,
          type = "category",
          categoryorder = "array",
          categoryarray = sort(tiss$tissue,decreasing = T))
y <- list(title = "Number of Specimen Records with Tissues (thousands)",
          tickfont = f,
          titlefont = f,
          showexponent="none")
p3 <- plot_ly(
        data = tiss,
        x = ~ASIHCode,
        y = ~tissue,
        type = "bar",
        marker = list(color = rep(c("black","grey"), times=length(tiss$ASIHCode), each=1)))%>%
        layout(xaxis = x,yaxis=y,margin=m)

export(p3,file ="../data/summary/figures/fig15.png",vheight = 1080 )




##fig16
##media
media <- preps[c(1,5)]
media <- media[complete.cases(media),]
media <- media[order(media$media,decreasing = T),]
x <- list(title = "ASIH Code",
          tickfont = f,
          titlefont = f,
          type = "category",
          categoryorder = "array",
          categoryarray = sort(media$media,decreasing = T))
y <- list(title = "Number of Specimen Records with Media",
          tickfont = f,
          titlefont = f,
          showexponent = "none")
p4 <- plot_ly(
        data = media,
        x = ~ASIHCode,
        y = ~media,
        type = "bar",
        marker = list(color = rep(c("black","grey"), times=length(media$ASIHCode), each=1)))%>%
        layout(xaxis = x,yaxis=y,margin=m)

export(p4,file ="../data/summary/figures/fig16.png",vheight = 1080 )



## Figure 17
## mediarecords

mDF <- read.csv("https://raw.githubusercontent.com/melanostomias/rstuff/master/data/summary/data/asih-by-mediarecords.csv",stringsAsFactors = F)
x <- list(title = "ASIH Code",
          tickfont = f,
          titlefont = f,
          type = "category",
          categoryorder = "array",
          categoryarray = sort(mDF$Count,decreasing = T))
y <- list(title = "Number of Media Records (thousands)",
          tickfont = f,
          titlefont = f,
          showexponent = "none")
p5 <- plot_ly(
        data = mDF,
        x = ~ASIHCode,
        y = ~Count,
        type = "bar",
        marker = list(color = rep(c("black","grey"), times=length(mDF$ASIHCode), each=1)))%>%
        layout(xaxis = x,yaxis=y)
p5
export(p5,file ="../data/summary/figures/fig17.png",vheight = 1080 )