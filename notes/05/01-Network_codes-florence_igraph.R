#setwd("/Users/hcn4/Dropbox/Georgetown/Presentations/Networks")
rm(list=ls())
library(igraph)
#load("florentine.RData")
library(network)
data(flo)
gflo <- graph_from_adjacency_matrix(flo)
gflo

plot(gflo)
deg <- degree(gflo)
betw <- betweenness(gflo)
ev <- evcent(gflo)$vector
clos <- closeness(gflo)


names <- names(V(gflo))
name.colors <- rep("purple",16)
name.colors[2] <- "black"
name.colors[15] <- "gold2"
name.colors[9] <- "red"
name.colors[10] <- "blue"
name.colors.2 <- rep("purple",16)
name.colors.2[c(2,9,10,15)] <- "black"


bet.names <- names[order(betw)]

##pdf("Florentine_Between.pdf",width=8,height=7)
par(mai=c(1,2,.5,1))
barplot(sort(betw), horiz=T, names.arg=bet.names, las=1, main="Betweenness Centrality for Florentine Families",col=name.colors[order(betw)])
##dev.off()

##pdf("Florentine_Degree.pdf",width=8,height=7)
deg.names <- names[order(deg)]
par(mai=c(1,2,.5,1))
barplot(sort(deg), horiz=T, names.arg=deg.names, las=1, main="Degree Centrality for Florentine Families", col= name.colors[order(deg)])
##dev.off()

##pdf("Florentine_EV.pdf",width=8,height=7)


ev.names <- names[order(ev)]
par(mai=c(1,2,.5,1))
barplot(sort(ev), horiz=T, names.arg=ev.names, las=1, main="Eigenvector Centrality for Florentine Families", col= name.colors[order(ev)])
#dev.off()

#pdf("Florentine_Close.pdf",width=8,height=7)



close.names <- names[order(clos)]
par(mai=c(1,2,.5,1))
barplot(sort(clos), horiz=T, names.arg=close.names, las=1, main="Closeness for Florentine Families", col= name.colors[order(clos)])
#dev.off()


plot(ev,deg)
#pdf("Florentine_Plot.pdf",width=8,height=8)

plot(gflo,
vertex.label.family="sans",vertex.label.font=2,vertex.label.cex=1, label.color=1, vertex.size=6, vertex.color= name.colors, vertex.frame.color= name.colors, vertex.label.dist=2, edge.arrow.mode=0, edge.width=2, edge.color="green4", vertex.label.color=name.colors.2)
#dev.off()



