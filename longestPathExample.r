longestPath<-function(){

library(igraph)
library(triangle)

#number of nodes
n<-7
#number of scenarios
Ns<-1000

#return vector
r<-vector(length=Ns)

#generate activity duration vectors
d<- c(1,4,5,7,2,3,1)

#create graph
g<-make_graph(c(1,2,1,3,2,4,3,4,3,5,4,6,6,7,5,7),directed=TRUE)
plot(g)

#generate all paths
path<-all_simple_paths(g,from=1,to=n)
print(path)

#find longesth path
l<-length(path)
maxd<-0
for (i in 1:l){
	pd<-sum(d[path[[i]]])
	if (pd > maxd) {
	maxd<-pd
	}	
}	
maxd
}