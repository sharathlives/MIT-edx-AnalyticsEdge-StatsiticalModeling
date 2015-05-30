#Load the datasets
users <- read.csv("users.csv")
edges <- read.csv("edges.csv")

#Exploratory Analysis
#Average number of friends per user
nrow(edges)*2/unique(edges) #292/59 = 4.95
table(users$school. users$locale) #B is the most common locale
table(users$school, users$gender) #Both school A and B are not all boy or all girl schools

#Create a new grpah object
install.packages("igraph")
library(igraph)
g = graph.data.frame(edges, FALSE, users) 

#Plot the graph
plot(g, vertex.size=5, vertex.label=NA)

#How many connected components with at least 2 nodes are there in the graph?
2
#How many users are there with no friends in the network?
7

#In our graph, the "degree" of a node is its number of friends.
mean(degree(g)) #average number of friends
table(degree(g) >= 10) #number of people who have more than 10 friends

V(g)$size = degree(g)/2+2 #To visually draw attention to people with more friends, we will change the vertex size 
plot(g, vertex.label=NA) #replot the graph
max(V(g)$size) #max size assigned to a vertex

#Change the color of the vertices
V(g)$color[V(g)$gender == "A"] = "red"
V(g)$color[V(g)$gender == "B"] = "gray"

#Change color based on school
V(g)$color = "black"
V(g)$color[V(g)$school == "A"] = "red"
V(g)$color[V(g)$school == "AB"] = "gray"
plot(g, vertex.label=NA)




