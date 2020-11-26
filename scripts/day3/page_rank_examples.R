##############################################
#  Prep work
##############################################

library(igraph)
library(MASS)
library(expm)   

##############################################
#  A simple PageRank example
##############################################

## Example 1 - from the sheets

WebGraph <- data.frame(from = c('A', 'A', 'A','C', 'C', 'D', 'D'),
                       to = c('B','C','D','A','D','A', 'C'))
g <- graph_from_data_frame(WebGraph, directed = TRUE)
pos <- cbind(c(1, 2, 1, 2), c(2.5, 2, 0.5, 1))
plot.igraph(g, edge.label = NA, edge.color = 'black', layout = pos, 
            vertex.label = V(g)$name, vertex.color = 'turquoise', 
            vertex.label.color = 'black', vertex.size = 25)

# naive approach - ignores dangling node

M = matrix(c(0, 1/3, 1/3, 1/3, 0, 0, 0, 0, 1/2, 0, 0, 1/2, 1/2, 0, 1/2, 0), nrow = 4, ncol = 4)
M
Id = diag(4)
OneCol= matrix(c(1, 1, 1, 1), nrow=4, ncol=1)
d=0.85
R = (ginv(Id-d*M))%*%((1-d)/4*OneCol)
R
sum(R) # what's wrong in the approach?

# try to fix - fill in the blanks

M = matrix(c(___), nrow = 4, ncol = 4)
M
Id = diag(4)
OneCol= matrix(c(1, 1, 1, 1), nrow=4, ncol=1)
d=0.85
R = (ginv(Id-d*M))%*%((1-d)/4*OneCol)
R
sum(R)

page_rank(g, vids = V(g), directed = TRUE, damping = 0.85,
          personalized = NULL, weights = NULL)

## Example 2

WebGraph <- data.frame(from = c('A', 'A', 'A', 'B','C', 'C', 'D', 'D'),
                       to = c('B','C','D','D','A','D','A', 'C'))

g <- graph_from_data_frame(WebGraph, directed = TRUE)

pos <- cbind(c(1, 2, 1, 2), c(2.5, 2, 0.5, 1))

plot.igraph(g, edge.label = NA, edge.color = 'black', layout = pos, 
            vertex.label = V(g)$name, vertex.color = 'turquoise', 
            vertex.label.color = 'black', vertex.size = 25)

# algebraic PageRank solution

M <- matrix(c(0, 1/3, 1/3, 1/3, 0, 0, 0, 1, 1/2, 0, 0, 1/2, 1/2, 0, 1/2, 0), nrow = 4, ncol = 4)
M
Id <- diag(4)
Id
OneCol <- matrix(c(1, 1, 1, 1), nrow = 4, ncol = 1)
d <- 0.85 # damping factor
R <- (ginv(Id-d*M)) %*% ((1-d)/4*OneCol)
R

# with a built-in function

page_rank(g, vids = V(g), directed = TRUE, damping = 0.85,
          personalized = NULL, weights = NULL)
