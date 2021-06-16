dir <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(dir);

#### Pagerank ####

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

#### BiRank - creating fraud detection features #### 

# Additional code to fit the Bipartite network
source("Gotcha.R")

# custom function to plot bigraphs
PlotGraph <- function(g, FrCl, FraudScores = NULL, PartyScores = NULL, 
                      PosLegend = "topleft", ...) {
  igraph::V(g)$color = sapply(igraph::V(g)$name, function(x) {
    if (x %in% FrCl) {
      "red"
    } else if (grepl("C", x)) {
      "green"
    } else {
      "grey"
    }
  })
  igraph::V(g)$shape = sapply(igraph::V(g)$name, function(x) {
    if (grepl("C", x)) {
      "circle"
    } else {
      "square"
    }
  })
  if(!all(sapply(list(FraudScores, PartyScores), is.null))) {
    igraph::V(g)$score = sapply(igraph::V(g)$name, function(x) {
      if (grepl("C", x)) {
        round(FraudScores[which(names(FraudScores) == x)], 2)
      } else {
        round(PartyScores[which(names(PartyScores) == x)], 2)
      }
    })
    igraph::V(g)$scoreCol = sapply(igraph::V(g)$name, function(x) {
      if (grepl("C", x)) {
        "black"
      } else {
        "darkgrey"
      }
    })
    set.seed(1)
    plot.igraph(g, vertex.label = igraph::V(g)$score, vertex.label.dist = 2, vertex.label.degree = 90,
                vertex.label.font = 2, vertex.label.cex = 1, vertex.label.color = igraph::V(g)$scoreCol, 
                vertex.color = "white",
                vertex.frame.color = "white", edge.color="white", edge.label = NA, ...)
    plot.igraph(g, edge.label = NA, edge.color = 'black', add = T,
                vertex.label = igraph::V(g)$name, vertex.color = igraph::V(g)$color, 
                vertex.label.color = 'black', main = "Social network",
                vertex.shape = igraph::V(g)$shape, ...)
  } else {
    plot.igraph(g, edge.label = NA, edge.color = 'black', 
                vertex.label = igraph::V(g)$name, vertex.color = igraph::V(g)$color, 
                vertex.label.color = 'black', main = "Social network",
                vertex.shape = igraph::V(g)$shape, ...)
  }
  legend(PosLegend, c("Unknown/ non-fraudulent claim", "Fraudulent claim", "Party (PH, broker, ...)"),
         pch = c(rep(21, 2), 22), col = "black", bty = "n", pt.bg = c("green", "red", "grey"),
         pt.cex = 2, y.intersp = 1.5)
}

edges = data.frame(
  startNode = c('P2', 'P3', 'P3', 'P2', 'P3', 'P3', 'P1', 'P4', 'P1', 'P4',
                'P5', 'P6', 'P7', 'P8', 'P9', 'P10', 'P5', 'P6', 'P8', 'P6', 'P5'),
  endNode = c('C3', 'C3', 'C4', 'C1', 'C1', 'C5', 'C1', 'C5', 'C2', 'C2',
              'C4', 'C4', 'C6', 'C7', 'C6', 'C8', 'C8', 'C7', 'C6', 'C3', 'C7'),
  stringsAsFactors = F
)

# create a graph based on a data frame of edges
graph = graph_from_data_frame(edges, directed = FALSE)

# mark these claims as fraudulent
fraudulent <- c('C4', 'C6', 'C7')

PlotGraph(graph, fraudulent)

# compute the distance between all nodes
distance_matrix <- distances(graph)

# neighbourhood features
n1.size <- rowSums(distance_matrix == 1)
n2.size <- rowSums(distance_matrix == 2)

matrix_column_product <- function(matrix, vector) {
  t(vector * t(matrix))
}

matrix_column_product(matrix(c(1,2,3,4), nrow = 2, ncol = 2),
                      c(50, 100))

n2.fraud <- rowSums(matrix_column_product(distance_matrix == 2, 
                                          colnames(distance_matrix) %in% fraudulent))


n2.RatioFraud <- n2.fraud / n2.size                    
n2.BinFraud <- n2.fraud > 0

neighbourhood_features <- data.frame(
  id = colnames(distance_matrix),
  n1.size = n1.size,
  n2.size = n2.size,
  n2.RatioFraud = n2.RatioFraud,
  n2.BinFraud = n2.BinFraud
) %>%
  filter(startsWith(id, 'C'))

#### Create score features ####

claims <- paste0('C', 1:8)
parties <- paste0('P', 1:10)

# edges have to be numeric for the PageRankB algorithm
edges_numeric <- edges %>%
  mutate(startNode = match(startNode, parties),
         endNode = match(endNode, claims))

# Bigraph pagerank implementation in the Gotcha file
score  = PageRankB(edges_numeric, 
                   data.frame(FraudInd = claims %in% fraudulent), 
                   QueryValueOne = F)

# reassign labels
score_named <- rbind(
  data.frame(arrange(score$ResultsClaims, ID), label = claims),
  data.frame(arrange(score$ResultsParties, ID), label = parties)
)

claim_score <- score_named$Score[match(claims, score_named$label)]
names(claim_score) <- claims

party_score <- score_named$Score[match(parties, score_named$label)]
names(party_score) <- parties

pos = layout_nicely(graph)
PlotGraph(graph, fraudulent, FraudScores = claim_score, PartyScores = party_score,
          PosLegend = "bottomleft", layout = pos)

# order score_named in the same order as the distance matrix
score_named <- score_named[match(colnames(distance_matrix), score_named$label), ]

score <- score_named$Score
n1.q1 <- apply(matrix_column_product(distance_matrix == 1, score),
               1,
               function(x) {quantile(x[x != 0], 0.25)})
n1.med <- apply(matrix_column_product(distance_matrix == 1, score),
               1,
               function(x) {median(x[x != 0])})
n1.max <- apply(matrix_column_product(distance_matrix == 1, score),
                1,
                function(x) {max(x[x != 0])})

n2.q1 <- apply(matrix_column_product(distance_matrix == 2, score),
               1,
               function(x) {quantile(x[x != 0], 0.25)})
n2.med <- apply(matrix_column_product(distance_matrix == 2, score),
                1,
                function(x) {median(x[x != 0])})
n2.max <- apply(matrix_column_product(distance_matrix == 2, score),
                1,
                function(x) {max(x[x != 0])})

score_features <- data.frame(
  id = colnames(distance_matrix),
  n1.q1 = n1.q1,
  n1.med = n1.med,
  n1.max = n1.max,
  n2.q1 = n2.q1,
  n2.med = n2.med,
  n2.max = n2.max
) %>%
  filter(startsWith(id, 'C'))
