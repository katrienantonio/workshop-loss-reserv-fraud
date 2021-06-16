########################################################################################
# GOTCHA! Propagation Algorithm
#
# Mainly based on R-code of V?ronique Van Vlasselaer
#
# Use Personalised PageRank algorithm to calculate fraud exposure score of each node in the network
########################################################################################

#### 0. Inner functions ####

#### 0.1 Adjacency matrix ####
AdjMat <- function(Network, decay = 0, Today = Sys.Date()) {
  aMat = 
    if (decay != 0) {
      w        = exp(-decay * as.numeric(difftime(Today, Network$Date, units = "weeks")))
      w[w > 1] = 1
      sparseMatrix(i = Network$startNode,
                   j = Network$endNode,
                   x = w)
    } else {
      sparseMatrix(i = Network$startNode,
                   j = Network$endNode,
                   x = 1)
    }
  return(aMat)
}

#### 0.2 Symmetrically normalized matrix ####
SNMM   <- function(aMat) {
  Dp = sparseMatrix(1:nrow(aMat), 1:nrow(aMat), x = 1 / sqrt(rowSums(aMat)))
  Dc = sparseMatrix(1:ncol(aMat), 1:ncol(aMat), x = 1 / sqrt(colSums(aMat)))
  S  = t(Dp %*% aMat %*% Dc)
  return(S)
}

#### 0.3 Query vector ####
QueryVector <- function(fraudMat, decay, Today, QueryValueOne = F) {
  c0 = 
    if(decay != 0) {
      Tmp = exp(-decay * as.numeric(difftime(Today, fraudMat$Date, units = "weeks"))) * fraudMat$FraudInd
      Tmp = 
        if(sum(Tmp) > 1e-14 & !QueryValueOne) {
          Tmp / sum(Tmp)
        } else {
          Tmp
        }
    } else {
      if(!QueryValueOne) {
        fraudMat$FraudInd / sum(fraudMat$FraudInd) 
      } else {
        fraudMat$FraudInd
      }
    }
  return(c0)
}


########################################################################################
# PersonalizedPageRank
# Computes the PageRank (i.e. fraud score) of each of the non-claims and claims
# Input:
#      - sNetwork: data frame with 2/3 columns: from - to - date (if 3 columns it will be time-weighted)
#      - fraudMat: matrix indicating which entries are fraud with 1/2 columns: fraud (0/1) - date (if 2 columns fraud will be time-weighted)
#      - Today: date of analysis (e.g. 1/1/2011)
#      - decayR: parameter for exponential decay of recency of relation (in weeks)
#      - decayF: parameter for exponential decay of recency of fraud (in weeks)
#      - damping: damping factor for propagation algorithm (return to start)
#      - maxiter: maximum number of iterations for propagation algorithm
#      - bipartite: bipartite graph (sNetwork "from" contains node type 1, "to" contains node type 2)
#      - fraudDim: if bipartite, it defines which node type is associated to fraud (1) for rows - default (2) for columns
#      - toExclude: vector indicating which nonclaims/claims to exclude from the network
# Output:
#      - exposure score of each element in matrix

##### 1. Pagerank algorithm ####
PageRankB <- function(sNetwork, fraudMat, Today = Sys.Date(), decayR = 0, decayF = 0, 
                                 alpha = 0.85, maxiter = 1e3, Epsilon = 1e-14, QueryValueOne = F, 
                                 PrintProgress = F, pInit = NULL, cInit = NULL, ConvCriterion = c("Sep", "Whole", "Order")) {
  if(!all(c("lubridate", "Matrix") %in% names(sessionInfo()$otherPkgs)))
    lapply(c("lubridate", "Matrix"), require, character.only = T, quiet = T)
  if(!is.null(pInit) | !is.null(cInit)) {
    if(any(!sapply(c(pInit, cInit), is.vector)))
      stop("Provide vectors for pInit/cInit.")
  }
  ConvCriterion = match.arg(ConvCriterion)
  ConvCriterion = 
    if(ConvCriterion == "Sep") {
      function(c, p, cOld, pOld, Epsilon = Epsilon) {
        as.vector(sqrt(crossprod(c - cOld)) / sqrt(crossprod(cOld))) < Epsilon &
        as.vector(sqrt(crossprod(p - pOld)) / sqrt(crossprod(pOld))) < Epsilon
      }
    } else if(ConvCriterion == "Whole") {
      function(c, p, cOld, pOld, Epsilon = Epsilon) {
        xi    = as.vector(rbind(c, p))
        xiOld = as.vector(rbind(cOld, pOld))
        as.vector(sqrt(crossprod(xi - xiOld)) / sqrt(crossprod(xiOld))) < Epsilon
      }
    } else {
      function(c, p, cOld, pOld, Epsilon = Epsilon) {
        as.vector(sqrt(crossprod(c - cOld)) / sqrt(crossprod(cOld))) < Epsilon &
        as.vector(sqrt(crossprod(p - pOld)) / sqrt(crossprod(pOld))) < Epsilon &
        crossprod(order(c) - order(cOld)) == 0 & crossprod(order(p) - order(pOld)) == 0
      }
    }
  
  #### 0. Inner functions ####
  
  #### 0.1 Adjacency matrix ####
  AdjMat <- function(Network, decay = 0, Today = Sys.Date()) {
    aMat = 
      if (decay != 0) {
        w        = exp(-decay * as.numeric(difftime(Today, Network$Date, units = "weeks")))
        w[w > 1] = 1
        sparseMatrix(i = Network$startNode,
                     j = Network$endNode,
                     x = w)
      } else {
        sparseMatrix(i = Network$startNode,
                     j = Network$endNode,
                     x = 1)
      }
    return(aMat)
  }
  
  #### 0.2 Symmetrically normalized matrix ####
  SNMM   <- function(aMat) {
    Dp = sparseMatrix(1:nrow(aMat), 1:nrow(aMat), x = 1 / sqrt(rowSums(aMat)))
    Dc = sparseMatrix(1:ncol(aMat), 1:ncol(aMat), x = 1 / sqrt(colSums(aMat)))
    S  = t(Dp %*% aMat %*% Dc)
    return(S)
  }
  
  #### 0.3 Query vector ####
  QueryVector <- function(fraudMat, decay, Today, QueryValueOne = F) {
    c0 = 
      if(decay != 0) {
        Tmp = exp(-decay * as.numeric(difftime(Today, fraudMat$Date, units = "weeks"))) * fraudMat$FraudInd
        Tmp = 
          if(sum(Tmp) > 1e-14 & !QueryValueOne) {
            Tmp / sum(Tmp)
          } else {
            Tmp
          }
      } else {
        if(!QueryValueOne) {
          fraudMat$FraudInd / sum(fraudMat$FraudInd) 
        } else {
          fraudMat$FraudInd
        }
      }
    return(c0)
  }
  
  
  #### 1. Function ####
  
  #### 1. Preparations ####
  sNetwork$Date = 
    if (decayR != 0 & !is.Date(sNetwork$Date)) {
      as.Date(sNetwork$Date, format = "%d/%m/%Y")
    } else if(decayR == 0) {
      1
    }
  if(any(colnames(fraudMat) == "Date")) {
    fraudMat$Date =
      if (decayF != 0 & !is.Date(fraudMat$Date)) {
        as.Date(fraudMat$Date, format = "%d/%m/%Y")
      } else if (decayF == 0) { 
        NULL
      }
  }
  
  cat("\n\nSetting up adjacency matrix.\n\n")
  aMat = AdjMat(sNetwork, decay = decayR, Today = Today)
  
  cat("\n\nNormalizing matrix.\n\n")
  S    = SNMM(aMat)
  
  cat("\n\nInitiating query vector.\n\n")
  c0   = QueryVector(fraudMat, decay = decayF, Today = Today, QueryValueOne = QueryValueOne)
  
  if(!is.null(pInit) | !is.null(cInit)) {
    if(length(pInit) != ncol(S) | length(cInit) != nrow(S))
      stop("Wrong length of pInit/cInit vectors")
  }
  
  pOld  = if(!is.null(pInit)) pInit else as.vector(runif(ncol(S)))
  cOld  = if(!is.null(cInit)) cInit else as.vector(runif(nrow(S)))
  Conv  = F
  iter  = 1
  
  cat("\n\nRunning algorithm.\n\n")
  while(!Conv) {
    c = alpha * (S %*% pOld) + (1 - alpha) * c0
    p = t(t(c) %*% S)
    if(ConvCriterion(c, p, cOld, pOld, Epsilon))
      break
    cOld = c
    pOld = p
    iter = iter + 1
    if(iter > maxiter) {
      warning("Maximum number of iterations has been reached.", immediate. = T)
      break
    }
    if(iter %% 1e2 == 0 & PrintProgress)
      cat("\n\nIteration number", iter, "\n\n")
  }
  
  c = as.vector(c)
  p = as.vector(p)
  
  ResultsClaims  = cbind.data.frame(ID          = seq_len(ncol(aMat)),
                                    Score       = c,
                                    StdScore    = scale(c),
                                    ScaledScore = (c - min(c)) / diff(range(c)))
  ResultsParties = cbind.data.frame(ID          = seq_len(nrow(aMat)),
                                    Score       = p,
                                    StdScore    = scale(p),
                                    ScaledScore = (p - min(p)) / diff(range(p)))
  ResultsClaims  = ResultsClaims[order(ResultsClaims$Score, decreasing = T), ]
  ResultsParties = ResultsParties[order(ResultsParties$Score, decreasing = T), ]
  
  Results = list(ResultsClaims   = ResultsClaims,
                 ResultsParties  = ResultsParties,
                 AdjacencyMatrix = aMat,
                 iter = iter,
                 Converged = iter < maxiter)
  class(Results) = "PageRankB"
  return(Results)
}

#### 2. Additional functions ####

print.PageRankB <- function(x) {
  cat("\n\nResults claims\n\n")
  print(head(x$ResultsClaims))
  cat("\n\nResults parties\n\n")
  print(head(x$ResultsParties))
}

SubSetClaims.PageRankB <- function(x, Nodes, BeginDate, NrClaims = 10L, 
                             ExcludeInvestClaims = T) {
  if(class(x) != "PageRankB")
    stop("Provide object of class PageRankB.")
  if(!all(x$ResultsClaims$ID %in% Nodes$node.id.claim))
    warning("Not all claim IDs found in Nodes database!!!", immediate. = T)
  if(!is.Date(BeginDate))
    stop("Please provide an object of type Date.")
  if(!is.integer(NrClaims))
    stop("Please provide an object of type integer.")
  
  if(!exists("eso_vec", envir = .GlobalEnv))
    eso_vec <- c("ClaimOutsideCoveragePeriod",
                 "ExaggerationDamage",
                 "FalseDeclarationClaim",
                 "FalseDeclarationSubscription",
                 "FictitiousClaim",
                 "IntentionalDamage",
                 "MoneyLaundering",
                 "MultipleInsurers")
  Nodes[, ':=' ( 
    FraudInd = 
      if(unique(label) == "claim") {
        as.numeric(Motivation_def %in% eso_vec)
      } else {
        rep(0, .N)
      },
    Date = 
      if(unique(label) == "claim") {
        as.Date(DSUV, format = '%d/%m/%Y')
      } else {
        rep(as.Date("01/01/1970", format = '%d/%m/%Y'), .N)
      },
    Type = 
      if(unique(label) == "claim") {
        claim_type
      } else {
        rep("", .N)
      }
  ), by = label]
  
  Claims = Nodes[Nodes$label == "claim"]
  NewCl  = Claims[Claims$Date >= BeginDate, get("technical_id")]
  Claims = Claims[Claims$technical_id %in% NewCl]
  if(ExcludeInvestClaims)
    Claims = Claims[Claims$Investigated_by_eso == 0]
  
  ResultsCl = x$ResultsClaims
  ResultsCl = ResultsCl[which(ResultsCl$ID %in% Claims$node.id.claim), ]
  
  ResultsCl = ResultsCl[seq_len(NrClaims), ]
  Claims    = Claims[Claims$node.id.claim %in% ResultsCl$ID]
  
  Results = list(ResultsClaims = ResultsCl,
                 ClaimsSubset  = Claims)
  class(Results) = "PageRankBSubSetClaims"
  return(Results)
}

SubSetParties.PageRankB <- function(x, Nodes, NrParties = 10L) {
  if(class(x) != "PageRankB")
    stop("Provide object of class PageRankB.")
  if(!all(x$ResultsParties$ID %in% Nodes$node.id.nonclaim))
    warning("Not all claim IDs found in Nodes database!!!", immediate. = T)
  if(!is.integer(NrParties))
    stop("Please provide an object of type integer.")

  RPA = x$ResultsParties
  RPA = RPA[seq_len(NrParties), ]
  IDP = RPA$ID
  
  PartiesSubSet = Nodes[Nodes$node.id.nonclaim %in% IDP]

  Results = list(ResultsParties = RPA,
                 PartiesSubset  = PartiesSubset)
  class(Results) = "PageRankBSubSetParties"
  return(Results)
}