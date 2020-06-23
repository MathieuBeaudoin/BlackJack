rm(list=ls())
MATRIX_SIDE <- 31
PAYOFF <- list(win=1, draw=-1, loss=-1)

# Returns the number of points for a given hand; can also update hand code (optional)
computeResult <- function(code=NULL, newcard=NULL, aces=0, sumOthers=0){
  # Can either supply aces with sumOthers, or code with new card value
  # Valid values for newcard: integer in {2,3,...,10} or "A"
  if(!is.null(code)) {
    if(code==21 | code==22) return(code)
    aces <- as.integer(substr(code,1,1)) # Extract number of aces from result code
    sumOthers <- as.integer(substr(code,3,nchar(code))) # Extract total from other cards
  }
  if(!is.null(newcard)) {
    if(newcard=="A") aces <- aces + 1
    else sumOthers <- sumOthers + as.integer(newcard)
  }
  result <- sumOthers + 11*aces # Compute gross result
  # Discount any available aces if result exceeds 21
  while(result > 21 & aces > 0) {
    aces <- aces - 1
    sumOthers <- sumOthers + 1
    result <- sumOthers + 11 * aces
  }
  if(result > 22) result <- 22
  # If the newcard parameter is supplied, also returns updated hand code
  if(!is.null(newcard)) {
    newcode <- paste0(aces, "A", sumOthers)
    return(c(result, newcode))
  }
  return(result)
}

# Returns hand code using matrix row index
reverseIndex <- function(i) {
  if(i == MATRIX_SIDE) return(22)
  else if(i == MATRIX_SIDE - 1) return(21)
  else if(i < MATRIX_SIDE - 1 & i > 19) return(paste0("1A", i-20))
  else if(i > 0 & i < 20) return(paste0("0A", i+1))
  else return("error")
}

# Evaluates whether a hand code represents a valid starting croupier state (single card hands)
validCroupierState <- function(code) {
  return((as.integer(substr(code, 1, 1)) == 0 &
            as.integer(substr(code, 3, nchar(code))) <= 10) |
           (as.integer(substr(code, 1, 1)) == 1 &
              as.integer(substr(code, 3, nchar(code))) == 0))
}

# Returns a vector of hand codes
applyIndex <- function() as.vector(sapply(1:MATRIX_SIDE, reverseIndex))

# Returns number of high aces, total from other cards and total score, using hand code
parseState <- function(code) {
  if(code %in% c(21,22)) return(list(aces=0, others=code, score=code))
  list(aces=as.integer(substr(code,1,1)),
       others=as.integer(substr(code,3,nchar(code))),
       score=computeResult(code=code))
}

# Returns the probability of a given value turning up (with replacement of cards... no counting)
Pr <- function(card) {
  if((card > 1 & card < 10) | card %in% c("A",11)) return(1/13) # Low cards, aces
  else if(card==10) return(4/13) # 10, J, Q, K
  else return(0)
}

# Computes transition probabilities
computeProbIJ <- function(i, j, absorbingStates) {
  IS <- reverseIndex(i) # Initial state
  FS <- reverseIndex(j) # Final state
  if(IS %in% absorbingStates) return(ifelse(IS == FS, 1, 0))
  else {
    init <- parseState(IS)
    final <- parseState(FS)
    # Dealing first with 21's and busts
    if(FS == 21) {
      mixed <- init$aces == 1 | init$others == 10
      final$aces <- ifelse(mixed, 1, 0)
      final$others <- ifelse(mixed, 10, 21)
    } else if(FS == 22) final$others <- 22
    if(init$aces == 1 & final$aces == 0) {
      # High ace converted ==> final$others = init$others + new card + 1
      delta <- final$others - init$others - 1
      if(21 - init$score < delta & delta < 11) {
        # Make sure we actually need to convert the high ace...
        return(Pr(delta))
      } else return(0)
    }
    else {
      # No high ace conversion
      if(final$score == 22 & init$others >= 12) {
        # Several possibilities can produce this outcome (bust)
        possibleRange <- (22-init$score):10
        return(sum(sapply(possibleRange, Pr)))
      }
      else {
        # First we deal with special cases
        if(init$aces == 0 & ((final$others - init$others == 11) |
                             (final$aces == 1 & final$others != init$others))) return(0)
        if((init$others > 10 | init$aces == 1) & final$others == init$others + 1) return(Pr("A"))
        # Default:
        return(Pr(final$score - init$score))
      }
    }
  }
}

# Generates individual transition matrices
genTransitionMatrix <- function(absorbingStates, lines=1:MATRIX_SIDE) {
  absorbingStates <- unique(c(absorbingStates, 21, 22))
  t(sapply(lines, function(i) {
    sapply(lines, function(j) computeProbIJ(i, j, absorbingStates))
  }))
}

# Generates the player's and the croupier's transition matrices
genMatrices <- function(absJoueur=NULL){
  playerMatrix <- genTransitionMatrix(absJoueur)
  colnames(playerMatrix) <- rownames(playerMatrix) <- states <- applyIndex()
  croupierAbsorbingStates <- c(paste0("0A", 17:20), paste0("1A", 6:9))
  croupierMatrix <- genTransitionMatrix(croupierAbsorbingStates) 
  # The following transforms one-turn transition probabilities into full-hand T/P's
  for(i in 1:4) croupierMatrix <- croupierMatrix %*% croupierMatrix
  colnames(croupierMatrix) <- rownames(croupierMatrix) <- states
  return(list(player=playerMatrix, croupier=croupierMatrix))
}

# Computes the probability of the croupier's hand ending in each possible final state,
# for each card they can start with
probasCroupier <- function(){
  k <- MATRIX_SIDE
  croupierProbs <- matrix(rep(0,(k-2)*6), byrow=T, nrow=(k-2))
  colnames(croupierProbs) <- c(17:22)
  rownames(croupierProbs) <- as.vector(sapply(1:(k-2), reverseIndex))
  # For each possible final state, sum up the transition probabilities from each initial state
  for(i in 17:22){
    targetCols <- as.vector(sapply(colnames(transMatrices$croupier), computeResult)) == i
    subMatrix <- transMatrices$croupier[-c(k-1,k), targetCols]
    if(is.null(dim(subMatrix))) croupierProbs[, i-16] <- subMatrix
    else croupierProbs[, i-16] <- rowSums(subMatrix)
  }
  return(croupierProbs)
}

# Computes the expectancy of the player's gain if they stand at a given state
gainExpectancyNoHit <- function(playerState, croupierState) {
  playerScore <- computeResult(code=playerState)
  if(playerScore>21) return(PAYOFF$loss) # Player busts
  croupierOutcomes <- croupierResultProbs[croupierState, ]
  # Outcome probabilities (from player's POV)
  winProb <- sum(croupierOutcomes[as.numeric(names(croupierOutcomes)) < playerScore]) + 
    croupierOutcomes["22"]
  drawProb <- ifelse(playerScore < 17, 
                     0, croupierOutcomes[names(croupierOutcomes) == playerScore])
  lossProb <- 1 - winProb - drawProb
  # Expectancy = sum[for all k]{ [payoff for outcome k] * [probability of outcome k]}
  return(as.numeric(PAYOFF$win * winProb + PAYOFF$draw * drawProb + PAYOFF$loss * lossProb))
}

# Computes all static expectancies of the player's gain; return a matrix
gainExpectanciesMatrix <- function() {
  states <- applyIndex()
  initCroupierStates <- states[sapply(states, validCroupierState)] # Keep only single-card hands
  expMatrix <- t(as.matrix(sapply(1:MATRIX_SIDE, function(i) {
    as.vector(sapply(1:length(initCroupierStates), function(j) {
      gainExpectancyNoHit(states[i], initCroupierStates[j])
    }))
  })))
  colnames(expMatrix) <- initCroupierStates
  rownames(expMatrix) <- states
  return(expMatrix)
}

# Computes the expectancy of the player's gain if they hit at a given state
gainExpectancyWithHit <- function(playerState, croupierState, 
                                  transitionMatrix=transMatrices$player, 
                                  expectancyMatrix=gainExpectancies) {
  probsRow <- transitionMatrix[playerState, ]
  expectancies <- expectancyMatrix[, croupierState]
  return(sum(probsRow * expectancies))
}

# Evaluates recursively whether the player's gain expectancy would improve by hitting
recursion <- function() {
  states <- applyIndex()
  decisionMatrix <- matrix(nrow=length(states)-2, ncol=dim(gainExpectancies)[2])
  rownames(decisionMatrix) <- states[-which(states %in% c(21,22))]
  colnames(decisionMatrix) <- croupierInitStates <- colnames(gainExpectancies)
  adjustedExpectancies <- gainExpectancies
  recursionOrder <- c(paste0("0A",20:10), paste0("1A",9:0), paste0("0A",9:2))
  for(rO in recursionOrder) {
    for(cIS in croupierInitStates) {
      hitExpectancy <- gainExpectancyWithHit(rO, cIS, expectancyMatrix=adjustedExpectancies)
      if(hitExpectancy >= adjustedExpectancies[rO, cIS]) {
        decisionMatrix[rO, cIS] <- 1  
        adjustedExpectancies[rO, cIS] <- hitExpectancy
      } else decisionMatrix[rO, cIS] <- 0
    }
  }
  return(decisionMatrix)
}

transMatrices <- genMatrices()
croupierResultProbs <- probasCroupier()
gainExpectancies <- gainExpectanciesMatrix()
DECISION_MATRIX <- recursion()
write.csv(DECISION_MATRIX, file="decision_matrix.csv")
DECISION_MATRIX