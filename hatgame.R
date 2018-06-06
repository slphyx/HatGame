##hatgame ?
#saralamba@gmail.com

require("igraph")

#generate network
randomNetwork <- function(npop,R0){
  g <- sample_gnp(npop, R0/npop)
  return(g)
}


# this will return the state vector
addInfectedNode <- function(g,npop=NULL,indx=NULL){
  #sensitive =0; infected = 1;
  if(is.null(npop)){
    npop <- length(ego_size(g))
  }
  if(is.null(indx)){
    indx <- sample.int(npop,1)
  }
  st <- rep(0,npop)
  st[indx] <- 1;
  return(st)
}

#check if its neighbors are infected
is.neighbors.infected <- function(g, indx, state){
  nb <- neighbors(g, indx)
  nb.state <- state[nb]
  if(sum(nb.state==1) > 0)
    return(TRUE)
  else
    return(FALSE)
}


# update the state based on its neighbors
updateState <- function(g, state){
  npop <- length(ego_size(g))
  if(npop != length(state)){
    stop("length(state) must be equal npop")
  }
  nstate <- state
  for(i in 1:npop){
    if(state[i]==0 && is.neighbors.infected(g,i,state)){
      nstate[i] <- 1
    }
    if(state[i]==1){
      nstate[i] <- 2
    }
  }
  return(nstate)
}

#give the color for each state
stateColors <- function(state){
  #sensitive = "green", infected = "red", recovered = "blue"
  state.cols <- rep(NA,length(state))
  for(i in 1:length(state)){
    if(state[i]==0){
      state.cols[i] <- "green"
    }
    if(state[i]==1){
      state.cols[i] <- "red"
    }
    if(state[i]==2){
      state.cols[i] <- "blue"
    }
  }
  return(state.cols)
}

#plot contact network
plotNetwork <- function(g, state, layout=NULL){
  state.cols <- stateColors(state)
  
  if(!is.null(layout)){
    plot(g, vertex.color=state.cols, layout=layout)
  }
  else
  {
    plot(g, vertex.color=state.cols)
  }
}

#count each state
stateCount <- function(state){
  s <- sum(state==0)
  i <- sum(state==1)
  r <- sum(state==2)
  
  return(c(s,i,r))
}

#plotting the infected cases
plotInfected <- function(df){
  n.points <- nrow(df)
  if(n.points > 2){
    plot(df, col = "red", type = 'l', xlab = "time", ylab = "infected cases")
  }else{
    NULL
  }
  
}

