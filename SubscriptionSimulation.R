#### Project: Subscription Intelligence
### Simulation of Markov chains

Actions = c("Nothing","Incentivize")
States = c("Satisfied Customer", "Dissatisfied Customer","Former Customer")

NothingTransitionMatrix = matrix(0,nrow = length(States),ncol = length(States))
#NothingTransitionMatrix[1,] = c(0.7,0.2,0.1) # Satisfied customer transition probabilities
#NothingTransitionMatrix[2,] = c(0.1,0.2,0.7) # Dissatisfied customer transition probabilities
#NothingTransitionMatrix[3,] = c(0.1,0,0.9) # Dissatisfied customer transition probabilities
#NothingReward = c(100,100,0)

IncentivizeTransitionMatrix = matrix(0,nrow = length(States),ncol = length(States))
#IncentivizeTransitionMatrix[1,] = c(0.7,0.2,0.1) # Satisfied customer transition probabilities
#IncentivizeTransitionMatrix[2,] = c(0.7,0.2,0.1) # Dissatisfied customer transition probabilities
#IncentivizeTransitionMatrix[3,] = c(0.1,0,0.9) # Dissatisfied customer transition probabilities
#IncentivizeReward = c(50,50,-50)

#NothingTransitionMatrix = matrix(0,nrow = length(States),ncol = length(States))
NothingTransitionMatrix[1,] = c(0.7,0.2,0.1) # Satisfied customer transition probabilities
NothingTransitionMatrix[2,] = c(0.1,0.2,0.7) # Dissatisfied customer transition probabilities
NothingTransitionMatrix[3,] = c(0.1,0,0.9) # Dissatisfied customer transition probabilities
NothingReward = c(100,100,0)

#IncentivizeTransitionMatrix = matrix(0,nrow = length(States),ncol = length(States))
IncentivizeTransitionMatrix[1,] = c(0.7,0.2,0.1) # Satisfied customer transition probabilities
IncentivizeTransitionMatrix[2,] = c(0.7,0.2,0.1) # Dissatisfied customer transition probabilities
IncentivizeTransitionMatrix[3,] = c(0.7,0.2,0.1) # Dissatisfied customer transition probabilities
IncentivizeReward = c(50,50,-50)

TransitionMatrix = list(Nothing = NothingTransitionMatrix, Incentivize = IncentivizeTransitionMatrix)
Reward = list(Nothing = NothingReward, Incentivize = IncentivizeReward)
RewardMat = cbind(NothingReward,IncentivizeReward)
rownames(RewardMat) = States

## VALUE ITERATION
# Step 0

V0 = rep(0,length(States))
V_Old <- V0
V_New <- rep(0,length(States))
discountrate = 0.1
discountfactor = exp(-discountrate)

# Step 1
i = 1
threshold <- 1
while(threshold > 0.0001) { 
  print(threshold)
  
  action0vec <- Reward[[1]] + discountfactor*(TransitionMatrix[[1]] %*% V_Old)
  action1vec <- Reward[[2]] + discountfactor*(TransitionMatrix[[2]] %*% V_Old)
  
  V_New <- pmax(action0vec, action1vec)
  
  policy <- V_New == action1vec
  
  m = min(V_New - V_Old)
  M = max(V_New - V_Old)
  
  if(m==0){ threshold <- 1
  } else{threshold <- (M - m)/m}
  
  V_Old <- V_New
  i = i +1
}
print(i)

## SARSA
# Step 0
epsilon = 0.05
learningRate = 0.3
discountFactor = 0.9

Q = matrix(0,ncol = length(States),nrow = length(Actions))
colnames(Q) = States
rownames(Q) = Actions
CurrentState = sample(3,1)
CurrentAction = sample(2,1)

iterations = 10000
plotfrequency = 100 
BestAction = matrix(0,ncol = length(States),nrow = iterations/plotfrequency)
colnames(BestAction) = States

for(i in 1:iterations){

NextState = sample(3,1,prob = TransitionMatrix[[CurrentAction]][CurrentState,])
NextAction = which.max(Q[,NextState])
if(runif(1,0,1)<epsilon){
  NextAction = sample(2,1)
}

Q[CurrentAction,CurrentState] = Q[CurrentAction,CurrentState] + 
                                learningRate*(Reward[[CurrentAction]][CurrentState] + 
                                discountFactor*Q[NextAction,NextState] -
                                Q[CurrentAction,CurrentState])
CurrentState = NextState
CurrentAction = NextAction

if(as.integer(i/plotfrequency)==i/plotfrequency){
BestAction[i/plotfrequency,] = apply(Q,2,which.max)
  }
}

## Plots
library(highcharter)

#plot(1:(iterations/plotfrequency),BestAction[,1])
hchart(BestAction, "scatter")




## Concurrent SARSA
epsilon = 0.4
learningRate = 0.7
discountFactor = 0.8
iterations = 500
agents = 100

Q = matrix(0,ncol = length(States),nrow = length(Actions))
colnames(Q) = States
rownames(Q) = Actions
CurrentStates = sample(3,agents,replace=TRUE)
CurrentActions = sample(2,agents,replace=TRUE)

plotfrequency = 10
BestAction = matrix(0,ncol = length(States),nrow = iterations/plotfrequency)
colnames(BestAction) = States
tempQ = array(0,dim = c(length(Actions),length(States),agents))

for(i in 1:iterations){
  NextActions = rep(0,agents)
  NextStates = rep(0,agents)
  for(a in 1:agents){
    NextStates[a]= sample(3,1,prob = TransitionMatrix[[CurrentActions[a]]][CurrentStates[a],])
  
  NextActions[a] = which.max(Q[,NextStates[a]])
  if(runif(1,0,1)<epsilon || i == 1){
    NextActions[a] = sample(2,1)
    }
  }
  
  
  #for(a in 1:length(CurrentActions)){
  #    Q[CurrentActions[a],CurrentStates[a]] = Q[CurrentActions[a],CurrentStates[a]] + 
  #      learningRate*(RewardMat[NextStates[a]] + 
  #                     discountFactor*Q[NextActions[a],NextStates[a]] -
  #                      Q[CurrentActions[a],CurrentStates[a]])
  #
  #}
  
  for(a in 1:agents){
      tempQ[CurrentActions[a],CurrentStates[a],a] = Q[CurrentActions[a],CurrentStates[a]] + 
        learningRate*(RewardMat[CurrentStates[a],CurrentActions[a]] + 
                       discountFactor*Q[NextActions[a],NextStates[a]] -
                        Q[CurrentActions[a],CurrentStates[a]])
  
  }
  avgtempQ = apply(tempQ,c(1,2),mean)
  Q = avgtempQ
  
  CurrentStates = NextStates
  CurrentActions = NextActions
  #learningRate = learningRate*0.9999
  
  if(as.integer(i/plotfrequency)==i/plotfrequency){
    BestAction[i/plotfrequency,] = apply(Q,2,which.max)
  }
  
}

## Plots
library(highcharter)

#plot(1:(iterations/plotfrequency),BestAction[,1])
hchart(BestAction, "scatter")
