#### Project: Subscription Intelligence
### Simulation of Markov chains

Actions = c("Nothing","Incentivize")
States = c("Satisfied Customer", "Dissatisfied Customer","Former Customer")

NothingTransitionMatrix = matrix(0,nrow = length(States),ncol = length(States))
NothingTransitionMatrix[1,] = c(0.7,0.2,0.1) # Satisfied customer transition probabilities
NothingTransitionMatrix[2,] = c(0.1,0.2,0.7) # Dissatisfied customer transition probabilities
NothingTransitionMatrix[3,] = c(0.1,0,0.9) # Dissatisfied customer transition probabilitie

NothingReward = c(100,100,0)

IncentivizeTransitionMatrix = matrix(0,nrow = length(States),ncol = length(States))
IncentivizeTransitionMatrix[1,] = c(0.7,0.2,0.1) # Satisfied customer transition probabilities
IncentivizeTransitionMatrix[2,] = c(0.7,0.2,0.1) # Dissatisfied customer transition probabilities
IncentivizeTransitionMatrix[3,] = c(0.1,0,0.9) # Dissatisfied customer transition probabilities

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
                                learningRate*(Reward[[NextAction]][NextState] + 
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
epsilon = 0.05
learningRate = 0.3
discountFactor = 0.9
iterations = 10000
agents = 2

Q = matrix(0,ncol = length(States),nrow = length(Actions))
colnames(Q) = States
rownames(Q) = Actions
CurrentStates = sample(3,agents)
CurrentActions = sample(2,agents)

plotfrequency = 100 
BestAction = matrix(0,ncol = length(States),nrow = iterations/plotfrequency)
colnames(BestAction) = States

for(i in 1:iterations){
  NextStates = sample(3,agents,prob = TransitionMatrix[[CurrentAction]][CurrentState,])
  NextActions = rep(0,lengths(agents))
  for(a in 1:agents){
  
  
  
  NextActions[a] = which.max(Q[,NextStates[a]])
  if(runif(1,0,1)<epsilon){
    NextActions[a] = sample(2,1)
    }
  }
  Q[CurrentActions,CurrentStates] = Q[CurrentActions,CurrentStates] + 
    learningRate*(RewardMat[NextStates] + 
                    discountFactor*Q[NextActions,NextStates] -
                    Q[CurrentActions,CurrentStates])
  CurrentStates = NextStates
  CurrentActions = NextActions
  
  if(as.integer(i/plotfrequency)==i/plotfrequency){
    BestAction[i/plotfrequency,] = apply(Q,2,which.max)
     }
  
}

## Plots
library(highcharter)

#plot(1:(iterations/plotfrequency),BestAction[,1])
hchart(BestAction, "scatter")
