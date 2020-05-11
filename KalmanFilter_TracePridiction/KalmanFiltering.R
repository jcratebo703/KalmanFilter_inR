t = 1
x0 = 0
y0 = 0
Vx0 = 1
Vy0 = 1
X_measure = c(x0, 1, 2, 3, 4)
Y_measure = c(y0, 40, 40, 40, 40)
Vx_measure = c(Vx0, 1, 1, 1, 1)
Vy_measure = c(Vy0, 0, 0, 0, 0)
H = rbind(c(1, 0, 0, 0), c(0, 1, 0, 0), c(0, 0, 1, 0), c(0, 0, 0, 1))
R = rbind(c(1, 0, 0, 0), c(0, 1, 0, 0), c(0, 0, 1, 0), c(0, 0, 0, 1)) #
F = rbind(c(1, 0, t, 0), c(0, 1, 0, t), c(0, 0, 1, 0), c(0, 0, 0, 1))
P0 = rbind(c(0.1, 0, 0, 0), c(0, 0.1, 0, 0), c(0, 0, 0.1, 0), c(0, 0, 0, 0.1)) # suppose x and y are independent
#P0 = rbind(c(0.01, 0, 0, 0), c(0, 0.01, 0, 0), c(0, 0, 0.01, 0), c(0, 0, 0, 0.01))
predictDf = data.frame(t=integer(),
                       x=double(),
                       y=double(),
                       Vx=double(),
                       Vy=double())
estimateDf = data.frame(t=integer(),
                         x=double(),
                         y=double(),
                         Vx=double(),
                         Vy=double())

Kg_vec = c()

##Kalman Functions
State_Ex = function(stateVec){ #X0n0
  F %*% stateVec #X1n0
}

Cov_Ex = function(Pc){ # Pc = P0n0
  F %*% Pc %*% t(F) #P1n0
}

Kalman_Gain = function(Pcp){ # Pcp = P1n0
  K = Pcp %*% t(H) / (H %*% Pcp %*% t(H) + R) #K1
  K[is.na(K)] = 0
  K
}

State_Up = function(Kalman, Measurement, Measure_Prediction){ #K1, Z1, X1n0
  Measure_Prediction + Kalman %*% (Measurement - H%*%Measure_Prediction) #X1n1
}

Cov_Up = function(Kalman, Cov_Predict){ #K1, P1n0
  #P1n1 = (diag(4) - K1%*%H) %*% P1n0
  I = diag(4)
  (I - Kalman%*%H) %*% Cov_Predict %*% t((I - Kalman%*%H)) + Kalman %*% R %*% t(Kalman) #P1n1
}


for(i in 1:(length(X_measure)-1)){
  
  #state 0
  if(i ==1){
    stateVec = c(X_measure[i], Y_measure[i], Vx_measure[i], Vy_measure[i])
    print(stateVec)
    predictDf = predictDf[0,]
    estimateDf = estimateDf[0,]
    Kg_vec = c()
    Pc = P0
  }
   
  
  
  ##State prediciton
  predictState = State_Ex(stateVec) 
  predictDf[nrow(predictDf)+1,] = c(i,predictState)
  print(predictState)
  
  ##Cov prediction
  predictCov = Cov_Ex(Pc) 
  print(predictCov)
  
  #state 1
  ##calculate kalman gain
  Kg = Kalman_Gain(predictCov)
  Kg_vec = c(Kg_vec, Kg[1,1])
  print(Kg)
  
  ##Update state
  measurement = c(X_measure[i+1], Y_measure[i+1], Vx_measure[i+1], Vy_measure[i+1])
  updateState = State_Up(Kg, measurement, predictState)
  estimateDf[nrow(estimateDf)+1,] = c(i,updateState)
  stateVec = updateState
  print(measurement)
  print(updateState)
  
  ##Update Cov
  Pc = Cov_Up(Kg, predictCov)
  print(Pc)
  
}

predictDf
estimateDf
Kg_vec
