#matrix(, nrow = 15, ncol = 10)
stateVec = c(x0, y0, Vx0, Vy0)

X1n0 = F %*% stateVec

P0 = rbind(c(1, 0, 0, 0), c(0, 1, 0, 0), c(0, 0, 1, 0), c(0, 0, 0, 1)) # suppose x and y are independent
P1n0 = F %*% P0 %*% t(F)
P1n0

K1 = P1n0 %*% t(H) / (H %*% P1n0 %*% t(H) + R)
K1[is.na(K1)] = 0
K1

Z1 = c(X_measure[1], Y_measure[1], Vx_measure[1], Vy_measure[1])
X1n1 = X1n0 + K1 %*% (Z1 - H%*%X1n0)
X1n1

#P1n1 = (diag(4) - K1%*%H) %*% P1n0
P1n1 = (diag(4) - K1%*%H) %*% P1n0 %*% t((diag(4) - K1%*%H)) + K1 %*% R %*% t(K1)
P1n1