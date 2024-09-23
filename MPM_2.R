#### PARAMETERS #####
name_1 = 'data.csv' # <<<<< CHANGE INPUT FILE HERE 
#################

file_readed = read.csv(name_1, header=TRUE) # <<<<< FUNCTION TO READ 

S = (ncol(file_readed)-2)/2
Etapas = colnames(file_readed)
Stages = Etapas[2:(2+S-1)]
A = data.matrix(file_readed)
time_units = A[,1]
Eggs = A[,(S+2)]
Nu = A[,2:(S+1)]
D = A[,(S+3):ncol(A)]

M = matrix(c(rep(0,length(time_units)*S)),ncol=S)

for (t in 1:(length(time_units)-1)){
  M[t+1,1] = Nu[t,1]-D[(t+1),1]-Nu[(t+1),1]
}

for (s in 2:S){
  for (t in 1:(length(time_units)-1)){
    M[t+1,s] = Nu[t,s]+M[(t+1),s-1]-D[(t+1),s]-Nu[(t+1),s]
  }
}

NN = colSums(Nu)
Dead = colSums(D)
Graduated = colSums(M)

G = Graduated/NN
R = Dead/NN
P = 1-G-R
U = diag(P)

for (i in 1:(S-1)){
  U[(i+1),i] = G[i]
}

fertility = sum(Eggs)/tail(NN,1)

FF = matrix(c(rep(0,length(U))),ncol=S)
FF[1,S] = fertility

# ************ STARTING CALCULATIONS ***********

###### EXPECTED VALUES
N=solve(diag(1,S)-U)
lambda = Re(eigen(U+FF)$values[1])
r = log(lambda)
L = colSums(N)[1] # 1'Ne1
R0 = colSums(FF%*%N)[1] # 1'FNe1
T = log(R0)/r
Ex_vls = c(lambda,r,R0,T,L)
print(Ex_vls)

# Extra
print(U)
print(FF)
A = U+F
print(A)
print(N)
