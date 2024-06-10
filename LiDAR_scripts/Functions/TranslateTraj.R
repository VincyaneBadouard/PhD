# Translate 

# Trajectory
traj <- fread("Strip_traj.txt",select = seq(1:4))

 
#GlobShift=matrix(data=c(1,0,0,-286608,0,1,0,-583966,0,0,1, -35,0,0,0,1), ncol=4, byrow=T) # Pour ramener les nuages autour de zéro             
GlobShift=diag(4)

ICPmat=as.matrix(read.table("ICP_OK.txt", header=F))

dat <- traj[,.(X,Y,Z)]
dat$C <- 1

S <- (as.matrix(dat) %*% t(GlobShift)) # Pour ramener les nuages autour de zéro
rm(dat)

T <- S %*% t(ICPmat) # pour appliquer la matrice de translation
rm(S)
F <- T %*% t(solve(GlobShift))
rm(T)
traj_cor = cbind(traj[,.(Time)],F[,1:3])
names(traj_cor) = c("Time","X","Y","Z")

fwrite(traj_cor, "Strip_traj_ICPCor.txt")