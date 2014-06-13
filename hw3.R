#problem 2
library(plot3D)
data = read.table(file="/Users/eotles/Downloads/PCA Service.csv",sep=",");
cleanData = data[,2:11]

pc = princomp(cleanData, cor=FALSE, center=TRUE, scale=TRUE)
pc$loading
pc$scores
plot(pc)

mat_2 = pc$loading[,1:2]
t(mat_2) %*% mat_2
plot(pc$scores[,1],pc$scores[,2])

scatter3D(pc$scores[,1],pc$scores[,2],pc$scores[,3])
