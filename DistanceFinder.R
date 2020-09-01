#How many simulations to run?
NumberOfSims<-10000
#What is the size of the grid?
a<-10
b<-5
set.seed(123)  # set the seed for the random number generator - this makes sure the results are reproducible when we are debugging

#Declare a variable to keep track of the sum of the distances across all simulations
distance1<-numeric()
distance2<-numeric()

for (i in 1:NumberOfSims){
  # generate z1 (so generate it's x and y coordinates)
  z1<-cbind(runif(1,0,a),runif(1,0,b))
  # generate z2 (likewise)
  z2<-cbind(runif(1,0,a),runif(1,0,b))
  # Use Pythagoras' theorem to find the distance between them
  # z3<-cbind(runif(1,0,sqrt(a*b)),runif(1,0,sqrt(a*b)))
  # z4<-cbind(runif(1,0,sqrt(a*b)),runif(1,0,sqrt(a*b)))
  E<-rbind(z1,z2)
  # D<-rbind(z3,z4)
  distance1[i]<-dist(E,p=2)
  # distance2[i]<-dist(D,p=2)
  # Add the distance to the variable Distance
}

# Divide Distance by NumberOfSims to calculate the average distance we observed across all sims
Distance1<-sum(distance1)
# Distance2<-sum(distance2)
AverageDistance1<-mean(distance1)
# AverageDistance2<-mean(distance2)
hist(distance1)
# hist(distance2)
# Judge<-AverageDistance1>AverageDistance2
# report that number
# cat("\nOur estimate of the expected distance between the two points is ",AverageDistance1, "\nand", AverageDistance2)
# cat("\nE>D is", Judge )
# better still, keep each distance in a vector and then plot a histrogram of its distribution? Is the distribution from a family you recognize? (e.g. Normal, Poisson, Exponential,...?)
