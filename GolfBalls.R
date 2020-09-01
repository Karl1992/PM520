# some pseudocode for the golf ball problem
# it uses the maximum frequency as the test statistic

# Set your random number seed
set.seed(45)

# Set up some global variables
NumberOfGolfBalls<-486   # how many golf balls were sampled in the observed dataset
NumberOfSamples<-10000
HighestNumberSeen<-4

# Here's a function that returns a set of golf balls numbers from a uniform distribution
SampleGolfBalls<-function(HowMany,HighestNumber){
  SupposedPopulation<-1:HighestNumber
  Sample<-sample(SupposedPopulation,size=HowMany,replace=TRUE)
  return (Sample)
}

# Here's a test statistic to try - the number of balls with the most common number
TestStatistic1<-function(BallVector){
  t<-table(BallVector)  # produces a table of counts of the frequency with which each number is seen
  return (max(t))  # max() returns the maximum of the frequencies when applied to a table
}

# Here's some pseudocode for calculating the null distribution of your test statistic
NullTestStatisticValues<-rep(0,NumberOfSamples)  # a vector in which we can store the simulated values of the test statistic under the null hypothesis
for (i in 1:NumberOfSamples){
  ThisSample<-SampleGolfBalls(NumberOfGolfBalls,HighestNumberSeen)
  NullTestStatisticValues[i]<-TestStatistic1(ThisSample)
}


hist(NullTestStatisticValues)
abline(v=138,col='red',lwd=4)

# How likely was the observed value, or something more extreme than that, under the null hypothesis? This is known as the "p-value".
Count<-(NullTestStatisticValues>138)
cat("\n There are",sum(Count)," values greater than that seen in our observed dataset.")
cat("\nSo our p-value is: ",sum(Count)/NumberOfSamples)

