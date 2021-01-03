
# Question 9. It was commonly believed that NU gives admission to students with at least 70% marks in F.Sc. Do you concur with this hypothesis based on the given data?
# Point to be noted: It is seen that the data in Intermediate is not correct because of the TOTAL is less than Obtained marks
# in most of the candidates rows so we cannot do hypothesis upon them beacuse the data is not accurate, I have checked the algorithm
# On the Matriculation data and the results are here with the sample size of 1000

# Marks are calculated Here
Matriculation_Percentages = c(as.numeric((Matriculation_Obtained_marks /Matriculation_Total_marks)*100))
Matriculation_Percentages[is.na(Matriculation_Percentages)] <- 0


# Taking the sample from the Marks
sample_result = sample(Matriculation_Percentages, 500, replace=FALSE)
print(sample_result)


# Sampling mean of the Candidates 
Sample_mean = mean(sample_result)
print(Sample_mean)

# Sampling Standard Deviation of the Candidates
Sample_standard_dev =sd(sample_result)
print(Sample_standard_dev)


# Matrix for the T test which contain the sample mean, sample stadard dev and the sample size of the 
sample_Matrix <- c(rnorm(500, mean = Sample_mean, sd = Sample_standard_dev))

# Performing the t test
t.test(sample_Matrix, mu = 70, alternative = "less",conf.level = 0.05)


# Calculating the t value
t = (Sample_mean-70)/(Sample_standard_dev/sqrt(500))

# Calculating the Critical value for the test
criticalValue = qt(0.05, 499,lower.tail = TRUE)

# Output matrix of the t test
Output_test = c(Calculated_Value=t,Critical_Value=criticalValue)
print(Output_test)

# Analyzing the result and providing the conclusion
if (criticalValue<0){
  if (t>criticalValue){
    print("As the calculated value is Greater than the critical value so We failed to reject Null Hypothesis")
  }
  else{
    print("We reject Null Hypothesis")
  }
} else {
  if (t<criticalValue){
    print("As the calculated value is less than the critical value so We failed to reject Null Hypothesis")
  }
  else{
    print("We reject Null Hypothesis")
  }
}

