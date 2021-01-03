# Question 10. Administration was told that all applicants given admission in NU had scored more than 50% in NU test. Administration is skeptical about this claim. Can you accept or reject it based on the given data.

# Declaring the NU BBA_TEST_MARKS
NU_BBA_Test_Marks = c(as.numeric(admission$BBA))

# Declaring the NU BS_TEST_MARKS
NU_BS_Test_Marks = c(as.numeric(admission$BS...24))
#NUtestBBA[is.na(NUtestBBA)] <- 0
#NUtestBS[is.na(NUtestBS)] <- 0

# Declaring the BBA Dataframe
NU_Admission_BBA = data.frame(percentages=NU_BBA_Test_Marks,Nuadmission=NU_Admission_Offer)
Selected_candidates_BBA <- data.frame(ifelse((!is.na(NU_Admission_BBA$nuoffer))&(!is.na(NU_Admission_BBA$percentages)) , NU_Admission_BBA$percentages, NA),stringsAsFactors = FALSE)
Candidates_BBA=na.omit(Selected_candidates_BBA)
Candidates_BBA = list(Candidates_BBA)


# Declaring the BS Dataframe
NU_Admission_BS = data.frame(percentages=NU_BS_Test_Marks,Nuadmission=NU_Admission_Offer)
Selected_candidates_BS <- data.frame(ifelse((!is.na(NU_Admission_BS$nuoffer))&(!is.na(NU_Admission_BS$percentages)) , NU_Admission_BS$percentages, NA),stringsAsFactors = FALSE)
Candidates_BS=na.omit(Selected_candidates_BS)
Candidates_BS = list(Candidates_BS)



# Candidates who have passed BS and BBA exams both and appending them
Nu_Passedtest_DataFrame = append(Candidates_BBA[1],Candidates_BS[1])
Nu_Passedtest_DataFrame <- data.frame(Nutest=matrix(unlist(Nu_Passedtest_DataFrame)),stringsAsFactors = FALSE)



# Sample of the Candidates who have passed the NU test for BBA and BS both
Sample_candidates_passed_NUTEST = sample(Nu_Passedtest_DataFrame$Nutest, 500, replace=FALSE)



# Sampling the Candidates passed NU test
print(Sample_candidates_passed_NUTEST)


# Sampling mean of the Candidates who have passed the NU Test
Sample_mean_Candidates = mean(Sample_candidates_passed_NUTEST)
print(Sample_mean_Candidates)

# Sampling Standard Deviation of the Candidates who have passed the NU Test
Sample_Standard_Dev_Candidates =sd(Sample_candidates_passed_NUTEST)
print(Sample_Standard_Dev_Candidates)


# Matrix for the T test which contain the sample mean, sample stadard dev and the sample size of the 
matrix_for_ttest <- c(rnorm(500, mean = Sample_mean_Candidates, sd = Sample_Standard_Dev_Candidates))

t.test(matrix_for_ttest, mu = 50, alternative = "less",conf.level = 0.05)


# Calculating the t test value for the test for null hyposisis mean,sample size 200
t2 = (Sample_mean_Candidates-50)/(Sample_Standard_Dev_Candidates/sqrt(500))
print(t2)

# Calculating the critical value for the test for confidence level=0.05 and the degree of freedom 199
criticalValue2 = qt(0.05, 499,lower.tail = TRUE)
print(criticalValue2)
Output_test = c(Calculated_Value=t2,Critical_Value=criticalValue2)
print(Output_test)

# Analyzing the result and providing the conclusion
if (criticalValue2<0){
  if (t2>criticalValue2){
    print("As the calculated value is Greater than the critical value so We failed to reject Null Hypothesis")
  }
  else{
    print("As the calculated value is less than so We reject Null Hypothesis")
  }
} else {
  if (t2<criticalValue2){
    print("As the calculated value is less than the critical value so We failed to reject Null Hypothesis")
  }
  else{
    print("As the calculated value is greater than so We reject Null Hypothesis")
  }
}














