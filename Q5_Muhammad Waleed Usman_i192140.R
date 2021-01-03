

# Question 5. Which BS program is a popular choice for admission (campus wise analysis required)?


# BS program reference for the Admission in NUCES
Degree_preference = data.frame(Deg= admission$Ist...9, stringsAsFactors = FALSE)

# Campus reference for the Admission in NUCES
Campus_preference = data.frame(Camp= admission$Ist...5, stringsAsFactors = FALSE)
# BS program and Campus reference for the Admission in NUCES
Campus_and_Degree  = data.frame(Camp=Campus_preference,Deg=Degree_preference)

# Aggregate table for the MOst popular BS program campus wise in the NUCES
Table_Aggregating_total_prefrences = data.frame(aggregate(numeric(nrow(Campus_and_Degree)), Campus_and_Degree[c("Camp", "Deg")], length))


# PLotting the Histogram
hist(Table_Aggregating_total_prefrences)

# Sorting the Table by aggregate
Sort_Table_Aggregate <- Table_Aggregating_total_prefrences[with(Table_Aggregating_total_prefrences, order(x,decreasing = TRUE)),]
View(Sort_Table_Aggregate)
