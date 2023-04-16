
data <- read.csv("C:/Users/ideapad/Desktop/Assignment R Big Data/Datast_diabetes.csv")
View(data)
#plot 10
# Combine all of the graphics from 1-8 and display them as three plots per Column
par(mfrow=c(3,3))

#plot 1
# A scatterplot between “Insulin” and “Glucose”
plot(x = data$Insulin,y = data$Glucose,xlab = "Insulin Concentration",ylab = "Glucose",main = "Relation between Glucose and Insulin", col = "darkblue")

#plot 2
# Histogram of Glucose value in blood
hist(x = data$Glucose ,main= "Glucose Value In Blood",xlab = "Glucose Value",xlim = c(50,150),breaks = 20,col = "darkblue",border = "black")

#plot 3
# Visualize “BloodPressure” Variable and show the outliers in the BloodPressure vector
boxplot(data$BloodPressure, data = data, notch = FALSE, varwidth = FALSE, xlab = "Blood Pressure",ylab = "Frequency", main ="Blood Pressure Measurements")

#plot 4
# A bar plot of the maximum “Insulin” dose grouped per age of patient.
max_insulin <- aggregate(data$Insulin, by = list(data$Age), FUN = max)
barplot(max_insulin$x, names.arg = max_insulin$Group.1,xlab = "Age",ylab = "Maximum Insulin Dose",main = "Maximum Insulin Dose by Age")

#plot 5
# A table for the “Pregnancies” attribute, then create the bar plot
pregnancies_table <- table(data$Pregnancies)
barplot(pregnancies_table, main="Pregnancies", xlab="Number of Pregnancies", ylab="Count")

#plot 6
insulin_summary <- c(mean = mean(data$Insulin), median = median(data$Insulin), min = min(data$Insulin), max = max(data$Insulin))
# Mean = 79.8
# Median = 30.5
# Min = 0
# Max = 846
labels <- c(paste("Mean:", insulin_summary[1]),paste("Median:", insulin_summary[2]),paste("Man:", insulin_summary[3]),paste("Max:", insulin_summary[4]))
pie(insulin_summary, labels)

#plot 7
# Create the density plot for Diabetes Pedigree Function
plot(density(data$DiabetesPedigreeFunction), main = "Diabetes Pedigree Function Distribution" , col = "blue")
# Create the density plot for Age
plot(density(data$Age), main = "Age Distribution", col ="red")

# Create a new plot window
#plot.new()

# Create the density plot for Diabetes Pedigree Function
#plot(density(data$DiabetesPedigreeFunction), main = "Density Plots for Diabetes Pedigree Function and Age")
#lines(data$DiabetesPedigreeFunction, col = "blue", lwd = 2)

# Create the density plot for Age
#lines(density(data$Age, adjust=2), col = "red", lwd = 2)
#legend("topright", legend = c("Diabetes Pedigree Function", "Age"), col = c("blue", "red"), lty = 1, lwd = 2)

#plot 8
# Visualize using dot plot the “DiabetesPedigreeFunction” per “outcome”
# Create a vector of colors
colors <- ifelse(data$Outcome == 1, "red", "darkblue")

# Create a dot chart
dotchart(data$DiabetesPedigreeFunction, 
         xlab = "Diabetes Pedigree Function per Diabetes outcome", 
         main = "Diabetes Pedigree Function per Diabetes Outcome", 
         col = colors)
#plot 9
#Plot the pairwise relationships
sample_data <- data.frame(data$Insulin, data$Age, data$Glucose, data$BloodPressure,data$DiabetesPedigreeFunction)

#create pairs plot 
pairs( sample_data )