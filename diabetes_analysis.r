
# import data set

DSP <- read.csv("SHDS_Assessment1_data.csv")

# inspect data set

head(DSP)
tail(DSP)
str(DSP)
summary(DSP)
nrow(DSP)
ncol(DSP)
is.null(DSP)
dim(DSP)
sapply(DSP, class)

is.na(DSP)
colSums(is.na(DSP))
# no missing value


# summary of numerical values
BMIsmr <- summary(DSP$BMI)
BMIsmr
BMIsd <- round(sd(DSP$BMI), 2)
BMIsd
BMIfrq <- table(DSP$BMI)
#BMImode <- sort(BMIfrq, decreasing = T)[1]
#BMImode


Agesmr <- summary(DSP$Age)
Agesmr
Agesd<- round(sd(DSP$Age), 2)
Agesd
Agefrq <- table(DSP$Age)
#Agemode <- sort(Agefrq, decreasing = T)[1]
#Agemode


# create frequency table for categorical data
table(DSP$Smoker)
DSP$Smoker <- factor(DSP$Smoker, levels = c(0,1), labels = c("non-smoker", "smoker"))
DSP$Smoker

table(DSP$Diabetes_012)
DSP$Diabetes_012 <- factor(DSP$Diabetes_012, levels=c(0,1), labels = c("no-diabetes", "diabetes"))
head(DSP$Diabetes_012, 15)

head(DSP)

table(DSP$HighChol)
DSP$HighChol <- factor(DSP$HighChol, levels = c(0,1), labels = c("no-highcol", "highcol"))

DSP$CholCheck <- factor(DSP$CholCheck, levels= c(0,1), labels = c("not-checked", "checked"))

DSP$Sex <- factor(DSP$Sex, levels= c(0,1), labels = c("female", "male"))

DSP$Stroke <- factor(DSP$Stroke, levels= c(0,1), labels = c("no-stroke", "yes"))

head(DSP)

table(DSP$Smoker)
smokerprop <- prop.table(table(DSP$Smoker)) * 100
round(smokerprop, 2)

Diabetesprop <- prop.table(table(DSP$Diabetes_012)) * 100
round(Diabetesprop, 2)

table(DSP$HighChol)
Highcolfreq <- prop.table(table(DSP$HighChol)) * 100
round(Highcolfreq, 2)

table(DSP$CholCheck)
CCfreq <- prop.table(table(DSP$CholCheck)) * 100
round(CCfreq, 2)

table(DSP$Stroke)
Strokefreq <- prop.table(table(DSP$Stroke)) * 100
round(Strokefreq, 2)

table(DSP$Sex)
Sexfreq <- prop.table(table(DSP$Sex)) * 100
round(Sexfreq, 2)

table(DSP$MentHlth)
prop.table(table(DSP$MentHlth))
sort(table(DSP$MentHlth), decreasing = T)

# Visualisations for the available data using ggplot2 function
SmokerplotSkel <- ggplot(data=DSP, aes(x=Smoker))
smokeplot1 <- SmokerplotSkel + geom_bar(aes(fill=Smoker)) +
ylab("Total number of Patients") + xlab("Smoking Status") + 
  ggtitle("Distribution oF Smoking Status") +
  theme(axis.title.x = element_text(color="DarkGreen", size=10), 
        axis.title.y = element_text(color="DarkGreen", size=10),
        axis.text.x = element_text(color="Red", size=7),
        axis.text.y = element_text(color="Red", size=7),
        
        legend.title = element_text(size = 10),
        legend.text = element_text(size= 7),
        legend.position = c(1,1),
        legend.justification = c(1,1),
        plot.title = element_text(color="DarkBlue", size = 15, 
                                  family = "Courier"))
smokeplot1

# plot for percentage of smokers to non smokers
Smoking_props <- data.frame(smokerprop)
Smoking_props
colnames(Smoking_props) <- c("Smoking_status", "percentage")
Smoking_props

# plot the percentage
Smokingplot <- ggplot(data=Smoking_props, aes(x= Smoking_status, y =percentage, fill=Smoking_status)) +
  geom_col(color="Black") + ylab("Percentage of total patients") +
  xlab("Smoking Status") + 
  ggtitle("Frequency of Smoking Status") +
  theme(axis.title.x = element_text(color="DarkGreen", size=10), 
        axis.title.y = element_text(color="DarkGreen", size=10),
        axis.text.x = element_text(color="Red", size=7),
        axis.text.y = element_text(color="Red", size=7),
        
        legend.title = element_text(size = 10),
        legend.text = element_text(size= 7),
        legend.position = c(1,1),
        legend.justification = c(1,1),
        plot.title = element_text(color="DarkBlue", size = 15, 
                                  family = "Courier"))

smokeplots <- smokeplot1 / Smokingplot


# plot for Diabetes


Diabetes_012plot <- ggplot(data=DSP, aes(x=Diabetes_012))
dplot <- Diabetes_012plot + geom_bar(aes(fill=Diabetes_012)) + 
  ylab("Total number of Patients") + xlab("Diabetes Status") + 
  ggtitle("Distribution oF Diabetes Status") +
  theme(axis.title.x = element_text(color="DarkGreen", size=10), 
        axis.title.y = element_text(color="DarkGreen", size=10),
        axis.text.x = element_text(color="Red", size=7),
        axis.text.y = element_text(color="Red", size=7),
        
        legend.title = element_text(size = 12),
        legend.text = element_text(size= 10),
        legend.position = c(1,1),
        legend.justification = c(1,1),
        plot.title = element_text(color="DarkBlue", size = 15, 
                                  family = "Courier"))

dplot

# to plot the percentage of Diabetes
dfD_props <- data.frame(Diabetesprop)
dfD_props
colnames(dfD_props) <- c("diabetes_status", "percentage")
dfD_props

dpplot <- ggplot(data=dfD_props, aes(x= diabetes_status, y =percentage, fill=diabetes_status)) +
                   geom_col(color="Black") + ylab("Percentage of total patients") +
  xlab("Diabetes Status") + 
  ggtitle("Frequency of Diabetes Status") +
  theme(axis.title.x = element_text(color="DarkGreen", size=10), 
        axis.title.y = element_text(color="DarkGreen", size=10),
        axis.text.x = element_text(color="Red", size=7),
        axis.text.y = element_text(color="Red", size=7),
        
        legend.title = element_text(size = 12),
        legend.text = element_text(size= 10),
        legend.position = c(1,1),
        legend.justification = c(1,1),
        plot.title = element_text(color="DarkBlue", size = 15, 
                                  family = "Courier"))
dpplot

#Add them together using patchwork package.
Diabetesplots <- dplot / dpplot
Diabetesplots

#plot for Age
Ageplot <- ggplot(data=DSP, aes(x=Age))
Ageplot + geom_histogram(binwidth = 10, fill= "Black", color="White") +
  ylab("Total number of patients") +
  xlab("Age Distribution") + 
  ggtitle("Age Distribution of Patient") +
  theme(axis.title.x = element_text(color="DarkGreen", size=15), 
        axis.title.y = element_text(color="DarkGreen", size=15),
        axis.text.x = element_text(color="Black", size=10),
        axis.text.y = element_text(color="Black", size=10),
        plot.title = element_text(color="DarkBlue", size = 15, 
                                  family = "Courier")) +
  scale_x_continuous(breaks = seq(0, 100, by=10))



Bmiplot <- ggplot(data=DSP, aes(x=BMI)) 
Bmiplot +  geom_histogram(binwidth = 10, fill= "Black", color="White") + 
  ylab("Total number of patients") +
  xlab("BMI Distribution") + 
  ggtitle("BMI Distribution of Patient") +
  theme(axis.title.x = element_text(color="DarkGreen", size=15), 
        axis.title.y = element_text(color="DarkGreen", size=15),
        axis.text.x = element_text(color="Black", size=10),
        axis.text.y = element_text(color="Black", size=10),
        plot.title = element_text(color="DarkBlue", size = 15, 
                                  family = "Courier"))


#diabbmi <- ggplot(data=DSP, aes(x=Diabetes_012, y=BMI, fill=Diabetes_012))
#diabbmi + geom_boxplot()

# proportion test to compare diabtes rate to 14%
table(DSP$Diabetes_012)
nrow(DSP)
prop.test(x=39977, n=253600, p=0.14)

# hypothesis test for BMI when mean = 28

t.test(x=DSP$BMI, mu=28)

#hypothesis test for Age = 55
t.test(x=DSP$Age, mu=55)


# testing for diabetes and smoking relationship

table(DSP$Smoker, DSP$Diabetes_012)
chisq.test(table(DSP$Smoker, DSP$Diabetes_012))



