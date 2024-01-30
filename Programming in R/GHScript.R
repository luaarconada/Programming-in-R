######### General Homework Script ############
library(ggplot2)
library(dplyr)
library(fdth) #para tablas de frecuencia
library(moments) #para skewness y kurtosis


########### Data Modifications ###########
data = read.csv("healthcare-dataset-stroke-data.csv", header = TRUE)
head(data)

#Remove NA's only in bmi column because is the only column with missing values
data$bmi = as.numeric(data$bmi) #Identificamos como numero
sum(is.na(data$bmi)) #Numero de NAs en bmi
colSums(is.na(data)) #Ver numero de NAs en cada columna
data1 = na.omit(data) #Quitamos los NA 
head(data1)

#Remove id column
data1 = select(data1, -id)
head(data1)

# Remove gender == Other because we have 1 observation
table(data1$gender)
data1 = filter(data1, gender!='Other')

#### QUITAR O NO QUITAR? PARA CLASIFICAR
# Remove smoking status because there are too many unknown
table(data1$smoking_status)
data1 = select(data1, -smoking_status)


# Rename some variables
data1 = rename(data1, AverageGlucose = avg_glucose_level, BMI = bmi)
head(data1)




########## Frequency tables ##############
#We use library(fdth) for the frequency tables
#f      = Frecuencia absoluta
#rf     = Frecuencia relativa
#rf(%)  = Frecuencia relativa porcentual
#cf     = Frecuencia acumulada
#cf(%)  = Frecuencia acumulada porcentual

### Age 
tabla_age = fdt(data1$age, k = 15)
print(tabla_age) #Tabla de frecuencia



# Average glucose level 
tabla_avglu = fdt(data1$AverageGlucose, k = 15)
print(tabla_avglu) #Tabla de frecuencia


# BMI 
tabla_avglu = fdt(data1$BMI, k = 15)
print(tabla_avglu) #Tabla de frecuencia


############ Measures of centrality, variability,... ############

# Age
data1 %>%
  summarise(Mean. = mean(age),
            Var. = var(age),
            Skew. = skewness(age),
            Kurt. = kurtosis(age),
            Min. = min(age),
            "1st Qu." = quantile(age, probs = .25),
            Median = median(age),
            "3st Qu." = quantile(age, probs = .75),
            Max. = max(age),) 

# Average Glucose Level
data1 %>%
  summarise(Mean. = mean(AverageGlucose),
            Var. = var(AverageGlucose),
            Skew. = skewness(AverageGlucose),
            Kurt. = kurtosis(AverageGlucose),
            Min. = min(AverageGlucose),
            "1st Qu." = quantile(AverageGlucose, probs = .25),
            Median = median(AverageGlucose),
            "3st Qu." = quantile(AverageGlucose, probs = .75),
            Max. = max(AverageGlucose)) 

# BMI
data1 %>%
  summarise(Mean. = mean(BMI),
            Var. = var(BMI),
            Skew. = skewness(BMI),
            Kurt. = kurtosis(BMI),
            Min. = min(BMI),
            "1st Qu." = quantile(BMI, probs = .25),
            Median = median(BMI),
            "3st Qu." = quantile(BMI, probs = .75),
            Max. = max(BMI),) 

############ Groups based on gender ############
dataMale = filter(data1, gender == "Male")
dataFemale = filter(data1, gender == "Female")

#Age 
data1 %>%
  group_by(gender) %>%
  summarise(Mean. = mean(age),
            Var. = var(age),
            Skew. = skewness(age),
            Kurt. = kurtosis(age),
            Min. = min(age),
            "1st Qu." = quantile(age, probs = .25),
            Median = median(age),
            "3st Qu." = quantile(age, probs = .75),
            Max. = max(age))


#Average Glucose
data1 %>%
  group_by(gender) %>%
  summarise(Mean. = mean(AverageGlucose),
            Var. = var(AverageGlucose),
            Skew. = skewness(AverageGlucose),
            Kurt. = kurtosis(AverageGlucose),
            Min. = min(AverageGlucose),
            "1st Qu." = quantile(AverageGlucose, probs = .25),
            Median = median(AverageGlucose),
            "3st Qu." = quantile(AverageGlucose, probs = .75),
            Max. = max(AverageGlucose))

#BMI
data1 %>%
  group_by(gender) %>%
  summarise(Mean. = mean(BMI),
            Var. = var(BMI),
            Skew. = skewness(BMI),
            Kurt. = kurtosis(BMI),
            Min. = min(BMI),
            "1st Qu." = quantile(BMI, probs = .25),
            Median = median(BMI),
            "3st Qu." = quantile(BMI, probs = .75),
            Max. = max(BMI))



############# Plots ##############

#Histogramas con densidad, si queremos quitar la densidad
#quitar geom_density y aes(y=..density..) para mostrar la frecuencia
# AGE
ggplot(data1, aes(age))+
  geom_histogram(aes(y = ..density..), color="darkturquoise", fill="darkslategray2", bins = 15)+
  geom_density(color="deepskyblue4")+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5))+
  labs(title = "Frequency of Age", x="Age", y="Frequency")

# Average Glucose level
ggplot(data1, aes(AverageGlucose))+
  geom_histogram(aes(y = ..density..),color="darkturquoise", fill="darkslategray2", bins = 15)+
  geom_density(color="deepskyblue4")+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5))+
  labs(title = "Frequency of Average Glucose", x="Average Glucose", y="Frequency")

# BMI
ggplot(data1, aes(BMI))+
  geom_histogram(aes(y = ..density..),color="darkturquoise", fill="darkslategray2", bins = 30)+
  geom_density(color="deepskyblue4")+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5))+
  labs(title = "Frequency of BMI", x="BMI", y="Frequency")


### QQ-plots (normal plots)
#Age
ggplot(data1, aes(sample = age))+
  stat_qq(color = "darkslategray2")+
  stat_qq_line(color = "deepskyblue4")+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5))+
  labs(title = "QQ-plot for age", x="", y = "")

#Average Glucose
ggplot(data1, aes(sample = AverageGlucose))+
  stat_qq(color = "darkslategray2")+
  stat_qq_line(color = "deepskyblue4")+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5))+
  labs(title = "QQ-plot for Average Glucose", x="", y = "")

#BMI
ggplot(data1, aes(sample = BMI))+
  stat_qq(color = "darkslategray2")+
  stat_qq_line(color = "deepskyblue4")+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5))+
  labs(title = "QQ-plot for BMI", x="", y = "")
# se asemeja a una normal porque se asemeja a la recta, lo unico que se puede detectar la presencia de heavy tails 

### Boxplots
#age
ggplot(data1, aes(age))+
  geom_boxplot(fill = "darkslategray2", color= "darkturquoise")+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5))+
  labs(title = "Boxplot for age", x="", y = "")

#Average Glucose
ggplot(data1, aes(AverageGlucose))+
  geom_boxplot(fill = "darkslategray2", color= "darkturquoise")+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5))+
  labs(title = "Boxplot for age", x="", y = "")

#Average Glucose
ggplot(data1, aes(BMI))+
  geom_boxplot(fill = "darkslategray2", color= "darkturquoise")+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5))+
  labs(title = "Boxplot for age", x="", y = "")
