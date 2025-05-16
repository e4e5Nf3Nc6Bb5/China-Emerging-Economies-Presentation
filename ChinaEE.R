rm(list=ls())
library(dplyr)
library(tidyverse)
library(readr)
library(ggplot2)
getwd()
setwd("/home/jovyan/data")

EducationSpending <- read.csv("~/data/ChinaEducationSpending.csv")
GNI <- read.csv("~/data/ChinaGNI.csv")
InterestRate <- read.csv("~/data/ChinaDepositInterestRate.csv")
GDPGrowth <- read.csv("~/data/ChinaGDPGrowth.csv")
GrossSavings <- read.csv("~/data/ChinaGrossSavingsofGDP.csv")
ExportValue <- read.csv("~/data/ChinaExportValue.csv")
Fertility <- read.csv("~/data/ChinaFertilityRate.csv")
RuralPop <- read.csv("~/data/ChinaRuralPopul.csv")
GDPperCPPP <- read.csv("~/data/ChinaGDPperCapitaPPP.csv")
FDI <- read.csv("~/data/ChinaFDInetFlowGNI.csv")
AgeDependency <- read.csv("~/data/ChinaAgeDependencyRatio.csv")
Pop65 <- read.csv("~/data/ChinaPopul65plus.csv")
GNIperC <- read.csv("~/data/ChinaGNIperCapita.csv")
ServicesEmployment <- read.csv("~/data/ChinaServicesEmployment.csv")
IndustryEmployment <- read.csv("~/data/ChinaIndustryEmployment.csv")
AgricultureEmployment <- read.csv("~/data/ChinaAgricEmployment.csv")
Savings <- read.csv("~/data/ChinaSavings.csv")
Under14 <- read.csv("~/data/ChinaUnder14.csv")
GDPcurrent <- read.csv("~/data/ChinaGDPcurrentUS.csv")
JapanGDPGrowth <- read.csv("~/data/JapanGDPGrowth.csv")
EducationofGDP <- read.csv("~/data/ChinaEducationofGDP.csv")
#df_list <- list(EducationSpending, GNI, InterestRate, GDPGrowth, GrossSavings, ExportValue, Fertility, RuralPop, GDPperCPPP, FDI, AgeDependency, Pop65, GNIperC, Savings, IndustryEmployment, ServicesEmployment, AgricultureEmployment)
#total <- rbind(EducationSpending, GNI, InterestRate, GDPGrowth, GrossSavings, ExportValue, Fertility, RuralPop, GDPperCPPP, FDI, AgeDependency, Pop65, GNIperC, Savings, IndustryEmployment, ServicesEmployment, AgricultureEmployment)
#View(total)

AgricultureEmployment$Value <-as.numeric(AgricultureEmployment$Value)
IndustryEmployment$Value <-as.numeric(IndustryEmployment$Value)
ServicesEmployment$Value <-as.numeric(ServicesEmployment$Value)
AgeDependency$Value <-as.numeric(AgeDependency$Value)
EducationSpending$Value <-as.numeric(EducationSpending$Value)
ExportValue$Value <-as.numeric(ExportValue$Value)
FDI$Value <-as.numeric(FDI$Value)
Fertility$Value <-as.numeric(Fertility$Value)
GDPGrowth$Value <-as.numeric(GDPGrowth$Value)
GDPperCPPP$Value <-as.numeric(GDPperCPPP$Value)
GNI$Value <-as.numeric(GNI$Value)
GNIperC$Value <-as.numeric(GNIperC$Value)
GrossSavings$Value <-as.numeric(GrossSavings$Value)
InterestRate$Value <-as.numeric(InterestRate$Value)
Pop65$Value <-as.numeric(Pop65$Value)
RuralPop$Value <-as.numeric(RuralPop$Value)
Savings$Value <-as.numeric(Savings$Value)
Under14$Value <-as.numeric(Under14$Value)
GDPcurrent$Value <-as.numeric(GDPcurrent$Value)
JapanGDPGrowth$Value <-as.numeric(JapanGDPGrowth$Value)
EducationofGDP$Value <-as.numeric(EducationofGDP$Value)

AgeDependencyVector <- as.vector(AgeDependency['Value'])
EducationSpendingVector <- as.vector(EducationSpending['Value'])
GNIVector <- as.vector(GNI['Value'])
InterestRateVector <- as.vector(InterestRate['Value'])
GDPGrowthVector <- as.vector(GDPGrowth['Value'])
GrossSavingsVector <- as.vector(GrossSavings['Value'])
ExportValueVector <- as.vector(ExportValue['Value'])
FertilityVector <- as.vector(Fertility['Value'])
RuralPopVector <- as.vector(RuralPop['Value'])
GDPperCPPPVector <- as.vector(GDPperCPPP['Value'])
FDIVector <- as.vector(FDI['Value'])
Pop65Vector <- as.vector(Pop65['Value'])
GNIperCVector <- as.vector(GNIperC['Value'])
SavingsVector <- as.vector(Savings['Value'])
IndustryEmploymentVector <- as.vector(IndustryEmployment['Value'])
ServicesEmploymentVector <- as.vector(ServicesEmployment['Value'])
AgricultureEmploymentVector <- as.vector(AgricultureEmployment$Value)
Under14Vector <- as.vector(Under14['Value'])
GDPcurrentVector <- as.vector(GDPcurrent['Value'])
JapanGDPGrowthVector <- as.vector(JapanGDPGrowth['Value'])
EducationofGDPVector <- as.vector(EducationofGDP['Value'])

#Employment <- cbind(data.frame(AgricultureEmploymentVector), data.frame(IndustryEmploymentVector), data.frame(ServicesEmploymentVector))
#View(Employment)
#List1 = c("Employment in agriculture (% of total employment) (modeled ILO estimate)" )
#List2 <- append(List1, AgricultureEmploymentVector)
#List3 = c("Employment in industry (% of total employment) (modeled ILO estimate)" )
#List4 <- append(List3, IndustryEmploymentVector)
# = c("Employment in services (% of total employment) (modeled ILO estimate)" )
#List6 <- append(List5, ServicesEmploymentVector)
#Employment2 <- data.frame(years=1975:2029, 
                                    # Agriculture=AgricultureEmploymentVector, 
                                     #Industry=IndustryEmploymentVector,
                                     #Services=ServicesEmploymentVector)
#Employment3 <- Employment2[-c(1:16, 50:55), ]
Employment4 <- data.frame(
  Year = 1975:2029,
  Agriculture = AgricultureEmploymentVector,
  Industry = IndustryEmploymentVector,
  Services = ServicesEmploymentVector
)
colnames(Employment4) <- c("Year", "Agriculture", "Industry", "Services")
Employment5 <-Employment4[-c(1:16, 50:55), ]
Employment_long <- pivot_longer(Employment5, 
                                cols = c("Agriculture", "Industry", "Services"), 
                                names_to = "Sector", 
                                values_to = "Percentage")
graph2<- ggplot(Employment_long, aes(x = Year, y = Percentage, color = Sector)) +
  geom_line(size = 1) +
  labs(title = "Working Population",
       x = "Year",
       y = "Percentage",
       color = "Sector") +
  theme_minimal() +
ylim(0, NA)
print(graph2)

graph3<- ggplot(Employment_long, aes(x = Year, y = Percentage, color = Sector)) +
  geom_line(size = 1) +
  geom_vline(xintercept = 2010, linetype = "dashed", color = "black", linewidth = 0.3) +
  annotate("text", x = 2010, y = 54, 
           label = "Age Dependency Ratio Turning Point", angle = 0, vjust = -0.7, hjust = 1) +
  geom_vline(xintercept = 2020, linetype = "dashed", color = "black",linewidth = 0.3) +
  annotate("text", x = 2020, y = 5, 
           label = "Lewisian Turning Point", angle = 0, vjust = -0.7, hjust = 0.7) +
  labs(title = "Working Population",
       x = "Year",
       y = "Percentage Working in Sector",
       color = "Sector") +
  scale_x_continuous(breaks = seq(min(Employment_long$Year), max(Employment_long$Year), by = 5)) +
  theme_minimal() +
  ylim(0, NA)
print(graph3)

library(ggplot2)
?geom_line()

Population1 <- data.frame(
  Year = 1975:2029,
  AgeDependency = AgeDependencyVector,
  over65 = Pop65Vector,
  Under14 =Under14Vector
)
colnames(Population1) <- c("Year", "AgeDependencyRatio", "PopulationOver65", "Under14")
Population2 <-Population1[-c(50:55), ]
Population_long <- pivot_longer(Population2, 
                                cols = c("AgeDependencyRatio", "PopulationOver65", "Under14"), 
                                names_to = "Variable", 
                                values_to = "Ratio")
#install.packages('gridExtra')
#library(gridExtra)
#graph4<- ggplot(Population_long, aes(x = Year, y = Ratio, color = Variable)) +
 # geom_line(linewidth = 0.7) +
  #labs(title = "Demographic Development",
   #    x = "Year",
    #   color = "Variable") +
#  theme_minimal() +
 # geom_hline(yintercept = 10.7, linetype = "dashed", color = "black") +
  #annotate("text", x = 2005, y = 10.7, 
#           label = "Post Dividend Age Treshhold", angle = 0, vjust = -0.7, hjust = 0.7, size = 3) +
 # ylim(0, NA)
#print(graph4)

graph5 <- ggplot(Population_long, aes(x = Year, y = Ratio, color = Variable)) +
  geom_line(linewidth = 0.7) +
  labs(title = "Demographic Development",
       x = "Year",
       y = "% of Working Age Population",  
       color = "Variable") +
  geom_hline(yintercept = 10.7, linetype = "dashed", color = "black", linewidth = 0.3) +
  geom_hline(yintercept = 19, linetype = "dashed", color = "black", linewidth = 0.3) +
  annotate("text", x = 2005, y = 10.7, 
           label = "Post Dividend Age Treshhold", angle = 0, vjust = -0.7, hjust = 0.7, size = 3) +
  scale_x_continuous(breaks = seq(min(Population_long$Year), max(Population_long$Year), by = 5)) +
  scale_y_continuous(
    limits = c(0, NA),
    name = "% of Working Age Population",
    sec.axis = sec_axis(~ ., name = "% of Total Population")  
  ) +
      theme_minimal() 
print(graph5)
?geom_line
#max_ratio <- max(Population_long$Ratio[Population_long$Sector == "AgeDependencyRatio"])
#max_fertility <- max(Population_long$Ratio[Population_long$Sector == "FertilityRate"])
#scale_factor <- max_ratio / max_fertility
#Population_long <- Population_long %>%
 # mutate(Ratio_scaled = case_when(
  #  Sector == "AgeDependencyRatio" ~ Ratio * scale_factor,
   # TRUE ~ Ratio
#  ))
#graph5 <-ggplot(Population_long, aes(x = Year)) +
 # geom_line(data = filter(Population_long, Sector == "FertilityRate"),
  #          aes(y = Ratio, color = "FertilityRate")) +
  #geom_line(data = filter(Population_long, Sector == "AgeDependencyRatio"),
   #         aes(y = Ratio_scaled, color = "AgeDependencyRatio")) +
  #scale_y_continuous(
  #  name = "Percentage of Population",
   # sec.axis = sec_axis(~ . / scale_factor, name = "Fertility Rate")
  #) +
  #scale_color_manual(values = c("Absolute" = "blue", "Percentage" = "red")) +
  #theme_minimal() +
  #labs(x = "Time", color = "Metric")
#print(graph5)

InterestRateData <- InterestRateVector[[1]]
GrossSavingsData <- GrossSavingsVector[[1]]
GrossSavingsData2 <- GrossSavingsData/10

Capital1 <- data.frame(
  Year = 1975:2029,
  InterestRate = InterestRateData,
  GrossSavings = GrossSavingsData2
)
Capital2 <-Capital1[-c(1:5,50:55), ]
Capital_long <- pivot_longer(Capital1, 
                           cols = c("InterestRate", "GrossSavings"), 
                           names_to = "Variable", 
                           values_to = "Value")

graph12 <- ggplot() +
  geom_line(data = Capital2, aes(x = Year, y = InterestRate),  color = "red") +
  geom_line(data = Capital2, aes(x = Year, y = GrossSavings), color = "blue") +
  labs(title = "China Capital Accumulation",
       x = "Year", y = "percentage") +
  scale_y_continuous( 
    limits = c(0, NA),
    breaks = seq(0, 10, by= 1),
    name = "% of GDP not consumed",
    sec.axis = sec_axis(
      ~ ., 
      name = "Interest Rate"
    )
  ) +
  theme_minimal()+
  theme(
    axis.title.y.left = element_text(color = "blue"),
    axis.title.y.right = element_text(color = "red"),
  )
print(graph12)

GDPUS1 <- data.frame(
  Year = 1975:2029,
  GDPcurrent = GDPcurrentVector
)
colnames(GDPUS1) <- c("Year", "GDP")
GDPUS1$GDP <- (GDPUS1$GDP/1000000000)
GDPUS2 <-GDPUS1[-c(50:55), ]
GDPUS3 <- GDPUS2 %>%
  mutate(across(where(is.numeric), ~ replace_na(., 0)))
GDPUS_long <- pivot_longer(GDPUS3, 
                                cols = c("GDP"), 
                                names_to = "Variable", 
                                values_to = "Value")
LOGGDPUS1 <- data.frame(
  Year = 1975:2029,
  GDPcurrent = GDPcurrentVector
)
colnames(GDPUS1) <- c("Year", "GDP")
GDPUS1$GDP <- (GDPUS1$GDP/1000000000)
GDPUS2 <-GDPUS1[-c(50:55), ]
GDPUS3 <- GDPUS2 %>%
  mutate(across(where(is.numeric), ~ replace_na(., 0)))
GDPUS_long <- pivot_longer(GDPUS3, 
                           cols = c("GDP"), 
                           names_to = "Variable", 
                           values_to = "Value")

perCPPP <- data.frame(
  Year = 1975:2029,
  GDPperCPPP = GDPperCPPPVector
)
colnames(perCPPP) <- c("Year", "GDPperCapitaPPP")
perCPPP2 <-perCPPP[-c(50:55), ]
perCPPP3 <- perCPPP2 %>%
  mutate(across(where(is.numeric), ~ replace_na(., 0)))
perCPPP_long <- pivot_longer(perCPPP3, 
                            cols = c("GDPperCapitaPPP"), 
                            names_to = "Variable", 
                            values_to = "Value")
GDP1 <-data.frame(
  Year = 1975:2029,
  GDPGrowth = GDPGrowthVector
)
colnames(GDP1) <- c("Year", "GDPAnnualGrowth")
GDP2 <-GDP1[-c(50:55), ]
GDP_long <- pivot_longer(GDP2, 
                            cols = c("GDPAnnualGrowth"), 
                            names_to = "Variable", 
                            values_to = "Value")

graph6 <- ggplot() +
  geom_bar(data = perCPPP_long, aes(x = Year, y = Value), stat = "identity", fill = "orange") +
  geom_bar(data = GDPUS_long, aes(x = Year, y = Value), stat = "identity", fill = "steelblue") +
  labs(title = "China's GDP",
       x = "Year", y = "GDP in current US") +
  scale_y_continuous( 
    limits = c(0, NA),
    name = "GDP in Billion current US$",
    sec.axis = sec_axis(
      ~ ., 
      name = "GDP per Capita in PPP",
      breaks = seq(min(perCPPP_long$Value, na.rm = TRUE), 
                   25000, 
                   by = 5000)
    )
  ) +
  theme_minimal()+
  theme(
    axis.title.y.left = element_text(color = "steelblue"),
    axis.title.y.right = element_text(color = "orange"),
  )
print(graph6)

graph7 <- ggplot() +
  geom_bar(data = perCPPP_long, aes(x = Year, y = Value), stat = "identity", fill = "orange") +
  geom_bar(data = GDPUS_long, aes(x = Year, y = Value), stat = "identity", fill = "steelblue") +
  labs(title = "China's GDP",
       x = "Year", y = "GDP in current US") +
  scale_y_continuous( 
    limits = c(0, NA),
    name = "GDP in Billion current US$",
    sec.axis = sec_axis(
      ~ ., 
      name = "GDP per Capita in PPP",
      breaks = seq(min(perCPPP_long$Value, na.rm = TRUE), 
                   25000, 
                   by = 5000)
    )
  ) +
  theme_minimal()+
  theme(
    axis.title.y.left = element_text(color = "steelblue"),
    axis.title.y.right = element_text(color = "orange"),
  )
print(graph7)

GDP1 <-data.frame(
  Year = 1975:2029,
  GDPGrowth = GDPGrowthVector
)
colnames(GDP1) <- c("Year", "GDPAnnualGrowth")
GDP2 <-GDP1[-c(50:55), ]
GDP_long <- pivot_longer(GDP2, 
                         cols = c("GDPAnnualGrowth"), 
                         names_to = "Variable", 
                         values_to = "Value")
avg_value <- mean(GDP_long$Value, na.rm = TRUE)

graph8 <- ggplot(GDP_long, aes(x = Year, y = Value, color = Variable, group = Variable)) +
  geom_line(linewidth=0.7) +
  labs(title = "Annual GDP Growth",
       x = "Year",
       y = "Growth in %") + 
  geom_hline(yintercept = avg_value, linetype = "dashed", color = "black", linewidth=0.3) +
  annotate("text", x = 2020, y = 9, 
           label = "Mean Growth", angle = 0, vjust = 0, hjust = 0.7, size = 3) +
  scale_y_continuous(breaks = c(-4,-3, -2,-1, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16)) +
  scale_x_continuous(breaks = seq(1975, 2023, by= 3)) +
  theme_minimal() 
print(graph8)

JapanGDPGrowth <- JapanGDPGrowth[ ,c(3:4)]
JapanGDPGrowth <- JapanGDPGrowth[-1, ]
JapanGDPGrowthVector <- as.vector(JapanGDPGrowth['Value'])

GDPGrowthData <- GDPGrowthVector[[1]]
JapanGDPGrowthData <- JapanGDPGrowthVector[[1]]
GDPGrowthData <- c(NA,NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, GDPGrowthData)
CompareGDP <- data.frame(
  Year = 1961:2029,
  GDPGrowth = GDPGrowthData,
  JapanGDPGrowth = JapanGDPGrowthData
)
colnames(CompareGDP) <- c("Year", "GDPAnnualGrowthCN", "GDPAnnualGrowthJP")
CompareGDP2 <-CompareGDP[-c(64:69), ]
CompareGDP_long <- pivot_longer(CompareGDP2, 
                         cols = c("GDPAnnualGrowthCN", "GDPAnnualGrowthJP"), 
                         names_to = "Variable", 
                         values_to = "Value")

graph9 <- ggplot(GDP_long, aes(x = Year, y = Value, color = Variable, group = Variable)) +
  geom_line(linewidth=0.7) +
  labs(title = "Annual GDP Growth",
       x = "Year",
       y = "Growth in %") + 
  geom_hline(yintercept = avg_value, linetype = "dashed", color = "black", linewidth=0.3) +
  annotate("text", x = 2020, y = 9, 
           label = "Mean Growth", angle = 0, vjust = 0, hjust = 0.7, size = 3) +
  scale_y_continuous(breaks = c(-4,-3, -2,-1, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16)) +
  scale_x_continuous(breaks = seq(1975, 2023, by= 3)) +
  theme_minimal() 
print(graph9)

FertilityData <- FertilityVector[[1]]
Fertility2 <- data.frame(
  Year = 1975:2029,
  Fertility = FertilityVector
)
graph10 <- ggplot(Fertility2, aes(x = Year, y = Value))+
  geom_line(linewidth = 0.7) +
  labs(title = "Fertility Rate",
       x = "Year",
       y = "expected Births per Women") +
  theme_minimal()
print(graph10)

EducationData <- EducationofGDPVector[[1]]
Education1 <- data.frame(
  Year = 1975:2029,
  EducationofGDP = EducationData
)
Education2 <- Education1 %>%
  mutate(EducationofGDP = approx(Year[!is.na(EducationofGDP)],
                                 EducationofGDP[!is.na(EducationofGDP)],
                                 Year)$y)
Education3 <-Education2[-c(49:55), ]

graph11 <- ggplot(Education3, aes(x = Year, y = EducationofGDP))+
  geom_line(linewidth = 0.7) +
  labs(title = "Education Spending as % of GDP",
       x = "Year",
       y = "% of GDP") +
  theme_minimal()
print(graph11)