
library(magrittr) # Load magrittr package
library(ggplot2)
library(reshape2)
library(RColorBrewer)
library(gplots)
Smoke_Dataset <- read.csv("smoke_detection_iot.csv")
Smoke_Dataset <- Smoke_Dataset[,-c(1,2,15)]
Smoke_Dataset
str(Smoke_Dataset)
#summary(Smoke_Dataset)

any(duplicated(Smoke_Dataset))

## Check for missing values  
sum(is.na(Smoke_Dataset))

## Count the number of missing values in each column
colSums(is.na(Smoke_Dataset))


summary(Smoke_Dataset)

## Check for outliers and handle them appropriately
boxplot(Smoke_Dataset$Temperature.C., Smoke_Dataset$Humidity..., Smoke_Dataset$eCO2.ppm., Smoke_Dataset$Fire.Alarm)

boxplot(Temperature.C. ~ eCO2.ppm. , data = Smoke_Dataset, xlab = "eCO2[ppm]", ylab = "Temperature[C]")

## Hypothesis testing
install.packages("magrittr") # Install magrittr package

smoke_present <- Smoke_Dataset %>% filter(Smoke_Dataset$Fire.Alarm == 1)
smoke_absent <- Smoke_Dataset %>% filter(Smoke_Dataset$Fire.Alarm == 0)
## create a data frame from smoke_present and smoke_absent
smoke_present_df <- data.frame(smoke_present)
smoke_absent_df <- data.frame(smoke_absent)
## x1 ~ x16 gowahom nulls
t.test(smoke_present_df$X6, smoke_absent_df$X6, alternative = "greater")

## EDA
ggplot(Smoke_Dataset, aes(x=Fire.Alarm)) +
  geom_bar(fill=c("orange", "lightblue")) +
  scale_fill_manual(values=c("orange", "lightblue")) +
  labs(x="Fire Alarm")+



# Relation data attributes to Fire Alarm status
windows()
par(mfrow=c(4,3), mar=c(2,4,3,2), oma=c(0,0,2,0))

boxplot(Smoke_Dataset[, "Temperature.C."]~Smoke_Dataset$Fire.Alarm, 
        xlab="Fire Alarm", 
        ylab="Temperature",
        main="Temprature Correlation with FireAlarm ",
        show.names=FALSE, 
        boxwex = 0.5,
        col = c("orange", "lightblue"))
axis(1, at=c(1,2), labels=c("No Alarm", "Alarm"))

boxplot(Smoke_Dataset[, "Humidity..."]~Smoke_Dataset$Fire.Alarm, 
        xlab="Fire Alarm", 
        ylab="Humidity",
        main="Humidity Correlation with FireAlarm ",
        
        show.names=FALSE, 
        boxwex = 0.5,
        col = c("orange", "lightblue"))
axis(1, at=c(1,2), labels=c("No Alarm", "Alarm"))

boxplot(Smoke_Dataset[, "TVOC.ppb."]~Smoke_Dataset$Fire.Alarm, 
        xlab="Fire Alarm", 
        ylab="TVOC.ppb.",
        main="TVOC.ppb. Correlation with FireAlarm ",
        ylim=c(0,2500),
        show.names=FALSE, 
        boxwex = 0.5,
        col = c("orange", "lightblue"))
axis(1, at=c(1,2), labels=c("No Alarm", "Alarm"))

boxplot(Smoke_Dataset[, "eCO2.ppm."]~Smoke_Dataset$Fire.Alarm, 
        xlab="Fire Alarm", 
        ylab="eCO2.ppm.",
        main=" eCO2.ppm Correlation with FireAlarm ",
        
        ylim=c(400,600),
        show.names=FALSE, 
        boxwex = 0.5,
        col = c("orange", "lightblue"))
axis(1, at=c(1,2), labels=c("No Alarm", "Alarm"))

boxplot(Smoke_Dataset[, "Raw.H2"]~Smoke_Dataset$Fire.Alarm, 
        xlab="Fire Alarm", 
        ylab="Raw.H2",
        main="Raw H2 Correlation with FireAlarm ",
        
        ylim=c(12200,13600),
        show.names=FALSE, 
        boxwex = 0.5,
        col = c("orange", "lightblue"))
axis(1, at=c(1,2), labels=c("No Alarm", "Alarm"))

boxplot(Smoke_Dataset[, "Raw.Ethanol"]~Smoke_Dataset$Fire.Alarm, 
        xlab="Fire Alarm", 
        ylab="Raw.Ethanol",
        main=" Raw Ethanol Correlation with FireAlarm ",
        
        ylim=c(19000,21500),
        show.names=FALSE, 
        boxwex = 0.5,
        col = c("orange", "lightblue"))
axis(1, at=c(1,2), labels=c("No Alarm", "Alarm"))

boxplot(Smoke_Dataset[, "Pressure.hPa."]~Smoke_Dataset$Fire.Alarm, 
        xlab="Fire Alarm", 
        ylab="Pressure.hPa.",
        main="Pressure hPa Correlation with FireAlarm ",
        ylim=c(936.5,940.0),
        show.names=FALSE, 
        boxwex = 0.5,
        col = c("orange", "lightblue"))
axis(1, at=c(1,2), labels=c("No Alarm", "Alarm"))

boxplot(Smoke_Dataset[, "PM1.0"]~Smoke_Dataset$Fire.Alarm, 
        xlab="Fire Alarm", 
        ylab="PM1.0",
        main="PM1.0 Correlation with FireAlarm ",
        ylim=c(0,3.5),
        show.names=FALSE, 
        boxwex = 0.5,
        col = c("orange", "lightblue"))
axis(1, at=c(1,2), labels=c("No Alarm", "Alarm"))

boxplot(Smoke_Dataset[, "PM2.5"]~Smoke_Dataset$Fire.Alarm, 
        xlab="Fire Alarm", 
        ylab="PM2.5",
        main="PM2.5 Correlation with FireAlarm ",
        
        ylim=c(0,4),
        show.names=FALSE, 
        boxwex = 0.5,
        col = c("orange", "lightblue"))
axis(1, at=c(1,2), labels=c("No Alarm", "Alarm"))

boxplot(Smoke_Dataset[, "NC0.5"]~Smoke_Dataset$Fire.Alarm, 
        xlab="Fire Alarm", 
        ylab="NC0.5",
        main="NC0.5 Correlation with FireAlarm ",
        show.names=FALSE,
        ylim=c(0,25),
        boxwex = 0.5,
        col = c("orange", "lightblue"))
axis(1, at=c(1,2), labels=c("No Alarm", "Alarm"))

boxplot(Smoke_Dataset[, "NC1.0"]~Smoke_Dataset$Fire.Alarm, 
        xlab="Fire Alarm", 
        ylab="NC1.0",
        main="NC1.0 Correlation with FireAlarm ",
        
        ylim=c(0,4),
        show.names=FALSE, 
        boxwex = 0.5,
        col = c("orange", "lightblue"))
axis(1, at=c(1,2), labels=c("No Alarm", "Alarm"))

boxplot(Smoke_Dataset[, "NC2.5"]~Smoke_Dataset$Fire.Alarm, 
        xlab="Fire Alarm", 
        ylab="NC2.5",
        main="NC2.5 Correlation with FireAlarm ",
        
        ylim=c(0.0,0.08),
        show.names=FALSE, 
        boxwex = 0.5,
        col = c("orange", "lightblue"))
axis(1, at=c(1,2), labels=c("No Alarm", "Alarm"))

### Correlation
library(ggplot2)
library(reshape2)

# Compute the correlation matrix
corr <- cor(Smoke_Dataset)

# Melt the correlation matrix into a long format
melt_corr <- melt(corr)

# Add a column for the correlation percentage
melt_corr$cor_percentage <- round(melt_corr$value, 1)

# Define the color scale
colors <- colorRampPalette(c("#7F0000", "#FFFFFF", "#007F00"))(256)

# Create the heatmap
windows()
ggplot(melt_corr, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile(color = "white", size = 0.5) +
  geom_text(aes(label = cor_percentage), color = "black", size = 3) +
  scale_fill_gradientn(colors = colors, na.value = "white",
                       limits = c(-1, 1), name = "Pearson\nCorrelation") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, size = 8, hjust = 1),
        axis.text.y = element_text(size = 8),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.line = element_blank(),
        ) +
  ggtitle("Heatmap Correlation")
