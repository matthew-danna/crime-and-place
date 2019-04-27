# load stuff
if (!require(audio)) install.packages('audio')
library(audio)
if (!require(devtools)) install.packages('devtools')
devtools::install_github("brooke-watson/BRRR")
devtools::install_github("johannesbjork/LaCroixColoR")
library(BRRR)
library(LaCroixColoR)
if (!require(googlesheets)) install.packages('googlesheets')
library(googlesheets)
if (!require(stringr)) install.packages('stringr')
library(stringr)
if (!require(dplyr)) install.packages('dplyr')
library(dplyr)
if (!require(data.table)) install.packages('data.table')
library(data.table)
if (!require(formattable)) install.packages('formattable')
library(formattable)
if (!require(tidyr)) install.packages('tidyr')
library(tidyr)

# colors
customGreen0 = "#DeF7E9"
customGreen = "#71CA97"
customRed = "#ff7f7f"

skrrrahh(0)

#log in and pause here, enter new auth code
gs_auth(new_user = TRUE)

# now, proceed
skrrrahh(0)

# accidents
acc07 <- gs_title("accidents 2007")
acc08 <- gs_title("accidents 2008")
acc09 <- gs_title("accidents 2009")
acc10 <- gs_title("accidents 2010")
acc11 <- gs_title("accidents 2011")
acc12 <- gs_title("accidents 2012")
acc13 <- gs_title("accidents 2013")
acc14 <- gs_title("accidents 2014")
acc15 <- gs_title("accidents 2015")
acc16 <- gs_title("accidents 2016")
acc17 <- gs_title("accidents 2017")
acc18 <- gs_title("accidents 2018")

df.acc07 <- gs_read(acc07)
df.acc08 <- gs_read(acc08)
df.acc09 <- gs_read(acc09)
df.acc10 <- gs_read(acc10)
df.acc11 <- gs_read(acc11)
df.acc12 <- gs_read(acc12)
df.acc13 <- gs_read(acc13)
df.acc14 <- gs_read(acc14)
df.acc15 <- gs_read(acc15)
df.acc16 <- gs_read(acc16)
df.acc17 <- gs_read(acc17)
df.acc18 <- gs_read(acc18)

acc <- rbind(df.acc07, df.acc08, df.acc09, df.acc10, df.acc11, df.acc12, df.acc13,
                   df.acc14, df.acc15, df.acc16, df.acc17, df.acc18)
acc$LAT <- as.numeric(acc$`Accident Address Latitude`)
acc$LON <- as.numeric(acc$`Accident Address Longitude`)
acc$Date <- as.Date(acc$`Accident Date and Time`, format = '%m/%d/%Y')
acc$Year <- str_sub(acc$Date, end = 4)
acc$time.temp <- str_sub(acc$`Accident Date and Time`, start = -8)
acc$Time <- str_sub(acc$time.temp, end = 5)
acc$Time <- gsub(":","",acc$Time)

names(acc)[names(acc) == 'Accident Address'] <- 'Address'
names(acc)[names(acc) == 'Accident Landmarks'] <- 'Landmark'
names(acc)[names(acc) == 'Accident Number'] <- 'Number'
names(acc)[names(acc) == 'Accident Type'] <- 'Type'

accidents <- acc[c(1,5:11,13)]

# yearly counts
acc.years <- group_by(acc, Year)
counts.acc.year <- summarise(acc.years, n=n())
counts.acc.year$n <- as.numeric(counts.acc.year$n)
counts.acc.year$Type <- "Crashes"

acc.sub <- subset(accidents, accidents$LAT !="")
acc.sub.years <- group_by(acc.sub, Year)
counts.acc.sub.year <- summarise(acc.sub.years, n=n())
counts.acc.sub.year$n <- as.numeric(counts.acc.sub.year$n)
skrrrahh(0)

# cfs
cfs07 <- gs_title("cfs 2007")
cfs08 <- gs_title("cfs 2008")
cfs09 <- gs_title("cfs 2009")
cfs10 <- gs_title("cfs 2010")
cfs11 <- gs_title("cfs 2011")
cfs12 <- gs_title("cfs 2012")
cfs13 <- gs_title("cfs 2013")
cfs14 <- gs_title("cfs 2014")
cfs15 <- gs_title("cfs 2015")
cfs16 <- gs_title("cfs 2016")
cfs17 <- gs_title("cfs 2017")
cfs18 <- gs_title("cfs 2018")

df.cfs07 <- gs_read(cfs07)
df.cfs08 <- gs_read(cfs08)
df.cfs09 <- gs_read(cfs09)
df.cfs10 <- gs_read(cfs10)
df.cfs11 <- gs_read(cfs11)
df.cfs12 <- gs_read(cfs12)
df.cfs13 <- gs_read(cfs13)
df.cfs14 <- gs_read(cfs14)
df.cfs15 <- gs_read(cfs15)
df.cfs16 <- gs_read(cfs16)
df.cfs17 <- gs_read(cfs17)
df.cfs18 <- gs_read(cfs18)

cfs <- rbind(df.cfs07, df.cfs08, df.cfs09, df.cfs10, df.cfs11, df.cfs12, df.cfs13,
             df.cfs14, df.cfs15, df.cfs16, df.cfs17, df.cfs18)

cfs$LAT <- as.numeric(cfs$`Incident Address Latitude`)
cfs$LON <- as.numeric(cfs$`Incident Address Longitude`)
cfs$Date <- as.Date(cfs$`Incident Date And Time`, format = '%m/%d/%Y')
cfs$Year <- str_sub(cfs$Date, end = 4)
cfs$time.temp <- str_sub(cfs$`Incident Date And Time`, start = -8)
cfs$Time <- str_sub(cfs$time.temp, end = 5)
cfs$Time <- gsub(":","",cfs$Time)

names(cfs)[names(cfs) == 'Incident Number'] <- 'Number'
names(cfs)[names(cfs) == 'Incident Type'] <- 'Type'

calls <- cfs[c(4:9,11)]

# yearly counts
calls.years <- group_by(calls, Year)
counts.calls.year <- summarise(calls.years, n=n())
counts.calls.year$n <- as.numeric(counts.calls.year$n)
counts.calls.year$Type <- "Calls"

calls.sub <- subset(calls, cfs$LAT !="")
calls.sub.years <- group_by(calls.sub, Year)
counts.calls.sub.year <- summarise(calls.sub.years, n=n())
counts.calls.sub.year$n <- as.numeric(counts.calls.sub.year$n)
skrrrahh(0)

# response timess

# crime
crime07 <- gs_title("cases 2007")
crime08 <- gs_title("cases 2008")
crime09 <- gs_title("cases 2009")
crime10 <- gs_title("cases 2010")
crime11 <- gs_title("cases 2011")
crime12 <- gs_title("cases 2012")
crime13 <- gs_title("cases 2013")
crime14 <- gs_title("cases 2014")
crime15 <- gs_title("cases 2015")
crime16 <- gs_title("cases 2016")
crime17 <- gs_title("cases 2017")
crime18 <- gs_title("cases 2018")

df.crime07 <- gs_read(crime07)
df.crime08 <- gs_read(crime08)
df.crime09 <- gs_read(crime09)
df.crime10 <- gs_read(crime10)
df.crime11 <- gs_read(crime11)
df.crime12 <- gs_read(crime12)
df.crime13 <- gs_read(crime13)
df.crime14 <- gs_read(crime14)
df.crime15 <- gs_read(crime15)
df.crime16 <- gs_read(crime16)
df.crime17 <- gs_read(crime17)
df.crime18 <- gs_read(crime18)

crime <- rbind(df.crime07, df.crime08, df.crime09, df.crime10, df.crime11, df.crime12,
               df.crime13, df.crime14, df.crime15, df.crime16, df.crime17, df.crime18)

crime$LAT <- as.numeric(crime$`Case Address Latitude`)
crime$LON <- as.numeric(crime$`Case Address Longitude`)
crime$Date1 <- as.Date(crime$`Case Occurred From Date`, format = '%m/%d/%Y')
crime$Date2 <- as.Date(crime$`Case Occurred Through Date`, format = '%m/%d/%Y')
crime$Date.Report <- as.Date(crime$`Case Reported Date And Time`, format = '%m/%d/%Y')
crime$Year <- str_sub(crime$Date.Report, end = 4)
crime$time1.temp <- str_sub(crime$`Case Occurred From Date`, start = -8)
crime$time2.temp <- str_sub(crime$`Case Occurred Through Date`, start = -8)
crime$timer.temp <- str_sub(crime$`Case Reported Date And Time`, start = -8)
crime$Time1 <- str_sub(crime$time1.temp, end = 5)
crime$Time2 <- str_sub(crime$time2.temp, end = 5)
crime$Time.Report <- str_sub(crime$timer.temp, end = 5)
crime$Time1 <- gsub(":","",crime$Time1)
crime$Time2 <- gsub(":","",crime$Time2)
crime$Time.Report <- gsub(":","",crime$Time.Report)

names(crime)[names(crime) == 'Case Number'] <- 'Number'
names(crime)[names(crime) == 'Case Occurred Incident Type'] <- 'Type'
names(crime)[names(crime) == 'Case Offense Statute Code'] <- 'Statute'
names(crime)[names(crime) == 'Case Offense Statute Crime Code'] <- 'Crime'
names(crime)[names(crime) == 'Case Offense Statute Description'] <- 
  'Statute.Description'
names(crime)[names(crime) == 'Case Offense Statute Crime Code Description'] <- 
  'Crime.Description'

crimes <- crime[c(3,5,7:10,12:17,21:23)]

crime.groups <- as.data.frame(table(crimes$Crime.Description))

# yearly counts
crime.years <- group_by(crimes, Year)
counts.crime.year <- summarise(crime.years, n=n())
counts.crime.year$n <- as.numeric(counts.crime.year$n)
counts.crime.year$Type <- "Crimes"

crime.sub <- subset(crimes, crime$LAT !="")
crime.sub.years <- group_by(crime.sub, Year)
counts.crime.sub.year <- summarise(crime.sub.years, n=n())
counts.crime.sub.year$n <- as.numeric(counts.crime.sub.year$n)
skrrrahh(0)

# outputs
write.csv(accidents, "C:/Users/matth/Desktop/Rcode/outputs/crim320/accidents.csv", row.names = FALSE)
write.csv(calls, "C:/Users/matth/Desktop/Rcode/outputs/crim320/cfs.csv", row.names = FALSE)
write.csv(crimes, "C:/Users/matth/Desktop/Rcode/outputs/crim320/crime.csv", row.names = FALSE)
skrrrahh(0)

# merge
all <- rbind(counts.acc.year,counts.calls.year,counts.crime.year)
names(all)[names(all) == 'n'] <- 'Count'
all$Count <- as.numeric(as.character(all$Count))

# transpose
t.crime <- data.frame(t(counts.crime.year))
colnames(t.crime) <- as.character(unlist(t.crime[1,]))
t.crime <- t.crime[-1, ]
t.crime$Type <- "Crime"

t.calls <- data.frame(t(counts.calls.year))
colnames(t.calls) <- as.character(unlist(t.calls[1,]))
t.calls <- t.calls[-1, ]
t.calls$Type <- "CFS"

t.acc <- data.frame(t(counts.acc.year))
colnames(t.acc) <- as.character(unlist(t.acc[1,]))
t.acc <- t.acc[-1, ]
t.acc$Type <- "Crashes"

# merge and clean and add stats
all.table <- rbind(t.crime, t.calls, t.acc)
all.table <- all.table[c(13,1:12)]
all.table$`2007` <- as.numeric(as.character(all.table$`2007`))
all.table$`2008` <- as.numeric(as.character(all.table$`2008`))
all.table$`2009` <- as.numeric(as.character(all.table$`2009`))
all.table$`2010` <- as.numeric(as.character(all.table$`2010`))
all.table$`2011` <- as.numeric(as.character(all.table$`2011`))
all.table$`2012` <- as.numeric(as.character(all.table$`2012`))
all.table$`2013` <- as.numeric(as.character(all.table$`2013`))
all.table$`2014` <- as.numeric(as.character(all.table$`2014`))
all.table$`2015` <- as.numeric(as.character(all.table$`2015`))
all.table$`2016` <- as.numeric(as.character(all.table$`2016`))
all.table$`2017` <- as.numeric(as.character(all.table$`2017`))
all.table$`2018` <- as.numeric(as.character(all.table$`2018`))
all.table$Average <- round(rowMeans(all.table[,-1]),0)
all.table$Change <- round(((all.table$`2018`- all.table$`2007`)/all.table$`2007`)*100,2)
rownames(all.table) <- NULL

all.table.new <- subset(all.table[c(1,12:13)])
names(all.table.new)[names(all.table.new) == '2018'] <- 'This Year'
names(all.table.new)[names(all.table.new) == '2017'] <- 'Last Year'
all.table.new$Difference <- all.table.new$`This Year` - all.table.new$`Last Year` 
all.table.new$Percentage <- round(((all.table.new$`This Year`-all.table.new$`Last Year`)/
                                     all.table.new$`Last Year`*100),2)

# let's get pretty
# for change arrows
change_formatter <- 
  formatter("span", 
            style = x ~ style(font.weight = "bold", 
                              color = ifelse(x > 0, customGreen, ifelse(x < 0, customRed, "black"))), 
            x ~ icontext(ifelse(x>0, "arrow-up", "arrow-down"), x)
  )

# for change star
change_formatter3 <- formatter("span", style = x ~ style(font.weight = "bold", 
                                                         color = ifelse(x > 0, customRed, ifelse(x < 0, customGreen, "black"))), 
                               x ~ icontext(ifelse(x == min(x), "thumbs-up", ""), x)
)

formattable(all.table, 
            align =c("l","c","c","c","c","c","c","c","c","c","c","c","c","r","r"), 
            list(`Type` = formatter(
              "span", style = ~ style(color = "grey",font.weight = "bold")),
              `2007`= color_tile(customGreen, customGreen0),
              `2008`= color_tile(customGreen, customGreen0),
              `2009`= color_tile(customGreen, customGreen0),
              `2010`= color_tile(customGreen, customGreen0),
              `2011`= color_tile(customGreen, customGreen0),
              `2012`= color_tile(customGreen, customGreen0),
              `2013`= color_tile(customGreen, customGreen0),
              `2014`= color_tile(customGreen, customGreen0),
              `2015`= color_tile(customGreen, customGreen0),
              `2016`= color_tile(customGreen, customGreen0),
              `2017`= color_tile(customGreen, customGreen0),
              `2018`= color_tile(customGreen, customGreen0),
              `Average` = color_bar(customRed),
              `Change` = change_formatter3
            ))

# compare this year to last year
formattable(all.table.new, align =c("l","c","c","c","c"), list(
  `Type` = formatter("span",
                               style = ~ style(color = "gray")), 
  `This Year`= formatter("span", style = ~ style(color = ifelse(`This Year` >`Last Year`, 
                                                                "red", "green")),
                    ~ icontext(ifelse(`This Year` >`Last Year`,"arrow-up", "arrow-down"), 
                               `This Year`)),
  `Percentage` = change_formatter3
))  

# bar plots
par(las=2)
par(mar=c(5,8,4,2))
par(mfrow=c(3,1), bg = '#FEFFFF') 
barplot(subset(all$Count, all$Type == "Crimes"), main = "Crimes per year", 
        horiz = FALSE, names.arg = subset(all$Year, all$Type == "Crimes"), col = 
          lacroix_palette("Pamplemousse", n = (nrow(counts.crime.year)), type = "continuous"), 
        cex.names=1.5)
barplot(subset(all$Count, all$Type == "Calls"), main = "Calls per year", 
        horiz = FALSE, names.arg = subset(all$Year, all$Type == "Calls"), col = 
          lacroix_palette("PeachPear", n = (nrow(counts.calls.year)), type = "continuous"), 
        cex.names=1.5)
barplot(subset(all$Count, all$Type == "Crashes"), main = "Crashes per year", 
        horiz = FALSE, names.arg = subset(all$Year, all$Type == "Crashes"), col = 
          lacroix_palette("PassionFruit", n = (nrow(counts.acc.year)), type = "continuous"), 
        cex.names=1.5)
title(main="", sub="City of Fairfax Police data")
skrrrahh(0)
