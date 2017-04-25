# Fire & EMS Analysis

# Load call file

calls <- read.csv(file = "fireemscalls.csv", head = T, sep = ",")

# Subset to filter only emergency response calls: 110, 111, 112, 113, 114, 120, 121, 122,
# 123, 130, 131, 132, 311, 321, 322, 323

calls <- subset(calls, Incident.Type == 111 | Incident.Type == 112 | Incident.Type == 113 |
                  Incident.Type == 114 | Incident.Type == 120 | Incident.Type == 121 | 
                  Incident.Type == 122 | Incident.Type == 123 | Incident.Type == 130 | 
                  Incident.Type == 131 | Incident.Type == 132 | Incident.Type == 311 |
                  Incident.Type == 321 | Incident.Type == 322 | Incident.Type == 323)

View(calls)
dim(calls)
# Modify address field
require(stringr)
calls$Address <- gsub("\\s+", " ", calls$Address) 
calls$Address <- str_trim(calls$Address)

# Modify date / time fields
require(lubridate)
#Remove trailing 000s
calls$Dispatch.Time <- gsub('.{4}$', '', calls$Dispatch.Time)
#Modify as date/time object
calls$Dispatch.Time <- as.POSIXct(calls$Dispatch.Time, format = "%Y-%m-%d %H:%M:%S")

#Repeat for other date/time fields
calls$Alarm.Time <- gsub('.{4}$', '', calls$Alarm.Time)
calls$Alarm.Time <- as.POSIXct(calls$Alarm.Time, format = "%Y-%m-%d %H:%M:%S")
calls$Arrival.Time <- gsub('.{4}$', '', calls$Arrival.Time)
calls$Arrival.Time <- as.POSIXct(calls$Arrival.Time, format = "%Y-%m-%d %H:%M:%S")

#create date/time variables

calls$Year <- year(calls$Dispatch.Time)
calls$Month <- month(calls$Dispatch.Time)
calls$Day <- day(calls$Dispatch.Time)
calls$Year <- factor(calls$Year, levels = c(2015:2016), ordered = T)
calls$Month <- factor(calls$Month, levels = c(1:12), ordered = T)
calls$Day <- factor(calls$Day, levels = c(1:31), ordered = T)

#Time of day
calls$Hour <- hour(calls$Dispatch.Time)
calls$Hour <- factor(calls$Hour, levels = c(0:23), ordered = T)

# Response time 1 (Difference between dispatch and arrival)
calls$DispArrival <- calls$Arrival.Time - calls$Dispatch.Time
# Response time 2 (difference between alarm and arrival)
calls$AlarmArrival <- calls$Arrival.Time - calls$Alarm.Time

# remove calls with response time greater than 100,000 (outlier, only 1 such case)
calls <- subset(calls, calls$AlarmArrival < 100000)

# dummy variables for Fire (if incident type starts with 1) and EMS (if incident type starts w/ 3)
calls$Fire <- as.factor(ifelse(calls$Incident.Type < 300 | calls$Incident.Type > 400, 1, 0))
calls$Labels <- ifelse(calls$Fire == 1, "Fire", "EMS")

calls$Incident.Type <- as.factor(calls$Incident.Type)
calls$district <- as.factor(calls$district)
View(calls)

dim(calls)
# create Fire dataset and EMS dataset
fires <- subset(calls, calls$Fire == 1)
ems <- subset(calls, calls$Fire == 0)
dim(ems)
under240 <- subset(fires, fires$AlarmArrival < 240)
dim(under240)
dim(fires)
122/217
# ANALYSIS of ALL CALLS first, then join parcel / building data and run analysis on that subset w/ successful match

require(ggplot2)

# Breakdown, fire and EMS by month and year
ggplot(calls, aes(Month, fill = Year)) + geom_bar(position = position_dodge()) + labs(x = "Year", y = "# of Complaints", title = "Number of Complaints by Year and Month") + scale_fill_discrete(name = "Month") + theme(plot.title = element_text(hjust = .5))

# fire breakdown by month *
ggplot(fires, aes(Month, fill = Year)) + geom_bar(position = position_dodge()) + labs(x = "Month", y = "# of Fire Calls", title = "Number of Fire Calls by Month and Year") + scale_fill_discrete(name = "Year") + theme(plot.title = element_text(hjust = .5))
aggbymonth <- aggregate(Address ~ Month, data = fires, FUN = length)
aggbymonth
# fire breakdown by hour *
ggplot(fires, aes(Hour)) + geom_bar(position = position_dodge()) + labs(x = "Hour of Day", y = "# of Fire Calls", title = "Number of Fire Calls by Time of Day (2015-16)") + scale_fill_discrete(name = "Month") + theme(plot.title = element_text(hjust = .5))
aggbyhourf <- aggregate(Address ~ Hour, data = fires, FUN = length)
aggbyhourf

# fire arrival time overall, with line for "4 minutes" *
ggplot(fires, aes(x = AlarmArrival)) + geom_histogram(binwidth = 15, colour = "black", fill = "white") + 
  geom_vline(aes(xintercept = 240), color = "red", linetype = "dashed", size = 1) + labs(x = "Response Time from Alarm to Arrival (in seconds)", y = "# of Fire Calls")
# fire arrival time broken down by hour of day
ggplot(fires, aes(x = AlarmArrival)) + geom_histogram(binwidth = 30, colour = "black", fill = "white") + facet_wrap(~Hour) +
  geom_vline(aes(xintercept = 240), color = "red", linetype = "dashed", size = 1)
aggfirerespbyhour <- aggregate(AlarmArrival ~ Hour, data = fires, FUN = mean)
aggfirerespbyhour
# fire arrival time broken down by district
ggplot(fires, aes(x = AlarmArrival)) + geom_histogram(binwidth = 30, colour = "black", fill = "white") + facet_wrap(~district, scales = "free_y") +
  geom_vline(aes(xintercept = 240), color = "red", linetype = "dashed", size = 1)

#Boxplot to show which time of day is not meeting target *
ggplot(fires, aes(x = Hour, y = AlarmArrival)) + geom_boxplot() +
  geom_hline(aes(yintercept = 240), color = "red", linetype = "dashed", size = 1)+ labs(y = "Response Time from Alarm to Arrival (in seconds)", x = "Hour of Day")

#Boxplot to show which months are not meeting target
ggplot(fires, aes(x = Month, y = AlarmArrival)) + geom_boxplot() +
  geom_hline(aes(yintercept = 240), color = "red", linetype = "dashed", size = 1)
#Boxplot to show which districts are not meeting target *
ggplot(fires, aes(x = district, y = AlarmArrival)) + geom_boxplot() +
  geom_hline(aes(yintercept = 240), color = "red", linetype = "dashed", size = 1)+ labs(y = "Response Time from Alarm to Arrival (in seconds)", x = "District")
# aggby district for pres stats
aggbydist <- aggregate(AlarmArrival ~ district, data = fires, FUN = mean, na.rm = T)
aggbydist

# ems breakdown by month *
ggplot(ems, aes(Month, fill = Year)) + geom_bar(position = position_dodge()) + labs(x = "Month", y = "# of EMS Calls", title = "Number of EMS Calls by Month and Year") + scale_fill_discrete(name = "Year") + theme(plot.title = element_text(hjust = .5))
aggbymonthe <- aggregate(Address ~ Month, data = ems, FUN = length)
aggbymonthe
# ems breakdown by hour *
ggplot(ems, aes(Hour, fill = Year)) + geom_bar(position = position_dodge()) + labs(x = "Hour of Day", y = "# of EMS Calls", title = "Number of EMS Calls by Time of Day") + scale_fill_discrete(name = "Year") + theme(plot.title = element_text(hjust = .5))
aggbyhoure <- aggregate(Address ~ Hour, data = ems, FUN = length)
aggbyhoure
View(ems)
# ems arrival time overall, with line for "4 minutes" *
ggplot(ems, aes(x = AlarmArrival)) + geom_histogram(binwidth = 15, colour = "black", fill = "white") + 
  geom_vline(aes(xintercept = 240), color = "red", linetype = "dashed", size = 1) + xlim(0, 1200) + labs(x = "Response Time from Alarm to Arrival (in seconds)", y = "# of EMS Calls")
under240e <- subset(ems, ems$AlarmArrival < 240)
dim(under240e)
dim(ems)
1994/10121
# ems arrival time broken down by hour of day
ggplot(ems, aes(x = AlarmArrival)) + geom_histogram(binwidth = 30, colour = "black", fill = "white") + facet_wrap(~Hour) +
  geom_vline(aes(xintercept = 240), color = "red", linetype = "dashed", size = 1)+ xlim(0, 1200)
aggrespbyhour <- aggregate(AlarmArrival ~ Hour, data = ems, FUN = mean)
aggrespbyhour
# ems arrival time broken down by district
ggplot(ems, aes(x = AlarmArrival)) + geom_histogram(binwidth = 60, colour = "black", fill = "white") + facet_wrap(~district) +
  geom_vline(aes(xintercept = 240), color = "red", linetype = "dashed", size = 1)+ xlim(0, 1200)

#Boxplot to show which time of day is not meeting target *
ggplot(ems, aes(x = Hour, y = AlarmArrival)) + geom_boxplot() + ylim(0,1200) +
  geom_hline(aes(yintercept = 240), color = "red", linetype = "dashed", size = 1) + labs(x = "Hour of Day", y = "Response Time from Alarm to Arrival (in seconds)")
#Boxplot to show which months are not meeting target
ggplot(ems, aes(x = Month, y = AlarmArrival)) + geom_boxplot() + ylim(0, 1200) +
  geom_hline(aes(yintercept = 240), color = "red", linetype = "dashed", size = 1)
#Boxplot to show which districts are not meeting target *
ggplot(ems, aes(x = district, y = AlarmArrival)) + geom_boxplot() + ylim(0, 1200) +
  geom_hline(aes(yintercept = 240), color = "red", linetype = "dashed", size = 1) + labs(x = "District", y = "Response Time from Alarm to Arrival (in seconds)")

aggbydist <- aggregate(AlarmArrival ~ district, data = ems, FUN = mean, na.rm = T)
aggbydist

# export for use in arcgis / elsewhere
write.csv(calls, "fireemscalls1516.csv")
write.csv(fires, "firecalls1516.csv")
write.csv(ems, "emscalls1516.csv")




# Add in ArcGis file with station info and distance to station

fireemsstations <- read.csv(file = "fireemswStations.csv", head = T, sep = ",")
#subset same as with calls
fireemsstations <- subset(fireemsstations, Incident_T == 111 | Incident_T == 112 | Incident_T == 113 |
                            Incident_T == 114 | Incident_T == 120 | Incident_T == 121 | 
                            Incident_T == 122 | Incident_T == 123 | Incident_T == 130 | 
                            Incident_T == 131 | Incident_T == 132 | Incident_T == 311 |
                            Incident_T == 321 | Incident_T == 322 | Incident_T == 323)


View(fireemsstations)
# create a mile distance field
fireemsstations$distMi <- fireemsstations$Distance / 5280
# remove records w/ failed lat/long coords
fireemsstations <- subset(fireemsstations, fireemsstations$latitude != 0)
View(fireemsstations)

# create subsets again
fires <- subset(fireemsstations, fireemsstations$Fire == 1)
ems <- subset(fireemsstations, fireemsstations$Fire == 0)

require(ggplot2)
# arrival time by distance, all calls *
ggplot(fireemsstations, aes(x = distMi, y = AlarmArriv)) + geom_point(shape = 1) + xlim(0, 3.5) + ylim(0, 1500) + 
  geom_smooth() + labs(x = "Distance of Call from Station (in miles)", y = "Response Time from Alarm to Arrival (in seconds)")
# Arrival time by station and distance, all calls *
ggplot(fireemsstations, aes(x = distMi, y = AlarmArriv)) + geom_point(shape = 1) + xlim(0, 3.5) + ylim(0, 1500) + 
  geom_smooth() + facet_wrap(~Station)+ labs(x = "Distance of Call from Station (in miles)", y = "Response Time from Alarm to Arrival (in seconds)")
# Arrival time by district and distance, all calls
ggplot(fireemsstations, aes(x = distMi, y = AlarmArriv)) + geom_point(shape = 1) + xlim(0, 3.5) + ylim(0, 1500) + 
  geom_smooth() + facet_wrap(~district)
# Arrival time by station and distance, fires
ggplot(fires, aes(x = distMi, y = AlarmArriv)) + geom_point(shape = 1) + xlim(0, 3.5) + ylim(0, 1500) + 
  geom_smooth() + facet_wrap(~Station)
# Arrival time by station and distance, ems
ggplot(ems, aes(x = distMi, y = AlarmArriv)) + geom_point(shape = 1) + xlim(0, 3.5) + ylim(0, 1500) + 
  geom_smooth() + facet_wrap(~Station)


regdistresp <- lm(AlarmArriv ~ distMi, data = fireemsstations)
summary(regdistresp)

predtest <- predict(regdistresp, newdata = data.frame(distMi = c(1, 2, 3)))

predtest


# Then, population growth trends. Find subset of permits for new construction since 2013. How many new units / bldgs, where?
# what impact have these new buildings had on fire / ems rates? If at all?
# compare to recent growth rates by neighborhood for population and see what areas are growing. See if that correlates
# with the new constr trends


# look at elderly population %, young population %, % below poverty level, etc. and see how this relates to neighb.
# fire / ems activity levels. (Measure activity levels of fire/ems per 1,000 people or per 1,000 parcels?)

View(aggbycat)

# create new version of parcel permit data.frame
parcelperm2 <- read.csv("parcelperm.csv", head = T, sep = ",")
View(parcelperm2)
# create dummy var for New Constr (e.g., growth) and Decline (e.g., demo)
parcelperm2$Growth <- ifelse(parcelperm2$Category == "Commercial, New Structure", 1,
                             ifelse(parcelperm2$Category == "Residential, Mobile Home Set", 1,
                                    ifelse(parcelperm2$Category == "Residential, New Home", 1,
                                           ifelse(parcelperm2$Category == "Residential, Multi-family new", 1, 0))))

parcelperm2$Decline <- ifelse(parcelperm2$Category == "Demo Residential Dwelling", 1,
                              ifelse(parcelperm2$Category == "Demo Commercial Structure", 1, 0))


# sub and view growth only
growth <- subset(parcelperm2, Growth == 1)

aggbytract <- aggregate(Growth ~ TRACT_NAME, data = growth, FUN = length)
View(aggbytract)

decline <- subset(parcelperm2, Decline == 1)

aggbytract2 <- aggregate(Decline ~ TRACT_NAME, data = decline, FUN = length)
View(aggbytract2)

# Load in pop growth data
tractgrowth <- read.csv(file = "2017-03-13 Pop Growth Tract.csv", head = T, sep = ",")
tractgrowth$TRACTS <- gsub("TRACT:", "", tractgrowth$TRACT.NAME)

View(tractgrowth)
write.csv(tractgrowth, "tractgrowth.csv")

#MAPPING
require(rgdal)
require(sp)
require(ggplot2)
require(ggmap)
tracts_kzoo <- readOGR(dsn = ".", "Tracts")

proj4string(tracts_kzoo)
tracts_kzoo = spTransform(tracts_kzoo, CRS("+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0"))

names(tracts_kzoo)

tracts_kzoo <- fortify(tracts_kzoo, region = "TRCT_KEY")
View(tracts_kzoo)
#join in permit data
tracts_kzoo <- merge(tracts_kzoo, tractgrowth, by.x = 'id', by.y = 'TRACTS', all.x = T)
tracts_kzoo <- tracts_kzoo[order(tracts_kzoo$order),]
View(tracts_kzoo)

parcels_kzoo <- readOGR(dsn = ".", "ParcelsDat")
proj4string(parcels_kzoo)
parcels_kzoo = spTransform(parcels_kzoo, CRS("+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0"))

names(parcels_kzoo)

parcels_kzoo <- fortify(parcels_kzoo, region = "PIN")

#Join in aggbyPIN complaint data
parcels_kzoo <- merge(parcels_kzoo, aggbyPIN, by.x = 'id', by.y = "PIN", all.x = T)
parcels_kzoo <- parcels_kzoo[order(parcels_kzoo$order),]


#KZOO BASE
kzoo<-get_map(location=c(left = -85.71350, bottom = 42.22343, right = -85.47947, top = 42.34078))
kzoo<-ggmap(kzoo)
kzoo

# Pop Growth
popgrowth <- kzoo + geom_polygon(aes(x = long, y = lat, group = group, fill = PopChange), data = tracts_kzoo) + geom_path(aes(x = long, y = lat, group = group), color = 'gray', data = tracts_kzoo) + scale_fill_gradient(low = "red", high = "green")+labs(fill='# of Permits (Q4 2016)')
popgrowth

# New Constr Permits
newconst <- kzoo + geom_polygon(aes(x = long, y = lat, group = group, fill = New.Const.Permits1316), data = tracts_kzoo) + geom_path(aes(x = long, y = lat, group = group), color = 'gray', data = tracts_kzoo) + scale_fill_gradient(low = "red", high = "green")+labs(fill='# of Permits (Q4 2016)')
newconst

# Demolition Permits
demos <- kzoo + geom_polygon(aes(x = long, y = lat, group = group, fill = Demo.Permits1316), data = tracts_kzoo) + geom_path(aes(x = long, y = lat, group = group), color = 'gray', data = tracts_kzoo) + scale_fill_gradient(low = "red", high = "green")+labs(fill='# of Permits (Q4 2016)')
demos

# Pop Above 65
oldpop <- kzoo + geom_polygon(aes(x = long, y = lat, group = group, fill = Pop652015), data = tracts_kzoo) + geom_path(aes(x = long, y = lat, group = group), color = 'gray', data = tracts_kzoo) + scale_fill_gradient(low = "red", high = "green")+labs(fill='# of Permits (Q4 2016)')
oldpop

# parcels num_complaints
numcomplaintsparc <- kzoo + geom_polygon(aes(x = long, y = lat, group = group, fill = NumComplaints), data = parcels_kzoo) + geom_path(aes(x = long, y = lat, group = group), color = 'gray', data = parcels_kzoo) + scale_fill_gradient(low = "red", high = "green") + labs(fill = '# of Enforcement Cases (2013-2016)')
numcomplaintsparc
# CREATE GIS MAPS FOR THESE AS WELL W/ MORE DETAIL





# we have parcel data, spatial join PIN, merged PIN

View(parcels)
View(fireemsstations)


# merge w/ parcels
parcels <- read.csv(file = "parcels_final.csv", head = T, sep = ",")
names(parcels)
# remove unnecessary columns
keepcols <- c(1, 11:13, 20, 25:26, 29:32, 34, 36, 37, 39:43, 47)
parcels <- parcels[, keepcols]
parcels$Address <- as.character(parcels$Address)
fireemsparcels$Address <- as.character(fireemsparcels$Address)
fireemsparcels <- merge(fireemsstations, parcels, by = "Address", all.x.y = T)

dim(fireemsparcels)
fireemsparcels <- fireemsparcels[25:6381, ]
View(fireemsparcels)



# Load in GIS file as well and combine the two different merge efforts
fireemsparcels2 <- read.csv(file = "fireemsparceljoin.csv", head = T, sep = ",")

# extract out only incident address, address_2, and PIN_1
names(fireemsparcels2)
keepcols <- c(1, 6, 39:40)
fireemsparcels2 <- fireemsparcels2[,keepcols]
fireemsparcels2$Address_2 <- gsub(" 50% UI1", "", fireemsparcels2$Address_2)
View(fireemsparcels2)

# agg by tract
fireemstract <- aggregate(PIN ~ TRACT, data = fireemsparcels, FUN = length)
View(fireemstract)

firetract <- aggregate(Fire ~ TRACT, data = fireemsparcels, FUN = sum)
View(firetract)

# subset ems and then aggregate
emsparcels <- subset(fireemsparcels, Fire == 0)
View(emsparcels)
emstract <- aggregate(Fire ~ TRACT, data = emsparcels, FUN = length)
View(emstract)




# tie in property characteristics to fire prevalence
# e.g., do older/younger buildings have more fires? do SF / MF have more? buildings w/ certain permit / enforcement
# characteristics, etc.?

# tie in permit / enforcement / building char. data so that we have "all parcels", 1s and 0s for had fire, had ems call, had enforcement type X, etc.


# load in Res bldg file

resbldg <- read.csv(file = "resbldgedits.csv", head = T, sep = ",")
names(resbldg)

# Create field for res type. 1 = SF, 0 = duplex, mobile home, townhome
resbldg$ResTypeBin <- ifelse(resbldg$ResType == "Single Family", 1, 0)

# Field for FrameCons, 1 = Wood Frame, 0 = Other (block, metal)
resbldg$FrameConsBin <- ifelse(resbldg$ResFrameCons == "Wood Frame", 1, 0)

# Field for elec type, 1 = Breakers, 0 = Fuses
resbldg$ElecBin <- ifelse(resbldg$ResElec1 == "Breakers", 1, 0)

# Nums for Building Cond
resbldg$BldgCondNum <- ifelse(resbldg$ResBldgCond == "Unsound", 0, 
                              ifelse(resbldg$ResBldgCond == "Very Poor", 1, 
                                     ifelse(resbldg$ResBldgCond == "Poor", 2, 
                                            ifelse(resbldg$ResBldgCond == "Fair", 3, 
                                                   ifelse(resbldg$ResBldgCond == "Average", 4, 
                                                          ifelse(resbldg$ResBldgCond == "Good", 5,
                                                                 ifelse(resbldg$ResBldgCond == "Very Good", 6,
                                                                        ifelse(resbldg$ResBldgCond == "Excellent", 7, 4))))))))


# field for garage type. 1 = detached, 0 = attached of any type
resbldg$GarageBin <- ifelse(resbldg$ResGarageConnect == "Detached", 1, 0)

# field for heat fuel, 1 = gas, 0 = other (elec, steam, oil, etc.)
resbldg$HeatFuelBin <- ifelse(resbldg$ResHeatFuel == "Gas", 1, 0)

View(resbldg)


#RESIDENTIAL ANALYSIS
#merge fireems data with res bldg data
View(fireemsparcels)
bldgfire <- merge(resbldg, fireemsparcels, by = "PIN", all.x = T)

bldgfire$Fire <- ifelse(is.na(bldgfire$Fire), 0, bldgfire$Fire)
View(bldgfire)





#summary stats of buildings w/ fire compared to non-fire

bldgfire1 <- subset(bldgfire, Fire == 0)
bldgfire2 <- subset(bldgfire, Fire == 1)
View(bldgfire2)
mean(bldgfire1$ResAppraisVal)
mean(bldgfire2$ResAppraisVal)
mean(bldgfire1$ResEffAge)
mean(bldgfire2$ResEffAge)
mean(bldgfire1$BldgCondNum)
mean(bldgfire2$BldgCondNum)

aggbyHeat <- aggregate(PIN ~ ResHeat, data = bldgfire1, FUN = length)
aggbyHeat2 <- aggregate(PIN ~ ResHeat, data = bldgfire2, FUN = length)
View(aggbyHeat)
View(aggbyHeat2)


# Now compare EMS compared to buildings w/out EMS
bldgems <- subset(bldgfire, Labels == "EMS")
View(bldgems)
#compare bldgems to bldgfire1
bldgNo <- subset(bldgfire, is.na(Labels))
View(bldgNo)

mean(bldgems$ResAppraisVal)
mean(bldgNo$ResAppraisVal)
mean(bldgems$ResEffAge)
mean(bldgNo$ResEffAge)
mean(bldgems$BldgCondNum)
mean(bldgNo$BldgCondNum)



# Bring in enforcement data
parcelcomp <- read.csv("parcelcomp.csv", head = T, sep = ",")

aggbycat <- aggregate(PIN ~ Category, data = parcelcomp, FUN = length)
View(aggbycat)


aggbyPIN <- aggregate(Category ~ PIN, data = parcelcomp, FUN = length)
names(aggbyPIN)[2] <- "NumComplaints"
View(aggbyPIN)

#DONT NEED TO JOIN THIS ONE IN BUT USE FOR ANALYSIS, ifelse
aggbyPINsuperoffenders <- subset(aggbyPIN, NumComplaints > 4)
View(aggbyPINsuperoffenders)
# Agg parcels by ems
aggbyems <- aggregate(Address ~ PIN, data = bldgems, FUN = length)
names(aggbyems)[2] <- "NumEMS"
View(aggbyems)

aggbyfire <- aggregate(Address ~ PIN, data = bldgfire2, FUN = length)
names(aggbyfire)[2] <- "NumFire"
View(aggbyfire)

# merge together
resbldg1 <- merge(resbldg, aggbyPIN, by = "PIN", all.x = T)
resbldg2 <- merge(resbldg1, aggbyems, by = "PIN", all.x = T)
resbldg3 <- merge(resbldg2, aggbyfire, by = "PIN", all.x = T)
View(resbldg3)


# stats for parcels WITH COMPLAINTS
subcomp <- subset(resbldg3, resbldg3$NumComplaints > 0)
View(subcomp)
# % of complaint parcels that had EMS call
874/6401
# total number of EMS calls
sum(subcomp$NumEMS, na.rm = T)
# % of complaint parcels that had fire call
47/6401
# total number of fire calls
sum(subcomp$NumFire, na.rm = T)


# compare to parcels with NO Complaints
View(resbldg3)
# 18804 total parcels All parcels both comp and no comp
6401/18804
# complaints account for 34% of all parcels
subnocomp <- subset(resbldg3, is.na(resbldg3$NumComplaints))
View(subnocomp)
#12403 parcels had no complaints
# % of non complaint parcels that had EMS call
904/12403
# total number of EMS calls
sum(subnocomp$NumEMS, na.rm = T)
# % of non complaint parcels that had fire call
29/12403
# total number of fire calls
sum(subnocomp$NumFire, na.rm = T)

# Now let's check stats for superoffenders (those with 5 or more complaints)
superoffenders <- subset(resbldg3, resbldg3$NumComplaints > 4)
View(superoffenders)
# 1027 total parcels
# % of super offender parcels that had ems call
198/1027
# total number of ems calls
sum(superoffenders$NumEMS, na.rm = T)
# % of superoffender parcels that had fire call
35/1027
# total number of fire calls
sum(superoffenders$NumFire, na.rm= T)


# total stats for comparison sake
sum(resbldg3$NumEMS, na.rm = T)
sum(resbldg3$NumFire, na.rm = T)
sum(resbldg3$NumComplaints, na.rm = T)


View(resbldg3)



#TRACT DATA AND EMS COMPARISON -- FINDING OF NO STATISTICAL RELATIONSHIP
#subset PIN and numsEMS/Fire, etc.

# load fireemstract data
tractjoin <- read.csv(file = "fireemstract.csv", head = T, sep = ",")
dim(tractjoin)

#agg by tract
aggbytract3 <- aggregate(PIN ~ TRACT, data = tractjoin, FUN = length)
names(aggbytract3)[2] <- "NumFireEMS"

# merge tract growth data with aggbytract
tractgrowth2 <- merge(aggbytract3, tractgrowth, by.x = "TRACT", by.y = "Tract", all.x.y = T)

#new field, calls per person
tractgrowth2$callsperperson <- tractgrowth2$NumFireEMS / tractgrowth2$Pop2015
View(tractgrowth2)

#load in addtl data
tractscensus <- read.csv(file = "tractsdata.csv", head = T, sep = ",")
tractscensus[11,5] <- 55.01

#merge
tractgrowth3 <- merge(tractscensus, tractgrowth2, by.x = "tract", by.y = "TRACT", all.x.y = T)
View(tractgrowth3)
names(tractgrowth3)
# run reg
tractreg <- lm(NumFireEMS ~ MedAge + PercRO + MHV + 
                 MGR + PerCollDeg + PercFamPov + Pop2015 + 
                 Pop652015 + New.Const.Permits1316 + Demo.Permits1316, data = tractgrowth3)
summary(tractreg)
#
ggplot(tractgrowth3, aes(x = PercFamPov, y = NumFireEMS)) + geom_point(shape = 1) + geom_smooth()

names(tractgrowth2)
tractreg <- lm(NumFireEMS ~ Pop2015 + HH2015 + Pop652010 + New.Const.Permits1316 + Demo.Permits1316, data = tractgrowth2)
summary(tractreg)





#COMMERCIAL
# load in commercial data
combldg <- read.csv(file = "comindbldgedits.csv", head = T, sep = ",")
View(combldg)
comprimbldg <- subset(combldg, combldg$ComBldgNum == 1)
View(comprimbldg)

combldgfire <- merge(comprimbldg, fireemsparcels, by = "PIN", all.x = T)
View(combldgfire)


comEMS <- subset(combldgfire, combldgfire$Labels == "EMS")
View(comEMS)
comfire <- subset(combldgfire, combldgfire$Fire == 1)
View(comfire)

aggbytype <- aggregate(PIN ~ CommBldgType, data = comEMS, FUN = length)
View(aggbytype)
aggbytypef <- aggregate(PIN ~ CommBldgType, data = comfire, FUN = length)
View(aggbytypef)


# agg by PIN ems
combyems <- aggregate(Address ~ PIN, data = comEMS, FUN = length)
names(combyems)[2] <- "NumEMS"
View(combyems)

# agg by type
commtypesems <- aggregate(Address ~ CommBldgType, data = comEMS, FUN = length)
View(commtypesems)

# agg by PIN fire
combyfire <- aggregate(Address ~ PIN, data = comfire, FUN = length)
names(combyfire)[2] <- "NumFire"
View(combyfire)


# merge together
combldg1 <- merge(comprimbldg, aggbyPIN, by = "PIN", all.x = T)
combldg2 <- merge(combldg1, combyems, by = "PIN", all.x = T)
combldg3 <- merge(combldg2, combyfire, by = "PIN", all.x = T)
View(combldg3)

mean(combldg3$CommEffAge, na.rm = T)
mean(combldg3$CommEffAge, na.rm = T)
View(resbldg3)

# total stats for comparison, 2387 parcels reviewed
sum(combldg3$NumEMS, na.rm = T)
sum(combldg3$NumFire, na.rm = T)
sum(combldg3$NumComplaints, na.rm = T)

#com parcels with complaints
comcomp <- subset(combldg3, combldg3$NumComplaints > 0)
View(comcomp)
# 690 commercial parcels with a complaint
# % of com parcels with a complaint that had an ems call
197/690
# total number of ems calls
sum(comcomp$NumEMS, na.rm= T)
# % of com parcels with a complaint that had a fire call
18/690
# total number of fire calls
sum(comcomp$NumFire, na.rm = T)

# parcels without complaints
comnocomp <- subset(combldg3, is.na(combldg3$NumComplaints))
View(comnocomp)
# 1697 comm parcels without a complaint
# % of non comp parcels that had an ems call
220/1697
# total number of ems calls
sum(comnocomp$NumEMS, na.rm = T)
# % of non comp parcels that had a fire call
23/1697
# total number of fire calls
sum(comnocomp$NumFire, na.rm = T)

# com parcels with 3 or more complaints, super offenders
superoffcom <- subset(combldg3, combldg3$NumComplaints > 2)
View(superoffcom)
# 183 comm parcels with 3 or more complaints 
# % of super offenders that had an ems call
68/183
# total number of ems calls
sum(superoffcom$NumEMS, na.rm = T)
# % of super offenders that had a fire call
24/183
# total number of fire calls
sum(superoffcom$NumFire, na.rm = T)


# Find impact of new constr parcels on # of fire / ems calls
#subset to find just new const parcels

View(growth)

growthems <- merge(growth, aggbyems, by = "PIN", all.x = T)
growthemsfire <- merge(growthems, aggbyfire, by = "PIN", all.x = T)
View(growthemsfire)
dim(growth)
dim(growthemsfire)

resgrowth <- merge(growth, resbldg3, by = "PIN", all.x = T)
dim(growth)
rescommgrowth <- merge(resgrowth, combldg3, by = "PIN", all.x = T)
dim(rescommgrowth)

#Minimal impact

#export resbldg and combldg data, create gis map showing where there's concentrations of
# superoffender parcels

write.csv(combldg3, "combldg3.csv")
write.csv(resbldg3, "resbldg3.csv")
















# MISC

fireemsparcels2 <- merge(fireemsparcels2, fireemsparcels, by = "Address", all.x.y = T)
names(fireemsparcels2)
keepcols <- c(1, 4, 14:16, 22:23)
fireemsparcels3 <- unique(fireemsparcels2[, keepcols])
View(fireemsparcels3)
deduped.data <- unique( yourdata[ , 1:3 ] )

fireemsparcels2 <- fireemsparcels2[!duplicated(fireemsparcels2),]


View(fireemsstations)

fireemsparcels2 <- merge(fireemsstations, fireemsparcels2, by = "Address")
fireemsparcels2 <- fireemsparcels2[!duplicated(fireemsparcels2),]
names(fireemsparcels2)
keepcols <- c(1, 5:32)
fireemsparcels2 <- fireemsparcels2[,keepcols]



# try new data frame
incidentaddress <- fireemsstations$Address
incidentaddress <- data.frame(incidentaddress)
View(incidentaddress)
incidentaddress$spjoinadd <- calls$Address_2


View(fireemsparcels2)
fireemsparcels2$UseAddress <- ifelse(fireemsparcels2$Address_2 != fireemsparcels$Address, )

