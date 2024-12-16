# All data Regression for All Variables Figure
## Constructing the specific relationships between each of the variables, working out the R-squared values

attach("Proxies.xlsx")
summary(Proxies) # shows a summary of the data for each of the variables
plot(Proxies, lower.panel=NULL) # Plotting the multi-plot to see the relationships between inertinite, co2, o2 and biomass
data(Proxies)

### CO2 and O2
wfdata.o2co2<-lm(CO2~O2, Proxies)
abline(reg=wfdata.o2co2)
summary(wfdata.o2co2)

### CO2 and time
wfdata.timeco2<-lm(CO2~time, Proxies)
abline(reg=wfdata.timeco2)
summary(wfdata.timeco2)

### CO2 and inertinite
wfdata7.inertco2<-lm(CO2~inertinite, Proxies)
abline(reg=wfdata7.inertco2)
summary(wfdata7.inertco2)

### CO2 and biomass
wfdata.bioco2<-lm(CO2~biomass, Proxies)
abline(reg=wfdata.bioco2)
summary(wfdata.bioco2)

### CO2 and temperature
wfdata.tempco2<-lm(CO2~temp, Proxies)
abline(reg=wfdata.tempco2)
summary(wfdata.tempco2)

### O2 and time
wfdata.timeo2<-lm(O2~time, Proxies)
abline(reg=wfdata.timeo2)
summary(wfdata.timeo2)

### O2 and inertinite
wfdata.inerto2<-lm(inertinite~O2, Proxies)
abline(reg=wfdata.inerto2)
summary(wfdata.inerto2)

### O2 and biomass
wfdata.bioo2<-lm(O2~biomass, Proxies)
abline(reg=wfdata.bioo2)
summary(wfdata.bioo2)

### O2 and temperature
wfdata.tempo2<-lm(O2~temp, Proxies)
abline(reg=wfdata.tempo2)
summary(wfdata.tempo2)

### inertinite and biomass
wfdata.inertbio<-lm(inertinite~biomass, Proxies)
abline(reg=wfdata.inertbio)
summary(wfdata.inertbio)

### inertinite and time
wfdata.inerttime<-lm(inertinite~time, Proxies)
abline(reg=wfdata.inerttime)
summary(wfdata.inerttime)

### inertinite and temperature
wfdata.inerttemp<-lm(inertinite~temp, Proxies)
abline(reg=wfdata.inerttemp)
summary(wfdata.inerttemp)

### inertinite and temperature
wfdata.biotemp<-lm(biomass~temp, Proxies)
abline(reg=wfdata.biotemp)
summary(wfdata.biotemp)

### temperature and time
wfdata.temptime<-lm(temp~time, Proxies)
abline(reg=wfdata.temptime)
summary(wfdata.temptime)

### biomass and time
wfdata.biotime<-lm(biomass~time, Proxies)
abline(reg=wfdata.biotime)
summary(wfdata.biotime)


attach("Proxies.xlsx")
summary(Proxies)
plot(Proxies, lower.panel=NULL)
data(Proxies)

install.packages("palaeoverse") # installing palaeoverse packages
library(palaeoverse) # attach palaeoverse



plot(biomass~time, xlim = rev(range(time)), col ="green4", pch = 17, Proxies)
axis_geo(side = 1, intervals = "epochs")


plot(CO2~time, xlim = rev(range(time)), col ="red4", pch = 18, Proxies)
axis_geo(side = 1, intervals = "epochs")


plot(inertinite~time, xlim = rev(range(time)), col ="sienna4", pch = 15, Proxies)
axis_geo(side = 1, intervals = "epochs")


plot(temp~time, xlim = rev(range(time)), col ="orangered", pch = 25, Proxies)
axis_geo(side = 1, intervals = "epochs")


plot(O2~time, xlim = rev(range(time)), col ="blue4", pch = 19, Proxies)
axis_geo(side = 1, intervals = "epochs")

