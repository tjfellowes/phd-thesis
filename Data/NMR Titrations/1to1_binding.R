#Define the data object (import from excel or csv)
data=bn_ebs_dmap

#Define the column names for the concentration and chemical shift
x=data$'c dmap'
y=data$'d shift'

#Define the binding function
binding = function(Bmax,Kd,conc) {(Bmax*conc)/(Kd + conc)}

#Defines sequences for the lines of best fit
line_x = seq(0,max(x,na.rm = TRUE),max(x,na.rm = TRUE)/100)
line_y = seq(0,max(y,na.rm = TRUE),max(y,na.rm = TRUE)/100)

#Sets up and plots the main data
par(fig = c(0,1,0,1), mar = c(5, 5, 1, 1))
plot(x,y,ylab = expression(Delta(delta)/ppm),xlab = "[base]/M", pch=16, cex = 1.3, cex.axis=1.5, cex.lab=1.8)

#Fits the model to the data
model=nls(y~binding(Bmax=Bmax,Kd=Kd,conc=x),start = list(Bmax=60,Kd=0.1))
params = model$m$getPars()

#Plots the model
lines(x = line_x, y = binding(params["Bmax"],params["Kd"],line_x))

#Sets up and plots the inset Scatchard plot with line from nls parameters (SCATCHARD PLOT IS NOT USED FOR FITTING)
par(fig = c(0.5,0.97,0.15,0.75), new=T)
plot(y,y/x,ylab = expression("[base]"/Delta(delta)),xlab = expression(Delta(delta)/ppm), pch=16, cex.axis=1.25, cex.lab=1.5)
lines(x = line_y, y = -1/params["Kd"]*line_y + params["Bmax"]/params["Kd"])

#Saves the plot as a pdf
dev.copy2pdf(file='plot.pdf', width=8, height=6)

#Calculates the important parameters for the model
esd = summary(model)$parameters[2,2] 
Kd=c(params["Kd"]-esd,params["Kd"],params["Kd"]+esd)
deltaG = 8.314*298*log(Kd)/4200
unc = signif(mean(deltaG[2]-deltaG[3],deltaG[1]-deltaG[2]), digits = 2)

#Prints the parameters
print(model)
cat('\n')
cat(c('delta G =',mean(deltaG[2]), '+/-', abs(unc), 'kcal/mol'),fill=TRUE)

