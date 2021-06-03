#Data should be entered into an excel spreadsheet data.xlsx with appropriate column names
sigma = data$`sigma x`
bondlength = data$`nmr energy`
bondlength2 = data$`r(se-n) morph` # to plot a second bond length on the same graph
labels = as.expression(x = parse(text = data$substituent)) #labels for the points

double = FALSE # set to true if bondlength2 is being used

#Set up the plot with appropriate bounds
par(mar=c(4,4.5,4,3.5))
plot.new()
if(double) { plot.window(xlim=c(min(sigma,na.rm = TRUE)-0.1,max(sigma,na.rm = TRUE)+0.1),ylim=c(min(c(bondlength,bondlength2), na.rm = TRUE)-0.01,max(c(bondlength,bondlength2),na.rm = TRUE)+0.01))
  } else {plot.window(xlim=c(min(sigma,na.rm = TRUE)-0.1,max(sigma,na.rm = TRUE)+0.1),ylim=c(min(bondlength, na.rm = TRUE)-0.01,max(bondlength,na.rm = TRUE)+0.01))}
box()
axis(1,)
axis(2, las=1)

#A number of useful axis labels are here. Uncomment or edit as required
title(xlab = expression(sigma["p"]^""),  cex.lab=1.8)
#title(ylab = "r(Se-N) /Å", cex.lab=1.5)
#title(xlab = expression("pK"["HB"]^""),  cex.lab=1.5)
#title(ylab = expression(rho["BCP"]), cex.lab=1.5)
#title(ylab = expression(nabla^2~rho["BCP"]), cex.lab=1.5)
#title(ylab = "gradient", cex.lab=1.5)
title(ylab = expression(Delta*"G / kcal/mol"), cex.lab=1.5)


#Plot the data and labels. Offsets can be adjusted here  
text(sigma+0.02, bondlength, labels = labels, cex = 1, adj = c(0,0.5))
points(sigma,bondlength, pch = 16, cex=1.2)

#Fit a straight line to the data and print out the results
linfit = lm(bondlength ~ sigma)
print(summary(linfit))
abline(linfit)

#Plot and fit again if bondlength2 is being used
if(double) {
  bondlength = bondlength2
  text(sigma+0.02, bondlength, labels = labels, cex = 1, adj = c(0,0.5))
  points(sigma,bondlength, pch = 16, cex=1.2)
  
  linfit = lm(bondlength ~ sigma)
  print(summary(linfit))
  abline(linfit)
}