### Sinusoidal temperatures for Melanie

# Here we place min and max temperatures which are as close as possible to IPCC
# average min and max for the future growing season while keeping the same average
# temperature for the period estimated by IPCC
temperatures <- data.frame(treatments = c("ambient", "heat wave"),
                        mean_temp = c(26.5,31.5),#average temperatures observed during regular growing season and heat wave in Thailand 
                        min_temp = c(22,27.1), #min temperatures observed during growing season; heat wave = observed mininum + 5C
                        max_temp = c(31,36)) #max temperatures observed during growing season; heat wave = observed maximum + 5C

## Function to create daily cycles
# This function creates an array filled with values based on a sine wave pattern
sinusoidalArray <- function (size = 10, min = -1, max = 1, subdivide = 1) {
  
  # Create an array 'ax' of the specified 'size', starting with zeros
  ax = array(0, size)
  
  # Loop through each index from 0 to 'size'
  for(i in 0:size) {
    
    # Calculate the sine wave value for the current index 'i'
    # Adjust the value to fit within the range specified by 'min' and 'max'
    ax[i] = min + (max - min) * (0.5 + 0.5 * sin(2 * pi * (i / subdivide) / 24))
  }
  
  # Return the filled array
  return(ax)
}

# Setting minimum and maximum temperatures 
size = 24 #hours in a day

ambient <- sinusoidalArray(size,temperatures$min_temp[temperatures$treatments=="ambient"],
                           temperatures$max_temp[temperatures$treatments=="ambient"])
heat_wave <- sinusoidalArray(size,temperatures$min_temp[temperatures$treatments=="heat wave"],
                           temperatures$max_temp[temperatures$treatments=="heat wave"])

sd(ambient)
sd(heat_wave)

# Ploting
par(mar = c(5, 6, 4, 2))
jpeg(filename = "myplot%d.jpeg", 
     width = 8, 
     height = 10, 
     units = "cm", 
     res = 600)

plot(ambient,type="l",col="dark blue",ylim=c(20,37), las=1,
     xlab="Time (hours)", ylab="Temperature (°C)",
     lwd=2, cex.axis=.9, cex.lab=1.1, bty = "l",
     xlim=c(0,25))
lines(heat_wave,type="l",col="goldenrod3",lwd=3, lty=3)
legend("bottomleft", legend=c("Season average", "Heatwave"),
       col=c("dark blue", "goldenrod3"), lty=c(1,3), 
       cex=.7, bty = "n", lwd=1.5)

dev.off()