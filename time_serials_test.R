kings <- scan("http://robjhyndman.com/tsdldata/misc/kings.dat",skip=3)

births <- scan("http://robjhyndman.com/tsdldata/data/nybirths.dat")
birthstimeseries <- ts(births, frequency=12, start=c(1946,1))

rain <- scan("http://robjhyndman.com/tsdldata/hurst/precip1.dat",skip=1)
rainseriels <- ts(rain,start = c(1813))
plot.ts(rainseriels)
rainseries <-HoltWinters(rainseriels,beta = F,gamma = F)
