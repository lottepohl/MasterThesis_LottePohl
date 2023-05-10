library(GENEAread)
library(e1071)

# GENEAread ####

#Some artificial data
time = 1:5000
#sum of two sine curves at 0.3 Hz and 0.05 Hz
f1 = 0.3; f2 = 0.05
sin1 = sin(time * f1 * 2*pi)
sin2 = sin(time * f2 * 2*pi)
#add a bit of noise
signal = sin1 + sin2 + 1*rnorm(5000)
#non-reassigned
GENEAread::stft(signal, plot = TRUE, reassign = FALSE, win = 100)
#reassigned
GENEAread::stft(signal, plot = TRUE, reassign = TRUE, win = 100)

#add a third component: varying frequency.
GENEAread::stft(signal + sin(cumsum(seq(f2, f1, length = 5000))*2*pi),
     plot = TRUE, reassign = TRUE, win = 100)

# Real data
binfile  = system.file("binfile/TESTfile.bin", package = "GENEAread")[1]

# Read in the entire file, calibrated
procfile<-read.bin(binfile)
# Default is mv
GENEAread::stft(procfile, plot.it = TRUE)
# Try sum?
GENEAread::stft(procfile, plot.it = TRUE, type = "sum", reassign = FALSE)

# Just look at the last 50% of the data
GENEAread::stft(procfile, start = 0.5, plot.it = TRUE)

# not reassigned, svm
GENEAread::stft(procfile, type = "svm", reassign = FALSE, plot.it = TRUE)
# a narrower 5 second window means better time resolution
GENEAread::stft(procfile, type = "svm", reassign = FALSE, plot.it = TRUE, win = 5)
# choose increments so as not to overlap
GENEAread::stft(procfile, type = "svm", reassign = FALSE, plot.it = TRUE, win = 5, inc = 5)
# uniform windows
GENEAread::stft(procfile, type = "svm", reassign = FALSE, plot.it = TRUE, wtype = "uniform.window")
# Svm, reassigned, quietly
obj = GENEAread::stft(procfile, type = "svm", quiet = TRUE)
plot(obj, cex = 3, showmax = FALSE, mode = "pval")

#example code
plot(GENEAread::stft(subs(mag, 0.94,0.96), win = 1024, plot = F, coef = 512), zlog = T, log="y")
plot(GENEAread::stft(subs(mag, 0.7,8), win = 1024, plot = F, coef = 512), zlog = T, log="y")
plot(GENEAread::stft(subs(mag, 0.0001,0.005), win = 1024, plot = F, coef = 512), zlog = T)
plot(GENEAread::stft(subs(mag, 0.7,0.8), win = 1024, plot = F), zlog = T, log = "y")

plot(GENEAread::stft(rep(1, 1000) +
            c(sin(1:500/ 10 * 2*pi), rep(0, 500)) +
            c(rep(0, 300),sin(1:500/ 20 * 2*pi), rep(0, 200)),
          freq = 1, plot.it = F), log="x")

# doesn't work
GENEAread::stft(sin(1:1000 / (1 +sqrt(1000:1)) * 2 * pi), freq = 1)
GENEAread::stft(rep(1, 1000) + sin(1:1000/ 10 * 2*pi), freq = 1)

# e1071 ####

x<-rnorm(500)
plot(x, type = "l")
y<-e1071::stft(x)
plot(y)
