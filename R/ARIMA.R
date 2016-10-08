#  http://freakonometrics.hypotheses.org/347
report <- read.table("Data/report-headphones.csv", skip = 4, header = TRUE, 
                     sep = ",", nrows = 464)
source("http://freakonometrics.blog.free.fr/public/code/H2M.R")
headphones = H2M(report, lang = FR, type = "ts")
plot(headphones)
