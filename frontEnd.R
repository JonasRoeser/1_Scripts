# install.packages("shiny")
library(shiny)
options(shiny.port = 4674) # set some port (maybe not necessary) --> set port from console
options(shiny.host = "130.82.249.4") # set IP by copying IPv4 Address from cmd --> ipconfig
runExample("01_hello")
