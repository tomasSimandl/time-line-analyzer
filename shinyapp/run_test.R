library(RUnit) 

source("shinyapp/server.R")

test.suite <- defineTestSuite("server tests",
                              dirs = file.path("shinyapp/tests"),
                              testFileRegexp = '^\\d+\\.R')

test.result <- runTestSuite(test.suite)

printTextProtocol(test.result)