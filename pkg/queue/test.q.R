source("../background/background.R")
source("q.R")

q <- redis.q$new()
q$push(1)
q$push(2)
q$push(3)

q$pop()
q$pop()
