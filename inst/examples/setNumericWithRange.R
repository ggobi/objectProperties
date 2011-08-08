newclass <- setNumericWithRange("Numeric", min = 1, max = 100)
newclass
par.gen <- setParameters("Graph", list(size = newclass))
pars <- par.gen$new(size = new(newclass, 200)) # out of range
pars <- par.gen$new(size = new(newclass, 5))
pars$size #current value is 5
pars$size <- 300 # out of range error
pars$size <- 10 #works
