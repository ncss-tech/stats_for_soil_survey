# advanced: inspecting the SoilProfileCollection S4 object

library(aqp, warn.conflicts = FALSE)

data("jacobs2000", package = "aqp")
jacobs2000 <- jacobs2000 # replace this with your own data

# only var in site table is the id
site(jacobs2000)

# look at object names
names(jacobs2000)

# the names of the names identify the slot source
site(jacobs2000)

# add a variable to the site table
site(jacobs2000)$foo1 <- runif(length(jacobs2000))
before <- names(jacobs2000)
names(before)

# now site1 site2 for foo1 and foo2 respectively
site(jacobs2000)$foo2 <- runif(length(jacobs2000))
after <- names(jacobs2000)
names(names(jacobs2000))

# should you still be able to get foo1 and foo2 values via e.g. spatial1 and spatial2 as if they were site?
# we could prevent _setting_ unless certain conditions are met
coordinates(jacobs2000) <- ~ foo1 + foo2
sptest <- names(jacobs2000)
names(sptest)
