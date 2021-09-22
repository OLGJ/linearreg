## ----setup--------------------------------------------------------------------
library(linearreg)

# linear regression will be calculated on the iris dataset.
a <- linreg(formula = Petal.Length~Species, data = iris)

# To see an overview:
a$print()

# Or check out the built in .self$ variables
a$residuals

# It's possible to press tab after a$ to get an overview.

# To access the methods of the package you proceed in a similar fashion.
# Accessing the methods can be done via:
a$summary()


