

orings <- read.csv("./data/oring.csv")
orings

colnames(orings)

out <- lm(orings  ~ Temperature, data = orings)

summary(out)

conjoint <- read.csv("./data/conjoint.csv")
conjoint

out <- lm(Rank~Pack_A+Pack_B+Brand_K+Brand_G+Pr19+Pr39+Seal+M_back,data=conjoint)
summary(out)
