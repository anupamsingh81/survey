jharx = jhar6 %>% select(one:Twenty.three)
library(psych)
library(GPArotation)

parallel <- fa.parallel(jharx, fm = 'minres', fa = 'fa')

fivefactor <- fa(jharx,nfactors = 5,rotate = "oblimin",fm="minres")
print(fivefactor)

print(fivefactor$loadings,cutoff = 0.3)

fa.diagram(fivefactor)

summary(fivefactor$loadings)

str(fivefactor$loadings)


detach("MASS")

detach("package:MASS", unload=TRUE)

