raneff = function(x){
  tmp = as.data.frame(m.cf[[x]]$Sol)[, -(1:8)]
  tmp1 = cbind(gsub('person.', '', names(tmp)), as.vector(apply(tmp, 2, mean)))
  colnames(tmp1) = c('person','GV')
  cfexp = merge(cfexp[, -21], tmp1, by = 'person')
  return(cfexp)
}

cv.rmse = function(x){
  MSE = vector()
  for (i in 1: 5){
    t.0 = as.formula(paste(x, 'ECM + ERI + ERE + EPE + EPI + EEI + ECE + class1 - 1', sep = ' ~ '))
    t.1 = MCMCglmm(t.0, random = ~person, data = cfexp[cfexp$fold != i, ], family = 'poisson', prior = prior, verbose = F)
    t.5 = as.vector(summary(t.1)$solutions[,1])
    t.6 = cfexp[cfexp$fold == i, c(3:16, 19, 21)]
    t.6$fitted = floor(exp(as.matrix(t.6[, c(1:7, 15)]) %*% t.5 + as.numeric(levels(t.6$GV))[t.6$GV]))
    MSE = c(MSE, (t.6$fitted - t.6$CM)^2)
  }
  return(sqrt(sum(MSE)/length(MSE)))
}

dname = names(cfexp)[10:16]
cfrmse = vector()

library('MCMCglmm')

for (i in 1:7) {
  cfexp = raneff(i)
  cfrmse[i] = cv.rmse(dname[i])
}

library('boot')
xprmse = vector()
for (i in 1:4) {
  xprmse[i] = sqrt(cv.glm(xpexp, m.xp[[i]], K = 10)$delta[1])
}
