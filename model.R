source('xperi.R')
source('crpr.R')
library('caret')

cfexp = data.frame()
for (i in 1:length(flist)){
  tmp = rcnt(f[[i]])
  cfexp = rbind(cfexp, tmp)
}

xpexp = data.frame()
for (i in 1:length(flist)){
  tmp = rcnt1(f[[i]])
  xpexp = rbind(xpexp, tmp)
}

cfexp$class1 = as.numeric(cfexp$class) - 1

cfexp.all = cfexp
cfexp = subset(cfexp.all, forum == 'diabetes_exchange' )

tmp = cfexp[!duplicated(cfexp$person), ]
tmp1 = createFolds(tmp$class, k = 5)
tmp$fold = 0

for (i in 1:5) tmp$fold[tmp1[[i]]] = i
cfexp = merge(cfexp, tmp[, c(1, 20)], by = 'person')

library('MCMCglmm')
m.cf = list()
prior = list(R = list(V = diag(1), nu = 0.002), G = list(G1 = list(V = diag(1), nu = 0.002)))

m.cf[[1]] = MCMCglmm(CM ~ ECM + ERI + ERE + EPE + EPI + EEI + ECE + class1 - 1, random = ~person, 
                  data = cfexp, family = 'poisson', prior = prior, pr = T)
m.cf[[2]] = MCMCglmm(RI ~ ECM + ERI + ERE + EPE + EPI + EEI + ECE + class1 - 1, random = ~person, 
                  data = cfexp, family = 'poisson', prior = prior, pr = T)
m.cf[[3]] = MCMCglmm(RE ~ ECM + ERI + ERE + EPE + EPI + EEI + ECE + class1 - 1, random = ~person, 
                  data = cfexp, family = 'poisson', prior = prior, pr = T)
m.cf[[4]] = MCMCglmm(PE ~ ECM + ERI + ERE + EPE + EPI + EEI + ECE + class1 - 1, random = ~person, 
                  data = cfexp, family = 'poisson', prior = prior, pr = T)
m.cf[[5]] = MCMCglmm(PI ~ ECM + ERI + ERE + EPE + EPI + EEI + ECE + class1 - 1, random = ~person, 
                  data = cfexp, family = 'poisson', prior = prior, pr = T)
m.cf[[6]] = MCMCglmm(EI ~ ECM + ERI + ERE + EPE + EPI + EEI + ECE + class1 - 1, random = ~person, 
                  data = cfexp, family = 'poisson', prior = prior, pr = T)
m.cf[[7]] = MCMCglmm(CE ~ ECM + ERI + ERE + EPE + EPI + EEI + ECE + class1 - 1, random = ~person, 
                  data = cfexp, family = 'poisson', prior = prior, pr = T)

#output the parameters

out = matrix(nrow = 7, ncol = 10)
colnames(out) = c('CM', 'RI', 'RE', 'PE', 'PI', 'EI', 'CE', 'CL', 'GV', 'RV')
for (i in 1:7){
  tmp = summary(m.cf[[i]])
  out[i, ] = c(tmp$solutions[,1], sqrt(tmp$Gcovariances[1]), sqrt(tmp$Rcovariances[1]))
}
write.csv(out, file = 'paras.csv', row.names = F)

com = vector()
for (i in 1:7){
  tmp = rbind(summary(cfexp[,(9+i)]), summary(simulate.MCMCglmm(m.cf[[i]], type = 'terms')))
  com = rbind(com, tmp)  
}

write.csv(com, file = 'sim.csv', row.names = F)

xpexp$tot = apply(xpexp[, 3:9], 1, sum)
xpexp.all = xpexp
xpexp = subset(xpexp.all, forum == 'diabetes_exchange' )

prior1 = list(R = list(V = diag(1), nu = 0.002))
m.xp = list()
m.xp[[1]] = glm(RI ~ ERI + ERE + EPE + EPI + EEI, data = xpexp, family = 'poisson')
m.xp[[2]] = glm(RE ~ ERI + ERE + EPE + EPI + EEI, data = xpexp, family = 'poisson')
m.xp[[3]] = glm(PE ~ ERI + ERE + EPE + EPI + EEI, data = xpexp, family = 'poisson')
m.xp[[4]] = glm(PI ~ ERI + ERE + EPE + EPI + EEI, data = xpexp, family = 'poisson')
with(m.xp[[4]], cbind(res.deviance = deviance, df = df.residual, p = pchisq(deviance, df.residual, lower.tail=FALSE)))

out = matrix(nrow = 4, ncol = 6)
colnames(out) = c('IN', 'RI', 'RE', 'PE', 'PI', 'EI')
for (i in 1:4){
  tmp = summary(m.xp[[i]])
  out[i, ] = c(tmp$coefficients[,1])
}
write.csv(out, file = 'paras1.csv', row.names = F)

com1 = vector()
for (i in 1:4){
  tmp = rbind(summary(xpexp[,(10+i)]), summary(simulate(m.xp[[i]])$sim_1))
  com1 = rbind(com1, tmp)  
}
write.csv(com1, file = 'sim1.csv', row.names = F)

tmp2 = as.matrix(xpexp[, 4:8])
tmp3 = rpois(16959, exp(tmp2 %*% out[1, 2:6] + out[1, 1]))
