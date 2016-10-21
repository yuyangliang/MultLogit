library('DBI')

source('dbk.R')

#connect to the database
c =  dbConnect(RMySQL::MySQL(), 
               username = u, 
               password = p,
               host = h, 
               dbname = d
)

flist = list.files(path = './Classification Result', pattern = '.csv')
f = list()
dir = './Classification Result/'
for (i in 1:length(flist)) f[[i]] = read.csv(file = paste0(dir, flist[i]), header = T, stringsAsFactors = F)
for (i in 1:length(flist)) f[[i]]$forum = sub("^([^.]*).*", "\\1", flist[i])

#obtain inferred_replies
ptdc = function(tbl){
  sql = sprintf("SELECT uniqueID, inferred_replies FROM %s", tbl)
  t = dbGetQuery(c, sql)
  return(t)
}

#manipulate the dataset
rcnt = function(f1){
  for (i in 1:nrow(f1)){
    f1$type[i] = paste0(f1[i,9], f1[i,10], f1[i,11], f1[i,12], f1[i,13])
  }
  f1$datetime = as.POSIXct(f1$date, format = '%m/%d/%Y %R', tz = 'EST', usetz = F)
  f1 = f1[order(f1$qid, f1$localID, f1$datetime), ]
  if (nrow(f1) > 0 ){
    f1$status = f1$CM = f1$RI = f1$RE = f1$PE = f1$PI = f1$EI = f1$CE = 0
    f1$status[f1$type == '00000'] = 1
    f1$status[f1$type == '10000'] = 2
    f1$status[f1$type == '01000'] = 3
    f1$status[f1$type == '00100'] = 4
    f1$status[f1$type == '00010'] = 5
    f1$status[f1$type == '00001'] = 6
    f1$status[f1$type == '00011'] = 7
    f1$status[f1$type == '10010'] = 8
    
    f1$CM[f1$type == '10000'] = 1
    f1$RI[f1$type == '01000'] = 1
    f1$RE[f1$type == '00100'] = 1
    f1$PE[f1$type == '00010'] = 1
    f1$PI[f1$type == '00001'] = 1
    f1$EI[f1$type == '00011'] = 1
    f1$CE[f1$type == '10010'] = 1
    
    f1 = subset(f1, status > 1) #categorize the posts
    
    t = ptdc(f1$forum[1])
    f2 = subset(merge(f1, t, by = 'uniqueID'), select = c(uniqueID, qid, localID, poster, datetime, forum, inferred_replies, status,
                                                          CM, RI, RE, PE, PI, EI, CE))
    f2 = f2[order(f2$datetime), ]
    
    plist = unique(f1$poster) #a list of unique posters
    f7 = vector()
    for (k in 1:length(plist)){ #for each poster
      f3 = subset(f2, poster == plist[k])
      f4 = subset(f3, select = c(qid, poster, datetime, status, CM, RI, RE, PE, PI, EI, CE))
      f4 = f4[order(f4$datetime), ] #a list of posts created by the poster 
      
      names(f4) = c('qid', 'poi', 'poidate', 'status', 'CM', 'RI', 'RE', 'PE', 'PI', 'EI', 'CE')
      rownames(f4) = NULL
      if (nrow(f4) > 1){#for each post, obtain a list of posts from the whole forum that came before the post
        #f4$tdif = c(0, log(as.numeric(f4$poidate[2:nrow(f4)]) - as.numeric(f4$poidate[1:(nrow(f4)-1)]) + 1))
        f4$month = cut(f4$poidate, 'month')
        f5 = subset(f2, f2$datetime < f4$poidate[1])
        f51 = table(factor(f5$status, levels = 2:8))
        for (l in 2:nrow(f4)){
          f6 = subset(f2, (f2$datetime < f4$poidate[l]) & (f2$datetime > f4$poidate[l-1]))
          f61 = table(factor(f6$status, levels = 2:8))
          f51 = rbind(f51, f61)
        }
        rownames(f51) = NULL
        colnames(f51) = seq(2, 8)
        f7 = rbind(f7, data.frame(f51, f4))
        f7$status = factor(f7$status, levels = 2:8)
      }
    }
    # f7 = as.data.frame(f7)
    # f7$poi = rownames(f7)
    return(as.data.frame(f7))
  }
}

cnth = list()
for (i in 1:length(flist)){
  cnth[[i]] = rcnt(f[[i]])
}

cnth = rcnt(f[[1]])

t = aggregate(c(cnth[,1:7]), list(cnth$poi, cnth$month), sum)
u = aggregate(c(cnth[,12:18]), list(cnth$poi, cnth$month), sum)
v = merge(t, u, by = c('Group.1', 'Group.2'))
v$tot = apply(v[, 10:16], 1, sum)

adlst = rbind(subset(crlst, corpus == 'add_and_adhd_exchange'), subset(frlst, corpus == 'add_and_adhd_exchange'))
v = merge(v, adlst, by.x = 'Group.1', by.y = 'person')
v$class[v$class == 'F'] = 0
v$class[v$class == 'C'] = 1

names(v)[names(v) == 'Group.1'] = 'person'
v$person = as.factor(v$person)
v$class = as.factor(v$class)

library(glmmADMB)
m = glmmadmb(tot ~ X2 + X3 + X4 + X5 + X6 + X7 + X8 + class + (1|person), data = v, family = 'nbinom1')
plot(fitted(m), residuals(m, type="pearson")) 
lines(smooth.spline(fitted(m), residuals(m, type="pearson")))  
m = glmmadmb(CM ~ X2 + X3 + X4 + X5 + X6 + X7 + X8 + class + (1|person), data = v, family = 'nbinom1')

#output the parameters
out = summary(m)
out.1 = matrix(nrow = 1, ncol = 10)
out.1[1, ] = c(out$b, out$sd_S$person[1])
colnames(out.1) = c('IN', 'CM', 'RI', 'RE', 'PE', 'PI', 'IE', 'CE', 'CL', 'RS')
write.csv(out.1, file = 'paras.csv', row.names = F)

library(RVAideMemoire)
plotresid(m)

# #the multinomial model
# 
# library('MCMCglmm')
# 
# k = length(levels(cnth$status))
# I = diag(k-1) 
# J = matrix(rep(1, (k-1)^2), c(k-1, k-1))
# priors = list(R = list(fix=1, V=(1/k) * (I + J), n = k - 1), G = list(G1 = list(V = diag(k - 1), n = k - 1))) 
# priors = list(R = list(fix=1, V=(1/k) * (I + J), n = k - 1), G = list(G1 = list(V = k, n = k - 1))) 
# 
# m = MCMCglmm(status ~ -1 + X2 + X3 + X4 + X5 + X6 + X7 + X8 + tdif + trait,
#              random = ~idh(trait):poi,
#              rcov = ~idh(trait):units,
#              prior = priors,
#              family = "categorical",
#              data = cnth)
# 
