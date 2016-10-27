rcnt1 = function(f1){
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
    
    #t = ptdc(f1$forum[1])
    f2 = subset(f1, select = c(uniqueID, qid, localID, poster, datetime, forum, status,
                               CM, RI, RE, PE, PI, EI, CE))
    f2 = f2[order(f2$datetime), ]
    f2.1 = subset(prlst, corpus == f2$forum[1])
    if (nrow(f2.1) > 0 ){
      f2 = merge(f2, f2.1, by.x = 'poster', by.y = 'person')
      
      plist = unique(f2$poster) #a list of unique posters
      f7 = vector()
      for (k in 1:length(plist)){ #for each poster
        f3 = subset(f2, poster == plist[k])
        f4 = subset(f3, select = c(qid, poster, datetime, status, CM, RI, RE, PE, PI, EI, CE))
        f4 = f4[order(f4$datetime), ] #a list of posts created by the poster 
        
        names(f4) = c('qid', 'poi', 'poidate', 'status', 'CM', 'RI', 'RE', 'PE', 'PI', 'EI', 'CE')
        rownames(f4) = NULL
        #f4$tdif = c(0, log(as.numeric(f4$poidate[2:nrow(f4)]) - as.numeric(f4$poidate[1:(nrow(f4)-1)]) + 1))
        f4$month = cut(f4$poidate, 'month')
        f5 = subset(f2, (f2$datetime < f4$poidate[1]) & (f2$datetime >= f4$poidate[1] - 2592000) )
        f51 = matrix(nrow = nrow(f4), ncol = 7)
        f51[1, ] = as.vector(table(factor(f5$status, levels = 2:8)))
        if (nrow(f4) > 1){
          for (l in 2:nrow(f4)){
            f6 = subset(f2, (f2$datetime < f4$poidate[l]) & (f2$datetime > max(f4$poidate[l-1], f4$poidate[l] - 2592000)))
            f51[l, ] = as.vector(table(factor(f6$status, levels = 2:8)))
          }
        }
        
        f7 = rbind(f7, data.frame(f51, f4))
        f7$status = factor(f7$status, levels = 2:8)
      }
      f7 = as.data.frame(f7)
      # f7$poi = rownames(f7)
  
      f8 = aggregate(c(f7[,1:7]), list(f7$poi, f7$month), sum)
      f9 = aggregate(c(f7[,12:18]), list(f7$poi, f7$month), sum)
      f10 = merge(f8, f9, by = c('Group.1', 'Group.2'))
      f10$forum = f2$forum[1]
  
      names(f10) = c('person', 'month', 'ECM', 'ERI', 'ERE', 'EPE', 'EPI', 'EEI', 'ECE',
                     'CM', 'RI', 'RE', 'PE', 'PI', 'EI', 'CE', 'forum')
      f10$person = as.factor(f10$person)
  
      return(as.data.frame(f10))
    }
  }
}
