#function to compute capture rate for a round stimulus 
#with a uniform fixation distribution.

capturerate = function(acc, pre, size) {
  
  #set parameters
  iter = 100000 
  xll = 0
  xul = size
  yll = 0
  yul = size
  
  #generate x,y positions
  true_x = runif(iter, xll, xul)
  true_y = runif(iter, yll, yul)
  
  #generate offset x,y positions
  angle = runif(iter, 0, 2*pi)
  dist = rnorm(iter, acc, pre)
  offset_x = true_x + cos(angle)*dist
  offset_y = true_y + sin(angle)*dist
  
  #join data and identify circle perimeter
  dat = data.frame(true_x,true_y,offset_x,offset_y)
  dat$distance = sqrt((dat$true_x - xul/2)^2 + (dat$true_y - yul/2)^2)
  dat$circle = ifelse(dat$distance <= xul/2, 1, 0)
  dat$off_dist = sqrt((dat$offset_x - xul/2)^2 + (dat$offset_y - yul/2)^2)
  
  #compute capture rate
  dat$capture = ifelse(dat$off_dist <= xul/2, 1, 0)
  capturerate = mean(dat$capture[dat$circle == "1"])
  return(capturerate)
  
}


#function to compute capture rate for a square stimulus 
#with a uniform fixation distribution.
capturerate_poly = function(acc, pre, height, width) {
  
  #set parameters
  iter = 100000 
  xll = 0
  xul = width
  yll = 0
  yul = height
  
  #generate x,y positions
  true_x = runif(iter, xll, xul)
  true_y = runif(iter, yll, yul)
  
  #generate offset x,y positions
  angle = rep(runif(1, 0, 2*pi),iter)
  dist = rnorm(iter, acc, pre)
  offset_x = true_x + cos(angle)*dist
  offset_y = true_y + sin(angle)*dist
  
  #joining data 
  dat = data.frame(true_x,true_y,offset_x,offset_y)
  
  #compute capture rate
  dat$capture = ifelse(dat$offset_x >= xll & 
                         dat$offset_x <= xul &
                         dat$offset_y >= yll &
                         dat$offset_y <= yul, 1, 0) 
  capturerate = mean(dat$capture)
  return(capturerate)
  
} 