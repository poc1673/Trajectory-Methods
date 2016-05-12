#  Peter Caya
#  The purpose of this script is to build up a market model for several different securities using an industry and general market index.
#  The training period for the data will be from the beginning of 2014 to June 2015.
#  The testing period for the data will be from the ending of the training period to the twentieth of April.
#  The first stock I will test will be FSC:  Fifth Street Financial corporation.



# 1  -  Download Data -----------------------------------------------------

libs = c('quantmod','Quandl', 'ggplot2','reshape')
lapply( libs,library,character.only = T   )

#  Fifth Street will be compared with the SP 500, several competitors, a few possible general market indexes, and a few industry indexes.
FSC.eqs = c('FSC','ARCC' , 'GLAD') 
IND = c('XLF','KBE','VFH','IYF','RYF' )
GEN = c('SPY', 'SLY','^IXIC','^DJI' )  

getSymbols(FSC.eqs,source='yahoo')
getSymbols(IND,source='yahoo')
getSymbols(GEN,source='yahoo')

allstocks = c(FSC.eqs,IND,'SPY','SLY','IXIC','DJI')
for (i in 1:length(allstocks))
{
  assign(allstocks[i], as.data.frame(eval(parse(text = allstocks[i])))   )
}


# 2  -   Format Data -------------------------------------------------------------

train.range = c('2014-01-01','2015-06-01')
test.range =  c('2015-06-01', '2016-04-20')

training.data = FSC.train[6]

for (i in 1:length(allstocks))
{
  assign(allstocks[i], as.data.frame(eval(parse(text = allstocks[i])))   )
  assign(paste(allstocks[i],'.train',sep = ''),  eval(parse(text = allstocks[i]))[rownames(eval(parse(text = allstocks[i])))>=train.range[1] & rownames(eval(parse(text = allstocks[i])))<=train.range[2]  ,]         )
  training.data = cbind(training.data,   eval(parse(text = paste(allstocks[i],'.train',sep='')    ))[6]   )   
}

training.data = training.data[-1]
colnames(training.data) = allstocks
training.data = as.xts(training.data)
training.data = as.data.frame(training.data/lag(training.data,1)-1)

i=2
test = eval(parse(text = allstocks[i]))[rownames(eval(parse(text = allstocks[i])))>=test.range[1] & rownames(eval(parse(text = allstocks[i])))<=test.range[2]  ,]
test = test[6]
for (i in 3:length(allstocks))
{
  assign(paste(allstocks[i],'.test',sep = ''),  eval(parse(text = allstocks[i]))[rownames(eval(parse(text = allstocks[i])))>=test.range[1] & rownames(eval(parse(text = allstocks[i])))<=test.range[2]  ,]         )
  test = cbind(test,   eval(parse(text = paste(allstocks[i],'.test',sep='')    ))[6]   )   
}

colnames(test) = allstocks[2:length(allstocks)]
test= as.xts(test)
test = as.data.frame(test/lag(test,1)-1)

assign(paste(allstocks[1],'.test',sep = ''),  eval(parse(text = allstocks[1]))[rownames(eval(parse(text = allstocks[1])))>=test.range[1] & rownames(eval(parse(text = allstocks[1])))<=test.range[2]  ,]         )
FSC.test = FSC.test[6]
colnames(FSC.test)[1] = 'FSC'
FSC.test= as.xts(FSC.test)
FSC.test = as.data.frame(FSC.test/lag(FSC.test,1)-1)
#  Now use forward stepwise regression in order to estimate a market model using the information I have provided.


# 3  -  Stepwise Regression -----------------------------------------------

reg = lm(FSC~.,data= training.data)
stepreg = step(reg,direct='backward')

# Take the model about and then test it on the test data.

varbs = names(stepreg$coefficients)[2:length(names(stepreg$coefficients))]
pred.data = as.data.frame(test[,colnames(test)==varbs[1]])

for (i in 2:length(varbs))
  {
  pred.data = cbind(pred.data,   test[,varbs[i]==colnames(test)]  )
}

colnames(pred.data ) = varbs
rownames(pred.data) = rownames(test)
# test.data = coln

test.results =as.data.frame( predict.lm(stepreg,   newdata = pred.data  ))
colnames(test.results)[1] = "PredictedFSC"

# plot(as.Date(rownames(test.results)),test.results$PredictedFSC)
# par(new=TRUE)
# plot(as.Date(rownames(FSC.test)),FSC.test$FSC,col='red')

dates = as.Date(rownames(FSC.test))
difference = FSC.test - test.results
plot(as.Date(rownames(difference)),difference$FSC)
SE= summary(stepreg)$sigma


#  Calculate the standardized residuals. 
SD.residuals = difference/SE


# plot(dates,SD.residuals$FSC)
# abline(h=1.96)
# abline(h=-1.96)
# abline(h=2.58)
# abline(h=-2.58)

#  Set things up to use the ggplot function:
SD.residuals = data.frame(seq(1:dim(SD.residuals)[1]),SD.residuals)
colnames(SD.residuals)[1] = "Dates"
colnames(SD.residuals)[2] = 'vals'
SD.residuals[1] = as.Date(rownames(SD.residuals))

#  Use the melt function to try to put things together.  The geom_abline turns out to be more convenient, but the below
#  is kept to demonstrate how to use the melt function in the future.

# test.length = dim(SD.residuals)[1]
# conf.95 = data.frame(seq(1:test.length),rep(1.96,test.length))
# conf.99 = data.frame(seq(1:test.length),rep(2.58,test.length))
# negconf.95 = data.frame(seq(1:test.length),rep(-1.96,test.length))
# negconf.99 = data.frame(seq(1:test.length),rep(-2.58,test.length))
# 
# names(conf.95)[1:2] = c('index', 'vals')
# names(conf.99)[1:2] = c('index', 'vals')
# names(negconf.95)[1:2] = c('index', 'vals')
# names(negconf.99)[1:2] = c('index', 'vals')
# names(SD.residuals  )[1:2] = c('index', 'vals')
# 
# newData <- melt(list(df1 = SD.residuals, df2 = conf.95, df3 = conf.99,df4 = negconf.95,df5 = negconf.99), id.vars = "index")
# names(newData)

curplot = ggplot(SD.residuals, aes(x=Dates, y=vals))+geom_point()
curplot = curplot +geom_abline(intercept = 1.96,slope=0,color = 'blue')+geom_abline(intercept = -1.96,slope=0,color = 'blue')  
curplot = curplot +geom_abline(intercept = 2.58,slope=0,color = 'red')+geom_abline(intercept = -2.58,slope=0,color = 'red')  
curplot = curplot +scale_x_date(name="Dates",date_breaks = "1 month")+ggtitle("Studentized Residuals for FSC") + theme(axis.title.y = element_blank())
curplot


# Generate the wealth relative for the training period:

# Calculate the wealth relative and plot it using ggplot2 -----------------

FSC.test.period =  FSC[rownames(FSC)>=test.range[1] & rownames(FSC)<=test.range[2],][c(-1,-2,-3,-4,-5)]
FSC.test.period = data.frame(rownames(FSC.test.period),FSC.test.period)

model.returns = 1+test.results
model.returns.compounded = 1

holdnum = nrow(model.returns)-1
for(i in 1:holdnum)
{
  model.returns.compounded[i+1] = model.returns.compounded[i]*model.returns[i+1,]
  FSC[rownames(FSC)>=test.range[1] & rownames(FSC)<=test.range[2],][c(-1,-2,-3,-4,-5)]
}
model.returns.compounded = data.frame(rownames(FSC.test.period),model.returns.compounded*FSC.test.period[1,2])        

names(model.returns.compounded) = c('Date','Price')
names(FSC.test.period)= c('Date','Price')
plot(model.returns.compounded[,2],type  = 'l')
par(new=T)
plot(FSC.test.period[,2],type = 'l', col = 'blue')

FSC.test.period= data.frame(melt(FSC.test.period,id.vars = 'Date'),'df1')
model.returns.compounded = data.frame(melt(model.returns.compounded,id.vars = 'Date'),'df2')
names(FSC.test.period ) = c('Date','variable','value','type')
names(model.returns.compounded ) = c('Date','variable','value','type')

newData =rbind(FSC.test.period,model.returns.compounded)

ggplot(data = newData, aes(x = Date, y = value, group = type, colour = type)) + geom_line()


# 
# newData = data.frame(FSC.test.period,model.returns.compounded)                             
# 
# newData <- melt.data.frame(FSC.test.period, id.vars = "Date" )                                  
# 
# # GGplot needs a lot of work.  Read documentation on melt and ggplot2 before cotinuing further.
#   ggplot(data = newData, aes(x = Date, y = value, group = variable, colour = type)) + geom_line()                                  
# holdplot
# 
# ggplot(data = newData, aes(x=Date,y=value,colour = L1))+geom_line()
# 
# 
# ggplot() + geom_line(  data = newData ,  aes(x = Date,y= variable))
#                                   
#                                   
#                                   