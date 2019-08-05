
library(gdata)
library(XLConnect)
library(utils)

perl='C:\\Strawberry\\perl\\bin\\perl5.30.0.exe'

# load socio-economic factors from NOMIS database
gender_density <- read.xls('20150429_PostElectionsAnalysis_NOMIS_DDJ_DecisionTreeData.xls', stringsAsFactors=FALSE, sheet='Census 2011 Resident Population', perl=perl)
age <- read.xls('20150429_PostElectionsAnalysis_NOMIS_DDJ_DecisionTreeData.xls',stringsAsFactors=FALSE,sheet='Census 2011 Age Structure', perl=perl)
ethnic <- read.xls('20150429_PostElectionsAnalysis_NOMIS_DDJ_DecisionTreeData.xls',stringsAsFactors=FALSE,sheet='Census 2011 Ethnic Group', perl=perl)
house <- read.xls('20150429_PostElectionsAnalysis_NOMIS_DDJ_DecisionTreeData.xls',stringsAsFactors=FALSE,sheet='Census 2011 Tenure', perl=perl)
socgrade <- read.xls('20150429_PostElectionsAnalysis_NOMIS_DDJ_DecisionTreeData.xls',stringsAsFactors=FALSE,sheet='Census 2011 Social Grade', perl=perl)
claimants <-read.xls('20150429_PostElectionsAnalysis_NOMIS_DDJ_DecisionTreeData.xls',stringsAsFactors=FALSE,sheet='Claimant Count Mar 2015', perl=perl)
business<-read.xls('20150429_PostElectionsAnalysis_NOMIS_DDJ_DecisionTreeData.xls',stringsAsFactors=FALSE,sheet='UK Business Counts 2014', perl=perl)
jobsdensity<-read.xls('20150429_PostElectionsAnalysis_NOMIS_DDJ_DecisionTreeData.xls',stringsAsFactors=FALSE,sheet='Jobs Density 2013', perl=perl)
educ<-read.xls('20150429_PostElectionsAnalysis_NOMIS_DDJ_DecisionTreeData.xls',stringsAsFactors=FALSE,sheet='Census 2011 Qualifications', perl=perl)

# A sheet named APsheet isn't in the .xlsx file, so I'm just guessing
APsheet <- read.xls('20150429_PostElectionsAnalysis_NOMIS_DDJ_DecisionTreeData.xls',stringsAsFactors=FALSE,sheet='Annual Population Survey Dec 20', perl=perl)

# normalise data
business$Micro.Perc <- business$Micro..0.to.9./business$Total
business$Small.Perc <- business$Small..10.to.49./business$Total
business$Medium.Perc <- business$Medium.sized..50.to.249./business$Total
business$Large.Perc <- business$Large..250../business$Total
business<-business[,c('Code','Micro.Perc','Small.Perc','Medium.Perc','Large.Perc')]

socgrade$AB.Perc <- socgrade$AB.Higher.and.intermediate.managerial.administrative.professional.occupations/socgrade$All.categories..Approximated.social.grade
socgrade$C1.Perc <- socgrade$C1.Supervisory..clerical.and.junior.managerial.administrative.professional.occupations/socgrade$All.categories..Approximated.social.grade
socgrade$C2.Perc <- socgrade$C2.Skilled.manual.occupations/socgrade$All.categories..Approximated.social.grade
socgrade$DE.Perc <- socgrade$DE.Semi.skilled.and.unskilled.manual.occupations..unemployed.and.lowest.grade.occupations/socgrade$All.categories..Approximated.social.grade
socgrade<-socgrade[,c('Code','AB.Perc','C1.Perc','C2.Perc','DE.Perc')]

names(gender_density)[5:7]<-c('Male','Female','Density')
gender_density<-gender_density[,c(1:3,5:7)]
age <- age[,c(2:4)]

data_all<-merge(gender_density,age)
ethnic<-ethnic[,c(2,6,8,10,12,14)]
names(ethnic)<-c('Code','White','Mixed','Asian.British','Black.British','Other')
data_all<-merge(data_all,ethnic)
names(house)[c(6,8,10,12,14)]<-c('Owner.Perc','Shared.Owner','Soc.Rent','Private.Renter','Rent.Free')
house <- house[,c(2,6,8,10,12,14)]
data_all<-merge(data_all,house)
data_all<-merge(data_all,socgrade)
names(claimants)[[4]] <- 'Claimants'
claimants<-claimants[,c(2,4)]
data_all<-merge(data_all,claimants)
data_all<-merge(data_all,business)
jobsdensity<-jobsdensity[,c(2,3)]
data_all<-merge(data_all,jobsdensity)
educ <- educ[,c(2,4,8,14)]
names(educ)<-c('Code','QualNO','GCSE.Max','HE.Above')
data_all<-merge(data_all,educ,all.x=TRUE)

# APsheet isn't in the .xlsx file
# let's guess they wanted Code (2), employment rate (9), and unemployment rate 13
APsheet <- APsheet[,c(2, 9, 13)]
data_all <- merge(data_all,APsheet)
# data_all$AP.Name <- sapply(as.character(data_all$AP.Name),function(x) gsub('&amp;','&',x),USE.NAMES = FALSE)

#
# #### get elections data #### 
# 
# 
hocl <- read.csv('hocl-ge2015-results-summary.csv')

Votes.C <- hocl$con
Votes.Lab <- hocl$lab
Votes.LD <- hocl$ld
Votes.UKIP <- hocl$ukip
Votes.Green <- hocl$green
Votes.SNP <- hocl$snp
Votes.DUP <- hocl$dup

# I think this is what was meant by shares
Votes.C.Share <- hocl$con / hocl$valid_votes
Votes.Lab.Share <- hocl$lab / hocl$valid_votes
Votes.LD.Share <- hocl$ld / hocl$valid_votes
Votes.UKIP.Share <- hocl$ukip / hocl$valid_votes
Votes.Green.Share <- hocl$green / hocl$valid_votes
Votes.SNP.Share <- hocl$snp / hocl$valid_votes
Votes.DUP.Share <- hocl$dup / hocl$valid_votes

tots_temp <- Votes.SNP.Share+Votes.Green.Share+Votes.LD.Share+Votes.C.Share+Votes.Lab.Share+Votes.UKIP.Share+Votes.DUP.Share
tots_temp_2 <- Votes.SNP+Votes.Green+Votes.LD+Votes.C+Votes.Lab+Votes.UKIP+Votes.DUP
tots_others <- (100*tots_temp_2/tots_temp)-tots_temp_2
tots_others_perc <- 100*tots_others/(100*tots_temp_2/tots_temp)

# Get that 2010 general election data.
before <- sapply(hocl$result, function(res) {
  if (length(res) > 0 && grepl('hold', res)) {
    return(gsub(' hold', '', res))
  } else {
    return(gsub('[A-Za-z]+ gain from ', '', res))
  }
})

temp <- data.frame(Names=hocl$constituency_name,
                   X2015.result=hocl$first_party,
                   X2010.result=before,
                   Votes.C=Votes.C,
                   Votes.Lab=Votes.Lab,
                   Votes.LD=Votes.LD,
                   Votes.UKIP=Votes.UKIP,
                   Votes.Green=Votes.Green,
                   Votes.SNP=Votes.SNP,
                   Votes.C.Share=Votes.C.Share,
                   Votes.Lab.Share=Votes.Lab.Share,
                   Votes.LD.Share=Votes.LD.Share,
                   Votes.UKIP.Share=Votes.UKIP.Share,
                   Votes.Green.Share=Votes.Green.Share,
                   Votes.SNP.Share=Votes.SNP.Share,
                   Votes.Others=tots_others,
                   Votes.Others.Share=tots_others_perc)


results2015<-temp
results2015$Code = hocl$ons_id

results2015$X2015.result<-as.character(hocl$first_party)
results2015$Names<-as.character(results2015$Names)
results2015$X2010.result<-as.character(results2015$X2010.result)
nrow(results2015)

# # fix the parties names
results2015$X2015.result[results2015$X2015.result=='Con']<-'Conservative'
results2015$X2015.result[results2015$X2015.result=='Lab']<-'Labour'
results2015$X2015.result[results2015$X2015.result=='LD']<-'Liberal Democrats'
results2015$X2015.result[results2015$X2015.result=='UKIP']<-'Ukip'
results2015$X2015.result[results2015$X2015.result=='SNP']<-'Scottish National Party'

parties <- c('Conservative','Labour','Liberal Democrats','Ukip','Scottish National Party')
results2015$X2015.result[!(results2015$X2015.result %in% parties)] <- 'Other'

results2015$X2010.result[results2015$X2010.result=='Con']<-'Conservative'
results2015$X2010.result[results2015$X2010.result=='Lab']<-'Labour'
results2015$X2010.result[results2015$X2010.result=='LD']<-'Liberal Democrats'
results2015$X2010.result[results2015$X2010.result=='UKIP']<-'Ukip'
results2015$X2010.result[results2015$X2010.result=='SNP']<-'Scottish National Party'
results2015$X2010.result[!(results2015$X2010.result %in% parties)] <- 'Other'

# names(results2015)[[1]]<-'AP.Name'
data_all<-merge(data_all,results2015,all.x=TRUE)
data_all[is.na(data_all$X2015.result),]

# 
# # match new results to codes data
table(results2015$X2015.result)
table(results2015[,c('X2015.result','X2010.result')])
sum(table(results2015$X2015.result))
# 
table(data_all[,c('X2015.result','Region')])


#### get turnout by const #####

load('turnout.RData')
turnout2010<-read.csv('turnout2010.csv')

names(turnout_data)[[2]]<-'Turnout.2015'
data_all<-merge(data_all,turnout_data)

data_all$Electorate <- 100*(data_all$Votes.C + data_all$Votes.Lab + data_all$Votes.LD + data_all$Votes.UKIP + data_all$Votes.Green +
                              data_all$Votes.SNP+data_all$Votes.Others ) / data_all$Turnout.2015

data_all<-merge(data_all,turnout2010)

######## TUNROUT ANALYSIS #######

data_all$ElectorateHasIncreased <- data_all$Electorate>data_all$Votes2010
paste(sum(data_all$Electorate),sum(data_all$Votes2010),sep=',')
data_all$TurnoutHasIncreased <- data_all$Turnout.2015>data_all$Turnout.2010
data_all$AllVoted.2015 <- data_all$Electorate*data_all$Turnout.2015/100
data_all$AllVoted.2010 <- data_all$Votes2010*data_all$Turnout.2010/100
paste(sum(data_all$AllVoted.2015),sum(data_all$AllVoted.2010),sep=',')
summary(data_all$Turnout.2015)

data_all$TurnoutDifference <- data_all$Turnout.2015-data_all$Turnout.2010
table(data_all[,c('ElectorateHasIncreased','X2015.result')])
table(data_all[,c('TurnoutHasIncreased','X2015.result')])
aggregate(TurnoutDifference~X2015.result,median,data=data_all)
aggregate(TurnoutDifference~X2015.result,mean,data=data_all)
aggregate(TurnoutDifference~X2015.result,sd,data=data_all)
data_all$SwingSeats <- data_all$X2015.result != data_all$X2010.result

quantile(data_all$TurnoutDifference,seq(0,1,0.1))

data_all$Engaged <- 'Normal'
data_all$Engaged[data_all$TurnoutDifference>3] <- 'Engaged'
data_all$Engaged[data_all$TurnoutDifference<=-1.1] <- 'Disengaged'

table(data_all[,c('X2015.result','Engaged')])
table(data_all[,c('SwingSeats','Engaged')])

temp<-data_all[data_all$Engaged=='Disengaged' & data_all$SwingSeats==TRUE,]

data_structured <- data_all[,c(34,42:47,49,50)]
library(reshape)
data_structured<-melt(data_structured,id=c(1,9))

data_structured <- data_all[,c(34,42:47,49,50,30)]
data_structured<-melt(data_structured,id=c(1,9,10))

data_structured <- data_all[,c(34,42:47,49,50,53)]
data_structured<-melt(data_structured,id=c(1,9,10))
data_structured$TurnoutDifference<-data_structured$Turnout.2015-data_structured$Turnout.2010

#### prepare data for tree ####

node.fun1 <- function(x, labs, digits, varlen)
{
  paste(labs,paste(',',paste((x$frame$n-x$frame$dev),paste("over", x$frame$n))))
  
}

data_all$Mean.Age<-NULL
# Rename Median.Age to Age
names(data_all)[[8]] <- 'Age'

# Rename Micro.Perc and Small.Perc to Micro.bsns and Small.Bsns, respectively
names(data_all)[24:25] <- c('Micro.Bsns','Small.Bsns')
data_all$Big.Bsns <- business$Medium.Perc+business$Large.Perc
data_all$OneMinusSmallBsns <- business$Medium.Perc+business$Large.Perc

# Drop .Perc from these column names
names(data_all)[19:22]<-c('AB','C1','C2','DE')

# Rename Other to Other.Eth
names(data_all)[[13]]<-'Other.Eth'

data_all$Owner <- house$Owner.Perc+house$Shared.Owner
data_all$Owner.Perc<-NULL
data_all$Shared.Owner<-NULL
data_all$Region[data_all$Region=='North East' | data_all$Region=='North West' | data_all$Region=='Yorkshire and The Humber'] <- 'North'
data_all$Region[data_all$Region=='West Midlands' | data_all$Region=='East Midlands'] <- 'Midlands'
data_all$Region[data_all$Region=='South West' | data_all$Region=='South East'] <- 'South'
data_all$Scotland<-'RestUK'
data_all$Scotland[data_all$Region=='Scotland']<-'Scotland'
data_all$SmMcr.Bsns <- data_all$Micro.Bsns+data_all$Small.Bsns
data_all$Prv.Rent <- data_all$Private.Renter+data_all$Rent.Free
data_all$NS.Renter <- data_all$Private.Renter+data_all$Rent.Free


scotland_data <- data_all[data_all$Region=='Scotland',]
summary(scotland_data)

library(rpart)
library(rpart.plot)

######## Explore trees as results come in ###########

# without scotland
tree <- rpart(X2015.result~Soc.Rent+GCSE.Max+Male+Female+Density+Age+Claimants, 
              data=data_all[data_all$Region!='Scotland',],
              control=rpart.control(minsplit=2, cp=0.0001, minbucket=3, maxdepth=3), method='class')

prp(tree, extra=100, under=T, yesno=T, node.fun=node.fun1,main ="All Constituencies (No Scotland)",branch.type=5)
print(tree)

#### Swing ####

tree <- rpart(X2015.result~Male+Female+Density+Age+White+Mixed+Asian.British+Black.British+Other.Eth+
                Scotland+Big.Bsns+Claimants,data=data_all[data_all$X2010.result=='Liberal Democrats',],
              control=rpart.control(minsplit=3, cp=0.01,minbucket=3))

prp(tree, extra=100, under=T, yesno=F, node.fun=node.fun1,main ="Lib Dem in 2010",branch.type=5)



