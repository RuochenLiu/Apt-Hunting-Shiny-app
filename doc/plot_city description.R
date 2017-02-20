### Plot for Jobs
## Donut Chart for Job Opportunity type

library(ggplot2)

setwd('/Users/limengchen/Desktop/Spr2017-proj2-grp8-master/data/data for plot in City Description')
Job=read.csv('jobs numbers by types.csv')

Job=data.frame(count=Job$Total,category=Job$job.types)
Job$fraction=Job$count/sum(Job$count) 
Job=Job[order(Job$fraction),]
Job$ymax=cumsum(Job$fraction)
Job$ymin=c(0,head(Job$fraction,n=-1))

Job_type = ggplot(Job, aes(fill=category,ymax=ymax,ymin=ymin,xmax=4,xmin=3))+
  geom_rect()+
  coord_polar(theta='y')+
  #xlim(c(0,10))+
  theme(panel.grid=element_blank())+
  theme(axis.text = element_blank())+
  theme(axis.ticks = element_blank())+
  annotate('text',x=0,y=0,label='Jobs by type')+
  labs(title='')

## job by city
job=read.csv('job by cities.csv')
library(ggplot2)
Job_city=ggplot(job,aes(x=City, fill=City, y=Total))+
  geom_bar(stat = 'identity')+
  scale_fill_hue(c=40)+
  coord_flip()

library(gridExtra)
grid.arrange(Job_type,Job_city,ncol=1,nrow=2)


#### Plot for wages

## salary by city
salary=read.csv('wage_city.csv')
City=c(rep('NY',69),rep('Chicago',58),rep('Austin',72),rep('LA',68), rep('SF',37))
value=c(salary$NY[1:69],salary$Chicago[1:58],salary$Austin[1:72],salary$LA[1:68],salary$SF[1:37])
salary_city=data.frame(City,value)

salary_mean=aggregate(salary_city$value,by=list(salary_city$City),mean); 
colnames(salary_mean)=c('city','mean')
salary_sd=aggregate(salary_city$value,by=list(salary_city$City),sd);
colnames(salary_sd)=c('city','sd')
salary=merge(salary_mean,salary_sd, by.x=1, by.y=1)

ggplot(salary_city)+
  geom_point(aes(x=City, y=value), colour=rgb(0.8,0.7,0.1,0.4),size=5)+
  geom_point(data = salary,aes(x=city, y=mean),colour = rgb(0.6,0.5,0.4,0.7) , size = 8)+
  geom_errorbar(data=salary,aes(x=city, y=sd,ymin=mean-sd,
                                ymax=mean+sd),colour = rgb(0.4,0.8,0.2,0.4) , width = 0.7 , size=1.5)

### Plot for population and facilities

## Population by city
library(fmsb)

population=read.csv('/Users/limengchen/Desktop/population_city.csv')
max=max(population)
min=min(population)
population=rbind(rep(max,5),rep(min,5),population)

## park by city
park=as.data.frame(t(c(1466,279,913,1292,250)),ncol=5)
colnames(park)=c('NYC','Chicago','LA','SF','Austin')
max.park=max(park)
min.park=min(park)
park=rbind(rep(max.park,5),rep(min.park,5),park)

## health facility by city
health=as.data.frame(t(c(73,52,21,77,27)),ncol=5)
colnames(health)=c('NYC','Chicago','LA','SF','Austin')
max.health=max(health)
min.health=min(health)
health=rbind(rep(max.health,5),rep(min.health,5),health)

## library by city
library=library=as.data.frame(t(c(205,36,87,27,24)),ncol=5)
colnames(library)=c('NYC','Chicago','LA','SF','Austin')
max.library=max(library)
min.library=min(library)
library=rbind(rep(max.library,5),rep(min.library,5),library)

par(mfrow=c(2,2))
radarchart( population, axistype=1, 
            #custom polygon
            pcol=rgb(0.2,0.5,0.5,0.9), pfcol=rgb(0.2,0.5,0.5,0.5), plwd=4 , 
            #custom the grid
            cglcol="grey", cglty=1, axislabcol="grey", caxislabels=seq(min,max,5), cglwd=0.8,
            #custom labels
            vlcex=0.8,title='population for in cities')

park.plot=radarchart( park, axistype=1, pcol=rgb(0.2,0.5,0.5,0.9),pfcol=rgb(0.2,0.5,0.5,0.5) , plwd=4 , 
                      cglcol="grey", cglty=1, axislabcol="grey", caxislabels=seq(min.park,max.park,5), cglwd=0.8,
                      vlcex=0.8,title='park in cities')

radarchart( health, axistype=1, pcol=rgb(0.2,0.5,0.5,0.9) , pfcol=rgb(0.2,0.5,0.5,0.5) , plwd=4 , 
            cglcol="grey", cglty=1, axislabcol="grey", caxislabels=seq(min.health,max.health,5), cglwd=0.8,
            vlcex=0.8,title='health in cities')

radarchart( library, axistype=1,pcol=rgb(0.2,0.5,0.5,0.9) , pfcol=rgb(0.2,0.5,0.5,0.5) , plwd=4 , 
            cglcol="grey", cglty=1, axislabcol="grey", caxislabels=seq(min.library,max.library,5), cglwd=0.8,
            vlcex=0.8,itle='library in cities')
