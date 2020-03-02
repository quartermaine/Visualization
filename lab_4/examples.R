library(plotly) 
library(seriation) 
mtscaled=scale(mtcars[1:7]) 
rowdist<-dist(mtscaled)
coldist<-dist(t(mtscaled)) 
order1<-seriate(rowdist, "BBURCG") 
order2<-seriate(coldist, "BBURCG") 
ord1<-get_order(order1) 
ord2<-get_order(order2) 
reordmatr<-mtscaled[rev(ord1),ord2] 
dims=list() 
for( i in 1:ncol(reordmatr)){ dims[[i]]=list( label=colnames(reordmatr)[i], values=as.formula(paste("~",colnames(reordmatr)[i]))) } 
dims0=list() 
for( i in 1:ncol(mtscaled)){ dims0[[i]]=list( label=colnames(mtscaled)[i], values=as.formula(paste("~",colnames(mtscaled)[i]))) } 
p <- as.data.frame(mtscaled) %>% plot_ly(type = 'parcoords', #line = list(color = ~as.numeric(Species)), dimensions = dims0 ) 
p 
p1 <- as.data.frame(reordmatr) %>% plot_ly(type = 'parcoords', #line = list(color = ~as.numeric(Species)), dimensions = dims ) 
p1 plot_ly(x=colnames(reordmatr), y=rownames(reordmatr), z=reordmatr, type="heatmap", colors =colorRamp(c("yellow", "red"))) 



library(plotly) 
df=lattice::barley 
p<-ggplot(df, aes(y=variety, x=yield, color=year))+geom_point()+ facet_grid(site~.) 
p 
ggplotly(p) 

df1<-mpg 
p1<-ggplot(df1, aes(cty, displ))+geom_point()+geom_smooth()+ facet_wrap(~class, labeller = "label_both")
p1

df3<-MASS::Aids2 
Agerange<-lattice::equal.count(df3$age, number=6, overlap=0.03) #overlap is 3% 
L<-matrix(unlist(levels(Agerange)), ncol=2, byrow = T)
L1<-data.frame(Lower=L[,1],Upper=L[,2], Interval=factor(1:nrow(L))) 
ggplot(L1)+geom_linerange(aes(ymin = Lower, ymax = Upper, x=Interval)) 


index=c() 
Class=c() 
for(i in 1:nrow(L)){ 
  Cl=paste("[", L1$Lower[i], ",", L1$Upper[i], "]", sep="") 
  ind=which(df3$age>=L1$Lower[i] &df3$age<=L1$Upper[i]) 
  index=c(index,ind) 
  Class=c(Class, rep(Cl, length(ind))) } 


df4<-df3[index,] 
df4$Class<-as.factor(Class) 
ggplot(df4, aes(x=death-diag, fill="orange"))+ geom_histogram()+ facet_wrap(~Class, labeller = "label_both") 



age: continuous.
2. workclass: Private, Self-emp-not-inc, etc. 
3. fnlwgt: a population index. 
4. education: Bachelors, Some-college, etc. 
5. education-num: ordered Education variable.
6. marital-status: Married-civ-spouse, Divorced, etc. 
7. occupation: Tech-support, Craft-repair, etc. 
8. relationship: Wife, Own-child, etc. 
9. race: White, Asian-Pac-Islander etc. 
10. sex: Female, Male. 
11. capital-gain: continuous. 
12. capital-loss: continuous. 
13. hours-per-week: continuous.
14. native-country: United-States, Cambodia etc. 
15. Income level