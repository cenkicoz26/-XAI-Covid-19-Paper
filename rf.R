library(sf)
library(sp)
library(spdep)
library(RColorBrewer)
library(tmap)
library(randomForest)
library(DALEX)
library(ggplot2)
library(gridExtra)
library(tmap)
library(tmaptools)
library(spgwr)
library(grid)
library(car)
library(grid)
library(corrplot)
library(viridisLite)

data<- read.csv("son veri.csv",header=TRUE,dec = ".",sep= ";",check.names=TRUE,fileEncoding ="latin1")
data<-data[,-1]

# vif and correlation analysis 
m1<-lm(data$cumcase~., data[,-19])
vifres<-vif(m1)
vifres[vifres>10]
M<-cor(data)
M1<-round(M,2)
corrplot(M, method="circle")
#setting a seed
set.seed(1)
# splitting data into 70% training  and 30% testing 
data_split<-sample(nrow(data),nrow(data)*0.7)
train<-data[data_split,]
test<-data[-data_split,]
#### rf model
rf.covid <- randomForest(cumcase ~ ., data=train,
                            mtry = 7, importance = TRUE,localImp=TRUE)
yhat.rf <- predict(rf.covid, newdata = test)
importance(rf.covid)
# correcting variable names in variable importance plot
#rownames(rf.covid$importance)[14]<-"age20-39"
#rownames(rf.covid$importance)[15]<-"age40-59"
#rownames(rf.covid$importance)[16]<-"age60-79"
#rownames(rf.covid$importance)[17]<-"age80+"
varImpPlot(rf.covid,main= "Variable Importance")
imp <- importance(rf.covid)
impvar <- rownames(imp)[order(imp[, 1], decreasing=TRUE)]
### Fig 2
op <- par(mfrow=c(3, 4))
for (i in seq_along(impvar)) {
  partialPlot(rf.covid, data, impvar[i], xlab=impvar[i],
              main=paste("Partial Dependence on", impvar[i]),
              )
}
par(op)
#################
 explain_rf <- DALEX::explain(model = rf.covid,                                           data = data,
                              y = data$cumcase, 
                        label = "Random Forest ")
 
########################### İstanbul  
bd_rf_ist <- predict_parts(explainer = explain_rf,
                         new_observation = data[40,],
                           type = "break_down")
bd_rf
plot(bd_rf)
bd_rf <- predict_parts(explainer = explain_rf,
                       new_observation = data[40,],
                       type = "break_down_interactions")
i1<-plot(bd_rf)+ ggtitle("Break-down plot for İstanbul","") 
#####################################
shap_ist <- predict_parts(explainer = explain_rf, 
                            new_observation = data[40,], 
                            type = "shap",
                            B = 50)
i2<-plot(shap_ist)+ ggtitle("SHAP for İstanbul","") 
########################## LIME
library("DALEXtra")
library("lime")
model_type.dalex_explainer <- DALEXtra::model_type.dalex_explainer
predict_model.dalex_explainer <- DALEXtra::predict_model.dalex_explainer

lime_ist <- predict_surrogate(explainer = explain_rf, 
                                 new_observation = data[40,], 
                                 n_features = 5, 
                                 n_permutations = 1000,
                                 type = "lime")
i3<-plot(lime_ist)+ ggtitle("LIME plot for İstanbul","") 
################### cetaris paribus
cp_ist_rf <- predict_profile(explainer = explain_rf, 
                                 new_observation = data[40,])
i4<-plot(cp_ist_rf, variables = c("gdp_per_capita", "age20.39","average_household_size","population_density")) +
  ggtitle("Ceteris-paribus profile", "") 
################### histograms of positve contributing variables accordimg to LIME results

i5<-ggplot(data, aes(x=age20.39))+
  geom_histogram(binwidth = 0.05,fill="#5F9EA0", color="#4682B4", alpha=0.95)+ theme_minimal()
i6<-ggplot(data, aes(x=average_household_size))+
  geom_histogram(binwidth = 0.2,fill="#5F9EA0", color="#4682B4", alpha=0.95)+ theme_minimal()
i7<-ggplot(data, aes(x=gdp_per_capita))+
  geom_histogram(binwidth = 750,fill="#5F9EA0", color="#4682B4", alpha=0.95)+ theme_minimal()
i8<-ggplot(data, aes(x=population_density))+
  geom_histogram(binwidth = 200,fill="#5F9EA0", color="#4682B4", alpha=0.95)+ theme_minimal()
################ İstanbul için instance bazlı açıklama
grid.arrange(i1,i2,i3,i4,layout_matrix=rbind(c(1,1),c(2,3),c(4,4)))

################################################### Ankara 
bd_rf_ank <- predict_parts(explainer = explain_rf,
                           new_observation = data[7,],
                           type = "break_down")
bd_rf
plot(bd_rf)
bd_rf_ank <- predict_parts(explainer = explain_rf,
                       new_observation = data[7,],
                       type = "break_down_interactions")
a1<-plot(bd_rf_ank)+ ggtitle("Break-down plot for Ankara","") 
#####################################
shap_ank <- predict_parts(explainer = explain_rf, 
                          new_observation = data[7,], 
                          type = "shap",
                          B = 50)
a2<-plot(shap_ank)+ ggtitle("SHAP for Ankara","") 
########################## LIME
model_type.dalex_explainer <- DALEXtra::model_type.dalex_explainer
predict_model.dalex_explainer <- DALEXtra::predict_model.dalex_explainer

lime_ank <- predict_surrogate(explainer = explain_rf, 
                              new_observation = data[7,], 
                              n_features = 5, 
                              n_permutations = 1000,
                              type = "lime")
a3<-plot(lime_ank)+ ggtitle("LIME plot for Ankara","") 
################### cetaris paribus
cp_ank_rf <- predict_profile(explainer = explain_rf, 
                             new_observation = data[7,])
a4<-plot(cp_ank_rf, variables = c("gdp_per_capita", "so2","average_household_size","population_density")) +
  ggtitle("Ceteris-paribus profile", "") 
grid.arrange(a1,a2,a3,a4,layout_matrix=rbind(c(1,1),c(2,3),c(4,4)))

#################################### diyarbakır
bd_rf_diy <- predict_parts(explainer = explain_rf,
                           new_observation = data[26,],
                           type = "break_down_interactions")
d1<-plot(bd_rf_diy)+ ggtitle("Break-down plot for Diyarbakır","") 
#####################################
shap_diy <- predict_parts(explainer = explain_rf, 
                          new_observation = data[26,], 
                          type = "shap",
                          B = 50)
d2<-plot(shap_diy)+ ggtitle("SHAP for Diyarbakır","") 
########################## LIME
model_type.dalex_explainer <- DALEXtra::model_type.dalex_explainer
predict_model.dalex_explainer <- DALEXtra::predict_model.dalex_explainer

lime_diy <- predict_surrogate(explainer = explain_rf, 
                              new_observation = data[26,], 
                              n_features = 5, 
                              n_permutations = 1000,
                              type = "lime")
d3<-plot(lime_diy)+ ggtitle("LIME plot for Diyarbakır","") 
################### cetaris paribus
cp_diy_rf <- predict_profile(explainer = explain_rf, 
                             new_observation = data[26,])
d4<-plot(cp_diy_rf, variables = c("gdp_per_capita", "elderly_dep.ratio","average_household_size","automobile_number")) +
  ggtitle("Ceteris-paribus profile", "") 
grid.arrange(d1,d2,d3,d4,layout_matrix=rbind(c(1,1),c(2,3),c(4,4)))
################################# şırnak
bd_rf_sh <- predict_parts(explainer = explain_rf,
                       new_observation = data[72,],
                       type = "break_down_interactions")
sh1<-plot(bd_rf_sh)+ ggtitle("Break-down plot for Şırnak","") 
#####################################
shap_ist <- predict_parts(explainer = explain_rf, 
                          new_observation = data[72,], 
                          type = "shap",
                          B = 50)
sh2<-plot(shap_ist)+ ggtitle("SHAP for Şırnak","") 
########################## LIME
model_type.dalex_explainer <- DALEXtra::model_type.dalex_explainer
predict_model.dalex_explainer <- DALEXtra::predict_model.dalex_explainer

lime_sh <- predict_surrogate(explainer = explain_rf, 
                              new_observation = data[72,], 
                              n_features = 5, 
                              n_permutations = 1000,
                              type = "lime")
sh3<-plot(lime_sh)+ ggtitle("LIME plot for Şırnak","") 
################### cetaris paribus
cp_sh <- predict_profile(explainer = explain_rf, 
                             new_observation = data[72,])
sh4<-plot(cp_sh, variables = c("gdp_per_capita", "unemployment","average_household_size","elderly_dep.ratio","so2")) +
  ggtitle("Ceteris-paribus profile", "")
grid.arrange(sh1,sh2,sh3,sh4,layout_matrix=rbind(c(1,1),c(2,3),c(4,4)))
############################## Antalya
bd_rf_ant <- predict_parts(explainer = explain_rf,
                          new_observation = data[8,],
                          type = "break_down_interactions")
an1<-plot(bd_rf_ant)+ ggtitle("Break-down plot for Antalya","") 
#####################################
shap_ant <- predict_parts(explainer = explain_rf, 
                          new_observation = data[8,], 
                          type = "shap",
                          B = 50)
an2<-plot(shap_ant)+ ggtitle("SHAP for Antalya","") 
########################## LIME

model_type.dalex_explainer <- DALEXtra::model_type.dalex_explainer
predict_model.dalex_explainer <- DALEXtra::predict_model.dalex_explainer

lime_ant <- predict_surrogate(explainer = explain_rf, 
                             new_observation = data[8,], 
                             n_features = 5, 
                             n_permutations = 1000,
                             type = "lime")
an3<-plot(lime_ant)+ ggtitle("LIME plot for Antalya","") 
################### cetaris paribus
cp_ant <- predict_profile(explainer = explain_rf, 
                         new_observation = data[8,])
an4<-plot(cp_ant, variables = c("gdp_per_capita", "population_density","average_household_size","elderly_dep.ratio")) +
  ggtitle("Ceteris-paribus profile", "")
grid.arrange(an1,an2,an3,an4,layout_matrix=rbind(c(1,1),c(2,3),c(4,4)))

############################### muğla
bd_rf_m <- predict_parts(explainer = explain_rf,
                           new_observation = data[59,],
                           type = "break_down_interactions")
m1<-plot(bd_rf_m)+ ggtitle("Break-down plot for Muğla","") 
#####################################
shap_m <- predict_parts(explainer = explain_rf, 
                          new_observation = data[59,], 
                          type = "shap",
                          B = 50)
m2<-plot(shap_m)+ ggtitle("SHAP for Muğla","") 
########################## LIME

model_type.dalex_explainer <- DALEXtra::model_type.dalex_explainer
predict_model.dalex_explainer <- DALEXtra::predict_model.dalex_explainer

lime_m <- predict_surrogate(explainer = explain_rf, 
                              new_observation = data[59,], 
                              n_features = 5, 
                              n_permutations = 1000,
                              type = "lime")
m3<-plot(lime_m)+ ggtitle("LIME plot for Muğla","") 
################### cetaris paribus
cp_diy_m <- predict_profile(explainer = explain_rf, 
                             new_observation = data[59,])
m4<-plot(cp_diy_m, variables = c("gdp_per_capita", "population_density","average_household_size","elderly_dep.ratio","so2")) +
  ggtitle("Ceteris-paribus profile", "") 
grid.arrange(m1,m2,m3,m4,layout_matrix=rbind(c(1,1),c(2,3),c(4,4)))
################################### Samsun 

bd_rf_s <- predict_parts(explainer = explain_rf,
                         new_observation = data[67,],
                         type = "break_down_interactions")
s1<-plot(bd_rf_s)+ ggtitle("Break-down plot for Samsun","") 
#####################################
shap_m <- predict_parts(explainer = explain_rf, 
                        new_observation = data[67,], 
                        type = "shap",
                        B = 50)
s2<-plot(shap_m)+ ggtitle("SHAP for Samsun","") 
########################## LIME
model_type.dalex_explainer <- DALEXtra::model_type.dalex_explainer
predict_model.dalex_explainer <- DALEXtra::predict_model.dalex_explainer

lime_s <- predict_surrogate(explainer = explain_rf, 
                            new_observation = data[67,], 
                            n_features = 5, 
                            n_permutations = 1000,
                            type = "lime")
s3<-plot(lime_s)+ ggtitle("LIME plot for Samsun","") 
################### cetaris paribus
cp_diy_s <- predict_profile(explainer = explain_rf, 
                            new_observation = data[67,])
s4<-plot(cp_diy_s, variables = c("gdp_per_capita", "population_density","average_household_size","age40.59","age80.")) +
  ggtitle("Ceteris-paribus profile", "") 
grid.arrange(s1,s2,s3,s4,layout_matrix=rbind(c(1,1),c(2,3),c(4,4)))
#################################### Çorum
bd_rf_c <- predict_parts(explainer = explain_rf,
                         new_observation = data[24,],
                         type = "break_down_interactions")
c1<-plot(bd_rf_c)+ ggtitle("Break-down plot for Çorum","") 
#####################################
shap_c <- predict_parts(explainer = explain_rf, 
                        new_observation = data[24,], 
                        type = "shap",
                        B = 50)
c2<-plot(shap_c)+ ggtitle("SHAP for Çorum","") 
########################## LIME
library("DALEXtra")
library("lime")
model_type.dalex_explainer <- DALEXtra::model_type.dalex_explainer
predict_model.dalex_explainer <- DALEXtra::predict_model.dalex_explainer

lime_c <- predict_surrogate(explainer = explain_rf, 
                            new_observation = data[24,], 
                            n_features = 5, 
                            n_permutations = 1000,
                            type = "lime")
c3<-plot(lime_c)+ ggtitle("LIME plot for Çorum","") 
################### cetaris paribus
cp_diy_c <- predict_profile(explainer = explain_rf, 
                            new_observation = data[24,])
c4<-plot(cp_diy_c, variables = c("gdp_per_capita", "population_density","average_household_size","unemployment","age20.39")) +
  ggtitle("Ceteris-paribus profile", "") 
grid.arrange(c1,c2,c3,c4,layout_matrix=rbind(c(1,1),c(2,3),c(4,4)))

################################# Model fit according to local neighbors of  istanbul and Şırnak (Figure 4 Figure 5)

id_rfist <- predict_diagnostics(explainer = explain_rf,
                             new_observation = data[40,],
                             neighbours = 10)
plot(id_rfist)
id_rfdiy <- predict_diagnostics(explainer = explain_rf,
                                new_observation = data[26,],
                                neighbours = 10)
plot(id_rfdiy)

id_rfsir <- predict_diagnostics(explainer = explain_rf,
                                new_observation = data[72,],
                                neighbours = 10)
plot(id_rfsir)
### Diyarbakır's negihbors prediction according to average household size variable
id_rf_diyahh <- predict_diagnostics(explainer = explain_rf,
                                 new_observation = data[26,],
                                 neighbours = 10,
                                 variables = "average_household_size")
plot(id_rf_diyahh)
##################### local map according to results of  most important factor in LIME as percentage (pie plot in Figure 3)
limecc<- predict_surrogate(explainer = explain_rf, 
                                                                 new_observation = data[1:81,], 
                                                                  n_features = 5, 
                                                                  n_permutations = 1000,
                                                                 type = "lime")
prfact<-limecc$feature[seq(1,401,5)]
percentage<-table(prfact)*(1/length(prfact))
pie(percentage,labels=names(percentage),col=rainbow(5))
pie(percentage,labels=paste0(names(percentage)," ",round(100*as.numeric(percentage), digits = 0),"%"),col=rainbow(5))
##################### geographically weighted regression ########################
#linear regression models
model1<-lm(data$cumcase~data$unemployment+data$gdp_per_capita+data$average_household_size+data$elderly_dep.ratio+data$automobile_number+data$literacy) # r^2=0.44 
model2<-lm(formula = data$cumcase ~ data$gdp_per_capita) # r^2=0.24
########## residual maps of linear regressiongwr<-readOGR("D__covid yeni.shp",encoding="latin")
gwr@data[["NAME_1"]][c(2,4,11,12,13,17,22,23,24,26)]<-c("Adıyaman","Ağrı","Aydın",
                                                        "Balıkesir","Bartın","Bingöl","Çanakkale","Çankırı","Çorum","Diyarbakır")  
gwr@data[["NAME_1"]][c(27,29,32,35,38,40,41,42,43,49,50,51,54,59,60,61,62,68,71,73,77,81)]<-c("Düzce","Elazığ","Eskişehir","Gümüşhane",
                                                                                              "Iğdır", "İstanbul","İzmir","Kahramanmaraş",  "Karabük", "Kırıkkale","Kırklareli","Kırşehir",
                                                                                              "Kütahya","Muğla","Muş","Nevşehir","Niğde","Şanlıurfa","Şırnak","Tekirdağ","Uşak","Zonguldak" ) 
deg<-read.csv("son veri.csv",header=TRUE,dec = ".",sep= ";",check.names="FALSE",encoding="Turkish")
# merging data with shape file 
gwrsh<-merge(gwr,deg,by="NAME_1")
writeOGR(gwrsh,layer=getwd(),"gwrsh",driver="ESRI Shapefile",encoding="UTF-8")
resids <- round(residuals(model1),2)
map.resids <- cbind(gwrsh, resids) 
names(map.resids)[44] <- "resids"
qtm(map.resids, fill = "resids")+tm_text("resids",size=1/2)

############# GWR model
GWRbandwidth <- gwr.sel(cumcase~unemployment+gdp_per_capita+average_household_size+elderly_dep.ratio+automobile_number+literacy, data = gwrsh, adapt = T)
gwr.model = gwr(cumcase~unemployment+gdp_per_capita+average_household_size+elderly_dep.ratio+automobile_number+literacy,
                data = gwrsh,
                adapt=GWRbandwidth,
                hatmatrix=TRUE,
                se.fit=TRUE) 
results <-as.data.frame(gwr.model$SDF)
gwr.map <- cbind(gwrsh, as.matrix(results))
qtm(gwr.map, fill = "localR2")
results <-as.data.frame(gwr.model$SDF)
gwr.map2 <- st_as_sf(gwr.map)
###Figure 6
qtm(gwr.map2, fill = "localR2")+tm_layout(legend.outside=TRUE)
###
map1 <- tm_shape(gwr.map2) + 
  tm_fill("unemployment.1",
          n = 5,
          style = "quantile")  +
  tm_layout(frame = FALSE,
            legend.text.size = 0.7,
            legend.title.size = 1.2,legend.outside=TRUE,outer.margins=c(0,0,0,0),inner.margins=c(0,0,0,0))
map2 <- tm_shape(gwr.map2) + 
  tm_fill("gdp_per_capita.1",
          n = 5,
          style = "quantile")  +
  tm_layout(frame = FALSE,
            legend.text.size = 0.5,
            legend.title.size = 1.2,legend.outside=TRUE,outer.margins=c(0,0,0,0),inner.margins=c(0,0,0,0))
map3 <- tm_shape(gwr.map2) + 
  tm_fill("average_household_size.1",
          n = 5,
          style = "quantile")  +
  tm_layout(frame = FALSE,
            legend.text.size = 0.5,
            legend.title.size = 1.2,legend.outside=TRUE,outer.margins=c(0,0,0,0),inner.margins=c(0,0,0,0))
map4 <- tm_shape(gwr.map2) + 
  tm_fill("elderly_dep.ratio.1",
          n = 5,
          style = "quantile")  +
  tm_layout(frame = FALSE,
            legend.text.size = 0.5,
            legend.title.size = 1.2,legend.outside=TRUE,outer.margins=c(0,0,0,0),inner.margins=c(0,0,0,0))
map5 <- tm_shape(gwr.map2) + 
  tm_fill("automobile_number.1",
          n = 5,
          style = "quantile")  +
  tm_layout(frame = FALSE,
            legend.text.size = 0.5,
            legend.title.size = 1.2,legend.outside=TRUE,outer.margins=c(0,0,0,0),inner.margins=c(0,0,0,0))
map6 <- tm_shape(gwr.map2) + 
  tm_fill("literacy.1",
          n = 5,
          style = "quantile")  +
  tm_layout(frame = FALSE,
            legend.text.size = 0.5,
            legend.title.size = 1.2,legend.outside=TRUE,outer.margins=c(0,0,0,0),inner.margins=c(0,0,0,0))
#intercept.1
map7<-tm_shape(gwr.map2) + 
  tm_fill("X.Intercept.",
          n = 5,
          style = "quantile")  +
  tm_layout(frame = FALSE,
            legend.text.size = 0.5,
            legend.title.size = 0.6,legend.outside=TRUE)
# creates a clear grid
tiff('gwrdeneme.tiff', units="mm", width=210, height=297, res=300)
grid.newpage()

# assigns the cell size of the grid, in this case 2 by 2
pushViewport(viewport(layout=grid.layout(3,2)))

# prints a map object into a defined cell ( local coefficient maps of Geographically weighted Regression Figure 7)  
print(map1, vp=viewport(layout.pos.col = 1, layout.pos.row =1))
print(map2, vp=viewport(layout.pos.col = 2, layout.pos.row =1))
print(map3, vp=viewport(layout.pos.col = 1, layout.pos.row =2))
print(map4, vp=viewport(layout.pos.col = 2, layout.pos.row =2))
print(map5, vp=viewport(layout.pos.col = 1, layout.pos.row =3))
print(map6, vp=viewport(layout.pos.col = 2, layout.pos.row =3))

dev.off()

#################### Spatial Random Forest
#library(rgeos)
library("SpatialML")
cent     <- as.data.frame(gCentroid(gwr, byid = TRUE, id = gwr@data$NAME_1))
grf6 <- grf(cumcase ~        
             unemployment+             gdp_per_capita+                      
             average_household_size+   automobile_number+       
             literacy+   elderly_dep.ratio                     
           , dframe=data, bw=20,
           kernel="adaptive", coords=cent,importance="impurity")
############################ local Spatial RF importance Figure 8
localvarImpper<-data.frame( grf6$LocalModelSummary$l.VariableImportance$Mean)
colnames(localvarImpper)="var"
df<- data.frame(variables=c("unemployment",             "gdp_per_capita",                      
                             "average_household_size",   "automobile_number",       
                             "literacy",   "elderly_dep.ratio"  ),
                percentage= apply(X = localvarImpper,2 , function(X) (localvarImpper/sum(X))*100)
                                                                       )
df<-df[order(df$var,decreasing=TRUE),]
plt <- ggplot(df) +
  geom_col(aes(y=reorder(variables,+var),x=var), fill = "BLUE", width = 0.6) +
ggtitle("GRF model average variable importance (%)")+ xlab("%IncMSE")+ ylab("variables")
plt
###################################### grf local importance instances
locls<-grf6$Local.Variable.Importance
locls1<-locls[1:dim(locls)[1],]/apply(locls,1,sum)
locls1<-round(locls1,2)
indmax<- apply(locls, 1,  which.max)
primfact<-colnames(locls)[indmax]
dfloc<-cbind.data.frame(locls1,primfact)
dfloc<-cbind.data.frame(dfloc,ID=1:81)
################################### spdf yapma
grfsp<-merge(gwr,dfloc,by="ID")
writeOGR(grfsp,layer=getwd(),"grfsp",driver="ESRI Shapefile",encoding="UTF-8")
#################################### local maps Figure 9
#tiff('fig9.tiff', units="mm", width=210, height=297, res=300)
tmap_mode("plot")+
  tm_shape(grfsp) +
  tm_polygons(c("unemployment",             "gdp_per_capita",                      
                "average_household_size",   "automobile_number",       
                "literacy",   "elderly_dep.ratio"),n=10,style="pretty",title=paste("Spatial Distribution of Local Factors"))+ 
  tm_borders() + tm_scale_bar(position=c("right","bottom"),text.size=2.3,just="right",width=0.6)+
  tm_facets(ncol=2,free.scales.fill=FALSE)+
  tm_legend(legend.position = c("left", "bottom")) +
  tm_layout(inner.margins=c(0.25,0.05,0.3,0.05),legend.outside=TRUE,panel.labels=colnames(dfloc)[1:6],panel.label.color = "black"
            ,panel.label.size=1.1,panel.label.fontface="bold",legend.title.size=2.2,legend.title.fontface = "bold",legend.text.size=1.05)+
  tm_compass(position=c("right","top"),size=1,text.size = 1)
#dev.off()
########### pie graph Figure 10
yzd<-table(primfact)*(1/length(primfact))
pie(yzd,labels=names(yzd),col=c(""))
pie(yzd,labels=paste0(names(yzd)," ",round(100*as.numeric(yzd), digits = 0),"%"),col=rainbow(5))
################## Autocorrelation of residuals and  maps ( OOB and Predicted)
residuals<-grf6$LGofFit[,c(3,5)]
resd<-cbind.data.frame(residuals,ID=1:81)
grf6res<-merge(gwr,resd,by="ID")
tm_shape(grf6res) + 
  tm_fill("LM_ResOOB", midpoint=0,
          palette = "Blues", 
          style = "quantile", 
          title = "OOB Residuals") +
  tm_borders(alpha=.2)  +tm_layout(legend.outside = TRUE)
##########################
tm_shape(grf6res) + 
  tm_fill("LM_ResPred", midpoint=0,
          palette = "Blues", 
          style = "quantile", 
          title = "Predicted Residuals") +
  tm_borders(alpha=.2) + tm_layout(legend.outside = TRUE)
neighbours <- poly2nb(grf6res, queen = FALSE)
listw <- nb2listw(neighbours)
globalMoran <- moran.test(grf6res$LM_ResOOB, listw)
globalMoran
globalMoran2 <- moran.test(grf6res$LM_ResPred, listw)
globalMoran2

############################################## corrected primary factor maps
################ 1-) LIME
library(sf)
library(tmap)
library(ggplot2)
library(viridisLite)

tmap_mode("plot")
tmap_options(component.autoscale = FALSE)  

# =========================
# 1) Merge + prepare palette (guaranteed identical colors)
# =========================
prime_fac <- data.frame(
  ID = 1:81,
  prfact = as.character(prfact),
  stringsAsFactors = FALSE
)

gwr@data$ID <- 1:81
gwrpf <- merge(gwr, prime_fac, by = "ID", all.x = TRUE)

# sf + project to meters
gwrpf_sf_m <- st_transform(st_as_sf(gwrpf), 32636)

# fixed order (THIS controls color mapping)
levels_fixed <- c(
  "average_household_size",
  "elderly_dep.ratio",
  "gdp_per_capita",
  "population_density",
  "so2"
)

# enforce fixed levels
gwrpf_sf_m$prfact <- factor(gwrpf_sf_m$prfact, levels = levels_fixed)

# ONE shared palette (named) -> same for map + barplot
pal <- viridis(length(levels_fixed), option = "D")
names(pal) <- levels_fixed

# =========================
# 2) TMAP (separate map)
# =========================
limeprfacmap <- tm_shape(gwrpf_sf_m) +
  tm_polygons(
    fill = "prfact",
    fill.scale  = tm_scale_categorical(values = pal),
    fill.legend = tm_legend(title = "Top factor")
  ) +
  tm_compass(
    type = "rose",
    position = tm_pos_in("left", "top"),
    offset=c(0.01,-2.4),
    size = 2
  ) +
  tm_scale_bar(
    position = tm_pos_in("left", "bottom"),
    text.size = 0.6
  ) +
  tm_layout(  inner.margins = c(0.3, 0.01, 0.01, 0.01), 
                        outer.margins = c(0,0.1,0.1,0.1), 
                        meta.margins = c(0.2,0,0,0),
    legend.outside = TRUE,
    legend.outside.position = "left",
    frame = FALSE
  )

limeprfacmap

tmap_save(
  tm = limeprfacmap,
  filename = "lime_prfac300dpi.png",
  width = 8,
  height = 6,
  units = "in",
  dpi = 300
)


# =========================
# 3) BARPLOT (separate plot: percentages + descending)
# =========================
bar_df <- as.data.frame(table(gwrpf_sf_m$prfact))
colnames(bar_df) <- c("Factor", "Count")

bar_df$Percent <- 100 * bar_df$Count / sum(bar_df$Count)

# keep Factor levels fixed for color mapping
bar_df$Factor <- factor(as.character(bar_df$Factor), levels = levels_fixed)

# sort by Percent (display order)
bar_df <- bar_df[order(-bar_df$Percent), ]

ggplot(bar_df, aes(x = reorder(Factor, Percent), y = Percent, fill = Factor)) +
  geom_col(width = 0.7) +
  scale_fill_manual(values = pal, drop = FALSE) +
  coord_flip() +
  ylab("Percentage (%)") +
  theme_minimal(base_size = 11) +
  theme(
    legend.position = "none",
    axis.title.y = element_blank(),
    panel.grid.minor = element_blank()
  )
###### Fİg 10 reviewer changes 

tmap_mode("plot")
tmap_options(component.autoscale = FALSE)

# ---- 1) Convert to sf + project to meters ----
grfsp_sf_m <- st_transform(st_as_sf(grfsp), 32636)

# ---- 2) Fixed order (controls color mapping) ----
levels_fixed <- c(
  "average_household_size",
  "elderly_dep.ratio",
  "gdp_per_capita",
  "unemployment" ,
  "literacy",
  "automobile_number"
)

# Ensure prfact exists and is character first (prevents weird factor carry-over)
grfsp_sf_m$prfact <- as.character(grfsp_sf_m$primfact)

# Enforce fixed levels
grfsp_sf_m$prfact <- factor(grfsp_sf_m$primfact, levels = levels_fixed)

# ---- 3) ONE shared named palette ----
pal <- viridis(length(levels_fixed), option = "D")
names(pal) <- levels_fixed

# ---- 4) tmap ----
grfsp_map <- tm_shape(grfsp_sf_m) +
  tm_polygons(
    fill = "primfact",
    fill.scale  = tm_scale_categorical(values = pal),
    fill.legend = tm_legend(title = "Top factor"),
    col = "grey60",
    lwd = 0.2
  ) +
  tm_compass(
    type = "rose",
    position = tm_pos_in("left", "top"),
    offset = c(0.01, -2.4),
    size = 2
  ) +
  tm_scalebar(
    position = tm_pos_in("left", "bottom"),
    text.size = 0.6
  ) +
  tm_layout(
    inner.margins = c(0.25, 0.01, 0.01, 0.01),
    outer.margins = c(0,0.1,0.1,0.1), 
    meta.margins = c(0.22,0,0,0),
    legend.outside = TRUE,
    legend.outside.position = "left",
    frame = FALSE
  )

grfsp_map

# ---- 5) save 300 dpi ----
tmap_save(grfsp_map, "grfsp_prfact_300dpi.png",
          width = 8, height = 6, units = "in", dpi = 300)

# barplot

# Convert to sf if needed
grfsp_sf <- st_as_sf(grfsp)

# FIXED order (same as tmap)

# Make sure the variable is character first (avoids factor carryover)
grfsp_sf$primfact <- as.character(grfsp_sf$primfact)

# Enforce fixed levels (same mapping as tmap)
grfsp_sf$primfact <- factor(grfsp_sf$primfact, levels = levels_fixed)

# Named palette (must be length = number of levels)
pal <- viridis(length(levels_fixed), option = "D")
names(pal) <- levels_fixed

# Frequency table from the SAME factored column
bar_df <- as.data.frame(table(grfsp_sf$primfact))
colnames(bar_df) <- c("Factor", "Count")

# Drop NA (values not in levels_fixed become NA)
bar_df <- bar_df[!is.na(bar_df$Factor), ]

# Percent
bar_df$Percent <- 100 * bar_df$Count / sum(bar_df$Count)

# Keep the same level order for coloring (important)
bar_df$Factor <- factor(as.character(bar_df$Factor), levels = levels_fixed)

# Sort for display (does NOT affect fill mapping)
bar_df <- bar_df[order(-bar_df$Percent), ]

# Plot
bar_plot <- ggplot(bar_df,
                   aes(x = reorder(Factor, Percent),
                       y = Percent,
                       fill = Factor)) +
  geom_col(width = 0.7) +
  scale_fill_manual(values = pal, drop = FALSE) +
  coord_flip() +
  ylab("Percentage (%)") +
  xlab(NULL) +
  theme_minimal(base_size = 11) +
  theme(
    legend.position = "none",
    panel.grid.minor = element_blank()
  )

bar_plot

ggsave("grfsp_barplot_300dpi.png",
       bar_plot,
       width = 6,
       height = 4,
       units = "in",
       dpi = 300)



