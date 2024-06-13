
# required packages for the analyses
library(sf)
library(spdep)
library(RColorBrewer)
library(tmap)
library(randomForest)
library(DALEX)
library(ggplot2)
library(gridExtra)
library(tmap)
library(rgdal)
library(tmap)
library(tmaptools)
library(spgwr)
library(grid)

data<- read.csv("data.csv",header=TRUE,dec = ".",sep= ";",check.names=TRUE,fileEncoding ="latin1")
data<-data[,-1]
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
 
########################### Istanbul  
bd_rf_ist <- predict_parts(explainer = explain_rf,
                         new_observation = data[40,],
                           type = "break_down")

bd_rf <- predict_parts(explainer = explain_rf,
                       new_observation = data[40,],
                       type = "break_down_interactions")
i1<-plot(bd_rf)+ ggtitle("Break-down plot for Istanbul","") 
#####################################
shap_ist <- predict_parts(explainer = explain_rf, 
                            new_observation = data[40,], 
                            type = "shap",
                            B = 50)
i2<-plot(shap_ist)+ ggtitle("SHAP for Istanbul","") 
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
i3<-plot(lime_ist)+ ggtitle("LIME plot for Istanbul","") 
################### cetaris paribus
cp_ist_rf <- predict_profile(explainer = explain_rf, 
                                 new_observation = data[40,])
i4<-plot(cp_ist_rf, variables = c("gdp_per_capita", "age20.39","average_household_size","population_density")) +
  ggtitle("Ceteris-paribus profile", "") 
################### histograms of positive contributing variables accordimg to LIME results

i5<-ggplot(data, aes(x=age20.39))+
  geom_histogram(binwidth = 0.05,fill="#5F9EA0", color="#4682B4", alpha=0.95)+ theme_minimal()
i6<-ggplot(data, aes(x=average_household_size))+
  geom_histogram(binwidth = 0.2,fill="#5F9EA0", color="#4682B4", alpha=0.95)+ theme_minimal()
i7<-ggplot(data, aes(x=gdp_per_capita))+
  geom_histogram(binwidth = 750,fill="#5F9EA0", color="#4682B4", alpha=0.95)+ theme_minimal()
i8<-ggplot(data, aes(x=population_density))+
  geom_histogram(binwidth = 200,fill="#5F9EA0", color="#4682B4", alpha=0.95)+ theme_minimal()
################ D0stanbul iC'in instance bazlD1 aC'D1klama
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
################################# Şırnak
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
cp_mug <- predict_profile(explainer = explain_rf, 
                             new_observation = data[59,])
m4<-plot(cp_mug, variables = c("gdp_per_capita", "population_density","average_household_size","elderly_dep.ratio","so2")) +
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
cp_sam <- predict_profile(explainer = explain_rf, 
                            new_observation = data[67,])
s4<-plot(cp_sam, variables = c("gdp_per_capita", "population_density","average_household_size","age40.59","age80.")) +
  ggtitle("Ceteris-paribus profile", "") 
grid.arrange(s1,s2,s3,s4,layout_matrix=rbind(c(1,1),c(2,3),c(4,4)))
#################################### Çorum
bd_rf_c <- predict_parts(explainer = explain_rf,
                         new_observation = data[24,],
                         type = "break_down_interactions")
c1<-plot(bd_rf_c)+ ggtitle("Break-down plot for Corum","") 
#####################################
shap_c <- predict_parts(explainer = explain_rf, 
                        new_observation = data[24,], 
                        type = "shap",
                        B = 50)
c2<-plot(shap_c)+ ggtitle("SHAP for Corum","") 
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
c3<-plot(lime_c)+ ggtitle("LIME plot for Corum","") 
################### cetaris paribus
cp_c <- predict_profile(explainer = explain_rf, 
                            new_observation = data[24,])
c4<-plot(cp_c, variables = c("gdp_per_capita", "population_density","average_household_size","unemployment","age20.39")) +
  ggtitle("Ceteris-paribus profile", "") 
grid.arrange(c1,c2,c3,c4,layout_matrix=rbind(c(1,1),c(2,3),c(4,4)))

################################# Model fit according to local neighbors of  istanbul and ED1rnak (Figure 4 Figure 5)

id_rfist <- predict_diagnostics(explainer = explain_rf,
                             new_observation = data[40,],
                             neighbours = 20)
plot(id_rfist)


id_rfsir <- predict_diagnostics(explainer = explain_rf,
                                new_observation = data[72,],
                                neighbours = 20)
plot(id_rfsir)
### DiyarbakD1r's neighbors prediction according to average household size variable
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
########## residual maps of linear regression
gwr<-readOGR("D__covid yeni.shp",encoding="latin")
gwr@data[["NAME_1"]][c(2,4,11,12,13,17,22,23,24,26)]<-c("Adıyaman","Ağrı","Aydın",
                                                        "Balıkesir","Bartın","Bingöl","Çanakkale","Çankırı","Çorum","Diyarbakır")  
gwr@data[["NAME_1"]][c(27,29,32,35,38,40,41,42,43,49,50,51,54,59,60,61,62,68,71,73,77,81)]<-c("Düzce","Elazığ","Eskişehir","Gümüşhane",
                                                                                              "Iğdır", "İstanbul","İzmir","Kahramanmaraş",  "Karabük", "Kırıkkale","Kırklareli","Kırşehir",
                                                                                              "Kütahya","Muğla","Muş","Nevşehir","Niğde","Şanlıurfa","Şırnak","Tekirdağ","Uşak","Zonguldak" ) 
deg<-read.csv("data.csv",header=TRUE,dec = ".",sep= ";",check.names="FALSE",encoding="Turkish")
# merging data with shape file 
gwrsh<-merge(gwr,deg,by="NAME_1")
#writeOGR(gwrsh,layer=getwd(),"gwrsh",driver="ESRI Shapefile",encoding="UTF-8")
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
library(rgeos)
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
pie(yzd,labels=paste0(names(yzd)," ",round(100*as.numeric(yzd), digits = 0),"%"),col=rainbow(6))
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
