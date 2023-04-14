library(multilevel)#ativa o pacote para execução do modelo ANOVA

#Modelo Nulo
Null.Model <- lme(PolAtt10~1, random = ~1|YC, data = LatinEuroMMfinal, control = list(opt="optim"), na.action=na.omit)
Null.Model

VarCorr(Null.Model)

0.8613843/(0.8613843+9.8211077)

Null.gls<-gls(PolAtt10 ~1,data=LatinEuroMMfinal, control=list(opt="optim"),na.action=na.omit)
logLik(Null.gls)*-2
#'log Lik.' 988025.4 (df=2)
logLik(Null.Model)*-2
#'log Lik.' 973293.5 (df=3)
988025.4-973293.5 
#14731.9 é a diferença entre os dois valores, que é significativamente em uma distribuição de 
#qui<-quadrado com 1 grau de liberdade, ou seja, indica significativa variação do intercepto.
anova(Null.gls, Null.Model)


Model1.1Att <- lme(PolAtt10~ Gndr+FxAge+Ed+GDP+HDI100+FREEDOM+ENEP+POLSYS_rec, 
                   random = ~1|YC, data = LatinEuroMMfinal, na.action=na.omit,
                   control = list(opt="optim"))
summary(Model1.1Att)

VarCorr(Model1.1Att)


# Modelo com inclinações variaveis

#Escolaridade
Model.2.AttEd<-lme(PolAtt10~Gndr+FxAge+Ed+GDP+HDI100+FREEDOM+ENEP+POLSYS_rec,random=~Ed|YC, data=LatinEuroMMfinal, na.action=na.omit, control=list(opt="optim"))
summary(Model.2.AttEd)
Model.2.Att.aEd<-update(Model.2.AttEd,random=~1|YC)
anova(Model.2.Att.aEd,Model.2.AttEd)

#Interação HDI e escolaridade
ModelAtt.HDI.Ed<-lme(PolAtt10~Gndr+FxAge+Ed+Ed:HDI100+GDP+HDI100+FREEDOM+ENEP+POLSYS_rec,random=~Ed|YC, data=LatinEuroMMfinal, na.action=na.omit, control=list(opt="optim"))
round(summary(ModelAtt.HDI.Ed)$tTable,dig=3)
summary(ModelAtt.HDI.Ed)

TDAT.Att.HDI.Ed<-data.frame(Ed=c(0,0,1,1),
                            Gndr=c(0,0,0,0),
                            FxAge=c(2,2,2,2),
                            GDP=c(3.021924, 3.021924, 3.021924, 3.021924),
                            HDI100=c(5.70, 9.48, 5.70, 9.48),
                            FREEDOM=c(1.643,1.643,1.643,1.643),
                            ENEP=c(4.89, 4.89, 4.89, 4.89),
                            POLSYS_rec=c(0,0,0,0))
predict(ModelAtt.HDI.Ed,TDAT.Att.HDI.Ed,level=0)
TDAT.Att.HDI.Ed$PolAtt<-predict(ModelAtt.HDI.Ed,TDAT.Att.HDI.Ed,level=0)
with(TDAT.Att.HDI.Ed,interaction.plot(Ed,HDI100,PolAtt, legend=F,xlab="Education", 
                                      ylab="mean of Cognitive Orientation for Politics", 
                                      main="Interaction Education and HDI"))
