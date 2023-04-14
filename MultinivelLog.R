#Instalação do pacote lme4
install.packages("lme4")
library(lme4)

#Modelo Nulo
Nulo.prot <- glmer(prot ~ (1 | PA), family = binomial("logit"), data = ppar)
summary(Nulo.prot)

#Modelo alternativo
Nulo.prot.A <- glm(prot ~ 1, data = ppar, family = binomial("logit"))
#Diferença 
logLik(Nulo.prot.A)-logLik(Nulo.prot)

#Caterpillar para o efeito-país
u0 <- ranef(Nulo.prot, postVar = TRUE)
u0se <- sqrt(attr(u0[[1]], "postVar")[1, , ])
commid <- as.numeric(rownames(u0[[1]]))
u0tab <- cbind("commid" = commid, "u0" = u0[[1]], "u0se" = u0se)
colnames(u0tab)[2] <- "u0"
u0tab <- u0tab[order(u0tab$u0), ]
u0tab <- cbind(u0tab, c(1:dim(u0tab)[1]))
u0tab <- u0tab[order(u0tab$commid), ]
colnames(u0tab)[4] <- "u0rank"
plot(u0tab$u0rank, u0tab$u0, type = "n", xlab = "u_rank", ylab = "Modas Condicionais de Protesto por pais", ylim = c(-2, 2))
segments(u0tab$u0rank, u0tab$u0 - 1.96*u0tab$u0se, u0tab$u0rank, u0tab$u0 + 1.96*u0tab$u0se)
points(u0tab$u0rank, u0tab$u0, col = "blue")
abline(h = 0, col = "red")

#Modelo Micro
Mod.prot.n1 <- glmer(prot ~ denr.evan + assb.n + educ + intpol.n + sdem.n + svida.n 
                      + confi.in + ideo + (1 | PA), family = binomial("logit"), data = ppar)
summary(Mod.prot.n1)
#Exponenciais
exp(fixef(Mod.prot.n1))
coef(Mod.prot.n1)

#Modelo Completo
Mod.prot.n2 <- glmer(prot ~ denr.evan + assb.n + educ + intpol.n + sdem.n + svida.n + confi.in + ideo +
                       vdem10 + Shipew10 + ARDA_SUPORTOFICIAL + (1 | PA), family = 
                       binomial("logit"), data = ppar)
summary(Mod.prot.n2)

#Modelo com Interações
Mod.prot.R.D <- glmer (prot ~ denr.evan + denr.evan:Shipew10 + assb.n + educ + intpol.n + sdem.n + 
  svida.n + confi.in + ideo + vdem10 + Shipew10 + ARDA_SUPORTOFICIAL + (1 | PA), family = 
  binomial("logit"), data = ppar)

summary(Mod.prot.R.D)

exp(fixef(Mod.prot.R.D))
