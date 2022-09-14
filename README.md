# AnaFatorial
require(readxl)

require(corrplot)

require(psych)

# url <- "https://github.com/Smolski/livroavancado/raw/master/creme_dental_exemplo1.xlsx"
# destfile <- "creme_dental_exemplo1.xlsx"
# curl::curl_download(url, destfile)
# creme_dental_exemplo1 <- read_excel(destfile)

creme_dental_exemplo1=dados=read.csv2(file.choose())
attach(creme_dental_exemplo1)

summary(creme_dental_exemplo1)

matcor <- cor(creme_dental_exemplo1)
print(matcor, digits = 2)


# PLOT !!!!!!!!!!!!!!!!!
corrplot(matcor, method="circle")

cortest.bartlett(creme_dental_exemplo1)

# A estatística KMO maior que  0,5  também concorda quanto ao fato de que a
# análise fatorial pode ser considerada uma técnica apropriada para analisar 
# a matriz de correlação.
KMO(creme_dental_exemplo1)

fit <- princomp(creme_dental_exemplo1,cor=TRUE)
fit

summary(fit)


screeplot(fit)


PCAdente <- principal(creme_dental_exemplo1, nfactors=2,
                    n.obs=30,rotate="none", scores=TRUE)
PCAdente

PCAdentevarimax <- principal(creme_dental_exemplo1, nfactors=2,
                           n.obs=30,rotate="varimax",scores=TRUE)
PCAdentevarimax


PCAdentevarimax$values


PCAdentevarimax$loadings



# PLOT !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
windows()+ biplot(PCAdentevarimax)


factor.scores(creme_dental_exemplo1,PCAdentevarimax, 
              Phi = NULL, 
              method = c("Thurstone", "tenBerge", "Anderson",
                         "Bartlett", "Harman","components"),
              rho=NULL)

# procurar rotação!!!!!!!!!!!
