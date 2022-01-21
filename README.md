# EVALUACION-FINAL


#####PARA EFECTOS DE LA EVALUACION 2, SE USARÁ COMO VARIABLE DEPENDIENTE PERCEPCION_4 

base_85["progresodechile"] <- as.numeric(as.character(base_85$percepcion_4))


## 5.1 Variable dummy 1 PERCEPCION_4 -- BINARIA
base_85$progresodechile.Dummy<-ifelse(base_85$progresodechile=="1",1,0)
table(base_85$progresodechile.Dummy)

base_85["age_N-2"] <- as.numeric(as.character(base_85$edad))

##### CORRELACION USANDO LA VARIABLE DEPENDIENTE PERCEPCION_4 Y LAS VARIABLES INDEPENDIENTES, EDAD, RELIGIÓN Y SEXO

cor.test(as.numeric(base_85$percepcion_4), as.numeric(base_85$religion_1), 
         method = c("pearson") )

cor.test(as.numeric(base_85$percepcion_4), as.numeric(base_85$`age_N-2`), 
         method = c("pearson") )

cor.test(as.numeric(base_85$percepcion_4), as.numeric(base_85$sexo), 
         method = c("pearson") )

### 10.1. Definir variables para regresión logistica

##### 10.1.1. Variable Dependiente: base_85$progresodechile.Dummy

# Variable Independiente  1: Edad
# Variable Independiente  2: Religion
# Variable Independiente  3: sexo

table(base_85$progresodechile.Dummy)

m3 = glm(base_85$progresodechile.Dummy ~ X, data = base_85,
         binomial())
summary(m3)

library(rsq)
# 12. Calcular bondad de ajuste
rsq(m3)

# 13. Tabular Regresion Logistica Binaria
library(stargazer)
stargazer(m3, title = "Modelo 3", align =TRUE, out = "resultados25.txt")

#####MODELO ADICIONAL  " Modelo 4"

m4 = glm(base_85$progresodechile.Dummy ~ base_85$`age_N-2` + 
           base_85$religion_1,
         data = base_85,
         binomial())
summary(m4)
stargazer(m4, title = "Modelo 4", align =TRUE, out = "resultados26.txt")
rsq(m4)

#### MODELO ADICIONAL "Modelo 5"

m5 = glm(base_85$progresodechile.Dummy ~ base_85$`age_N-2` + 
           base_85$religion_1 + base_85$sexo,
         data = base_85,
         binomial())
summary(m5)

stargazer(m5, title = "Modelo 5", align =TRUE, out = "resultados27.txt")

rsq(m5)

#### MODELO ADICIONAL "Modelo 6"

base_85["GSE-N"] <- as.numeric(as.character(base_85$gse))

m6 = glm(base_85$progresodechile.Dummy ~ base_85$`age_N-2` + 
           base_85$religion_1  + base_85$`GSE-N` + base_85$sexo,
         data = base_85,
         binomial())
summary(m6)
stargazer(m6, title = "Modelo 6", align =TRUE, out = "resultados28.txt")
rsq(m6)

#### TABULAR REGRESION LOGISTICA "Modelos 3 y 4"
stargazer(m3, m4, title = "Modelos 3 y 4", align =TRUE,  out = "resultados29.txt") 
          
stargazer(m5, m6, title = "Modelos 5 y 6", align =TRUE, 
                    out = "resultados30.txt")   
