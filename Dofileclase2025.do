****Aplicació 1: Mi primera regresión en Stata
****Profesor: William Sánchez
****Grupo Lambda 2025
*	~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
*	Resumen de principales estadísticos
*	~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
su lnsalario educ edad edad2 urban casado femenino

*	~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
*	Análisis Gráfico
*	~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

*  Boxplot/violin-like por sexo (box + strip)
graph box lnsalario, over(femenino) title("ln(salario) por sexo") ///
    ytitle("ln(salario)") marker(1, mcolor(%30)) nooutsides
stripplot lnsalario, over(femenino) jitter(7) vertical addplot(box) ///
    title("ln(salario) por sexo (detalle)") ytitle("ln(salario)")
	
* 	ln(sal) vs escolaridad (nube + LOESS)
twoway (scatter lnsalario educ, msize(small) mcolor(%40)) ///
       (lowess  lnsalario educ, bwidth(0.8)), ///
       title("Relación ln(salario) – escolaridad") ///
       xtitle("Años de escolaridad") ytitle("ln(salario)")
	   
* 	ln(sal) vs edad (nube + ajuste cuadrático)
	   twoway (scatter lnsalario edad, msize(small) mcolor(%35)) ///
       (qfitci  lnsalario edad), ///
       title("Relación ln(salario) – edad (cuadrática)") ///
       xtitle("Edad") ytitle("ln(salario)")
	   
* 	Densidades superpuestas por formalidad
twoway (kdensity lnsalario if femenino==1, lwidth(medthick)) ///
       (kdensity lnsalario if femenino==0, lpattern(dash)), ///
       legend(order(1 "Femenino" 2 "Masculino") pos(6) ring(0)) ///
       title("Distribuciones de ln(salario) por género") xtitle("ln(salario)")

* 	 Matriz de dispersión (rápida)
graph matrix lnsalario educ edad edad2, half ///
    title("Matriz de dispersión (selección de variables)")

*	~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
*	Análisis de Regresiones
*	~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
***	Instalar outreg2 para elaborar tablas en base a los resultados
ssc install outreg2

reg lnsalario educ
estimate store model1
reg lnsalario educ edad edad2 urban
estimate store model2
reg lnsalario educ edad edad2 urban casado femenino
estimate store model3
*** incorporando una variable de interacción educ con género
gen educxfemenino=educ*femenino
reg lnsalario educ edad edad2 urban casado femenino educxfemenino
estimate store model4

***reg lnsalario educ edad edad2 urban casado femenino c.educ#c.femenino
outreg2 [model1 model2 model3 model4] using Resultado.xls, replace
outreg2 [model1 model2 model3 model4] using Resultado1.xls, replace ///
addstat("R-cuadrado ajustado", e(r2_a))

*** Mostrar AIC y BIC en la ventana de resultados
estimates stats model1 model2 model3 model4, all
reg lnsalario educ edad edad2 urban casado femenino c.educ#c.femenino
estat ic
reg lnsalario educ edad edad2 urban casado femenino
estat ic
*	~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
*	Prueba de hipótesis
*	~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

*** Prueba de hipótesis individual
reg lnsalario educ edad edad2 urban casado femenino
test femenino
test urban=casado

*** Prueba de hipótesis conjunta

test educ edad edad2 urban casado femenino

///Probar si la edad que maximiza el salario es 50
nlcom - _b[edad]/(2*_b[edad2]) - 50

gen lw_predicho=_b[edad]*edad +_b[edad2]*edad2
scatter lw_predicho edad

*	~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
*	Diagnóstico del modelo
*	~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
///Especificación del modelo
* Estima el modelo
reg lnsalario educ edad edad2 urban casado femenino
reg lnsalario educ edad edad2 urban casado femenino c.educ#c.femenino
* RESET de Ramsey (usa potencias del valor ajustado)
estat ovtest
***Interpretación:
***H0: el modelo está bien especificado (no hacen falta términos no lineales adicionales).
***p < 0.05 ⇒ rechazas H0 → posible mala especificación (faltan términos, forma funcional, interacciones, etc.).
///probando multicolinealidad
***VIF (Variance Inflation Factor) y Tolerancia
***VIF > 10 (o > 5 en enfoques más conservadores) → problema serio/probable.
estat vif
***Matriz de correlaciones entre regresores
***Correlaciones |ρ| ≥ 0.8–0.9 suelen ser señal de alerta (aunque ///
///multicolinealidad puede existir sin correlaciones bivariadas altas).
pwcorr educ edad edad2 urban casado viudo femenino, sig star(0.05)

***Una forma de corregir multicolinealidad
summ edad, meanonly
gen edad_c = edad - r(mean)
gen edad2_c = edad_c^2
reg  lnsalario  educ edad_c edad2_c urban casado femenino

///Probando normalidad y heterocedasticidad
regress lnsalario educ edad edad2 urban casado femenino
predict uhat,resid
 
kdensity uhat
sktest uhat, noadj

histogram uhat, normal kdensity

****Test para detectar Heterocedasticidad
hettest educ edad edad2 urban casado femenino, iid

///Corrigiendo heterocedasticidad  

reg lnsalario educ edad edad2 urban casado femenino, robust 
*****************************************************************************
*****Evaluando estabilidad de parámetros: test de Chow****************************
*****************************************************************************
**reg lnsalario educ edad edad2 urban casado viudo femenino if femenino==1 
**scalar ssrum=e(rss)
**scalar dfum=e(df_r)
**disp dfum

**reg lnsalario educ edad edad2 urban casado viudo femenino if femenino==0
**scalar ssruf=e(rss)
**scalar dfuf=e(df_r)

***Use the Chow to test for the separation of data
**scalar ssru=ssrum+ssruf
**scalar dfu=dfum+dfuf
**scalar g=dfc-dfu
**scalar numer=((ssrc-ssru)/g)
**scalar denom=(ssru/dfu)
**scalar Chow_test=numer/denom
**display Chow_test
**display Ftail(g,dfu,Chow_test)

*****************************************************************************
*****Evaluando estabilidad de parámetros cuando existe              heterocedasticidad***********************************************************
*****************************************************************************
***Use gender iteractions to test gender differences in wage effects  
gen i1=femenino*educ
gen i2=femenino*edad
gen i3=femenino*edad2
gen i4=femenino*urban
gen i5=femenino*casado


reg lnsalario educ edad edad2 urban casado femenino i1 i2 i3 i4 i5

reg lnsalario educ edad edad2 urban casado femenino i1 i2 i3 i4 i5, robust 

test i1 i2 i3 i4 i5
reg lnsalario educ edad edad2 urban casado femenino i1, robust
test educ edad edad2 urban casado femenino i1
*****************************************************************************
*****Regresiones cuantílicas***********************************************************
*****************************************************************************
/* Usamos qreg para nuestras regresiones. La opción quantile() nos
   especifica el cuantil. Por default Stata utiliza la mediana (0.5) */
qreg lnsalario educ edad edad2 urban casado femenino, quantile (0.1)
estimate store model1
qreg lnsalario educ edad edad2 urban casado femenino
estimate store model2 		
qreg lnsalario educ edad edad2 urban casado femenino, quantile (0.9)
estimate store model3
regress lnsalario educ edad edad2 urban casado femenino, robust
estimate store model4
outreg2 [model1 model2 model3 model4] using UNMSM2025-2.xls, replace

* Podemos guardar los valores ajustados de qreg para hacer un gráfico
qui qreg lnsalario educ, quantile (0.1)
predict q10
qui qreg lnsalario educ, quantile (0.9)
predict q90
twoway (scatter lnsalario educ) (lfit lnsalario educ) (lfit q10 educ) (lfit q90 educ)