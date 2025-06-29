
use "tu_base.dta", clear

* Declarar datos de series de tiempo trimestrales
gen tiempo = tq(2004q1) + _n - 1
format tiempo %tq
tsset tiempo

* Crear logaritmos
gen ln_M = ln(IMPOen2015)
gen ln_X = ln(EXPOen2015)
gen ln_PIB = ln(PBIen2015)
gen ln_TCRM = ln(ITCRM)
gen ln_PIBG = ln(PBIG)

///Análisis de estacionariedad 
//Testeo lags necesarios
varsoc ln_M
varsoc ln_X
varsoc ln_PIB
varsoc ln_TCRM
varsoc ln_PIBG

//Ahora si
* Test de raíz unitaria con constante (drift)
dfuller ln_M,     lags(2) drift
dfuller ln_X,     lags(4) drift
dfuller ln_PIB,   lags(4) drift
dfuller ln_TCRM,  lags(1) drift
dfuller ln_PIBG,  lags(4) drift

//Testeo en primeras diferencias
varsoc d_ln_M
varsoc d_ln_X
varsoc d_ln_PIB
varsoc d_ln_TCRM
varsoc d_ln_PIBG

* TEST ADF para series en diferencias, con rezagos óptimos según varsoc

* 1. Demanda de importaciones
dfuller d_ln_M, lags(4) drift

* 2. Exportaciones
dfuller d_ln_X, lags(4) drift

* 3. PIB nacional
dfuller d_ln_PIB, lags(4) drift

* 4. Tipo de cambio real multilateral
dfuller d_ln_TCRM, lags(0) drift

* 5. PIB global
dfuller d_ln_PIBG, lags(4) drift
* a ver si es I(2)
gen dd_ln_PIBG = D.d_ln_PIBG
dfuller dd_ln_PIBG, lags(4) drift

*La segunda diferencia (dd_ln_PIBG) es estacionaria. Las primeras diferencias no lo eran claramente. Por lo tanto, ln_PIBG es una serie integrada de orden 2, I(2). No podés hacer pruebas de cointegración con ln_X (exportaciones) y ln_PIBG directamente, porque las variables deben tener el mismo orden de integración para aplicar Engle–Granger. Por otro lado, para importaciones, tus variables (ln_M, ln_PIB, ln_TCRM) son todas I(1), así que sí podés aplicar Engle–Granger allí.


///Gráficos
tsline ln_M, ///
    title("Tendencia: ln(M) - Importaciones") ///
    ytitle("Logaritmo de M") ///
    xtitle("Tiempo")

tsline ln_PIB, ///
    title("Tendencia: ln(PIB) - Producto Interno Bruto") ///
    ytitle("Logaritmo de PIB") ///
    xtitle("Tiempo")

tsline ln_TCRM, ///
    title("Tendencia: ln(TCRM) - Tipo de Cambio Real Multilateral") ///
    ytitle("Logaritmo del TCRM") ///
    xtitle("Tiempo")

tsline ln_X, ///
    title("Tendencia: ln(X) - Exportaciones") ///
    ytitle("Logaritmo de X") ///
    xtitle("Tiempo")

tsline ln_PIBG, ///
    title("Tendencia: ln(PIBG) - PIB de Socios Comerciales") ///
    ytitle("Logaritmo de PIBG") ///
    xtitle("Tiempo")

tsline d_ln_M, ///
    title("Variación: Δln(M) - Importaciones") ///
    ytitle("Variación logarítmica") ///
    xtitle("Tiempo")

tsline d_ln_PIB, ///
    title("Variación: Δln(PIB)") ///
    ytitle("Variación logarítmica") ///
    xtitle("Tiempo")

tsline d_ln_TCRM, ///
    title("Variación: Δln(TCRM)") ///
    ytitle("Variación logarítmica") ///
    xtitle("Tiempo")

tsline d_ln_X, ///
    title("Variación: Δln(X) - Exportaciones") ///
    ytitle("Variación logarítmica") ///
    xtitle("Tiempo")

tsline d_ln_PIBG, ///
    title("Variación: Δln(PIBG)") ///
    ytitle("Variación logarítmica") ///
    xtitle("Tiempo")


///Cointegración
reg ln_M ln_PIB ln_TCRM
predict resid_M, resid
dfuller resid_M, noconstant

gen ln_M_hat = _b[ln_PIB]*ln_PIB + _b[ln_TCRM]*ln_TCRM + _b[_cons]
twoway (line ln_M ln_M_hat tiempo, lpattern(solid dash) lcolor(black blue)), ///
       legend(label(1 "ln_M (observado)") label(2 "ln_M ajustado")) ///
       title("Ajuste de largo plazo: Importaciones") xtitle("Año") ///
       ytitle("Log de Importaciones")
	   
tsline resid_M, title("Residuos de cointegración (Importaciones)") ///
ylabel(, angle(horizontal)) xtitle("Año")


///Paso 4. Test de Gregory–Hansen (con quiebre estructural)
ssc install ghansen
ghansen ln_M ln_PIB ln_TCRM, break(trend) lagmethod(aic) maxlags(4)

*Esto refuerza lo que observaste antes con Engle-Granger: hay cointegración en importaciones, y ahora tenés indicios de que esta relación cambia estructuralmente en torno a 2018q4. Esto tiene sentido si pensás en eventos como la crisis cambiaria y macroeconómica en Argentina a partir de 2018, que pudo haber alterado la elasticidad ingreso/precio de las importaciones.

gen break_dummy = (tin(2018q4, 2024q4))  // D=1 a partir del quiebre
twoway (tsline resid_M if break_dummy==0, lcolor(blue)) ///
       (tsline resid_M if break_dummy==1, lcolor(red)), ///
       legend(label(1 "Antes del quiebre") label(2 "Después del quiebre")) ///
       title("Residuos del modelo de largo plazo") xtitle("Trimestre") ///
       note("Quiebre estructural estimado: 2018q4")

///ECM
//si hay coint
gen L_resid_M = L.resid_M  // rezago del error

reg d_ln_M d_ln_PIB d_ln_TCRM L_resid_M L.d_ln_M L.d_ln_PIB L.d_ln_TCRM


///Paso 6. Exportaciones, repetir procedimiento. Las series de exportaciones no muestran cointegración, es decir, no hay evidencia clara de una relación de largo plazo estable entre variables como exportaciones, PIB de socios comerciales y tipo de cambio real. Esto implica que el modelo de corrección del error (ECM) no sería apropiado para exportaciones porque no hay equilibrio de largo plazo a corregir. Por eso, el análisis para exportaciones se enfocaría más en el corto plazo o en modelos alternativos. Trabajar con diferencias (corto plazo): Estimar modelos en primeras diferencias (o en tasas de crecimiento) sin término de corrección del error, porque no hay cointegración.

reg d_ln_X d_ln_PIBG d_ln_TCRM L.d_ln_X L.d_ln_PIBG L.d_ln_TCRM

///Para estimarlo en el mismo procedimiento
vec ln_M ln_PIB ln_TCRM, lags(4) trend(constant)


