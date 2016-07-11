;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;Declaracion de las estructuras que voy a usar.

(deftemplate Sector
  (slot nombre)
  (slot Var_dia)
  (slot capitalizacion)
  (slot PER)
  (slot RPD)
  (slot percent_ibex)
  (slot var_5_dias)
  (slot PER_3_Cons)
  (slot PER_5_cons)
  (slot var_mens)
  (slot var_tri)
  (slot var_sem)
  (slot var_anio))

(deftemplate Empresa
  (slot nombre)
  (slot precio)
  (slot var_dia)
  (slot capitalizacion)
  (slot PER)
  (slot RPD)
  (slot taman)
  (slot ibex_perc)
  (slot et_PER)
  (slot et_RPD)
  (slot sector )
  (slot var_5_dias)
  (slot PER_B_3_Cons)
  (slot PER_B_5_Cons)
  (slot var_sect)
  (slot var_sect_5)
  (slot var_mens)
  (slot var_tri)
  (slot var_sem)
  (slot var_anio)
)

(deftemplate Noticia
  (slot nombre)
  (slot tipo)
)

(deftemplate liquidez
	(slot dinero)
	(slot patrimonio)
)

(deftemplate EmpresaCartera
	(slot nombre)
	(slot acciones)
	(slot v_total)
)

(deftemplate valor_inestable
	(slot nombre)
	(slot duracion)
	(multislot motivo)
)

(deftemplate valor_estable
	(slot nombre)
	(slot duracion)
	(multislot motivo)
)

(deftemplate valor_peligroso
	(slot nombre)
	(multislot motivo)
)

(deftemplate valor_infravalorado
	(slot nombre)
	(multislot motivo)
)

(deftemplate valor_supervalorado
	(slot nombre)
	(multislot motivo)
)

(deftemplate Explicacion
	(slot nombre)
	(multislot motivo)
)

(deftemplate Proposicion
	(slot tipo)
	(slot nombre)
	(slot nombre2)
	(slot rendimiento_esperado)
	(multislot Explicacion)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;; BLOQUE DE LECTURA DE DATOS DESDE ARCHCIVOS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;Regla que se encarga de abrir el archivo con los datos actualizados de la
;;bolsa, que estan en el archivo bolsa_hoy.txt
(defrule consultaInic
  (declare (salience 10))
  =>
  (printout t "¿En que archivo se encuentran los datos de los sectores?" crlf)
  (bind ?a (read))
  (assert (archivo ?a))
  (assert (LeerSectores))
)


(defrule AbreSectores
  (declare (salience 10)) 
  ?l<-(LeerSectores)
  (archivo ?a)
  =>
  (printout t "abro " ?a crlf)
  (open ?a sectores_data)
  (retract ?l)
  (assert (ArchivoSectoresAbierto))
  (assert (SeguirLeyendo1))
)

;;Tiene error function name must be a symbol
(defrule LeerSectores 
  (declare (salience 9))
  ?f<-(ArchivoSectoresAbierto)
  ?s<-(SeguirLeyendo1)
  (archivo ?a)
  =>
  (printout t "estoy leyendo los sectores en " ?a crlf)
  (bind ?campo (read sectores_data))
  (retract ?s)
  ;;Compruebo si es EOF, o nuevoSector
  (if (neq ?campo EOF) then
    (assert(Sector
	(nombre ?campo)
	(Var_dia (read sectores_data))
	(capitalizacion (read sectores_data))
	(PER (read sectores_data))
	(RPD (read sectores_data))
	(percent_ibex (read sectores_data))
	(var_5_dias (read sectores_data))
	(PER_3_Cons (read sectores_data))
	(PER_5_cons (read sectores_data))
	(var_mens (read sectores_data))
	(var_tri (read sectores_data))
	(var_sem (read sectores_data))
	(var_anio (read sectores_data))))
    (assert (SeguirLeyendo1))
    ))

(defrule CierraArchivoSectores
  (declare (salience -12))
  ?s<-(ArchivoSectoresAbierto)
  (archivo ?a)
  =>
  (printout t "cierro archivo de sectores " ?a crlf)
  (close sectores_data)
  (assert (CerradosSectores))
  ;(assert (Consulta2))
)

(defrule consultaInic2
  (declare (salience 10))
  =>
  (printout t "¿En que archivo se encuentran los datos de las empresas?" crlf)
  (bind ?c (read))
  (assert (archivo2 ?c))
  (assert (LeerEmpresas))
)


(defrule AbrirCartera
  (declare (salience 10))
  (archivo2 ?c)
  ?l<-(LeerEmpresas)
  =>
  (printout t "abro " ?c crlf)
  (open ?c empresas_data)
  (assert (ArchivoEmpresasAbierto))
  (assert (SeguirLeyendo2))
  (retract ?l)
)

(defrule LeerEmpresas
	(declare (salience 9))
        ?f<-(ArchivoEmpresasAbierto)
        ?s<-(SeguirLeyendo2)
        (archivo2 ?c)
        =>
        (printout t "estoy leyendo las empresas desde " ?c crlf)
        (bind ?campo (read empresas_data))
        (retract ?s)
        ;;Compruebo si es EOF, o nuevoSector
        (if (neq ?campo EOF) then
                (assert (Empresa
                          (nombre ?campo)
			  (precio (read empresas_data))
			  (var_dia (read empresas_data))
			  (capitalizacion (read empresas_data))
			  (PER (read empresas_data))
			  (RPD (read empresas_data))
			  (taman (read empresas_data))
			  (ibex_perc (read empresas_data))
			  (et_PER (read empresas_data))
			  (et_RPD (read empresas_data))
			  (sector  (read empresas_data))
			  (var_5_dias (read empresas_data))
			  (PER_B_3_Cons (read empresas_data))
			  (PER_B_5_Cons (read empresas_data))
			  (var_sect (read empresas_data))
			  (var_sect_5 (read empresas_data))
			  (var_mens (read empresas_data))
			  (var_tri (read empresas_data))
			  (var_sem (read empresas_data))
			  (var_anio (read empresas_data))
                        ))
                (assert (SeguirLeyendo2))
        )
)

(defrule CierraArchivoEmpresas
        (declare (salience -12))
        ?f<-(ArchivoEmpresasAbierto)
        =>
        (close empresas_data)
        (assert (CerradasEmpresas))
)


;;Leemos el conjunto de las noticias


(defrule consultaInic3
  (declare (salience 10))
  =>
  (printout t "¿En que archivo se encuentran los datos de las noticias del dia?" crlf)
  (bind ?c (read))
  (assert (archivo3 ?c))
  (assert (LeerNoticias))
)

(defrule abrirNoticias
  (declare (salience 10))
  (archivo3 ?c)
  ?l<-(LeerNoticias)
  =>
  (printout t "abro " ?c crlf)
  (open ?c noticias_data)
  (assert (ArchivoNoticiasAbierto))
  (assert (SeguirLeyendo3))
  (retract ?l)
)

(defrule LeerNoticias
	(declare (salience 9))
        ?f<-(ArchivoNoticiasAbierto)
        ?s<-(SeguirLeyendo3)
        (archivo3 ?c)
        =>
        (printout t "estoy leyendo las noticias desde " ?c crlf)
        (bind ?campo (read noticias_data))
        (retract ?s)
        ;;Compruebo si es EOF, o nuevoSector
        (if (neq ?campo EOF) then
                (assert (Noticia
                          (nombre ?campo)
                          (tipo (read noticias_data))
                        ))
                (assert (SeguirLeyendo3))
        )
)

(defrule CierraNoticias
	(declare (salience -12))
	?a<-(ArchivoNoticiasAbierto)
	=>
	(close noticias_data)
	(assert (CerradasNoticias))
)

;;Cargar la cartera de acciones del usuario


(defrule consultaInic4
  (declare (salience 10))
  =>
  (printout t "¿En que archivo se encuentran los datos de la cartera de valores?" crlf)
  (bind ?c (read))
  (assert (archivo4 ?c))
  (assert (LeerCartera))
)

(defrule abrirCartera
  (declare (salience 10))
  (archivo4 ?c)
  ?l<-(LeerCartera)
  =>
  (printout t "abro " ?c crlf)
  (open ?c cartera_data)
  (assert (ArchivoCarteraAbierto))
  (assert (LeePrimeraLinea))
  (retract ?l)
)


(defrule LeerCantidad
	?l<-(LeePrimeraLinea)
	(archivo4 ?c)
	=>
	(printout t "Leo la primera linea de " ?c crlf)
	(read cartera_data) ;;Descarto el campo que dice disponible ya que yo lo voy a almacenar como un hecho liquidez
	(assert (liquidez
			(dinero (read cartera_data))
			(patrimonio (read cartera_data))))
	(assert (LecturaCartera))
	(assert (SeguirLeyendo4))
)


(defrule LeerValoresCartera
	?l<-(LecturaCartera)
	?s<-(SeguirLeyendo4)
	(archivo4 ?c)
	=>
	(printout t "estoy leyendo los valores de la cartera del usuario desde " ?c crlf)
        (bind ?campo (read cartera_data))
        (retract ?s)
        ;;Compruebo si es EOF, o nuevoSector
        (if (neq ?campo EOF) then
                (assert (EmpresaCartera
                		(nombre ?campo)
                		(acciones (read cartera_data))
                		(v_total (read cartera_data))
                		))
                (assert (SeguirLeyendo4))
        )
)

(defrule CerrarCartera
	(declare (salience -12))
	?a<-(ArchivoCarteraAbierto)
	=>
	(close cartera_data)
	(assert (CerradaCartera)))

;;Para pasar a calcular el potencial de los distintos valores
(defrule PasarValoresInestables
	(declare (salience 8))
	?c1<-(CerradasEmpresas)
	?c2<-(CerradosSectores)
	?c3<-(CerradasNoticias)
	?c4<-(CerradaCartera)
	=>
	(assert (CalculoValoresInestables)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;      CALCULO VALORES INESTABLES         ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defrule inestabilidad_valor_const
	(CalculoValoresInestables)
	(Empresa (nombre ?n)(sector Construccion))
	;(Sector (nombre Construccion))
	=>
	(assert (valor_inestable
		(nombre ?n)
		(motivo "inestable ya que pertenece al sector de la construccion.")))
	)

;
;Si la economía está bajando, los valores del sector servicios son inestables por defecto
;
;
;(defrule inestabilidad_valor
;	(Empresa (nombre ?n) )
;)

;
;Si la economía está bajando, los valores del sector servicios son inestables por defecto
;
;
(defrule inestable_noticia_negativa
	(CalculoValoresInestables)
	(Noticia (nombre ?n)(tipo Mala))
	(Empresa (nombre ?n))
	=>
	(assert (valor_inestable
			(nombre ?n)
			(motivo "inestable debido a que se ha recibido una mala noticia 
				sobre la empresa.")))
	)

;;regla por la cual si una noticia negativa referente a un sector entonces todas las empresas de ese
;;sector son negativas.

(defrule inestable_noticia_sector
	(CalculoValoresInestables)
	(Noticia (nombre ?n)(tipo Mala))
	(Sector (nombre ?n))
	(Empresa (nombre ?e)(sector ?n))
	=>
	(assert (valor_inestable
			(nombre ?e)
			(motivo "inestable debido a que se ha recibido una noticia negativa del sectoe al 
				que pertenece la empresa")))
	)

;;regla por la cual si hay una noticia negativa sobre la economía todos los valores pasan a ser
;;inestables.
(defrule inestable_global
	(CalculoValoresInestables)
	(Noticia (nombre ECONOMIA)(tipo Mala))
	(Empresa (nombre ?n))
	=>
	(assert (valor_inestable
			(nombre ?n)
			(motivo "inestable debido a que se ha recibido una noticia negativa sobre 
				la economía")))
)

;Regla por la cual un valor con una noticia positiva sobre el es estble (durante dos dias)
(defrule valor_estable_noticia
	(CalculoValoresInestables)
	(Noticia (nombre ?n)(tipo Buena))
	(Empresa (nombre ?n))
	=>
	(assert (valor_estable
			(nombre ?n)
			(motivo "estable ya que se ha recibido una noticia positiva sobre el valor")))
)

(defrule valor_estable_sector
	(CalculoValoresInestables)
	(Noticia (nombre ?n)(tipo Buena))
	(Empresa (nombre ?e)(sector ?n))
	=>
	(assert (valor_estable
			(nombre ?n)
			(motivo "estable por que se ha recibido una noticia positiva de su sector")))
)

(defrule PasarCalculos
	(declare (salience -1))
	?c<-(CalculoValoresInestables)
	=>
	(retract ?c)
	(assert (CalculoValores)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Módulo 1: Detector valores peligrosos
;;;; Solo aplicable a los valores de los que el usuario posee acciones
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


; Si un valor es inestable y está perdiendo de forma continua durante los últimos 3 dias es peligroso
(defrule valor_inestable_bajando
	(CalculoValores)
	(EmpresaCartera (nombre ?n))
	(valor_inestable (nombre ?n))
	(Empresa (nombre ?n)(PER_B_3_Cons TRUE))
	=>
	(assert (valor_peligroso
			(nombre ?n)
			(motivo "es inestable por que lleva tres dias bajando")))
)

;Si un valor está perdiendo durante los últimos 5 dias y la variación en esos 5 días con respecto a 
;la variación del sector es mayor de un ­5%, ese valor es peligroso

(defrule valor_peligroso
	(CalculoValores)
	(EmpresaCartera (nombre ?n))
	(Empresa (nombre ?n)(sector ?s)(PER_B_5_Cons TRUE)(var_sect ?v))
	(test (> ?v 5))
	=>
	(assert (valor_peligroso
			(nombre ?n)
			(motivo "es inestable por que lleva 5 dias bajando y su diferencia con el
				sector es mayor de 5%")))
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Modulo 2:Detector de valores sobrevalorados
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; General: Si el PER es Alto y el RPD bajo, la empresa está sobrevalorada
(defrule default_valor_supervalorado
	(Empresa (nombre ?n))(et_PER Alto)(et_RPD Bajo)
	=>
	(assert (valor_supervalorado
			(nombre ?n)
			(motivo "El valor del PER de es alto y el RPD bajo.")))
)


;;Lo puedo unificar como una sola regla?¿

;;Caso empresa pequeña.
(defrule valor_supervalorado_empresa_peq
	(CalculoValores)
	(Empresa (nombre ?n)(taman PEQUENIA)(et_PER Alto))
	=>
	(assert (valor_supervalorado
			(nombre ?n)
			(motivo "la empresa es pequeña y su PER es alto.")))
)
(defrule valor_supervalorado_empresa_peq2
	(CalculoValores)
	(Empresa (nombre ?n)(taman PEQUENIA)(et_PER Medio)(et_RPD bajo))
	=>
	(assert (valor_supervalorado
			(nombre ?n)
			(motivo "la empresa es pequeña, su PER es mediano y su RPD bajo.")))
)

;;Caso empresa grande
(defrule valor_supervalorado_empresa_grande
	(CalculoValores)
	(Empresa (nombre ?n)(taman GRANDE)(et_RPD ?rpd)(et_PER ?per))
	=>
	(if (eq ?rpd "Bajo") then
		(if (eq ?per Medio) then 
			(assert (valor_supervalorado
					(nombre ?n)
					(motivo "la empresa es grande, su RPD es bajo y su PER
						mediano"))))
		(if (eq ?per Alto) then 
			(assert (valor_supervalorado
					(nombre ?n)
					(motivo "la empresa es grande, su RPD es bajo y su PER
						alto"))))
	)
	(if (eq ?rpd Medio) then
		(if (eq ?per "Alto") then 
			(assert (valor_supervalorado
					(nombre ?n)
					(motivo "La empresa es grande, su RPD es mediano 
						y su PER es alto")))
		)
	)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Modulo 2:Detector de valores infravalorados
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; Si el PER es Bajo y el RPD alto,  la empresa está infravalorada

(defrule valor_infravalorado
	(CalculoValores)
	(Empresa (nombre ?n)(et_PER Bajo)(et_RPD Alto))
	=>
	(assert (valor_infravalorado
			(nombre ?n)
			(motivo "su PER es bajo y su RPD alto")))
)


;;;; Si  la empresa ha caído bastante (más de un 30%) (en los últimos 3, 6 o 12 ), ha subido 
;;;; pero no mucho en el último es, y el PER es bajo, la  empresa está infravalorada

;;;;;;;;;;ERROR AL HACER LA COMPROBACION DE LA CAIDA EN AÑO SEMESTRE O TRIMESTRE;;;;;;;;;;;;;
(defrule empresa_cae_mucho
	(CalculoValores)
	(Empresa (nombre ?n)(var_anio ?anio)(var_sem ?sem)(var_tri ?tri))
	(or (or (test(> ?anio 30)) (test(> ?sem 30))) (test(> ?tri 30)))
	=>
	(assert (valor_infravalorado
			(nombre ?n)
			(motivo "la empresa ha caido bastante aunque no en el ultimo mes
				pero su PER es bajo")))
)

;; Si la empresa es grande, el RPD es alto y el PER Medio, además no está bajando y se
;; comporta mejor que su sector, la empresa está infravalorada

(defrule empresa_grande_infravalorada
	(CalculoValores)
	(Empresa (nombre ?n)(et_RPD Alto)(et_PER Medio)(PER_B_5_Cons FALSE))
	=>
	(assert (valor_infravalorado
			(nombre ?n)
			(motivo "EL RPD es alto, el PER mediano pero la empresa no esta cayendo")))
)

(defrule PasarPropuestas
	(declare (salience 0))
	?c<-(CalculoValores)
	=>
	(assert (ConsiderarPropuestas))
	(retract ?c))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Modulo 4:Realizacion de propuestas
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;Regla que propone vender las acciones peligrosas del usuario
(defrule propuesta_de_venta
	(ConsiderarPropuestas)
	(EmpresaCartera (nombre ?n))
	(valor_infravalorado (nombre ?n))
	(Explicacion (nombre ?n)(motivo ?mot))
	(Empresa (nombre ?n)(var_mens ?mes)(var_sect ?sect)(RPD ?rpd))
	(test (< ?mes  0))
	(test (> ?sect  3))
	=>
	(assert (Proposicion
			(tipo "venta")
			(nombre ?n)
			(rendimiento_esperado (- 20 ?rpd))
			(Explicacion "La empresa es peligrosa por " ?mot  ".Además está entrando en 
				tendencia bajista con respecto a su sector. Según mi estimación
				existe una probabilidad no despreciable de que pueda caer al cabo del año
				un 20%, aunque produzca un" ?rpd "por dividendos perderiamos un "(- 20 ?rpd))))
)


;Regla que propone vender valores de empresas sobrevaloradas
(defrule inversion_infravaloradas
	(ConsiderarPropuestas)
	(Empresa (nombre ?n)(PER ?per)(RPD ?rpd)(sector ?s))
	(not (EmpresaCartera (nombre ?n)))
	(Sector (nombre ?s)(PER ?med))
	(valor_infravalorado (nombre ?n))
	(liquidez (dinero ?d))
	(test (> ?d 0))
	=>
	(assert (Proposicion
			(tipo "compra")
			(nombre ?n)
			(rendimiento_esperado (/ (* (- ?med ?per) 100)(+ (* 5 ?per)?rpd)))
			(Explicacion "Esta empresa está infravalorada y seguramente el PER tienda al PER medio en 5 años, con lo que deberiamos revalorizar
				 un  (PERMedio­PER)*100/(5*PER) anual a lo que habría que sumar el RDP% de beneficios por dividendos")))
)


;;Proponer vender acciones de empresas supervaloradas
(defrule venta_supervaloradas
	(ConsiderarPropuestas)
	(EmpresaCartera (nombre ?n))
	(Empresa (nombre ?n)(sector ?s)(PER ?per)(RPD ?rdp)(precio ?eur))
	(valor_supervalorado (nombre ?n))
	(Sector (nombre ?s)(PER ?permed)(RPD ?rdpmed))
	(test (< (+ ?rdp (/(- ?per ?permed)(* 5 ?per))) (+ ?eur 5)))
	=>
	(assert (Proposicion
			(tipo "venta")
			(nombre ?n)
			(rendimiento_esperado (+ ?rdp (/(- ?per ?permed)(* 5 ?per))))
			(Explicacion "Esta empresa está sobrevalorada, es mejor amortizar lo invertido,ya que seguramente el PER tan alto 
				deberá bajar el PERMedio del sector en unos 5 años, con lo que se debería devaluar un (PERMedio­PER)*100/(5*PER) anual, 
				así que aunque se pierda el RPD% de beneficios por dividendos saldría rentable"))
	)
)


(defrule cambiar_inversion
	(ConsiderarPropuestas)
	(Empresa (nombre ?n1)(PER ?per1)(RPD ?rpd1))
	(EmpresaCartera (nombre ?n2))
	(Empresa (nombre ?n2)(PER ?per2)(RPD ?rpd2))
	(and (not (valor_infravalorado (nombre ?n2)))(not (valor_supervalorado (nombre ?n1))))
	(test (> (- ?rpd1 ?rpd2) 0))
	=>
	(assert (Proposicion
			(tipo "cambio")
			(nombre ?n1)
			(nombre2 ?n2)
			(rendimiento_esperado (- ?rpd1 ?rpd2))
			(Explicacion ""?n1 debe tener una revalorización acorde con la evolución de la bolsa. Por dividiendos se espera un
				RDP% que es más alto que lo que está dando ?n2 ,por eso te propongo cambiar los valores de unas por las
				de esta otra. Aunque se pague el 1% del coste del cambio te saldría rentable.""))
	)
)


(defrule CalcularMejoresPropuestas
	(declare (salience -10))
	?c<-(ConsiderarPropuestas)
	=>
	(retract ?c)
	(assert (CalculaMejoresPropuestas))
	(assert (MuestaMenu)) ;;Para que siempre se muestre el menu)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Modulo 4:Realizacion de propuestas
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defglobal ?*cont* = 1)

(defrule SeleccionarMejoresPropuestas
	(MuestraPropuestas)
	(CalculaMejoresPropuestas)
	?p<-(Proposicion (tipo ?t)(nombre ?n)(nombre2 ?n2)(rendimiento_esperado ?r1)(Explicacion ?e))
	(not (Proposicion (rendimiento_esperado ?r2&:(> ?r2 ?r1))))
	=>
	(assert (ActualizaCartera))
	(if (<= ?*cont* 6) then
		(if (eq ?t cambio)then
			(printout t crlf "Mi consejo numero " ?*cont* " es: " crlf)
			(printout t "Realizar un " ?t " de acciones de " ?n " a " ?n2" ya que: " ?e crlf )
			(printout  t "Segun mis calculos obtendremos un rendimiento de: " ?r1 crlf)
			(retract ?p)
			) 
		(printout t crlf "Mi consejo numero " ?*cont* " es: " crlf)
		(printout t "Realizar una " ?t " de acciones de " ?n " ya que: " ?e crlf )
		(printout  t "Segun mis calculos obtendremos un rendimiento de: " ?r1 crlf)
		(retract ?p)
		)
	(bind ?*cont* (+ ?*cont* 1))
	;(bind ?*contadorMostrarPropuestas* (+ ?*contadorMostrarPropuestas* 1))
)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Modulo 4.1:Actualizacion de la cartera en base a estas propuestas.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defrule RecogeOpcionElegida
	?a<-(ActualizaCartera)
	=>
	(retract ?a)
	(printout t crlf "Elija 1, 2, 3 o 4 segun la accion a realizar: ")
	(printout t crlf "1 - Compra de acciones")
	(printout t crlf "2 - Venta de acciones")
	(printout t crlf "3 - Cambio de acciones")
	(printout t crlf "4 - Volver al menu anterior" crlf)
	(bind ?o (read))
	(assert (opcion1 ?o))
)

(defrule opcionElegida
	?a<-(opcion1 ?o)
	=>
	(if (eq ?o 1) then (assert (Compra)))
	(if (eq ?o 2) then (assert (Venta)))
	(if (eq ?o 3) then (assert (Cambio)))
	(if (eq ?o 4) then (assert (MuestaMenu)))
	(retract ?a)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Modulo 4.1.1:Compra de acciones
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defrule selectAccionAComprar
	?c<-(Compra)
	=>
	(printout t crlf "Introduzca el valor que va a comprar: ")
	(bind ?val (read))
	(assert (valor ?val))
	(printout t crlf "Introduzca la cantidad que va a comprar: ")
	(bind ?cant (read))
	(assert (cantidad ?cant))
	(assert (ActCompra))
	(retract ?c))

(defrule actualizaCarteraTrasCompra
	?c<-(ActCompra)
	?v<-(valor ?val)
	?ca<-(cantidad ?cant)
	(liquidez (dinero ?n))
	(Empresa (nombre ?val)(precio ?pre))
	=>
	(printout t crlf "El valor de la operacion ha sido: " (* ?cant ?pre))
	(if (> ?n (* ?cant ?pre)) then 
		(assert (EmpresaCartera
				(nombre ?val)
				(acciones ?cant)))
		(assert (ActualizaCartera))
		(retract ?c)
		(retract ?ca)
		(retract ?v)
	)
	(if (< ?n (* ?cant ?pre)) then 
		(printout t crlf "No puede comprar tantas acciones Introduzca el valor correcto")
		(assert (Compra)))
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Modulo 4.1.1:Venta de acciones
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defrule selectAccionAVender
	?c<-(Venta)
	=>
	(printout t crlf "Introduzca el valor que va a vender: ")
	(bind ?val (read))
	(assert (valorVenta ?val))
	(assert (ActVenta))
	(retract ?c))

(defrule actualizaCarteraTrasVenta
	?a<-(ActVenta)
	?v<-(valorVenta ?val)
	?vendida<-(EmpresaCartera (nombre ?val)(acciones ?cant))
	(EmpresaCartera (nombre ?val)(acciones ?cant))
	(Empresa (nombre ?val)(precio ?p))
	=>
	(printout t crlf "La ganancia de la venta ha sido " (* ?p ?cant))
	(assert (ActualizaCartera))
	(retract ?v)
	(retract ?a)
	(retract ?vendida)
	)

(defrule noEstaEnCartera
	(declare (salience -10))
	?a<-(ActVenta)
	?v<-(valorVenta ?val)
	=>
	(printout t crlf ?val " no esta en su cartera por lo que no puede venderla." crlf)
	(assert (Venta)))

;(defrule SalirVenta
;	?a<-(ActVenta)
;	?v<-(valorVenta ?val)
;	(eq ?val "SALIR")
;	=>
;	(assert (ActualizaCartera)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Modulo 4.1.1:Cambio de acciones
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defrule selectAccionesACambiar
        ?c<-(Cambio)
        =>
        (printout t crlf "Introduzca el valor que va a vender: ")
        (bind ?vend (read))
        (assert (cambio1 ?vend))
        (printout t crlf "Introduzca el valor que va a comprar: ")
        (bind ?comp (read))
        (assert (cambio2 ?comp))
        (assert (ActCambio))
        (retract ?c))

(defrule ActualizarCarteraTrasCAmbio
        ?c<-(ActCambio)
        ?a1<-(cambio1 ?vend)
        ?a2<-(cambio2 ?comp)
        ?el<-(EmpresaCartera (nombre ?vend)(acciones ?acc))
        (Empresa (nombre ?vend)(precio ?pre))
        (Empresa (nombre ?comp)(precio ?pre2))
        =>
        (assert (EmpresaCartera (nombre ?comp)(acciones (/ (* ?acc ?pre) ?pre2))))
        (retract ?el)
        (assert (ActualizaCartera))
        (retract ?c)
        (retract ?a1)
        (retract ?a2)
        )
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Modulo 5:Menu interactivo
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;pequeño menu para consultar el motivo de la clasificacion de un valor
;asi como para consultar las propuestas que el sistema realiza.

;No se pide explicitamente en el guion pero facilita la usabilidad

(defrule menu
	(declare (salience -13))
	?m<-(MuestaMenu)
	=>
	(printout t crlf "Buenas, las opciones disponibles son las siguientes:")
	(printout t crlf "1 - Consultar las diferentes propuestas en base a los datos que he obtenido.")
	(printout t crlf "2 - Consultar el por que un valor es estable")
	(printout t crlf "3 - Consultar el por que un valor es inestable")
	(printout t crlf "4 - Consultar el por que un valor esta supervalorado")
	(printout t crlf "5 - Consultar el por que un valor esta infravalorado")
	(printout t crlf "6 - Salir del sistema." crlf)
	(bind ?a (read))
	(assert (opcion ?a))
	(retract ?m)
)

(defrule opcion_elegida
	?o<-(opcion ?a)
	=>
	(if (eq ?a 1) then (assert (MuestraPropuestas)))
	(if (eq ?a 2) then (assert (ConsultaEstabilidad)))
	(if (eq ?a 3) then (assert (ConsultaInestabilidad)))
	(if (eq ?a 4) then (assert (ConsultaSupervalorado)))
	(if (eq ?a 5) then (assert (ConsultaInfravalorado)))
	(if (eq ?a 6) then (assert (Salir)))
	(retract ?o)
)

;Explicacion de estabilidad de un valor
(defrule lecturaDelValorConsultadoEstabilidad
	?c<-(ConsultaEstabilidad)
	=>
	(retract ?c)
	(printout t crlf "Introduzca el nombre del valor cuya estabilidad desea consultar: " crlf)
	(assert (ConsultadoEstabilidad (read)))
	(assert (ValorLeidoEstabilidad))
)

(defrule MuestraExplicacionEstabilidad
	?v<-(ConsultadoEstabilidad ?c)
	?l<-(ValorLeidoEstabilidad)
	(valor_estable (nombre ?c)(motivo ?m))
	=>
	(assert (MuestaMenu))
	(retract ?l)
	(retract ?v)
	(printout t crlf "El valor " ?c " es " ?m crlf)
)

;Explicacion de inestabilidad de un valor
(defrule lecturaDelValorConsultadoInestabilidad
	?c<-(ConsultaInestabilidad)
	=>
	(retract ?c)
	(printout t crlf "Introduzca el nombre del valor cuya inestabilidad desea consultar: " crlf)
	(assert (ConsultadoInestabilidad (read)))
	(assert (ValorLeidoInestabilidad))
)

(defrule MuestraExplicacionInestabilidad
	?v<-(ConsultadoInestabilidad ?c)
	?l<-(ValorLeidoInestabilidad)
	(valor_inestable (nombre ?c)(motivo ?m))
	=>
	(assert (MuestaMenu))
	(retract ?l)
	(retract ?v)
	(printout t crlf "El valor " ?c " es " ?m crlf)
)

;Explicacion de porque un valor esta supervalorado
(defrule lecturaValorConsultadoSupervalorado
	?c<-(ConsultaSupervalorado)
	=>
	(retract ?c)
	(printout t crlf "Introduzca el nombre del valor del que quiere saber por que esta supervalorado: " crlf)
	(assert (ConsultadoSupervalorado (read)))
	(assert (ValorLeidoSupervalorado))
)

(defrule MuestraExplicacionSupervalorado
	?v<-(ConsultadoSupervalorado ?c)
	?l<-(ValorLeidoSupervalorado)
	(valor_supervalorado (nombre ?c)(motivo ?m))
	=>
	(assert (MuestaMenu))
	(retract ?l)
	(retract ?v)
	(printout t crlf "El valor " ?c " esta supervalorado porque " ?m crlf)
)

;Explicacion de porque un valor esta infravalorado
(defrule lecturaValorConsultadoInfravalorado
	?c<-(ConsultaInfravalorado)
	=>
	(retract ?c)
	(printout t crlf "Introduzca el nombre del valor del que quiere saber por que esta infravalorado: " crlf)
	(assert (ConsultadoInfravalorado (read)))
	(assert (ValorLeidoInfravalorado))
)

(defrule MuestraExplicacionInfravalorado
	?v<-(ConsultadoInfravalorado ?c)
	?l<-(ValorLeidoInfravalorado)
	(valor_infravalorado (nombre ?c)(motivo ?m))
	=>
	(assert (MuestaMenu))
	(retract ?l)
	(retract ?v)
	(printout t crlf "El valor " ?c " esta infravalorado porque " ?m crlf)
)



;OPCION 5 salir.
(defrule salida
	?s<-(Salir)
	=>
	exit
)