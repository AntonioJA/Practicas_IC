;;;;personas.clp;;;;

;;añadir personas
(deftemplate Persona
 (multislot nombre)
 (slot edad)
 (multislot padre)
 (multislot conyuge))

;;relacion para ahorrar reglas a la hora de imprimir por pantalla
(deftemplate Relacion
  (slot tipo)
  (multislot nombre1)
  (multislot nombre2)
  (multislot nombre3)
)

(deffacts Familia

 (Persona
    (nombre "Juan Leyva")
    (edad  67)
    (conyuge "Rafaela Lirio"))

  (Persona
    (nombre "Rafaela Lirio")
    (edad 66)
    (conyuge "Juan Leyva")
  )

  (Persona
    (nombre "Tomas Ruiz")
    (edad 78)
    (conyuge "Mercedes Rodriguez"))

  (Persona
    (nombre "Mercedes Rodriguez")
    (edad 76)
    (conyuge "Tomas Ruiz"))

  (Persona
    (nombre "Pepi Ruiz")
    (edad 44)
    (padre "Tomas Ruiz")
    (conyuge "Antonio Leyva"))

  (Persona
    (nombre "Andres Ruiz")
    (edad 47)
    (padre "Tomas Ruiz")
    (conyuge "Teresa Sierra"))

  (Persona
    (nombre "Teresa Sierra")
    (edad 42)
    (conyuge "Andres Ruiz"))

  (Persona
    (nombre "Antonio Leyva")
    (edad 47)
    (padre "Juan Leyva")
    (conyuge "Pepi Ruiz")
  )

  (Persona
    (nombre "Juan Leyva")
    (edad 39)
    (padre "Juan Leyva")
    (conyuge "Maria Jose Mendoza"))

  (Persona
    (nombre "Juana Leyva")
    (edad 42)
    (padre "Juan Leyva")
    (conyuge "Fran Castillo")
  )

  (Persona
    (nombre "Maria Jose Mendoza")
    (edad 40)
    (conyuge "Juan Leyva"))

  (Persona
    (nombre "Fran Castillo")
    (edad 41)
    (conyuge "Juana Leyva Lirio"))

  (Persona
    (nombre "Rafa Leyva Ruiz")
    (edad 20)
    (padre "Antonio Leyva"))

  (Persona
    (nombre "Jose Antonio Leyva")
    (edad 18)
    (padre "Antonio Leyva"))

  (Persona
    (nombre "Andres Leyva")
    (edad 16)
    (padre "Antonio Leyva"))

  (Persona
    (nombre "Mercedes Leyva")
    (edad 8)
    (padre "Antonio Leyva"))


  (Persona
    (nombre "Teresa Ruiz")
    (edad 22)
    (padre "Andres Ruiz"))

  (Persona
    (nombre "Andrea Ruiz")
    (edad 19)
    (padre "Andres Ruiz"))

  (Persona
    (nombre "Alejandro Ruiz")
    (edad 12)
    (padre "Andres Ruiz"))

  (Persona
    (nombre "Juan Jose Leyva")
    (edad 15)
    (padre "Juan Leyva"))

  (Persona
    (nombre "Maria Jose Leyva")
    (edad 8)
    (padre "Juan Leyva"))

  (Persona
    (nombre "Juana Castillo")
    (edad 7)
    (padre "Fran Castillo"))

  (Persona
    (nombre "Lucia Leyva")
    (edad 9)
    (padre "Fran Castillo"))
)




;regla hijo
  (defrule Hijo
    (Persona (nombre ?p))
    (Persona (padre ?p) (nombre ?x))
    (Persona (conyuge ?p)(nombre ?m))
    =>
    (assert(Relacion
              (tipo "hijo")
              (nombre1 ?x)
              (nombre2 ?p)
              (nombre3 ?m)
              )
            )
      )

;regla hermano
  (defrule hermano
    (Persona (padre ?x)(nombre ?n))
    (Persona (padre ?x)(nombre ?m))
    (test (neq ?n ?m))
    =>
    (assert(Relacion
              (tipo "hermano")
              (nombre1 ?n)
              (nombre2 ?m)
            )
    )
  )
;;regla primo
  (defrule Primo
    (Persona (nombre ?n) (padre ?p))
    (Persona (nombre ?n2) (padre ?p2))
    (es_hermano ?p ?p2)
    =>
    (assert(Relacion
              (tipo "Primo")
              (nombre1 ?n)
              (nombre2 ?n2)
              )
            )
    )

  (defrule Abuelo
    (Persona (nombre ?n) (padre ?p))
    (Persona (nombre ?p) (padre ?a))
    (Persona (nombre ?a) (conyuge ?x))
    =>
    (assert(Relacion
                (tipo "nieto")
                (nombre1 ?n)
                (nombre2 ?a)
                (nombre3 ?x)
              )
            )
    )

  (defrule Cuñado
    (Persona (nombre ?n) (conyuge ?c))
    (Persona (nombre ?x))
    (Relacion (tipo "hermano")(nombre1 ?c)(nombre2 ?x))
    =>
    (assert (Relacion
              (tipo "cuñado")
              (nombre1 ?n)
              (nombre2 ?x)
            )
          )
    )

  (defrule tio
    (Persona (nombre ?n)(padre ?p))
    (Persona (nombre ?n2))
    (Relacion (tipo "hermano")(nombre1 ?p)(nombre2 ?n2))
    =>
    (assert (Relacion
                (tipo "tio")
                (nombre1 ?n2)
                (nombre2 ?n)
              )
            )
    )


  ;reglas para el control del programa
  (defrule PedirNombre1
    =>
    (printout t "Introduzca Nombre de la persona: " crlf)
    (bind ?buscado1 (readline))
    (assert (Buscado1 ?buscado1)
      ))

  (defrule PedirNombre2
    (Buscado1 ?n)
    =>
    (printout t "Introduzca Nombre de la persona: " crlf)
    (bind ?buscado2 (readline))
    (assert (Buscado2 ?buscado2)
      ))

;  (defrule BuscaNombre1
;    (Buscado (nombre ?n))
;    (Persona (nombre ?n))
;    =>
;    (assert (buscado1 ?n))
;  );

;  (defrule BuscaNombre2
;    (Persona (nombre ?n))
;    (?n = ?buscado2)
;    =>
;    (assert (buscado2 ?n2))
;  )

  (defrule QueSon
    (Buscado1  ?n)
    (Buscado2  ?n2)
    (Relacion (tipo ?t)(nombre1 ?n)(nombre2 ?n2))
    =>
    (printout t ?n " es " ?t " de "  ?n2 crlf)
  )
