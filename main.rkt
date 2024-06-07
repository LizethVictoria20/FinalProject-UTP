#lang racket

;Ventana del Juego
(require(lib "graphics.ss" "graphics"))
  (open-graphics)

(define ventana (open-viewport "Pacman" 600 600))
;ventana 2 para el pre-preocesado para que al reflejar en V1 no deje rastro.

(begin
    ((draw-viewport ventana) "black"))
    (define mensaje "fondo-in.jpg")
    ((draw-pixmap ventana) mensaje (make-posn 0 0))

(sleep 4)

(define ventana2 (open-pixmap "ventana-invisible" 600 600))
;diseño del fondo con pixmap para crgar archivos jpg y png
(define fondo "fondo.jpg")
((draw-pixmap ventana) fondo (make-posn 0 0))

;Intregro el logo (muñeco de pacman)
(define logo "pacman.png")
((draw-pixmap ventana) logo (make-posn 50 40))

;Definimos todo lo necesario para el timer

(define segundos 0)
(define termino 0)
(define segundosfinal "")
(define juego-finalizado #f) ; Bandera para indicar si el juego ha finalizado o no


(define (timer)
  (thread
   (lambda ()
     (let loop ()
     ;set! es una forma especial que se usa para cambiar el valor de una variable que ya ha sido definida
       (set! segundos (+ segundos 1))
       (set! segundosfinal (number->string segundos))
       (define segundos-str (number->string segundos))
       ((draw-solid-rectangle ventana)(make-posn 430 530) 170 50 (make-rgb 1 0.6 0)); Borra el área del contador anterior
       ((draw-string ventana) (make-posn 435 558) "TIEMPO :")
       ((draw-string ventana) (make-posn 505 558) segundos-str) ; Dibuja el nuevo valor de los segundos
       ((draw-string ventana) (make-posn 540 558) "segundos")
       (sleep 1) ; Espera 1 segundo
       (if juego-finalizado
           (void) ; Si el juego ha finalizado, no continúes contando
           (loop)))))) ; Si el juego no ha finalizado, sigue contando



(define (finalizar-juego final-time)
  ; Código para mostrar la ventana de finalización
  (define ventana-final (open-viewport "Fin del Juego" 300 200))
  ((draw-solid-rectangle ventana-final) (make-posn 0 0) 300 200 "white")
  ((draw-string ventana-final) (make-posn 30 50) "¡Juego terminado!")
  ((draw-string ventana-final) (make-posn 30 80) (string-append "Tiempo: " final-time " segundos"))
  (read-char) ; Espera hasta que el usuario presione una tecla para cerrar
  (close-viewport ventana-final)
  (close-viewport ventana)
  (close-viewport ventana2)
 )
;Usando begin porque son varias acciones las que se van a cubrir


(define (pacMan x y lado)
  (cond
    [(equal? lado 'ar) ;Referencia
      (begin
      ((draw-pixmap ventana2) fondo (make-posn 0 0))
      ((draw-pixmap ventana2) logo (make-posn x y))
    )]
    [(equal? lado 'ab)
        (begin
        ((draw-pixmap ventana2) fondo (make-posn 0 0))
        ((draw-pixmap ventana2) logo (make-posn x y))
        )
    ]
    [(equal? lado 'izq)
      (begin
      ((draw-pixmap ventana2) fondo (make-posn 0 0))
      ((draw-pixmap ventana2) logo (make-posn x y))
      )
    ]
    [(equal? lado 'der)
      (begin
      ((draw-pixmap ventana2) fondo (make-posn 0 0))
      ((draw-pixmap ventana2) logo (make-posn x y))
      )
    ]
    [else
      (void)
    ]
  )
; se usa copy-viewport para pasar todo lo de ventana2 a la ventana principal
  (copy-viewport ventana2 ventana)
  ((clear-viewport ventana2))
  )

;Eventos en el teclado para darle el movimiento usando key-value
;La logica es que cuando se mueva a la derecha vaya sumando en el eje x y cuando se mueva a la izq reste en el eje
;lo mismo con el eje y, abajo va sumando y arriba va restando
;creamos tambien condiciones para extralimitar el objeto y que no se salga de un límite establecido

(define (teclado x y tecla)
;límites
  (cond
    [(< x 0)
     (begin
       (pacMan 0 y 'izq)
       (teclado 0 y (key-value(get-key-press ventana))))
    ]
    [(> x 540)
     (begin
       (pacMan 540 y 'izq)
       (teclado 540 y (key-value(get-key-press ventana))))
    ]
    [(< y 0)
     (begin
       (pacMan x 0 'ar)
       (teclado x 0 (key-value(get-key-press ventana))))
    ]
    [(> y 540)
     (begin
       (pacMan x 540  'ar)
       (teclado x 540 (key-value(get-key-press ventana))))
    ]


    ;Limites en y del laberinto

    ;SE ACABO EL JUEGO---------------------------------------

    [(and (or (> x 250)(< x 280)) (>= y 485))
     (begin
       (define final-time segundosfinal)
       (finalizar-juego final-time) ; Función para mostrar la ventana de finalización
       (set! juego-finalizado #t) ; Indicar que el juego ha finalizado
       )]

    ;limite 1
    [(and(< x 118)(and(> y 125)(< y 500)))
     (begin
       (pacMan 118 y 'izq)
       (teclado 118 y (key-value(get-key-press ventana))))
    ]

    ;limite 2
    [(and(and(> x 126)(< x 170))(and(> y 125)(< y 425)))
     (begin
       (pacMan 126 y 'izq)
       (teclado 126 y (key-value(get-key-press ventana))))
    ]
    [(and(and(> x 180)(< x 197))(and(> y 125)(< y 425)))
     (begin
       (pacMan 197 y 'izq)
       (teclado 197 y (key-value(get-key-press ventana))))
    ]

    ;limite 3
    [(and(and(> x 201)(< x 243))(and(> y 158)(< y 283)))
     (begin
       (pacMan 201 y 'izq)
       (teclado 201 y (key-value(get-key-press ventana))))
    ]
    [(and(and(> x 253)(< x 270))(and(> y 158)(< y 283)))
     (begin
       (pacMan 270 y 'izq)
       (teclado 270 y (key-value(get-key-press ventana))))
    ]

    ;limite 4
    [(and(and(> x 271)(< x 313))(and(> y 125)(< y 197)))
     (begin
       (pacMan 271 y 'izq)
       (teclado 271 y (key-value(get-key-press ventana))))
    ]
    [(and(and(> x 323)(< x 340))(and(> y 125)(< y 197)))
     (begin
       (pacMan 340 y 'izq)
       (teclado 340 y (key-value(get-key-press ventana))))
    ]

    ;limite 5
    [(and(and(> x 271)(< x 313))(and(> y 223)(< y 356)))
     (begin
       (pacMan 271 y 'izq)
       (teclado 271 y (key-value(get-key-press ventana))))
    ]
    [(and(and(> x 323)(< x 339))(and(> y 272)(< y 358)))
     (begin
       (pacMan 339 y 'izq)
       (teclado 339 y (key-value(get-key-press ventana))))
    ]

    ;limite 6
    [(and(and(> x 340)(< x 383))(and(> y 271)(< y 420)))
     (begin
       (pacMan 340 y 'izq)
       (teclado 340 y (key-value(get-key-press ventana))))
    ]
    [(and(and(> x 393)(< x 410))(and(> y 271)(< y 420)))
     (begin
       (pacMan 410 y 'izq)
       (teclado 410 y (key-value(get-key-press ventana))))
    ]

    ;limite 7
    [(and(> x 417)(and(> y 125)(< y 500)))
     (begin
       (pacMan 417 y 'izq)
       (teclado 417 y (key-value(get-key-press ventana))))
    ]




    ;limites en x del laberinto
    ;lane 1
    [(and (and(> y 76)(< y 110))(and(> x 105)(< x 260)))
     (begin
       (pacMan x 76 'ab)
       (teclado x 76 (key-value(get-key-press ventana))))
    ]
    [(and (and(> y 127)(< y 144))(and(> x 105)(< x 260)))
     (begin
       (pacMan x 144 'ab)
       (teclado x 144 (key-value(get-key-press ventana))))
    ]

    ;lane 2
    [(and (and(> y 76)(< y 110))(and(> x 274)(< x 480)))
     (begin
       (pacMan x 76 'ab)
       (teclado x 76 (key-value(get-key-press ventana))))
    ]
    [(and (and(> y 127)(< y 144))(and(> x 274)(< x 480)))
     (begin
       (pacMan x 144 'ab)
       (teclado x 144 (key-value(get-key-press ventana))))
    ]

    ;lane  3
    [(and (and(> y 148)(< y 190))(and(> x 201)(< x 406)))
     (begin
       (pacMan x 148 'ab)
       (teclado x 148 (key-value(get-key-press ventana))))
    ]
    [(and (and(> y 200)(< y 217))(and(> x 201)(< x 406)))
     (begin
       (pacMan x 217 'ab)
       (teclado x 217 (key-value(get-key-press ventana))))
    ]

    ;lane 4
    [(and (and(> y 223)(< y 264))(and(> x 271)(< x 406)))
     (begin
       (pacMan x 223 'ab)
       (teclado x 223 (key-value(get-key-press ventana))))
    ]
    [(and (and(> y 274)(< y 290))(and(> x 271)(< x 406)))
     (begin
       (pacMan x 290 'ab)
       (teclado x 290 (key-value(get-key-press ventana))))
    ]

    ;lane  5
    [(and (and(> y 292)(< y 332))(and(> x 143)(< x 320)))
     (begin
       (pacMan x 292 'ab)
       (teclado x 292 (key-value(get-key-press ventana))))
    ]
    [(and (and(> y 342)(< y 359))(and(> x 143)(< x 320)))
     (begin
       (pacMan x 359 'ab)
       (teclado x 359 (key-value(get-key-press ventana))))
    ]

    ;lane  6
    [(and (and(> y 369)(< y 411))(and(> x 201)(< x 406)))
     (begin
       (pacMan x 369 'ab)
       (teclado x 369 (key-value(get-key-press ventana))))
    ]
    [(and (and(> y 421)(< y 438))(and(> x 201)(< x 406)))
     (begin
       (pacMan x 438 'ab)
       (teclado x 438 (key-value(get-key-press ventana))))
    ]


     ;lane  7
    [(and (and(> y 450)(< y 486))(and(> x 100)(< x 260)))
     (begin
       (pacMan x 450 'ab)
       (teclado x 450 (key-value(get-key-press ventana))))
    ]
    [(and (and(> y 503)(< y 520))(and(> x 100)(< x 260)))
     (begin
       (pacMan x 520 'ab)
       (teclado x 520 (key-value(get-key-press ventana))))
    ]


     ;lane  8
    [(and (and(> y 435)(< y 486))(and(> x 271)(< x 480)))
     (begin
       (pacMan x 435 'ab)
       (teclado x 435 (key-value(get-key-press ventana))))
    ]
    [(and (and(> y 503)(< y 520))(and(> x 271)(< x 480)))
     (begin
       (pacMan x 520 'ab)
       (teclado x 520 (key-value(get-key-press ventana))))
    ]

    ;evento teclado
    [(equal? tecla 'up)
      (begin
        (pacMan x (- y 10) 'ar)
        (teclado x (- y 10)(key-value(get-key-press ventana))))
    ]
    [(equal? tecla 'down)
      (begin
        (pacMan x (+ y 10) 'ab)
        (teclado x (+ y 10)(key-value(get-key-press ventana))))
    ]
    [(equal? tecla 'left)
      (begin
        (pacMan (- x 10) y 'izq)
        (teclado (- x 10) y (key-value(get-key-press ventana))))
    ]
    [(equal? tecla 'right)
      (begin
        (pacMan (+ x 10) y 'der)
        (teclado (+ x 10) y (key-value(get-key-press ventana))))
    ]
    [else
      ;si no se cumplen las  anteriores, un "else" para que no me haga nada
      (teclado x y (key-value(get-key-press ventana)))
    ]

  )
)

;nuevamente el copy-viewport
(copy-viewport ventana2 ventana)
((clear-viewport ventana2))


(timer) ; Iniciar el contador de segundos en un hilo separado

(teclado 266 80 'down)

