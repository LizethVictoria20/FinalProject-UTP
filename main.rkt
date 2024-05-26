#lang racket

;Ventana del Juego

(require(lib "graphics.ss" "graphics"))
  (open-graphics)

(define ventana (open-viewport "Pacman" 600 600))
;ventana 2 para el pre-preocesado para que al reflejar en V1 no deje rastro.

(define ventana2 (open-pixmap "ventana-invisible" 600 600))
;diseño del fondo con pixmap para crgar archivos jpg y png
(define fondo "pacman.png")
((draw-pixmap ventana2) fondo (make-posn 0 0))


;Intregro el logo (muñeco de pacman)
 ((draw-solid-ellipse ventana) (make-posn 50 40) 20 20 "pink")
;((draw-pixmap ventana2) logo (make-posn 50 40))


;definimos por medio de (if) las posiciones que va a cubrir el logo usando X y Y para darle posición al objeto (logo) 
;Usando begin porque son varias acciones las que se van a cubrir


(define (pacMan x y lado)
  ;Se usa if porque queremos que solo una orden se ejecute y solo una sea true.
  (cond
    [(equal? lado 'ar)
      (begin
        ((draw-solid-ellipse ventana2) (make-posn x y) 20 20 "pink")
    )]
    [(equal? lado 'ab)
        (begin
          ((draw-solid-ellipse ventana2) (make-posn x y) 20 20 "pink")
        )
    ]
    [(equal? lado 'izq)
      (begin
        ((draw-solid-ellipse ventana2) (make-posn x y) 20 20 "pink")
      )
    ]
    [(equal? lado 'der)
      (begin
        ((draw-solid-ellipse ventana2) (make-posn x y) 20 20 "pink")
      )
    ]
    [else
      (void)
    ]
  )
  
;si no se cumplen ningunas de las cond anteriores, usamos (void) para que no haga nada

  ; se usa copy-viewport para pasar todo lo de ventana2 a la ventana principal 
  (copy-viewport ventana2 ventana)
  ((clear-viewport ventana2))
)

;Eventos en el teclado para darle el movimiento usando key-value
;La logica es que cuando se mueva a la derecha vaya sumando en el eje x y cuando se mueva a la izq reste en el eje
;lo mismo con el eje y, abajo va sumando y arriba va restando
;creamos tambien condiciones (if) para extralimitar el objeto y que no se salga de un límite establecido

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

(teclado 253 200 'down)
