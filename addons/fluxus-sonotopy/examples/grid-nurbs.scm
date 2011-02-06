(require racket/vector)
(require fluxus-017/fluxus-sonotopy)

(clear)

(init-sonotopy)

(define sw 40)
(define sh 100)
(sonotopic-grid-size (vector sw sh 0))

; nurbs plane has an extra control point row on each side
(define p (build-nurbs-plane (- sh 1) (- sw 1)))

(with-primitive p
    (hint-none)
    (hint-wire)
    (hint-anti-alias)
    (translate #(0 -10 -50))
    (rotate #(-90 0 -90))
    (scale (vector sh sw (/ sh 5)))
    (wire-opacity .3))

; flattens the 2d sonotopic grid
(define (vector2d->vector1d v)
    (apply vector-append (vector->list v)))

(define (render)
    (let ([grid (vector2d->vector1d (sonotopic-grid-pattern))])
        (with-primitive p
            (pdata-index-map!
                (lambda (i p)
                  (vector (vx p) (vy p) (* .3 (vector-ref grid i))))
                "p"))))

(every-frame (render))

