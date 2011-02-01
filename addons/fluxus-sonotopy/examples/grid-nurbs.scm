(require racket/vector)
(require fluxus-017/fluxus-sonotopy)

(clear)

(init-sonotopy)

(define-values (sw sh _) (vector->values (sonotopic-grid-size)))

; nurbs plane has an extra control point row on each side
(define p (build-nurbs-plane (- sw 1) (- sh 1)))

(with-primitive p
    (hint-none)
    (hint-wire)
    (hint-anti-alias)
    (translate #(0 -3 0))
    (rotate #(-80 0 0))
    (scale 8)
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

