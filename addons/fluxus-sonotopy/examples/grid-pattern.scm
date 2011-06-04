(require racket/vector)
(require fluxus-017/fluxus-sonotopy)

(clear)

(init-sonotopy)

(set-camera-transform (mtranslate #(0 0 -10)))
(scale #(21 16 1))

(define sw 40)
(define sh 30)
(grid-size (vector sw sh 0))
(define p (build-pixels sw sh))
(define contrast 5.0)

; flattens the 2d grid
(define (vector2d->vector1d v)
    (apply vector-append (vector->list v)))

(define (render)
    (let ([pattern (vector2d->vector1d (grid-pattern))])
        (with-primitive p
            (pdata-index-map!
                (lambda (i c)
                  (expt (vector-ref pattern i) contrast))
                "c")
            (pixels-upload))))

(every-frame (render))

