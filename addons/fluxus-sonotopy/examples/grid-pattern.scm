(require racket/vector)
(require fluxus-017/fluxus-sonotopy)

(clear)

(init-sonotopy)

(set-camera-transform (mtranslate #(0 0 -10)))
(scale #(21 16 1))

(define-values (sw sh _) (vector->values (grid-size)))
(define p (build-pixels sw sh))
(define contrast 15.0)

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

