(require fluxus-017/fluxus-sonotopy)

(clear)

(init-sonotopy)

(set-camera-transform (mtranslate #(0 0 -10)))
(scale #(21 16 1))

(define sw (get-sonotopic-grid-width))
(define sh (get-sonotopic-grid-height))
(define p (build-pixels sw sh))

(define (render)
    (let ([grid (sonotopic-grid)])
        (with-primitive p
            (pdata-index-map!
                (lambda (i c)
                    (let-values ([(y x) (quotient/remainder i sw)])
                        (vector-ref (vector-ref grid y) x)))
                "c")
            (pixels-upload))))

(every-frame (render))