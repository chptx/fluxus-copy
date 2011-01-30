(require racket/vector)
(require racket/math)
(require fluxus-017/fluxus-sonotopy)

(clear)

(set-camera-transform (mtranslate #(0 0 -10)))
(ortho)

(init-sonotopy)

(hint-ignore-depth)
(colour #(1 .5))

(define r (build-ribbon 128))
(with-primitive r
    (hint-unlit)
    (pdata-index-map!
        (lambda (i w)
            (* .03 (sin (* pi (/ i (pdata-size))))))
        "w"))

(define (add-p np)
    (with-primitive r
        (when (> (vdist (pdata-ref "p" (- (pdata-size) 1))
                        np) .03)
            (pdata-index-map!
                (lambda (i p)
                    (if (< i (- (pdata-size) 2))
                        (pdata-ref "p" (add1 i))
                        np))
                "p"))))

(define (render)
    (add-p (vsub (sonotopic-grid-path) #(.5 .5 0)))
    (with-primitive r
        (identity)
        (rotate (vector 0 (vane) 0))
        (apply-transform)))

(every-frame (render))

