(require racket/vector)
(require racket/math)
(require fluxus-018/fluxus-sonotopy)

(clear)

(set-camera-transform (mtranslate #(-0.5 -0.5 -10)))
(ortho)

(init-sonotopy)

(define r (build-ribbon 10))
(with-primitive r
    (hint-unlit)
    (pdata-index-map!
        (lambda (i w)
            (* .01 (sin (* pi (/ i (pdata-size))))))
        "w"))

(define (add-p np)
    (with-primitive r
        (when (not (zero? (vdist (pdata-ref "p" (- (pdata-size) 1))
                        np)))
            (pdata-index-map!
                (lambda (i p)
                    (if (< i (- (pdata-size) 2))
                        (pdata-ref "p" (add1 i))
                        np))
                "p"))))

(define (render)
    (add-p (path-cursor)))

(every-frame (render))
