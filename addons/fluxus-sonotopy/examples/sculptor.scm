(require racket/vector)
(require racket/math)
(require fluxus-018/fluxus-sonotopy)

(clear)

(set-camera-transform (mtranslate #(0 0 -10)))
(ortho)

(init-sonotopy)

(hint-ignore-depth)
(colour #(1 .7))

(define r (build-ribbon 35))
(with-primitive r
    (hint-unlit)
    (pdata-index-map!
        (lambda (i w)
            (* .01 (sin (* pi (/ i (pdata-size))))))
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
    (add-p (vsub (path-cursor) #(.5 .5 0)))
    (with-primitive r
        (identity)
        (rotate (vector (* 10 (delta)) (* 300 (* (delta) (beat))) 0))
        (apply-transform)))

(every-frame (render))

