(require fluxus-017/fluxus-sonotopy)
(init-sonotopy)

(clear)

(scale .45)
(translate #(-19 0 0))

(define (bars)
    (for ([i (in-range (get-num-spectrum-bins))])
        (translate #(1.1 0 0))
        (with-state
            (scale (vector 1 (+ .1 (* 10 (spectrum-bin i))) 1))
            (translate #(0 .5 0))
            (draw-cube))))

(every-frame (bars))
