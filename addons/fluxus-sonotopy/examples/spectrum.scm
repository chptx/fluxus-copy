(require fluxus-017/fluxus-sonotopy)
(init-sonotopy)

(define (bars c)
    (cond ((not (negative? c))
        (translate (vector 1.1 0 0))
        (with-state
            (colour (vector 1 0 (spectrum-bin c)))
            (scale (vector 1 (+ 0.1 (* 5 (spectrum-bin c))) 1))
            (draw-cube))
        (bars (- c 1)))))

(every-frame (bars (- (get-num-spectrum-bins) 1)))
