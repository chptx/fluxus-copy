(require racket/class)
(require racket/math)
(require fluxus-018/fluxus-sonotopy)

(clear)
(init-sonotopy)

(set-camera-transform (mtranslate #(0 0 -10)))
(ortho)

(define dancer%
    (class object%
        (field [current-pos 0]
            [angle 0]
            [angle-offset 0]
            [angle-dir 0]
            [speed-factor 0]
            [speed-offset 0]
            [thickness 0]
            [r (build-ribbon 10)])
        
        (define (reset)
            (set! current-pos (crndvec))
            (set! angle-offset (* 2 pi (rndf)))
            (set! angle-dir (if (zero? (random 2)) 1 -1))
            (set! speed-factor (+ .7 (* .3 (rndf))))
            (set! speed-offset (* .2 (crndf)))
            (set! thickness (+ .001 (* .005 (rndf))))
            
            (with-primitive r
                (hint-unlit)
                (pdata-index-map!
                    (lambda (i w)
                        (* thickness (sin (* pi (/ i (pdata-size))))))
                    "w")
                (pdata-map!
                    (lambda (p)
                        current-pos)
                    "p")))
        
        (reset)
        
        (define (add-p np)
            (with-primitive r
                (pdata-index-map!
                    (lambda (i p)
                        (if (< i (- (pdata-size) 2))
                            (pdata-ref "p" (add1 i))
                            np))
                    "p")))
        
        (define (out-of-bounds?)
            (with-primitive r
                (pdata-fold
                    (lambda (p a)
                        (and a
                            (or (< (vx p) -1) (> (vx p) 1)
                                (< (vy p) -1) (> (vy p) 1))))
                    #t
                    "p")))
                  
        (define/public (update)
            (let ([v (+ angle-offset (* angle-dir (vane)))]
                  [speed (* (+ speed-offset (beat)) speed-factor)])
                (set! angle (lerp angle v .2))
                (set! current-pos
                    (vadd current-pos
                        (vmul (vector (cos angle) (sin angle) 0)
                            (* speed (delta)))))
                (add-p current-pos))
            (when (out-of-bounds?)
                (reset)))
        
        (super-new)))

(define dancers (build-list 50 (lambda (x) (make-object dancer%))))

(define (render)
    (for-each
        (lambda (d)
            (send d update))
        dancers))

(every-frame (render))

