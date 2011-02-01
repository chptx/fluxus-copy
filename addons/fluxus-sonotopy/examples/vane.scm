(require racket/math)
(require fluxus-017/fluxus-sonotopy)

(clear)
(init-sonotopy)

(define (render)
  (rotate (vector 90 (* 360 (/ (vane) (* pi 2))) 0))
  (scale (vector (beat) 0.1 0.1))
  (translate (vector 0.5 0 0))
  (draw-cube))

(every-frame (render))
