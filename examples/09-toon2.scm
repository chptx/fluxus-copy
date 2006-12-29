(desiredfps 100000)
(show-fps 1)
;17.3
; a smaller example of toonshading that should be fast enough to 
; light every frame - also an example of reference geometry

(clear)

(define dirlight1 (vtransform (vector 0 1 0) (mrotate (vector 45 45 0))))
(texture (load-texture "textures/gradient.png"))
(hint-unlit)

; the software lighting function, uses a dot product to calculate the amount
; the normal faces into the light direction, 1 = full, 0 = perpendicular, 
; -1 means it's facing away
(define (toon-light n)
    (let ((lighting (vdot (pdata-get "n" n) dirlight1)))
        (if (< lighting 0) (set! lighting 0.1))     ; reverse facing polys are nearly black
        (pdata-set "t" n (vector lighting 0 0)))
    (if (< n 1)
        0
        (toon-light (- n 1))))

; deform the object so it's more interesting to light. this function uses reference points and
; normals (copied from the original shape after it's made below). this is done so the object 
; doesn't get progressively destroyed, but retains it's initial shape
(define (deform n)
    (pdata-set "p" n (vadd (pdata-get "pref" n) ; add deformation with the original geometry
        (vmul (pdata-get "nref" n)              ; push and pull along the original normal
            (* (sin (* (+ (vector-ref (pdata-get "p" n) 1) (* (time) 0.3)) 9)) 0.2))))
    (if (< n 1)
        0
        (deform (- n 1))))


(colour (vector 0.9 0.5 1))
; build a sphere...
(define s (build-sphere 20 20))
(grab s)
; and copy it's points and normals before we deform them
(pdata-copy "p" "pref")
(pdata-copy "n" "nref")
(ungrab)

(define (render)
    (grab s)
    (pdata-copy "pref" "p") ; reset the points back
    (deform (pdata-size))
    (recalc-normals 1)
    (toon-light (pdata-size))
    (ungrab))

(every-frame (render)) 
