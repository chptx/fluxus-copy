;; [ Copyright (C) 2008 Dave Griffiths : GPLv2 see LICENCE ]

;; This script the work normally done by boot.scm and the Fluxus main
;; application when running under MrEd
;;
;; Note: user can pass in window arguments by defining top-level variables
;;
;; fluxus:win-label	-- name of window
;; fluxus:win-min-width	-- window width (default 720)
;; fluxus:win-min-height - window heigh (default 576)
;; fluxus:win-fovy	-- vertical fovy (default 90)
;; fluxus:win-near	-- near clip plane (default 1)
;; fluxus:win-far	-- far clip plane (default 10000)
;;

#lang scheme/base

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; execute the USER CONFIG SCRIPT, if it exists PRIOR to loading everything
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define user-script (string-append (getenv "HOME") "/.fluxus.scm"))
(when (file-exists? user-script)
  (load user-script))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; from boot.scm
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require scheme/class
         mred/mred
         fluxus-016/fluxus
         (prefix-in gl- sgl/sgl))

(provide 
 (all-from-out fluxus-016/fluxus))

(provide defined? *canvas* *frame* restart set-fov)

(define (defined? s . def)
  (namespace-variable-value s #t (lambda () (if (null? def) #f (car def)))))

(define fluxus-collects-location (path->string (car (current-library-collection-paths))))
(define fluxus-version "016")
(define fluxus-name (string-append "fluxus-" fluxus-version))
;; need a better way to do this (environment variables? top-levels?)
(define fluxus-data-location (defined? 'fluxus-data-location 
			       (string-append fluxus-collects-location "/" fluxus-name )))

;; things normally done by boot.scm
(init-help (string-append fluxus-collects-location "/" fluxus-name "/helpmap.scm"))
(set-searchpaths (list
                  "./"
                  (string-append fluxus-data-location "/material/textures/")
                  (string-append fluxus-data-location "/material/shaders/")
                  (string-append fluxus-data-location "/material/meshes/")
		  (string-append fluxus-data-location "/material/fonts/")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Backwards Compatibility
;; (todo: remove all below at some point!)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide time pdata-set pdata-get build-line)
;; override the built in time function for pre 0.12 compatibility
(define time flxtime)
;; for compatibility pre 0.13
(define pdata-set pdata-set!)
(define pdata-get pdata-ref)
;; for compatibility pre 0.15
(define build-line build-ribbon)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Fluxus Application section
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define fluxus-canvas%
  (class* canvas% ()
    (inherit with-gl-context swap-gl-buffers)
    
    (define/override (on-paint)
      (with-gl-context
       (lambda ()           
         (fluxus-frame-callback)
         
         ; swap the buffers 
         (swap-gl-buffers)
         (super on-paint))))
    
    ;; route the events from mred into fluxus - this mostly consists of making
    ;; mred behave like glut with it's input formats (for the moment)
    (define/override (on-size width height)
      (with-gl-context
       (lambda () 
         (fluxus-reshape-callback width height)
	 (set-fov last-fovy last-near last-far)	;; reset FOV for new window size
	 )))
    
    ; mouse
    (define/override (on-event event)
      (let* ((button (cond
                       ((send event get-left-down) 0)
                       ((send event get-middle-down) 1)
                       ((send event get-right-down) 2)
                       (else -1)))
             (state (cond 
                      ((send event button-down? 'any) 0)
                      (else 1))))
        
        (when (send event button-changed? 'any)
          (fluxus-input-callback 0 button -1 state (send event get-x) (send event get-y) 0))          
        (when (send event dragging?)
          (fluxus-input-callback 0 -1 -1 -1 (send event get-x) (send event get-y) 0))))
    
    ; keyboard
    (define/override (on-char event)
      (cond 
        ((equal? 'release (send event get-key-code))
         (clear-down)) ; todo: how to get key code from release?
        (else
         (fluxus-input-callback (send event get-key-code) 0 0 0 0 0 0))))
    
    (define (fluxus-canvas-new)  
      (super-instantiate () (style '(gl)))
      (with-gl-context
       (lambda () 
         (clear-texture-cache)      
         (fluxus-init))))
    
    (fluxus-canvas-new)))

(define *frame* (instantiate frame% ("drflux")))
(define *canvas* (instantiate fluxus-canvas% (*frame*)
			      (min-width (defined? 'fluxus:win-min-width 720))
			      (min-height (defined? 'fluxus:win-min-height 576))))

(define (loop) 
  (send *canvas* on-paint)
  (sleep 0.0001)	;; smoother update
  (loop))

(define fluxus-thread #f)
(define (restart)
  ;; stop old thread if it exists
  (when fluxus-thread (kill-thread fluxus-thread))
  (thread loop))

(define last-fovy 90)	;; vertical FOV
(define last-near 1)
(define last-far 10000)

(define (set-fov fovy near far)
  ;; Specify vertical FOV in degrees and clip info to make sure it's consistent
  (let* ([ymax (* near (tan (* (/ (* fovy 3.141592) 180) 0.5)))]
	 [ymin (- ymax)]
	 [scrsize (get-screen-size)]
	 [aspect (/ (vector-ref scrsize 0) (vector-ref scrsize 1))]	;; width/height
	 )
    (set! last-fovy fovy)	;; remember so we can handle reshapes
    (set! last-near near)
    (set! last-far far)
    (clip near far)
    (frustum (* ymin aspect) (* ymax aspect) ymin ymax)))

(define (init-me)
  ;; make and show the window and canvas 
  (fluxus-reshape-callback (defined? 'fluxus:win-min-width 720) (defined? 'fluxus:win-min-height 576))
  (set-fov (defined? 'fluxus:win-fovy last-fovy) (defined? 'fluxus:win-near last-near) (defined? 'fluxus:win-far last-far))
  (send *frame* set-label (defined? 'fluxus:win-label (string-append "drflux " fluxus-version)))
  (send *frame* show #t)
  ; start fluxus main loop
  (restart))

(init-me)

(define (waitforinit)
  ;; Let the update loop cycle once before trying to use libfluxus
  (when (= (flxtime) 0.0)
	(sleep 0.05)
	(waitforinit)
	))

(waitforinit)