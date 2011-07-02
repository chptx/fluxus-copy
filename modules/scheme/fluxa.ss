;; [ Copyright (C) 2008 Dave Griffiths : GPLv2 see LICENCE ]

; an experimental non-determinsistic synth

;; StartSectionDoc-en
;; fluxa
;; Fluxa is the fluxus audio synth for livecoding, it contains quite basic atomic components which can be
;; used together to create more complicated sounds. It uses an experimental and fairly brutal method of graph node
;; garbage collection which gives it certain non-deterministic qualities. It's also been battle tested in
;; many a live performance. The fluxa server needs to be run and connected to jack in order for you to hear
;; anything. Also, fluxa is not in the default namespace, so use eg (require fluxus-017/fluxa).
;; Example:
;; EndSectionDoc
#lang racket/base

(require "scratchpad.ss"
		"tasks.ss"
		"fluxus-modules.ss"
		scheme/list)
(provide
		play play-now seq clock-map clock-split volume pan max-synths note searchpath reset eq comp
		sine saw tri squ white pink adsr add sub mul div pow mooglp moogbp mooghp formant sample
		crush distort klip echo ks xfade s&h t&h ramp deltrig lfo-sine lfo-saw lfo-revsaw lfo-tri 
		lfo-squ kas-filter scrub load zmod sync-tempo sync-clock fluxa-init fluxa-debug set-global-offset set-bpm-mult 
		logical-time inter pick set-scale)

(define time-offset 0.0)
(define sync-offset 0.0)
(define bpm-mult 1)
(define fluxa-searchpaths (get-searchpaths))

(define TERMINAL 0) (define SINE 1) (define SAW 2) (define TRI 3) (define SQU 4)
(define WHITE 5) (define PINK 6) (define ADSR 7) (define ADD 8) (define SUB 9)
(define MUL 10) (define DIV 11) (define POW 12) (define MOOGLP 13) (define MOOGBP 14)
(define MOOGHP 15) (define FORMANT 16) (define SAMPLE 17) (define CRUSH 18)
(define DISTORT 19) (define CLIP 20) (define ECHO 21) (define KS 22) (define XFADE 23)
(define SAMPNHOLD 24) (define TRACKNHOLD 25) (define RAMP 26) (define DELTRIG 27)
(define LFOSIN 28) (define LFOSAW 29) (define LFOREVSAW 30) (define LFOTRI 31)
(define LFOSQU 32) (define KASF 33) (define SCRUB 34)

(define (fluxa-init)
  (osc-destination "osc.udp://127.0.0.1:4004")
  (osc-source "4444")
  (osc-send "/setclock" "" '())
  (for-each
	  searchpath
	  fluxa-searchpaths)
  (spawn-task go-flux 'fluxa-update-task))

;------------------------------
; infrastructure

(define current-id 0)

(define (new-id)
  (let ((ret (+ current-id 1)))
    (set! current-id ret)
    ret))

(define-struct node (id))

(define (get-node-id v)
  (cond ((node? v)
         (node-id v))
        (else
         (let ((id (new-id)))
           (osc-send "/create" "iif" (list id TERMINAL v))
           id))))

(define (make-args id operands)
  (let ((index -1))
    (foldl
     (lambda (a l)
       (set! index (+ index 1))
       (append l (list id index (get-node-id a))))
     '()
     operands)))

(define (make-format operands)
  (make-string (* 3 (length operands)) #\i))

(define (operator op operands)
  (let ((id (new-id)))
    (osc-send "/create" "ii" (list id op))
    (osc-send "/connect"
              (make-format operands)
              (make-args id operands))
    (make-node id)))

(define current-sample-id 0)
(define samples '())

;; StartFunctionDoc-en
;; reload
;; Returns: void
;; Description:
;; Causes samples to be reloaded if you need to restart the fluxa server
;; Example:
;; (reload)
;; EndFunctionDoc

(define (reload)
  (set! samples '()))

(define (get-sample-id filename)
  (let ((t (assoc filename samples)))
    (cond ((list? t) (cadr t))
          (else
           (osc-send "/addtoqueue" "is" (list current-sample-id filename))
           (set! samples (cons (list filename current-sample-id) samples))
           (set! current-sample-id (+ current-sample-id 1))
           (- current-sample-id 1)))))

;------------------------------
; synthesis

;; StartFunctionDoc-en
;; sine frequency-number-or-node
;; Returns: node-id-number
;; Description:
;; Creates a sine wave generator node
;; Example:
;; (play-now (mul (sine 440) (adsr 0.1 0.1 0 0)))
;; EndFunctionDoc

;; StartFunctionDoc-pt
;; sine nó-ou-número-frequência
;; Retorna: número-nó-id
;; Descrição:
;; Cria um nó gerador de onda sinusoidal.
;; Exemplo:
;; (play-now (mul (sine 440) (adsr 0.1 0.1 0 0)))
;; EndFunctionDoc

(define (sine a)
  (if (not (or (number? a) (node? a)))
    (raise-type-error 'sine "number-or-node" a)
    (operator SINE (list a))))

;; StartFunctionDoc-en
;; saw frequency-number-or-node
;; Returns: node-id-number
;; Description:
;; Creates a saw wave generator node
;; Example:
;; (play-now (mul (saw 440) (adsr 0.1 0.1 0 0)))
;; EndFunctionDoc

;; StartFunctionDoc-pt
;; saw nó-número-frequência
;; Retorna: número-nó-id
;; Descrição:
;; Cria um nó gerador de onda serrada.
;; Exemplo:
;; (play-now (mul (saw 440) (adsr 0.1 0.1 0 0)))
;; EndFunctionDoc

(define (saw a)
  (if (not (or (number? a) (node? a)))
    (raise-type-error 'saw "number-or-node" a)
    (operator SAW (list a))))
    
;; StartFunctionDoc-en
;; tri frequency-number-or-node
;; Returns: node-id-number
;; Description:
;; Creates a triangle wave generator node
;; Example:
;; (play-now (mul (tri 440) (adsr 0.1 0.1 0 0)))
;; EndFunctionDoc

;; StartFunctionDoc-pt
;; tri nó-número-frequencia
;; Retorna: número-nó-id
;; Descrição:
;; Cria um gerador de onda triangular
;; Exemplo:
;; (play-now (mul (tri 440) (adsr 0.1 0.1 0 0)))
;; EndFunctionDoc

(define (tri a)
  (if (not (or (number? a) (node? a)))
    (raise-type-error 'tri "number-or-node" a)
    (operator TRI (list a))))
    
;; StartFunctionDoc-en
;; squ frequency-number-or-node
;; Returns: node-id-number
;; Description:
;; Creates a square wave generator node
;; Example:
;; (play-now (mul (squ 440) (adsr 0.1 0.1 0 0)))
;; EndFunctionDoc

;; StartFunctionDoc-pt
;; squ nó-número-frequencia
;; Retorna: número-nó-id
;; Descrição:
;; Cria um gerador de onda quadrada.
;; Exemplo:
;; (play-now (mul (squ 440) (adsr 0.1 0.1 0 0)))
;; EndFunctionDoc

(define (squ a)
  (if (not (or (number? a) (node? a)))
    (raise-type-error 'squ "number-or-node" a)
    (operator SQU (list a))))

;; StartFunctionDoc-en
;; white frequency-number-or-node
;; Returns: node-id-number
;; Description:
;; Creates a white noise generator node
;; Example:
;; (play-now (mul (white 5) (adsr 0.1 0.1 0 0)))
;; EndFunctionDoc

;; StartFunctionDoc-pt
;; white nó-número-frequência
;; Retorna: nó-número-id
;; Descrição:
;; Cria um gerador de noise white.
;; Exemplo:
;; (play-now (mul (white 5) (adsr 0.1 0.1 0 0)))
;; EndFunctionDoc

(define (white a)
  (if (not (or (number? a) (node? a)))
    (raise-type-error 'white "number-or-node" a)
    (operator WHITE (list a))))

;; StartFunctionDoc-en
;; pink frequency-number-or-node
;; Returns: node-id-number
;; Description:
;; Creates a pink noise generator node
;; Example:
;; (play-now (mul (pink 5) (adsr 0.1 0.1 0 0)))
;; EndFunctionDoc

;; StartFunctionDoc-pt
;; pink nó-número-frequência
;; Retorna: nó-número-id
;; Descrição:
;; Cria um gerador de pink white.
;; Exemplo:
;; (play-now (mul (pink 5) (adsr 0.1 0.1 0 0)))
;; EndFunctionDoc

(define (pink a)
  (if (not (or (number? a) (node? a)))
    (raise-type-error 'pink "number-or-node" a)
    (operator PINK (list a))))

;; StartFunctionDoc-en
;; add number-or-node number-or-node
;; Returns: node-id-number
;; Description:
;; Maths node - adds two signals together
;; Example:
;; (play-now (mul (add (sine 440) (sine 220)) (adsr 0.1 0.1 0 0)))
;; EndFunctionDoc

;; StartFunctionDoc-pt
;; add número-or-nó número-or-nó
;; Retorna: nó-número-id
;; Descrição:
;; Nó matemático - adiciona dois sinais.
;; Exemplo:
;; (play-now (mul (add (sine 440) (sine 220)) (adsr 0.1 0.1 0 0)))
;; EndFunctionDoc

(define (add a b)
	(cond
		((not (or (number? a) (node? a))) (raise-type-error 'add "number-or-node" 0 a b))
		((not (or (number? b) (node? b))) (raise-type-error 'add "number-or-node" 1 a b))
		(else (operator ADD (list a b)))))

;; StartFunctionDoc-en
;; sub number-or-node number-or-node
;; Returns: node-id-number
;; Description:
;; Maths node - subtracts two signals
;; Example:
;; (play-now (mul (sub (sine 440) (sine 220)) (adsr 0.1 0.1 0 0)))
;; EndFunctionDoc

;; StartFunctionDoc-pt
;; sub número-or-nó número-or-nó
;; Retorna: nó-número-id
;; Descrição:
;; Nó matemático - subtrai dois sinais.
;; Exemplo:
;; (play-now (mul (sub (sine 440) (sine 220)) (adsr 0.1 0.1 0 0)))
;; EndFunctionDoc

(define (sub a b)
	(cond
		((not (or (number? a) (node? a))) (raise-type-error 'sub "number-or-node" 0 a b))
		((not (or (number? b) (node? b))) (raise-type-error 'sub "number-or-node" 1 a b))
		(else (operator SUB (list a b)))))
		
;; StartFunctionDoc-en
;; mul number-or-node number-or-node
;; Returns: node-id-number
;; Description:
;; Maths node - multiplies two signals
;; Example:
;; (play-now (mul (mul (sine 440) (sine 220)) (adsr 0.1 0.1 0 0)))
;; EndFunctionDoc

;; StartFunctionDoc-pt
;; mul número-or-nó número-or-nó
;; Retorna: nó-número-id
;; Descrição:
;; Nó matemático - multiplica dois sinais.
;; Exemplo:
;; (play-now (mul (mul (sine 440) (sine 220)) (adsr 0.1 0.1 0 0)))
;; EndFunctionDoc

(define (mul a b)
	(cond
		((not (or (number? a) (node? a))) (raise-type-error 'mul "number-or-node" 0 a b))
		((not (or (number? b) (node? b))) (raise-type-error 'mul "number-or-node" 1 a b))
		(else (operator MUL (list a b)))))

;; StartFunctionDoc-en
;; div number-or-node number-or-node
;; Returns: node-id-number
;; Description:
;; Maths node - divides two signals
;; Example:
;; (play-now (mul (div (sine 440) 2) (adsr 0.1 0.1 0 0)))
;; EndFunctionDoc

;; StartFunctionDoc-pt
;; div número-or-nó número-or-nó
;; Retorna: nó-número-id
;; Descrição:
;; Nó matemático - divide dois sinais.
;; Exemplo:
;; (play-now (mul (div (sine 440) 2) (adsr 0.1 0.1 0 0)))
;; EndFunctionDoc

(define (div a b)
	(cond
		((not (or (number? a) (node? a))) (raise-type-error 'div "number-or-node" 0 a b))
		((not (or (number? b) (node? b))) (raise-type-error 'div "number-or-node" 1 a b))
		(else (operator DIV (list a b)))))
		
;; StartFunctionDoc-en
;; pow number-or-node number-or-node
;; Returns: node-id-number
;; Description:
;; Maths node - produces a signal raised to the power of another
;; Example:
;; (play-now (mul (pow (adsr 0 0.1 0 0) 10) (sine 440)))
;; EndFunctionDoc

;; StartFunctionDoc-pt
;; pow número-or-nó número-or-nó
;; Retorna: nó-número-id
;; Descrição:
;; Nó matemático - produz um sinal elevado a potência de outro.
;; Exemplo:
;; (play-now (mul (pow (adsr 0 0.1 0 0) 10) (sine 440)))
;; EndFunctionDoc

(define (pow a b)
	(cond
		((not (or (number? a) (node? a))) (raise-type-error 'pow "number-or-node" 0 a b))
		((not (or (number? b) (node? b))) (raise-type-error 'pow "number-or-node" 1 a b))
		(else (operator POW (list a b)))))

;; StartFunctionDoc-en
;; adsr attack-number-or-node decay-number-or-node sustain-number-or-node release-number-or-node
;; Returns: node-id-number
;; Description:
;; Creates an envelope generator node
;; Example:
;; (play-now (mul (sine 440) (adsr 0.1 0.1 0 0)))
;; EndFunctionDoc

;; StartFunctionDoc-pt
;; adsr nó-número-attack número-nó-decay número-no-sustain nó-número-release
;; Retorna: nó-número-id
;; Descrição:
;; Cria um nó gerador de envelope.
;; Exemplo:
;; (play-now (mul (sine 440) (adsr 0.1 0.1 0 0)))
;; EndFunctionDoc

  
(define (adsr a d s r)
	(cond
		((not (or (number? a) (node? a))) (raise-type-error 'adsr "number-or-node" 0 a d s r))
		((not (or (number? d) (node? d))) (raise-type-error 'adsr "number-or-node" 1 a d s r))
		((not (or (number? s) (node? s))) (raise-type-error 'adsr "number-or-node" 2 a d s r))
		((not (or (number? r) (node? r))) (raise-type-error 'adsr "number-or-node" 3 a d s r))
		(else (operator ADSR (list a d s r)))))

;; StartFunctionDoc-en
;; mooglp signal-node cutoff-number-or-node resonance-number-or-node
;; Returns: node-id-number
;; Description:
;; Creates an low pass moog filter node
;; Example:
;; (play-now (mul (mooglp (squ 440) 0.1 0.4) (adsr 0.1 0.1 0 0)))
;; EndFunctionDoc

;; StartFunctionDoc-pt
;; mooglp nó-signal nó-número-cutoff nó-número-resonance
;; Retorna: nó-número-id
;; Descrição:
;; Cria um nó filtro low pass moog
;; Exemplo:
;; (play-now (mul (mooglp (squ 440) 0.1 0.4) (adsr 0.1 0.1 0 0)))
;; EndFunctionDoc

(define (mooglp in cutoff resonance)
	(cond
		((not (node? in)) (raise-type-error 'mooglp "node" 0 in cutoff resonance))
		((not (or (number? cutoff) (node? cutoff))) (raise-type-error 'mooglp "number-or-node" 1 in cutoff resonance))
		((not (or (number? resonance) (node? resonance))) (raise-type-error 'mooglp "number-or-node" 2 in cutoff resonance))
		(else (operator MOOGLP (list in cutoff resonance)))))

;; StartFunctionDoc-en
;; moogbp signal-node cutoff-number-or-node resonance-number-or-node
;; Returns: node-id-number
;; Description:
;; Creates an band pass moog filter node
;; Example:
;; (play-now (mul (moogbp (squ 440) 0.1 0.4) (adsr 0.1 0.1 0 0)))
;; EndFunctionDoc

;; StartFunctionDoc-pt
;; moogbp nó-signal nó-número-cutoff nó-número-resonance
;; Retorna: nó-número-id
;; Descrição:
;; Cria um nó filtro band pass moog.
;; Exemplo:
;; (play-now (mul (moogbp (squ 440) 0.1 0.4) (adsr 0.1 0.1 0 0)))
;; EndFunctionDoc

(define (moogbp in cutoff resonance)
	(cond
		((not (node? in)) (raise-type-error 'moogbp "node" 0 in cutoff resonance))
		((not (or (number? cutoff) (node? cutoff))) (raise-type-error 'moogbp "number-or-node" 1 in cutoff resonance))
		((not (or (number? resonance) (node? resonance))) (raise-type-error 'moogbp "number-or-node" 2 in cutoff resonance))
		(else (operator MOOGBP (list in cutoff resonance)))))

;; StartFunctionDoc-en
;; mooghp signal-node cutoff-number-or-node resonance-number-or-node
;; Returns: node-id-number
;; Description:
;; Creates an high pass moog filter node
;; Example:
;; (play-now (mul (mooghp (squ 440) 0.1 0.4) (adsr 0.1 0.1 0 0)))
;; EndFunctionDoc

;; StartFunctionDoc-pt
;; mooghp nó-signal nó-número-cutoff nó-número-resonance
;; Retorna: nó-número-id
;; Descrição:
;; Cria um filtro high pass moog.
;; Exemplo:
;; (play-now (mul (mooghp (squ 440) 0.1 0.4) (adsr 0.1 0.1 0 0)))
;; EndFunctionDoc

(define (mooghp in cutoff resonance)
	(cond
		((not (node? in)) (raise-type-error 'mooghp "node" 0 in cutoff resonance))
		((not (or (number? cutoff) (node? cutoff))) (raise-type-error 'mooghp "number-or-node" 1 in cutoff resonance))
		((not (or (number? resonance) (node? resonance))) (raise-type-error 'mooghp "number-or-node" 2 in cutoff resonance))
		(else (operator MOOGHP (list in cutoff resonance)))))

;; StartFunctionDoc-en
;; formant signal-node vowel-number-or-node 
;; Returns: node-id-number
;; Description:
;; Creates a formant filter node, vowel selection ranges from 0 to 4
;; Example:
;; (play-now (mul (formant (squ 440) 3) (adsr 0.1 0.1 0 0)))
;; EndFunctionDoc

;; StartFunctionDoc-pt
;; formant nó-signal nó-número-cutoff nó-número-resonance
;; Retorna: nó-número-id
;; Descrição:
;; Cria um nó filtro formant
;; Exemplo:
;; (play-now (mul (formant (squ 440) 0.1 0.4) (adsr 0.1 0.1 0 0)))
;; EndFunctionDoc

  
(define (formant in vowel)
	(cond
		((not (node? in)) (raise-type-error 'formant "node" 0 in vowel))
		((not (or (number? vowel) (node? vowel))) (raise-type-error 'formant "number-or-node" 1 in vowel))
		(else (operator FORMANT (list in vowel 0)))))

;; StartFunctionDoc-en
;; sample sample-filename-string frequency-number-or-node
;; Returns: node-id-number
;; Description:
;; Creates a sample playback node
;; Example:
;; (play-now (sample "sample.wav" 440))
;; EndFunctionDoc

;; StartFunctionDoc-pt
;; sample string-nome-arquivo-sample nó-número-frequêcia
;; Retorna: nó-número-id
;; Descrição:
;; Cria um nó para tocar amostras.
;; Exemplo:
;; (play-now (sample "helicopter.wav" 440))
;; EndFunctionDoc

(define (sample filename freq)
	(cond
		((not (string? filename)) (raise-type-error 'sample "string" 0 filename freq))
		((not (or (number? freq) (node? freq))) (raise-type-error 'sample "number-or-node" 1 filename freq))
		(else (operator SAMPLE (list (get-sample-id filename) freq)))))

;; StartFunctionDoc-en
;; crush signal-node frequency-number-or-node bit-depth-number-or-node
;; Returns: node-id-number
;; Description:
;; Creates a crush effect node
;; Example:
;; (play-now (crush (sine 440) 0.4 8))
;; EndFunctionDoc

;; StartFunctionDoc-pt
;; crush nó-signal nó-número-frequência nó-número-bit-depth
;; Retorna: nó-número-id
;; Descrição:
;; Cria um nó de efeito crush.
;; Exemplo:
;; (play-now (crush (sine 440) 0.4 8))
;; EndFunctionDoc

(define (crush in freq bits)
	(cond
		((not (node? in)) (raise-type-error 'crush "node" 0 in bits freq))
		((not (or (number? bits) (node? bits))) (raise-type-error 'crush "number-or-node" 1 in bits freq))
		((not (or (number? freq) (node? freq))) (raise-type-error 'crush "number-or-node" 2 in bits freq))
		(else (operator CRUSH (list in bits freq)))))

;; StartFunctionDoc-en
;; distort signal-node amount-number-or-node
;; Returns: node-id-number
;; Description:
;; Creates a distortion effect node
;; Example:
;; (play-now (distort (sine 440) 0.9))
;; EndFunctionDoc

;; StartFunctionDoc-pt
;; distort nó-signal nó-número-quantidade
;; Retorna: nó-número-id
;; Descrição:
;; Cria um nó de efeito distort.
;; Exemplo:
;; (play-now (distort (sine 440) 0.9))
;; EndFunctionDoc

(define (distort in amount)
	(cond
		((not (node? in)) (raise-type-error 'distort "node" 0 in amount))
		((not (or (number? amount) (node? amount))) (raise-type-error 'distort "number-or-node" 1 in amount))
		(else (operator DISTORT (list in amount)))))

;; StartFunctionDoc-en
;; klip signal-node amount-number-or-node
;; Returns: node-id-number
;; Description:
;; Creates a hard clipping distortion effect node
;; Example:
;; (play-now (klip (sine 440) 0.9))
;; EndFunctionDoc

;; StartFunctionDoc-pt
;; klip nó-signal nó-número-quantidade
;; Retorna: nó-número-id
;; Descrição:
;; Cria um nó de efeito hard clipping distortion.
;; Exemplo:
;; (play-now (klip (sine 440) 0.9))
;; EndFunctionDoc

(define (klip in amount)
	(cond
		((not (node? in)) (raise-type-error 'klip "node" 0 in amount))
		((not (or (number? amount) (node? amount))) (raise-type-error 'klip "number-or-node" 1 in amount))
		(else (operator CLIP (list in amount)))))

;; StartFunctionDoc-en
;; echo signal-node delay-time-number-or-node feedback-number-or-node
;; Returns: node-id-number
;; Description:
;; Creates a echo effect node
;; Example:
;; (play-now (echo 3 (sine 440)))
;; EndFunctionDoc

;; StartFunctionDoc-pt
;; echo nó-signal nó-número-atraso-tempo nó-número-feedback
;; Retorna: nó-número-id
;; Descrição:
;; Cria um nó de efeito echo.
;; Exemplo:
;; (play-now (echo 3 (sine 440)))
;; EndFunctionDoc

(define (echo in delaytime feedback)
	(cond
		((not (node? in)) (raise-type-error 'echo "node" 0 in delaytime feedback))
		((not (or (number? delaytime) (node? delaytime))) (raise-type-error 'echo "number-or-node" 1 in delaytime feedback))
		((not (or (number? feedback) (node? feedback))) (raise-type-error 'echo "number-or-node" 2 in delaytime feedback))
		(else (operator ECHO (list in delaytime feedback)))))

;; StartFunctionDoc-en
;; ks freq cutoff resonance
;; Returns: node-id-number
;; Description:
;; Karplus strong "ocillator" node. Plucks virtual strings by filtering white 
;; noise in a feedback delay loop set resonate at the desired frequency. 
;; Example:
;; (play-now (ks (random 1000) 0.9 0.1))
;; EndFunctionDoc

(define (ks freq cutoff resonance)
	(cond
		((not (number? freq)) (raise-type-error 'ks "number" 0 freq cutoff resonance))
		((not (or (number? cutoff) (node? cutoff))) (raise-type-error 'ks "number-or-node" 1 freq cutoff resonance))
		((not (or (number? resonance) (node? resonance))) (raise-type-error 'ks "number-or-node" 2 freq cutoff resonance))
		(else (operator KS (list freq cutoff resonance)))))

;; StartFunctionDoc-en
;; xfade signal1-number-or-node signal2-number-or-node mix-number-or-node
;; Returns: number if all arguments are numbers, else node-id-number
;; Description:
;; Crossfader. Linearly crossfades between two signals or values 
;; "mix" ranges from 0 t0 1.
;; Example:
;; (play-now (xfade (sine 200) (saw 100) (lfo-sine 1))
;; EndFunctionDoc

(define (xfade s0 s1 mix) 
	(cond
		((not (or (number? s0) (node? s0))) (raise-type-error 'xfade "number-or-node" 0 s0 s1 mix))
		((not (or (number? s1) (node? s1))) (raise-type-error 'xfade "number-or-node" 1 s0 s1 mix))
		((not (or (number? mix) (node? mix))) (raise-type-error 'xfade "number-or-node" 2 s0 s1 mix))
		((and (number? s0) (number? s1) (number? mix)) (+ s0 (* (- s1 s0) (min 1 (max 0 mix)))))
		(else (operator XFADE (list s0 s1 mix)))))


;; StartFunctionDoc-en
;; s&h signal1-number-or-node CV-number-or-node 
;; Returns: node-id-number
;; Description:
;; Sample&Hold. Samples the input at positive zero crossings of the CV signal
;; Example:
;; (play-now (sine (add 1000 (mul 500 (s&h (white 440) (sine 8))))))
;; EndFunctionDoc

(define (s&h sig cv)
	(cond
		((not (or (number? sig) (node? sig))) (raise-type-error 's&h "number-or-node" 0 sig cv))
		((not (or (number? cv) (node? cv))) (raise-type-error 's&h "number-or-node" 1 sig cv))
		(else (operator SAMPNHOLD (list sig cv)))))

;; StartFunctionDoc-en
;; t&h signal1-number-or-node CV-number-or-node 
;; Returns: node-id-number
;; Description:
;; Track&Hold. Like s&h, except it samples at any positive CV value
;; Example:
;; (play-now (sine (add 1000 (mul 500 (t&h (white 440) (sine 4))))))
;; EndFunctionDoc

(define (t&h sig cv)
	(cond
		((not (or (number? sig) (node? sig))) (raise-type-error 't&h "number-or-node" 0 sig cv))
		((not (or (number? cv) (node? cv))) (raise-type-error 't&h "number-or-node" 1 sig cv))
		(else (operator TRACKNHOLD (list sig cv)))))

;; StartFunctionDoc-en
;; ramp start-number-or-node end-number-or-node duration-number-or-node 
;; Returns: node-id-number
;; Description:
;; ramp. Linearly ramps from a start value to a end value over a given duration
;; Example:
;; (play-now (sine (ramp 220 440 2)))
;; EndFunctionDoc


(define (ramp a b dur)
	(cond
		((not (or (number? a) (node? a))) (raise-type-error 'ramp "number-or-node" 0 a b dur))
		((not (or (number? b) (node? b))) (raise-type-error 'ramp "number-or-node" 1 a b dur))
		((not (or (number? dur) (node? dur))) (raise-type-error 'ramp "number-or-node" 2 a b dur))
		(else (operator RAMP (list a b dur)))))

;; StartFunctionDoc-en
;; deltrig signal-node delay-time-number-or-node 
;; Returns: node-id-number
;; Description:
;; Delays the triger of a node by a set amount of time
;; Example:
;; (play-now (sine (deltrig (ramp 330 1000 1) 2)))
;; EndFunctionDoc

(define (deltrig sig del)
	(cond
		((not  (node? sig)) (raise-type-error 'deltrig "node" 0 sig del))
		((not (or (number? del) (node? del))) (raise-type-error 'deltrig "number-or-node" 1 sig del))
		(else (operator DELTRIG (list sig del)))))

;; StartFunctionDoc-en
;; lfo-sine period-number-or-node 
;; Returns: node-id-number
;; Description:
;; Sine-wave oscillator made for modulation. Takes a period of time as input, output ranges from 0 to 1
;; Syncs to note triggers
;; Example:
;; (play-now (mooglp (saw 330) (lfo-sine 1) 0.3))
;; EndFunctionDoc
  
(define (lfo-sine per)
 	(if (not (or (number? per) (node? per)))
    	(raise-type-error 'lfo-sine "number-or-node" per)
    	(operator LFOSIN (list per))))

;; StartFunctionDoc-en
;; lfo-saw period-number-or-node 
;; Returns: node-id-number
;; Description:
;; Saw-wave oscillator made for modulation. Takes a period of time as input, output ranges from 0 to 1
;; Syncs to note triggers
;; Example:
;; (play-now (mooglp (saw 330) (lfo-saw 1) 0.3))
;; EndFunctionDoc

(define (lfo-saw per)
 	(if (not (or (number? per) (node? per)))
    	(raise-type-error 'lfo-saw "number-or-node" per)
    	(operator LFOSAW (list per))))

;; StartFunctionDoc-en
;; lfo-revsaw period-number-or-node 
;; Returns: node-id-number
;; Description:
;; Reverse saw-wave oscillator made for modulation. Takes a period of time as input, output ranges from 0 to 1
;; Syncs to note triggers
;; Example:
;; (play-now (mooglp (saw 330) (lfo-revsaw 1) 0.3))
;; EndFunctionDoc

(define (lfo-revsaw per)
 	(if (not (or (number? per) (node? per)))
    	(raise-type-error 'lfo-revsaw "number-or-node" per)
    	(operator LFOREVSAW (list per))))

;; StartFunctionDoc-en
;; lfo-tri period-number-or-node 
;; Returns: node-id-number
;; Description:
;; Sine oscillator made for modulation. Takes a period of time as input, output ranges from 0 to 1
;; Syncs to note triggers
;; Example:
;; (play-now (mooglp (saw 330) (lfo-tri 1) 0.3))
;; EndFunctionDoc

(define (lfo-tri per)
 	(if (not (or (number? per) (node? per)))
    	(raise-type-error 'lfo-tri "number-or-node" per)
    	(operator LFOTRI (list per))))

;; StartFunctionDoc-en
;; lfo-squ period-number-or-node 
;; Returns: node-id-number
;; Description:
;; Sine oscillator made for modulation. Takes a period of time as input, output ranges from 0 to 1
;; Syncs to note triggers
;; Example:
;; (play-now (mooglp (saw 330) (lfo-squ 1) 0.3))
;; EndFunctionDoc

(define (lfo-squ per)
 	(if (not (or (number? per) (node? per)))
    	(raise-type-error 'lfo-squ "number-or-node" per)
    	(operator LFOSQU (list per))))

;; StartFunctionDoc-en
;; kas-filter input-node cutoff-number-or-node resonance-number-or-node shape-optional-number-or-node
;; Returns: node-id-number
;; Description:
;; Undersampling-based lowpass filter with cutoff, resonance and optional wave-shaping at the cutoff frequency.
;; Based on periodic crossfading between 2 sample&holds. Causes intentional artefacts.
;; Example:
;; (play-now (kas-filter (saw 330) 500 0.3))
;; EndFunctionDoc
  
(define (kas-filter in cut res [drive 0])
	(cond
		((not (node? in)) (raise-type-error 'kas-filter "node" 0 in cut res drive))
		((not (or (number? cut) (node? cut))) (raise-type-error 'kas-filter "number-or-node" 1 in cut res drive))
		((not (or (number? res) (node? res))) (raise-type-error 'kas-filter "number-or-node" 2 in cut res drive))
		((not (or (number? drive) (node? drive))) (raise-type-error 'kas-filter "number-or-node" 3 in cut res drive))
		(else (operator KASF (list in cut res drive)))))

;; StartFunctionDoc-en
;; scrub sample-name-string input-node 
;; Returns: node-id-number
;; Description:
;; Sample Scrubber. Uses the input (normalised to 0 to 1) as a pointer to scan through the sample.
;; Example:
;; (play-now (scrub "sample.wav" (lfo-sine 1)))
;; EndFunctionDoc
  
(define (scrub filename freq)
	(cond
		((not (string? filename)) (raise-type-error 'scrub "string" 0 filename freq))
		((not (or (number? freq) (node? freq))) (raise-type-error 'scrub "number-or-node" 1 filename freq))
		(else (operator SCRUB (list (get-sample-id filename) freq)))))

;; StartFunctionDoc-en
;; play time node optional-pan
;; Returns: void
;; Description:
;; Plays a supplied node at the specified time.
;; Example:
;; (play (+ (time-now) 10) (mul (adsr 0 0.1 0 0) (sine 440)))
;; EndFunctionDoc

;; StartFunctionDoc-pt
;; play tempo nó
;; Retorna: void
;; Descrição:
;; Toca um nó fornecido no tempo especificado.
;; Exemplo:
;; (play (+ (time-now) 10) (mul (adsr 0 0.1 0 0) (sine 440)))
;; EndFunctionDoc

(define (play time node (pan 0) (f '()))
	(cond
		((not (number? time)) (raise-type-error 'play "number" 0 time node pan f))
		((not (node? node)) (raise-type-error 'play "node" 1 time node pan f))
		((not (number? pan)) (raise-type-error 'play "number" 2 time node pan f))
		((not (or (null? f) (procedure? f))) (raise-type-error 'play "procedure-or-null" 3 time node pan f))
  		(else (let ((time (time->timestamp time)))
    			(osc-send "/play" "iiif" (list (vector-ref time 0)
                                   (vector-ref time 1)
                                   (node-id node) pan)))
  				(when (not (null? f))
    			(spawn-timed-task time f)))))


;; StartFunctionDoc-en
;; play-now node
;; Returns: void
;; Description:
;; Plays a supplied node as soon as possible
;; Example:
;; (play-now (mul (adsr 0 0.1 0 0) (sine 440)))
;; EndFunctionDoc

;; StartFunctionDoc-pt
;; play-now node
;; Retorna: void
;; Descrição:
;; Toca um nó fornecido tão logo quanto possível.
;; Exemplo:
;; (play-now (mul (adsr 0 0.1 0 0) (sine 440)))
;; EndFunctionDoc

(define (play-now node (pan 0))
	(cond
		((not (node? node)) (raise-type-error 'play-now "node" 1 play-now node pan))
		((not (number? pan)) (raise-type-error 'play-now "number" 2 play-now node pan))
  		(else (osc-send "/play" "iiif" (list 0 0 (node-id node) pan)))))

;------------------------------
; global controls

;; StartFunctionDoc-en
;; fluxa-debug true-or-false
;; Returns: void
;; Description:
;; Turns on or off fluxa debugging, the server will print information out to stdout
;; Example:
;; (fluxa-debug #t)
;; EndFunctionDoc

;; StartFunctionDoc-pt
;; fluxa-debug verdadeiro-ou-falso
;; Retorna: void
;; Descrição:
;; Desliga ou desliga debugging em fluxa, o servidor vai imprimir
;; informação em stdout.
;; Exemplo:
;; (fluxa-debug #t)
;; EndFunctionDoc

(define (fluxa-debug v)
	(if (not (boolean? v))
    	(raise-type-error 'fluxa-debug "boolean" v)
		(osc-send "/debug" "i" (list v))))

;; StartFunctionDoc-en
;; volume amount-number
;; Returns: void
;; Description:
;; Sets the global volume
;; Example:
;; (volume 2.5)
;; EndFunctionDoc

;; StartFunctionDoc-pt
;; volume número-quantidade
;; Retorna: void
;; Descrição:
;; Ajusta o volume global
;; Exemplo:
;; (volume 2.5)
;; EndFunctionDoc

(define (volume v)
	(if (not (number? v))
    	(raise-type-error 'volume "number" v)
		(osc-send "/globalvolume" "f" (list v))))

;; StartFunctionDoc-en
;; pan pan-number
;; Returns: void
;; Description:
;; Sets the global pan where -1 is left and 1 is right (probably)
;; Example:
;; (pan 0)
;; EndFunctionDoc

;; StartFunctionDoc-pt
;; pan número-pan
;; Retorna: void
;; Descrição:
;; Ajusta o pan global aonde -1 é esquerda e 1 é direita (provavelmente).
;; Exemplo:
;; (pan 0)
;; EndFunctionDoc

(define (pan v)
	(if (not (number? v))
    	(raise-type-error 'pan "number" v)
		(osc-send "/pan" "f" (list v))))

;; StartFunctionDoc-en
;; max-synths number
;; Returns: void
;; Description:
;; Sets the maximum amount of synth graphs fluxa will run at the same time. This is a processor usage safeguard,
;; when the count is exceeded the oldest synth graph will be stopped so it's nodes can be recycled.
;; The default count is 10.
;; Example:
;; (max-synths 10)
;; EndFunctionDoc

;; StartFunctionDoc-pt
;; max-synths número
;; Retorna: void
;; Descrição:
;; Ajusta a quantidade máxima de synth que fluxa vai rodar ao mesmo
;; tempo. Isto é um salvaguarda da usagem do processador, quando a
;; contagem é excedida o synth graph mais velho é parado de forma que
;; seus nós possam ser reciclados.
;; O padrão é 10.
;; Exemplo:
;; (max-synths 10)
;; EndFunctionDoc

(define (max-synths s)
	(if (not (exact-positive-integer? s))
    	(raise-type-error 'max-synths "exact-positive-integer" s)
		(osc-send "/maxsynths" "i" (list s))))

;; StartFunctionDoc-en
;; searchpath path-string
;; Returns: void
;; Description:
;; Add a searchpath to use when looking for samples
;; Example:
;; (searchpath "/path/to/my/samples/)
;; EndFunctionDoc

;; StartFunctionDoc-pt
;; searchpath string-path
;; Retorna: void
;; Descrição:
;; Adiciona um caminho de busca quando estiver procurando por
;; amostras.
;; Exemplo:
;; (searchpath "/path/to/my/samples/)
;; EndFunctionDoc

(define (searchpath path)
	(if (not (string? path))
    	(raise-type-error 'searchpath "string" path)
    	(begin
  			(unless (member path fluxa-searchpaths)
	  			(set! fluxa-searchpaths (cons path fluxa-searchpaths)))
  			(osc-send "/addsearchpath" "s" (list path)))))

;; StartFunctionDoc-en
;; eq bass-number middle-number high-number
;; Returns: void
;; Description:
;; Sets a simple global equaliser. This is more as a last resort when performing without a mixer.
;; Example:
;; (eq 2 1 0.5) ; bass boost
;; EndFunctionDoc

;; StartFunctionDoc-pt
;; eq número-baixo número-médio número-alto
;; Retorna: void
;; Descrição:
;; Ajusta um equalizador global. Isto é mais um último recurso quando
;; estiver performando sem um mixador.
;; Exemplo:
;; (eq 2 1 0.5) ; bass boost
;; EndFunctionDoc

(define (eq l m h)
	(cond
		((not (number? l)) (raise-type-error 'eq "number" 0 l m h))
		((not (number? m)) (raise-type-error 'eq "number" 1 l m h))
		((not (number? h)) (raise-type-error 'eq "number" 2 l m h))
  		(else (osc-send "/eq" "fff" (list l m h)))))

;; StartFunctionDoc-en
;; comp attack-number release-number threshold-number slope-number
;; Returns: void
;; Description:
;; A global compressor. Not sure if this works yet.
;; Example:
;; (comp 0.1 0.1 0.5 3)
;; EndFunctionDoc

;; StartFunctionDoc-pt
;; comp número-attack número-release número-threshold número-slope
;; Retorna: void
;; Descrição:
;; Um compressor global. Sem certeza se isso funciona já.
;; Exemplo:
;; (comp 0.1 0.1 0.5 3)
;; EndFunctionDoc

(define (comp a r t s)
	(cond
		((not (number? a)) (raise-type-error 'comp "number" 0 a r t s))
		((not (number? r)) (raise-type-error 'comp "number" 1 a r t s))
		((not (number? t)) (raise-type-error 'comp "number" 2 a r t s))
		((not (number? s)) (raise-type-error 'comp "number" 3 a r t s))
  		(else (osc-send "/comp" "ffff" (list a r t s)))))

;; StartFunctionDoc-en
;; note note-number
;; Returns: frequency-number
;; Description:
;; Returns the frequency for the supplied note. Fluxa uses just intonation by default.
;; Example:
;; (note 35)
;; EndFunctionDoc

;; StartFunctionDoc-pt
;; note número-nota
;; Retorna: número-frequência
;; Descrição:
;; Retorna a frequência para a nota fornecida. Fluxa usa apenas
;; entonação por padrão.
;; Exemplo:
;; (note 35)
;; EndFunctionDoc

(define (set-scale s) (set! flx-scale s)) 

(define flx-scale '(1 1 1 1 1 1 1 1 1 1 1))

(define (note n)
	(if (not (number? n))
    	(raise-type-error 'note "number" n)
  		(list-ref scale-lut (modulo (inter flx-scale (inexact->exact (round n))) (length scale-lut)))))

;; StartFunctionDoc-en
;; reset
;; Returns: void
;; Description:
;; Resets the fluxa server.
;; Example:
;; (reset)
;; EndFunctionDoc

;; StartFunctionDoc-pt
;; reset
;; Retorna: void
;; Descrição:
;; Reinicia o servidor fluxa.
;; Exemplo:
;; (reset)
;; EndFunctionDoc

(define (reset)
  (osc-send "/reset" "" '()))

;------------------------------
; sundry items

;; just intonation (erm I think...)
(define scale-lut (list 58.2705 61.7354 65.4064 69.2957 73.4162 77.7817
                        82.4069 87.3071 92.4986 97.9989 103.826 110 116.541 123.471 130.813 138.591
                        146.832 155.563 164.814 174.614 184.997 195.998 207.652 220 233.082 246.942
                        261.626 277.183 293.665 311.127 329.628 349.228 369.994 391.995 415.305 440
                        466.164 493.883 523.251 554.365 587.33 622.254 659.255 698.456 739.989 783.991
                        830.609 880 932.328 987.767 1046.5 1108.73 1174.66 1244.51 1318.51 1396.91
                        1479.98 1567.98 1661.22 1760 1864.66 1975.53 2093 2217.46 2349.32 2489.02
                        2637.02 2793.83 2959.96 3135.96 3322.44 3520 3729.31 3951.07 4186.01 4434.92
                        4698.64 4978.03 5274.04 5587.65 5919.91 6271.93 6644.88 7040 7458.62 7902.13
                        8372.02 8869.84 9397.27 9956.06 10548.1 11175.3 11839.8 12543.9 13289.8 14080
                        14917.2 15804.3 16744 17739.7 18794.5 19912.1 21096.2 22350.6 23679.6 25087.7
                        26579.5 28160 29834.5 31608.5 33488.1 35479.4 37589.1 39824.3 42192.3 44701.2
                        47359.3 50175.4 53159 56320))

; converts from UTC time to get a 64bit NTP timestamp
(define (time->timestamp time)
  ; january 1972 UTC -> january 1900 NTP era (overflow in 2036...)
  (let ((adjusted (+ time 2208988800L0)))
    ; floor the time for the seconds
    (let ((seconds (inexact->exact (floor adjusted))))
      ; get the remainder and scale to max unsigned int for the fraction of the second
      (let ((frac (inexact->exact (floor (* (- adjusted seconds) 4294967295)))))
        (vector seconds frac)))))

; ... and back the other way
(define (timestamp->time timestamp)
  (+ (- (vector-ref timestamp 0) 2208988800L0) (/ (vector-ref timestamp 1) 4294967295.0)))

;------------------------------
; sequencing forms

;; StartFunctionDoc-en
;; clock-map
;; Returns: void
;; Description:
;; A way of using lists as sequences. The lists can be of differing length, leading to polyrhythms.
;; Example:
;; (seq (lambda (time clock)
;;    (clock-map
;;      (lambda (nt cutoff)
;;        (play time (mul (adsr 0 0.1 0 0)
;;          (mooglp (saw (note nt)) cutoff 0.4))))
;;      clock
;;      (list 39 28 3)
;;      (list 0.1 0.1 0.4 0.9)) .15))
;; EndFunctionDoc

;; StartFunctionDoc-pt
;; clock-map
;; Retorna: void
;; Descrição:
;; Uma forma de usar listas como sequências. As listas podem ser de
;; tamanhos diferentes, resultando em muitos rítmos.
;; Exemplo:
;; (seq (lambda (time clock)
;;    (clock-map
;;      (lambda (nt cutoff)
;;        (play time (mul (adsr 0 0.1 0 0)
;;          (mooglp (saw (note nt)) cutoff 0.4))))
;;      clock
;;      (list 39 28 3)
;;      (list 0.1 0.1 0.4 0.9)) .15))
;; EndFunctionDoc

(define-syntax clock-map
  (syntax-rules ()
    ((_ proc clock data ...)
     (proc (list-ref data (modulo clock (length data))) ...))))

(define-syntax clock-split
  (syntax-rules ()
    ((_ clock div proc ...)
     (clock-split-imp clock div (list proc ...)))))

(define (clock-split-imp clock div proclist)
  ((list-ref proclist
             (modulo (quotient clock div)
                     (length proclist)))))

;; StartFunctionDoc-en
;; zmod clock-number count-number
;; Returns: true-or-false
;; Description:
;; Just shorthand for (zero? (modulo clock-number count-number)), as it can be used a lot.
;; Example:
;; (seq (lambda (time clock)
;;     (when (zmod clock 4) ; play the note every 4th beat
;;         (play time (mul (adsr 0 0.1 0 0) (sine 440)))) .15))
;; EndFunctionDoc

;; StartFunctionDoc-pt
;; zmod número-relógio número-contador
;; Retorna: verdadeiro-ou-falso
;; Descrição:
;; Apenas atalho para (zero? (modulo número-relógio número-contador)),
;; já que pode ser usado bastante.
;; Exemplo:
;; (seq (lambda (time clock)
;;     (when (zmod clock 4) ; play the note every 4th beat
;;         (play time (mul (adsr 0 0.1 0 0) (sine 440)))) .15))
;; EndFunctionDoc

(define (zmod clock n)
	(cond
		((not (integer? clock)) (raise-type-error 'zmod "integer" 0 clock n))
		((not (integer? n)) (raise-type-error 'zmod "integer" 1 clock n))
		(else (zero? (modulo clock n)))))

;; StartFunctionDoc-en
;; seq proc
;; Returns: void
;; Description:
;; Sets the global fluxa sequence procedure, which will be called automatically
;; in order to create new events. seq can be repeatedly called to update the procedure
;; as in livecoding.
;; Example:
;; (seq (lambda (time clock)
;;     (when (zmod clock 4) ; play the note every 4th beat
;;         (play time (mul (adsr 0 0.1 0 0) (sine 440)))) .15))
;; EndFunctionDoc

;; StartFunctionDoc-pt
;; seq proc
;; Retorna: void
;; Descrição:
;; Ajusta o procedure da sequencia global de fluxa, que vai ser
;; chamado automaticamente para criar eventos. Seq pod ser chamado
;; repetitivamente para atualizar o procedure como em livecoding.
;; Exemplo:
;; (seq (lambda (time clock)
;;     (when (zmod clock 4) ; play the note every 4th beat
;;         (play time (mul (adsr 0 0.1 0 0) (sine 440)))) .15))
;; EndFunctionDoc

(define (seq p)
	(cond
		((not (procedure? p)) (raise-type-error 'seq "procedure" p))
		((not (eq? (procedure-arity p) 2)) (error "seq function should take 2 arguments but takes" (procedure-arity p)))
  		(else (set! proc p))))

(define proc
  (lambda (time clock)
    0))

;---------------------------------------
; fluxus implementation

(define logical-time (time-now))
(define clock 0)
(define next-load-queue (time-now))
(define tempo 0.1)
(define sync-tempo 0.5)
(define sync-clock 0)
(define bpb 4)
(define on-sync #f)

(define (set-on-sync s)
  (set! on-sync s))

(define (set-global-offset s)
  (set! sync-offset s))

(define (set-bpm-mult s)
  (set! bpm-mult s))

; figures out the offset to the nearest tick
(define (calc-offset timenow synctime tick)
  (let ((p (/ (- synctime timenow) tick)))
    (let ((f (- p (floor p))))
      (if (< f 0.5)
          (* f tick)
          (- (* (- 1 f) tick))))))

(define (fluxa-error-handler n)
  (printf "fluxa error:~a~n" n))

(define (go-flux)
  ; check for sync messages
  (cond ((osc-msg "/sync")
         (set! sync-tempo (* (/ 1 (* (osc 3) bpm-mult)) 60))
         (set! bpb (osc 2))
         (let* ((sync-time (+ sync-offset (timestamp->time (vector (osc 0) (osc 1)))))
                (offset (calc-offset logical-time sync-time sync-tempo)))
           (printf "time offset: ~a~n" offset)
           (set! logical-time (+ logical-time offset))
           (set! sync-clock 0)
       (when on-sync (on-sync)))))

  (cond ((> (- (time-now) logical-time) 3)
         (set! logical-time (time-now))))

  ; time for an update?
  (cond ((> (time-now) logical-time)
       ; todo: fall back on last thunk if there is an error
     (with-handlers ([(lambda (x) #t) fluxa-error-handler])
     	(let ((temp (proc (+ logical-time (* bpb tempo)) clock)))
     		(cond 
     			((not (number? temp)) (error "seq function should return a number!"))
     			((negative? temp) (error "seq function should not return a negative number!"))
           		(else (set! tempo temp)))))
         (set! logical-time (+ logical-time tempo))
         (set! clock (+ clock 1))
         (set! sync-clock (+ sync-clock 1))))

  ; send a loadqueue request every 5 seconds
  (cond ((> (time-now) next-load-queue)
         (osc-send "/loadqueue" "" '())
         (set! next-load-queue (+ next-load-queue 5)))))

(fluxa-init)

;---------------------------------------
; drscheme implementation

#;(define (go)
    (define (loop tempo time clock)
      (sleep 0.01)
      (cond
        ((> (time-now) time)
         (proc (+ time time-offset) clock)
         (set! time (+ time tempo))
         (set! clock (+ clock 1))))
      (loop tempo time clock))

    (display "going...")(newline)
    (osc-send "/setclock" "" '())
    (loop tempo (time-now) 0))

;(define thr (thread go))

;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
; experimental things

(define (inter l c)
  (define (_ l c cc a)
    (cond ((eq? c cc) a)
          (else
           (_ l c (+ cc 1) (+ a (pick l cc))))))
  (_ l c 0 0))

(define (pick l c)
  (list-ref l (modulo c (length l))))
