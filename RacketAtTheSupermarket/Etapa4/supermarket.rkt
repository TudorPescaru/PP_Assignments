#lang racket
(require racket/match)
(require "queue.rkt")

(provide (all-defined-out))

(define ITEMS 5)

(define-struct counter (index tt et queue closed?) #:transparent)

; TODO
; Aveți libertatea să vă structurați programul cum doriți (dar cu restricțiile
; de mai jos), astfel încât funcția serve să funcționeze conform specificației.
; 
; Restricții (impuse de checker):
; - trebuie să existe în continuare funcția (empty-counter index)
; - cozile de la case trebuie implementate folosind noul TDA queue

(define (empty-counter index)
  (make-counter index 0 0 empty-queue 0))

(define (update f counters index)
  (update-helper f counters index '()))

(define (update-helper f counters index updated-counters)
  (cond
    [(null? counters) (reverse updated-counters)]
    [(equal? (counter-index (car counters)) index) (update-helper f (cdr counters) index (cons (f (car counters)) updated-counters))]
    [else (update-helper f (cdr counters) index (cons (car counters) updated-counters))]))

(define tt+
  (λ (minutes)
    (λ (C) (match C
             [(counter _ tt _ _ _)
              (struct-copy counter C [tt (+ tt minutes)])]))))

(define et+
  (λ (minutes)
    (λ (C) (match C
             [(counter _ _ et _ _)
              (struct-copy counter C [et (+ et minutes)])]))))

(define (add-to-counter name items)
  (λ (C)
    (match C
      [(counter index tt et queue _) (cond
                                       [(queue-empty? queue) (car (update (et+ items) (update (tt+ items) (list (struct-copy counter C [queue (enqueue (cons name items) queue)])) index) index))]
                                       [else (car (update (tt+ items) (list (struct-copy counter C [queue (enqueue (cons name items) queue)])) index))])])))

(define min
  (λ (f)
    (λ (L)
      (cond
        [(equal? (length L) 1) (car L)]
        [else (f (car L) ((min f) (cdr L)))]))))

(define (tt-compare counter1 counter2)
  (cond
    [(equal? (counter-tt counter1) (counter-tt counter2)) (if (< (counter-index counter1) (counter-index counter2)) counter1 counter2)]
    [else (if (< (counter-tt counter1) (counter-tt counter2)) counter1 counter2)]))

(define (et-compare counter1 counter2)
  (cond
    [(equal? (counter-et counter1) (counter-et counter2)) (if (< (counter-index counter1) (counter-index counter2)) counter1 counter2)]
    [else (if (< (counter-et counter1) (counter-et counter2)) counter1 counter2)]))

(define min-tt
  (λ (counters)
    (let ([open-counters (filter (λ (C) (zero? (counter-closed? C))) counters)])
      (cond
        [(null? open-counters) null]
        [else (let ([min-counter ((min tt-compare) open-counters)])
                (cons (counter-index min-counter)
                      (counter-tt min-counter)))]))))

(define min-et
  (λ (counters)
    (let ([min-counter ((min et-compare) counters)])
      (cons (counter-index min-counter)
            (counter-et min-counter)))))

(define (remove-first-from-counter C)
  (match C
    [(counter _ _ _ queue _)
     (struct-copy counter C
                  [tt (sum-queue (dequeue queue) 0)]
                  [et (cond
                        [(queue-empty? (dequeue queue)) 0]
                        [else (cdr (top (dequeue queue)))])]
                  [queue (dequeue queue)])]))

(define (sum-queue q acc)
  (cond
    [(queue-empty? q) acc]
    [else (sum-queue (dequeue q) (+ acc (cdr (top q))))]))

(define (pass-time-through-counter minutes)
  (λ (C)
    (match C
      [(counter _ tt et queue _)
       (cond
         [(queue-empty? queue) (struct-copy counter C [tt (max (- tt minutes) 0)] [et (max (- et minutes) 0)])]
         [else (struct-copy counter C [tt (- tt minutes)] [et (- et minutes)])])])))
  
  
; TODO
; Implementați funcția care simulează fluxul clienților pe la case.
; ATENȚIE: Față de etapa 3, apare un nou tip de cerere, așadar
; requests conține 5 tipuri de cereri (cele moștenite din etapa 3 plus una nouă):
;   - (<name> <n-items>) - persoana <name> trebuie așezată la coadă la o casă              (ca înainte)
;   - (delay <index> <minutes>) - casa <index> este întârziată cu <minutes> minute         (ca înainte)
;   - (ensure <average>) - cât timp tt-ul mediu al caselor este mai mare decât
;                          <average>, se adaugă case fără restricții (case slow)           (ca înainte)
;   - <x> - trec <x> minute de la ultima cerere, iar starea caselor se actualizează
;           corespunzător (cu efect asupra câmpurilor tt, et, queue)                       (ca înainte)
;   - (close <index>) - casa index este închisă                                            (   NOU!   )
; Sistemul trebuie să proceseze cele 5 tipuri de cereri în ordine, astfel:
; - persoanele vor fi distribuite la casele DESCHISE cu tt minim; nu se va întâmpla
;   niciodată ca o persoană să nu poată fi distribuită la nicio casă                       (mică modificare)
; - când o casă suferă o întârziere, tt-ul și et-ul ei cresc (chiar dacă nu are clienți);
;   nu aplicați vreun tratament special caselor închise                                    (ca înainte)
; - tt-ul mediu (ttmed) se calculează pentru toate casele DESCHISE, 
;   iar la nevoie veți adăuga case slow una câte una, până când ttmed <= <average>         (mică modificare)
; - când timpul prin sistem avansează cu <x> minute, tt-ul, et-ul și queue-ul tuturor 
;   caselor se actualizează pentru a reflecta trecerea timpului; dacă unul sau mai mulți 
;   clienți termină de stat la coadă, ieșirile lor sunt contorizate în ordine cronologică. (ca înainte)
; - când o casă se închide, ea nu mai primește clienți noi; clienții care erau deja acolo
;   avansează normal, până la ieșirea din supermarket                                    
; Rezultatul funcției serve va fi o pereche cu punct între:
; - lista sortată cronologic a clienților care au părăsit supermarketul:
;   - fiecare element din listă va avea forma (index_casă . nume)
;   - dacă mai mulți clienți ies simultan, sortați-i crescător după indexul casei
; - lista cozilor (de la case DESCHISE sau ÎNCHISE) care încă au clienți:
;   - fiecare element va avea forma (index_casă . coadă) (coada este de tip queue)
;   - lista este sortată după indexul casei
(define (serve requests fast-counters slow-counters)
  (serve-helper requests fast-counters slow-counters '()))

(define (serve-helper requests fast-counters slow-counters clients)
  
  (define (add-to-best-counter name n-items)
    (cond
      [(<= n-items ITEMS) (cond
                            [(and (not (null? (min-tt fast-counters)))
                                  (<= (cdr (min-tt fast-counters)) (cdr (min-tt slow-counters))))
                             (cons (update (add-to-counter name n-items) fast-counters (car (min-tt fast-counters))) slow-counters)]
                            [else (cons fast-counters (update (add-to-counter name n-items) slow-counters (car (min-tt slow-counters))))])]
      [else (cons fast-counters (update (add-to-counter name n-items) slow-counters (car (min-tt slow-counters))))]))

  (define (apply-delay index minutes)
    (cons (update (tt+ minutes) (update (et+ minutes) fast-counters index) index) (update (tt+ minutes) (update (et+ minutes) slow-counters index) index)))

  (define (calculate-ttmed counters)
    (let ([open-counters (filter (λ (C) (zero? (counter-closed? C))) counters)])
      (/ (foldr (λ (C acc) (+ acc (counter-tt C))) 0 open-counters) (length open-counters))))

  (define (add-slow slow average)
    (cond
      [(<= (calculate-ttmed (append fast-counters slow)) average) slow]
      [else (add-slow (append slow (list (empty-counter (+ (counter-index (car (reverse slow))) 1)))) average)]))

  (define (counters-to-remove-from counters indexes)
    (cond
      [(null? counters) (reverse indexes)]
      [(match (car counters)
         [(counter index tt et queue _) (and (zero? et) (not (queue-empty? queue)))])
       (counters-to-remove-from (cdr counters) (cons (counter-index (car counters)) indexes))]
      [else (counters-to-remove-from (cdr counters) indexes)]))

  (define (customer-to-remove counters index)
    (cond
      [(equal? (counter-index (car counters)) index) (cons index (car (top (counter-queue (car counters)))))]
      [else (customer-to-remove (cdr counters) index)]))

  (define (remove-from-counters fast slow cl indexes)
    (cond
      [(null? indexes) (list fast slow cl)]
      [else (remove-from-counters (update remove-first-from-counter fast (car indexes))
                                  (update remove-first-from-counter slow (car indexes))
                                  (cons (customer-to-remove (append fast slow) (car indexes)) cl)
                                  (cdr indexes))]))

  (define (pass-time x fast slow cl)
    (let ([new-fast (map (pass-time-through-counter x) fast)] [new-slow (map (pass-time-through-counter x) slow)])
      (remove-from-counters new-fast new-slow cl (counters-to-remove-from (append new-fast new-slow) '()))))

  (define (counters-with-customers L)
    (filter (λ (C) (not (queue-empty? (counter-queue C)))) L))

  (define (min-et-or-0 counters)
    (cond
      [(null? counters) 0]
      [else (cdr (min-et counters))]))
  
  (define (pass-total-time x fast slow cl)
    (cond
      [(zero? x) (list fast slow cl)]
      [(zero? (min-et-or-0 (counters-with-customers (append fast slow)))) (pass-time x fast slow cl)]
      [(< x (min-et-or-0 (counters-with-customers (append fast slow)))) (let ([res (pass-time x fast slow cl)])
                                                                          (pass-total-time (- x x) (car res) (cadr res) (caddr res)))]
      [else (let* ([new-x (min-et-or-0 (counters-with-customers (append fast slow)))] [res (pass-time new-x fast slow cl)])
              (pass-total-time (- x new-x) (car res) (cadr res) (caddr res)))]))

  (define close-counter
    (λ (C)
      (match C
        [(counter _ _ _ _ closed?)
         (struct-copy counter C [closed? 1])])))

  (define (convert-to-queues counters queues)
    (cond
      [(null? counters) (reverse queues)]
      [(not (queue-empty? (counter-queue (car counters)))) (convert-to-queues (cdr counters) (cons (cons (counter-index (car counters)) (counter-queue (car counters))) queues))]
      [else (convert-to-queues (cdr counters) queues)]))
    
  (if (null? requests)
      (cons (reverse clients) (convert-to-queues (append fast-counters slow-counters) '()))
      (match (car requests)
        [(list 'close index) (serve-helper (cdr requests) (update close-counter fast-counters index) (update close-counter slow-counters index) clients)]
        [(list 'ensure average) (serve-helper (cdr requests) fast-counters (add-slow slow-counters average) clients)]
        [(list name n-items) (let ([res (add-to-best-counter name n-items)]) (serve-helper (cdr requests) (car res) (cdr res) clients))]
        [(list 'delay index minutes) (let ([res (apply-delay index minutes)]) (serve-helper (cdr requests) (car res) (cdr res) clients))]
        [x (let ([res (pass-total-time x fast-counters slow-counters clients)]) (serve-helper (cdr requests) (car res) (cadr res) (caddr res)))])))
        
