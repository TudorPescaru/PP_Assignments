#lang racket
(require racket/match)
(require "queue.rkt")

(provide (all-defined-out))

(define ITEMS 5)

;; ATENȚIE: Pentru această etapă a temei este necesar să implementați
;;          întâi TDA-ul queue în fișierul queue.rkt.
;; Reveniți la sarcinile din acest fișier după ce ați implementat tipul 
;; queue și ați verificat implementarea folosind checker-ul.


; Structura counter nu se modifică.
; Ceea ce se modifică este implementarea câmpului queue:
; - în loc de listă, acesta va fi o coadă (o structură de tip queue)
; - acest lucru nu este vizibil în definiția structurii counter,
;   ci în implementarea operațiilor acestui tip de date (tipul counter)
(define-struct counter (index tt et queue) #:transparent)


; TODO
; Actualizați funcțiile de mai jos astfel încât ele să folosească
; o structură de tip queue pentru reprezentarea cozii de persoane.
; Elementele cozii continuă să fie perechi (nume . nr_produse).
; Este esențial să respectați "bariera de abstractizare", adică să
; operați cu coada doar folosind interfața acestui TDA:
; - empty-queue
; - queue-empty?
; - enqueue
; - dequeue
; - top
; Obs: Doar câteva funcții vor necesita actualizări.
(define (empty-counter index)           ; testată de checker
  (make-counter index 0 0 empty-queue))

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
             [(counter _ tt _ _)
              (struct-copy counter C [tt (+ tt minutes)])]))))

(define et+
  (λ (minutes)
    (λ (C) (match C
             [(counter _ _ et _)
              (struct-copy counter C [et (+ et minutes)])]))))

(define (add-to-counter name items)     ; testată de checker
  (λ (C)                                ; nu modificați felul în care funcția își primește argumentele
    (match C
      [(counter index tt et queue) (cond
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
    (cons (counter-index ((min tt-compare) counters))
          (counter-tt ((min tt-compare) counters)))))

(define min-et
  (λ (counters)
    (cons (counter-index ((min et-compare) counters))
          (counter-et ((min et-compare) counters)))))

(define (remove-first-from-counter C)   ; testată de checker
  (match C
    [(counter _ _ _ queue)
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


; TODO
; Implementați o funcție care calculează starea unei case după un număr dat de minute.
; Funcția presupune, fără să verifice, că în acest timp nu a ieșit nimeni din coadă, 
; deci va avea efect doar asupra câmpurilor tt și et.
; (cu alte cuvinte, este responsabilitatea utilizatorului să nu apeleze această funcție
; cu minutes > timpul până la ieșirea primului client din coadă)
; Atenție: casele fără clienți nu trebuie să ajungă la timpi negativi!
(define (pass-time-through-counter minutes)
  (λ (C)
    (match C
      [(counter _ tt et queue)
       (cond
         [(queue-empty? queue) (struct-copy counter C [tt (max (- tt minutes) 0)] [et (max (- et minutes) 0)])]
         [else (struct-copy counter C [tt (- tt minutes)] [et (- et minutes)])])])))
  

; TODO
; Implementați funcția care simulează fluxul clienților pe la case.
; ATENȚIE: Față de etapa 2, apar modificări în:
; - formatul listei de cereri (parametrul requests)
; - formatul rezultatului funcției (explicat mai jos)
; requests conține 4 tipuri de cereri (3 moștenite din etapa 2 plus una nouă):
;   - (<name> <n-items>) - persoana <name> trebuie așezată la coadă la o casă            (ca înainte)
;   - (delay <index> <minutes>) - casa <index> este întârziată cu <minutes> minute       (ca înainte)
;   - (ensure <average>) - cât timp tt-ul mediu al tuturor caselor este mai mare decât
;                          <average>, se adaugă case fără restricții (case slow)         (ca înainte)
;   - <x> - trec <x> minute de la ultima cerere, iar starea caselor se actualizează
;           corespunzător (cu efect asupra câmpurilor tt, et, queue)                     (   NOU!   )
; Obs: Au dispărut cererile de tip remove-first, pentru a lăsa loc unui mecanism mai 
; sofisticat de a scoate clienții din coadă (pe măsură ce trece timpul).
; Sistemul trebuie să proceseze cele 4 tipuri de cereri în ordine, astfel:
; - persoanele vor fi distribuite la casele cu tt minim (dintre casele la care au voie)  (ca înainte)
; - când o casă suferă o întârziere, tt-ul și et-ul ei cresc (chiar dacă nu are clienți) (ca înainte)
; - tt-ul mediu (ttmed) se calculează pentru toate casele (și cele fast, și cele slow), 
;   iar la nevoie veți adăuga case slow una câte una, până când ttmed <= <average>       (ca înainte)
; - când timpul prin sistem avansează cu <x> minute, tt-ul, et-ul și queue-ul tuturor 
;   caselor se actualizează pentru a reflecta trecerea timpului; dacă unul sau mai mulți 
;   clienți termină de stat la coadă, ieșirile lor sunt contorizate în ordine cronologică.
; Rezultatul funcției serve va fi o pereche cu punct între:
; - lista sortată cronologic a clienților care au părăsit supermarketul
;   - fiecare element din listă va avea forma (index_casă . nume)
;   - dacă mai mulți clienți ies simultan, sortați-i crescător după indexul casei
; - lista caselor în starea finală (ca rezultatul din etapele 1 și 2)
; Obs: Pentru a contoriza ieșirile din cozi, puteți să lucrați într-o funcție ajutătoare
; (cu un parametru în plus față de funcția serve), pe care serve doar o apelează.
(define (serve requests fast-counters slow-counters)
  (serve-helper requests fast-counters slow-counters '()))

(define (serve-helper requests fast-counters slow-counters clients)
  
  (define (add-to-best-counter name n-items)
    (cond
      [(<= n-items ITEMS) (if (<= (cdr (min-tt fast-counters)) (cdr (min-tt slow-counters)))
                              (cons (update (add-to-counter name n-items) fast-counters (car (min-tt fast-counters))) slow-counters)
                              (cons fast-counters (update (add-to-counter name n-items) slow-counters (car (min-tt slow-counters)))))]
      [else (cons fast-counters (update (add-to-counter name n-items) slow-counters (car (min-tt slow-counters))))]))

  (define (apply-delay index minutes)
    (cons (update (tt+ minutes) (update (et+ minutes) fast-counters index) index) (update (tt+ minutes) (update (et+ minutes) slow-counters index) index)))

  (define (calculate-ttmed counters)
    (/ (foldr (λ (C acc) (+ acc (counter-tt C))) 0 counters) (length counters)))

  (define (add-slow slow average)
    (cond
      [(<= (calculate-ttmed (append fast-counters slow)) average) slow]
      [else (add-slow (append slow (list (empty-counter (+ (counter-index (car (reverse slow))) 1)))) average)]))

  (define (counters-to-remove-from counters indexes)
    (cond
      [(null? counters) (reverse indexes)]
      [(match (car counters)
         [(counter index tt et queue) (and (zero? et) (not (queue-empty? queue)))])
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
    (remove-from-counters (map (pass-time-through-counter x) fast)
                          (map (pass-time-through-counter x) slow)
                          cl
                          (counters-to-remove-from (append (map (pass-time-through-counter x) fast) (map (pass-time-through-counter x) slow)) '())))

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
      [(< x (min-et-or-0 (counters-with-customers (append fast slow)))) (pass-total-time (- x x)
                                                                                         (car (pass-time x fast slow cl))
                                                                                         (cadr (pass-time x fast slow cl))
                                                                                         (caddr (pass-time x fast slow cl)))]
      [else (pass-total-time (- x (min-et-or-0 (counters-with-customers (append fast slow))))
                             (car (pass-time (min-et-or-0 (counters-with-customers (append fast slow))) fast slow cl))
                             (cadr (pass-time (min-et-or-0 (counters-with-customers (append fast slow))) fast slow cl))
                             (caddr (pass-time (min-et-or-0 (counters-with-customers (append fast slow))) fast slow cl)))]))
  
  (if (null? requests)
      (cons (reverse clients) (append fast-counters slow-counters))
      (match (car requests)
        [(list 'ensure average) (serve-helper (cdr requests) fast-counters (add-slow slow-counters average) clients)]
        [(list name n-items) (serve-helper (cdr requests) (car (add-to-best-counter name n-items)) (cdr (add-to-best-counter name n-items)) clients)]
        [(list 'delay index minutes) (serve-helper (cdr requests) (car (apply-delay index minutes)) (cdr (apply-delay index minutes)) clients)]
        [x (serve-helper (cdr requests)
                         (car (pass-total-time x fast-counters slow-counters clients))
                         (cadr (pass-total-time x fast-counters slow-counters clients))
                         (caddr (pass-total-time x fast-counters slow-counters clients)))])))
        
