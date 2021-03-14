#lang racket
(require racket/match)

(provide (all-defined-out))

(define ITEMS 5)

;; C1, C2, C3, C4 sunt case într-un supermarket.
;; C1 acceptă doar clienți care au cumparat maxim ITEMS produse (ITEMS este definit mai sus).
;; C2 - C4 nu au restricții.
;; Considerăm că procesarea fiecărui produs la casă durează un minut.
;; Casele pot suferi întarzieri (delay).
;; La un moment dat, la fiecare casă există 0 sau mai mulți clienți care stau la coadă.
;; Timpul total (tt) al unei case reprezintă timpul de procesare al celor aflați la coadă,
;; adică numărul de produse cumpărate de ei + întârzierile suferite de casa respectivă (dacă există).
;; Ex:
;; la C3 sunt Ana cu 3 produse și Geo cu 7 produse,
;; și C3 nu are întârzieri => tt pentru C3 este 10.


; Definim o structură care descrie o casă prin:
; - index (de la 1 la 4)
; - tt (timpul total descris mai sus)
; - queue (coada cu persoanele care așteaptă)
(define-struct counter (index tt queue) #:transparent)


; TODO
; Implementați o functie care intoarce o structură counter goală.
; tt este 0 si coada este vidă.
; Obs: la definirea structurii counter se creează automat o funcție make-counter pentru a construi date de acest tip
(define (empty-counter index)
  (make-counter index 0 '()))


; TODO
; Implementați o funcție care crește tt-ul unei case cu un număr dat de minute.
(define (tt+ C minutes)
  (match C
    [(counter index tt queue)
     (struct-copy counter C [tt (+ tt minutes)])]))


; TODO
; Implementați o funcție care primește o listă de case și intoarce o pereche dintre:
; - indexul casei (din listă) care are cel mai mic tt
; - tt-ul acesteia
; Obs: când mai multe case au același tt, este preferată casa cu indexul cel mai mic
(define (min-tt counters)
  (cons (counter-index (min-tt-helper (cdr counters) (car counters)))
        (counter-tt (min-tt-helper (cdr counters) (car counters)))))

(define (min-tt-helper counters res)
  (cond
    [(null? counters) res]
    [(tt-compare (car counters) res) (min-tt-helper (cdr counters) (car counters))]
    [else (min-tt-helper (cdr counters) res)]))

(define (tt-compare counter1 counter2)
  (< (counter-tt counter1) (counter-tt counter2)))

; TODO
; Implementați o funcție care adaugă o persoană la o casă.
; C = casa, name = numele persoanei, n-items = numărul de produse cumpărate
; Veți întoarce o nouă structură obținută prin așezarea perechii (name . n-items)
; la sfârșitul cozii de așteptare.
(define (add-to-counter C name n-items)
  (match C
    [(counter index tt queue)
     (tt+ (struct-copy counter C [queue (reverse (cons (cons name n-items) (reverse queue)))]) n-items)]))


; TODO
; Implementați funcția care simulează fluxul clienților pe la case.
; requests = listă de cereri care pot fi de 2 tipuri:
; - (<name> <n-items>) - persoana <name> trebuie așezată la coadă la o casă
; - (delay <index> <minutes>) - casa <index> este întârziată cu <minutes> minute
; C1, C2, C3, C4 = structuri corespunzătoare celor 4 case
; Sistemul trebuie să proceseze aceste cereri în ordine, astfel:
; - persoanele vor fi distribuite la casele cu tt minim (dintre casele la care au voie)
; - când o casă suferă o întârziere, tt-ul ei crește
(define (serve requests C1 C2 C3 C4)

  ; puteți să vă definiți aici funcții ajutătoare (define în define)
  ; - avantaj: aveți acces la variabilele requests, C1, C2, C3, C4 fără a le retrimite ca parametri
  ; puteți de asemenea să vă definiți funcțiile ajutătoare în exteriorul lui "serve"
  ; - avantaj: puteți să vă testați fiecare funcție imediat ce ați implementat-o

  (define (add-to-best-counter name n-items)
    (cond
      [(<= n-items ITEMS) (add-to-counter (min-tt-helper (list C1 C2 C3 C4) C1) name n-items)]
      [else (add-to-counter (min-tt-helper (list C2 C3 C4) C2) name n-items)]))

  (if (null? requests)
      (list C1 C2 C3 C4)
      (match (car requests)
        [(list 'delay index minutes) (cond
                                        [(equal? index 1) (serve (cdr requests) (tt+ C1 minutes) C2 C3 C4)]
                                        [(equal? index 2) (serve (cdr requests) C1 (tt+ C2 minutes) C3 C4)]
                                        [(equal? index 3) (serve (cdr requests) C1 C2 (tt+ C3 minutes) C4)]
                                        [(equal? index 4) (serve (cdr requests) C1 C2 C3 (tt+ C4 minutes))]
                                        [else (serve (cdr requests) C1 C2 C3 C4)])]
        [(list name n-items) (match (add-to-best-counter name n-items)
                               [(counter index _ _) (cond
                                                      [(equal? index 1) (serve (cdr requests) (add-to-best-counter name n-items) C2 C3 C4)]
                                                      [(equal? index 2) (serve (cdr requests) C1 (add-to-best-counter name n-items) C3 C4)]
                                                      [(equal? index 3) (serve (cdr requests) C1 C2 (add-to-best-counter name n-items) C4)]
                                                      [(equal? index 4) (serve (cdr requests) C1 C2 C3 (add-to-best-counter name n-items))])])])))
