#lang racket
(require racket/match)

(provide (all-defined-out))

(define ITEMS 5)

;; Actualizăm structura counter cu informația et:
;; Exit time (et) al unei case reprezintă timpul până la ieșirea primului client de la casa respectivă,
;; adică numărul de produse de procesat pentru acest client + întârzierile suferite de casă (dacă există).
;; Ex:
;; la C3 s-au așezat întâi Ana cu 3 produse și apoi Geo cu 7 produse,
;; și C3 a fost întârziată cu 5 minute => et pentru C3 este 3 + 5 = 8 (timpul până când va ieși Ana).


; Redefinim structura counter.
(define-struct counter (index tt et queue) #:transparent)


; TODO
; Actualizați implementarea empty-counter astfel încât să conțină și câmpul et.
(define (empty-counter index)
  (make-counter index 0 0 '()))


; TODO
; Implementați o funcție care aplică o transformare casei cu un anumit index.
; f = funcție unară care se aplică pe un argument de tip casă, counters = listă de case,
; index = indexul casei din counters căreia îi veți aplica transformarea f
; Veți întoarce lista actualizată de case.
; Dacă nu există în counters o casă cu acest index, veți întoarce lista nemodificată.
(define (update f counters index)
  (update-helper f counters index '()))

(define (update-helper f counters index updated-counters)
  (cond
    [(null? counters) (reverse updated-counters)]
    [(equal? (counter-index (car counters)) index) (update-helper f (cdr counters) index (cons (f (car counters)) updated-counters))]
    [else (update-helper f (cdr counters) index (cons (car counters) updated-counters))]))

; TODO
; Memento: tt+ crește tt-ul unei case cu un număr dat de minute.
; Actualizați implementarea tt+ pentru:
; - a ține cont de noua reprezentare a unei case
; - a permite ca operații de tip tt+ să fie pasate ca argument funcției update în cel mai facil mod
; Obs: Pentru compatibilitatea cu primul argument al lui update, trebuie să obținem ușor din tt+ 
; o funcție unară care se aplică pe un argument de tip casă (v. funcții curry).
; Am eliminat parametrii lui tt+ din define-ul de mai jos astfel încât voi să determinați
; cum este cel mai bine ca tt+ să își primească parametrii.
; Din acest motiv checker-ul nu testează separat această funcție, dar asistentul va verifica
; faptul că ați implementat-o conform cerințelor.
(define (tt+ minutes)
  (λ (C) (match C
           [(counter _ tt _ _)
            (struct-copy counter C [tt (+ tt minutes)])])))


; TODO
; Implementați o funcție care crește et-ul unei case cu un număr dat de minute.
; Păstrați formatul folosit pentru tt+.
; Checker-ul nu testează separat această funcție.
(define (et+ minutes)
  (λ (C) (match C
           [(counter _ _ et _)
            (struct-copy counter C [et (+ et minutes)])])))


; TODO
; Memento: add-to-counter adaugă o persoană (reprezentată prin nume și număr de produse) la o casă. 
; Actualizați implementarea add-to-counter pentru aceleași rațiuni pentru care am modificat tt+.
; Atenție la cum se modifică tt și et!
; Checker-ul nu testează separat această funcție.
(define (add-to-counter name n-items)
  (λ (C) (match C
           [(counter index tt et queue) (cond
                                          [(zero? (length queue)) (car (update (et+ n-items) (update (tt+ n-items) (list (struct-copy counter C [queue (reverse (cons (cons name n-items) (reverse queue)))])) index) index))]
                                          [else (car (update (tt+ n-items) (list (struct-copy counter C [queue (reverse (cons (cons name n-items) (reverse queue)))])) index))])])))


; TODO
; Întrucât vom folosi atât min-tt (implementat în etapa 1) cât și min-et (funcție nouă),
; definiți-vă o funcție mai abstractă din care să derive ușor atât min-tt cât și min-et.
; Prin analogie cu min-tt, definim min-et astfel:
; min-et = funcție care primește o listă de case și intoarce o pereche dintre:
; - indexul casei (din listă) care are cel mai mic et
; - et-ul acesteia
; (când mai multe case au același et, este preferată casa cu indexul cel mai mic)
(define (min f L)
  (cond
    [(equal? (length L) 1) (car L)]
    [else (f (car L) (min f (cdr L)))]))

(define (tt-compare counter1 counter2)
  (cond
    [(equal? (counter-tt counter1) (counter-tt counter2)) (if (< (counter-index counter1) (counter-index counter2)) counter1 counter2)]
    [else (if (< (counter-tt counter1) (counter-tt counter2)) counter1 counter2)]))

(define (et-compare counter1 counter2)
  (cond
    [(equal? (counter-et counter1) (counter-et counter2)) (if (< (counter-index counter1) (counter-index counter2)) counter1 counter2)]
    [else (if (< (counter-et counter1) (counter-et counter2)) counter1 counter2)]))

(define (min-tt counters)
  (cons (counter-index (min tt-compare counters))
        (counter-tt (min tt-compare counters)))) ; folosind funcția de mai sus

(define (min-et counters)
  (cons (counter-index (min et-compare counters))
        (counter-et (min et-compare counters)))) ; folosind funcția de mai sus


; TODO
; Implementați o funcție care scoate prima persoană din coada unei case.
; Funcția presupune, fără să verifice, că există minim o persoană la coada casei C.
; Veți întoarce o nouă structură obținută prin modificarea cozii de așteptare.
; Atenție la cum se modifică tt și et!
; Dacă o casă tocmai a fost părăsită de cineva, înseamnă că ea nu mai are întârzieri.
(define (remove-first-from-counter C)
  (struct-copy counter C
               [tt (foldl (λ (x acc) (+ acc (cdr x))) 0 (cdr (counter-queue C)))]
               [et (cond [(null? (cdr (counter-queue C))) 0] [else (cdr (car (cdr (counter-queue C))))])]
               [queue (cdr (counter-queue C))]))

; TODO
; Implementați funcția care simulează fluxul clienților pe la case.
; ATENȚIE: Față de etapa 1, funcția serve operează cu următoarele modificări:
; - nu mai avem doar 4 case, ci:
;   - fast-counters (o listă de case pentru maxim ITEMS produse)
;   - slow-counters (o listă de case fără restricții)
;   (Sugestie: folosiți funcția update pentru a obține comportamentul pe liste de case)
; - requests conține 4 tipuri de cereri (cele 2 din etapa anterioară plus 2 noi):
;   - (<name> <n-items>) - persoana <name> trebuie așezată la coadă la o casă
;   - (delay <index> <minutes>) - casa <index> este întârziată cu <minutes> minute
;   - (remove-first) - cea mai avansată persoană părăsește casa la care se află
;   - (ensure <average>) - cât timp tt-ul mediu al tuturor caselor este mai mare decât
;                          <average>, se adaugă case fără restricții (case slow)
; Sistemul trebuie să proceseze cele 4 tipuri de cereri în ordine, astfel:
; - persoanele vor fi distribuite la casele cu tt minim (dintre casele la care au voie)
;   (ca înainte, dar acum folosiți fast-counters și slow-counters)
; - când o casă suferă o întârziere, tt-ul și et-ul ei cresc (chiar dacă nu are clienți)
;   (puțin diferit față de etapa 1, și acum folosiți fast-counters și slow-counters)
; - persoana care părăsește supermarketul este persoana fruntașă la casa cu et-ul minim
;   (dintre casele care au clienți); dacă nicio casă nu are clienți, cererea e ignorată
; - tt-ul mediu (ttmed) se calculează pentru toate casele (și cele fast, și cele slow), 
;   iar la nevoie veți adăuga case slow una câte una, până când ttmed <= <average>
; Considerați că indecșii caselor încep de la 1 și sunt mereu în ordine.
; Ex:
; fast-counters conține casele 1-2, slow-counters conține casele 3-15
; => la nevoie veți adăuga întâi casa 16, apoi casa 17, etc.
; (puteți determina matematic de câte case noi este nevoie sau
;  puteți adăuga recursiv una câte una până când condiția este îndeplinită)
(define (serve requests fast-counters slow-counters)

  (define (add-to-best-counter name n-items)
    (cond
      [(<= n-items ITEMS) (if (<= (cdr (min-tt fast-counters)) (cdr (min-tt slow-counters)))
                              (cons (update (add-to-counter name n-items) fast-counters (car (min-tt fast-counters))) slow-counters)
                              (cons fast-counters (update (add-to-counter name n-items) slow-counters (car (min-tt slow-counters)))))]
      [else (cons fast-counters (update (add-to-counter name n-items) slow-counters (car (min-tt slow-counters))))]))

  (define (apply-delay index minutes)
    (cons (update (tt+ minutes) (update (et+ minutes) fast-counters index) index) (update (tt+ minutes) (update (et+ minutes) slow-counters index) index)))

  (define (counters-with-customers L)
    (filter (λ (C) (not (zero? (length (counter-queue C))))) L))

  (define (remove-first-customer fast slow)
    (cond
      [(and (null? fast) (null? slow)) (cons fast-counters slow-counters)]
      [(and (not (null? fast)) (not (null? slow))) (if (<= (cdr (min-et fast)) (cdr (min-et slow)))
                                                 (cons (update remove-first-from-counter fast-counters (car (min-et fast))) slow-counters)
                                                 (cons fast-counters (update remove-first-from-counter slow-counters (car (min-et slow)))))]
      [else (if (null? slow)
                (cons (update remove-first-from-counter fast-counters (car (min-et fast))) slow-counters)
                (cons fast-counters (update remove-first-from-counter slow-counters (car (min-et slow)))))]))

  (define (calculate-ttmed counters)
    (/ (foldr (λ (C acc) (+ acc (counter-tt C))) 0 counters) (length counters)))

  (define (add-slow slow average)
    (cond
      [(<= (calculate-ttmed (append fast-counters slow)) average) slow]
      [else (add-slow (append slow (list (empty-counter (+ (counter-index (car (reverse slow))) 1)))) average)]))
  
  (if (null? requests)
      (append fast-counters slow-counters)
      (match (car requests)
        [(list 'ensure average) (serve (cdr requests) fast-counters (add-slow slow-counters average))]
        [(list name n-items) (serve (cdr requests) (car (add-to-best-counter name n-items)) (cdr (add-to-best-counter name n-items)))]
        [(list 'delay index minutes) (serve (cdr requests) (car (apply-delay index minutes)) (cdr (apply-delay index minutes)))]
        [(list remove-first) (serve (cdr requests)
                                    (car (remove-first-customer (counters-with-customers fast-counters)
                                                                (counters-with-customers slow-counters)))
                                    (cdr (remove-first-customer (counters-with-customers fast-counters)
                                                                (counters-with-customers slow-counters))))])))

