#lang typed/racket

; Searches for a given key in an association list and returns a pair containing
; the first associated value found and an association list with the found key-value pair removed.
; If the key is not found, the value is `#f` and the list remains unchanged.
(: pluck (All (a b) (-> a
                        (Listof (Pairof a b))
                        (Pairof (Option b) (Listof (Pairof a b))))))
(define (pluck key assoc-list)
  (match assoc-list
    ('() '(#f . ()))
    (`((,k . ,v) . ,rest)
     (if (equal? key k) `(,v . ,rest)
         (match-let ((`(,result . ,new-rest) (pluck key rest)))
           `(,result . ((,k . ,v) . ,new-rest)))))))


(define lst '((1 . "one") (2 . "two") (3 . "three") (4 . "four")))

(displayln (pluck 2 lst))  ;; Output: (some "two" '((1 . "one") (3 . "three") (4 . "four")))
(displayln (pluck 5 lst))  ;; Output: #f
