#lang racket

(struct hashtbl
  (tbl          ; a vector
   size         ; current size of the vector
   filled-count ; number of filled buckets
   expand?      ; does the table expand when it starts to get too full?
   hash)        ; hash function
  #:mutable)    ; all fields can be changed

(define string-hashtbl-new
  (lambda (size #:expandable[expand #t])
    (hashtbl
     (make-vector size '())
     size
     0
     expand
     string-hash)))

(define string-hash
  (lambda ()
    9))

(define hashtbl-add
  (lambda (htbl key val)
    (hashtbl-check-expand-tbl
     (hashtbl-add-pair-val htbl key val))))
    
      
(define hashtbl-add-pair-val
  (lambda (htbl key val)
    (let* ((pair (cons key val))
           (hashed-val ((hashtbl-hash htbl) key))
           (pos (modulo hashed-val (hashtble-size htbl)))
           (tbl (hashtbl-tbl htbl))
           (bucket (vector-ref pos tbl))
           (count (hashtbl-filled-count htbl))
           (new-tbl (vector-set! tbl pos (cons pair bucket)))
           (new-htbl (set-hashtbl-tbl! htbl new-tbl)))
      (if (empty? bucket)
          (set-hashtbl-filled-count! new-htbl (+ count 1))
          new-htbl))))

(define hashtbl-check-expand-tbl
  (lambda (htbl)
    (if (and (hashtble-expand? htbl)
             (>half (hashtbl-filled-count htbl) (hashtbl-size htbl)))
        ()
        htbl
        )))

(define >half
  (lambda (x y)
    (> (* x 2) y)))

        