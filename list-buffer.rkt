#lang racket

; An implementation of the Buffer ADT using lists. A linked list is
; in many ways a poor choice for a buffer, because the list works like a stack,
; while a buffer works like a queue. In this implementation adding new vals
; takes constant time, while the time required to get the first val in the
; buffer increases with the number of vals in the buffer.


; Generate a buffer type with a constructor and accessors for each field.
; The fields are:
;  *lst - a list for storing buffered vals
;  *size - current number of vals in buffer
;  *max - max size of buffer
; buffer : List(schemeVal) * int * int -> buffer
; buffer-lst : buffer -> List(schemeVal)
; buffer-size : buffer -> int
; buffer-max : buffer -> int
(struct buffer (lst size max))

; Make a new empty buffer with a set maximum size.
; buffer-new : int -> buffer
(define buffer-new
  (lambda (max)
    (buffer '() 0 max)))

; Chack if the buffer contains values. If `buffer-empty?` returns `#t`, then
; `buffer-get` will call `report-buffer-empty-error`.
; buffer-empty? : buffer -> bool
(define buffer-empty?
  (lambda (buf)
    (zero? (buffer-size buf))))

; Check if the buffer is at maximum capacity. If `buffer-full?` returns `#t`,
; then `buffer-add` will call `report-buffer-full-error`.
; buffer-full? : buffer -> bool
(define buffer-full?
  (lambda (buf)
    (= (buffer-size buf) (buffer-max buf))))

; Add a new value to the end of a buffer. Calls `report-buffer-full-error`
; if there is no more space in the buffer.
; In practice the val is cons-ed to the front of the list.
; buffer-add : schemeVal * buffer -> buffer | error
(define buffer-add
  (lambda (x buf)
    (if (buffer-full? buf)
        (report-buffer-full-error)
        (buffer
         (cons x (buffer-lst buf))
         (+ (buffer-size buf) 1)
         (buffer-max buf)))))

; Add a list of values to the end of a buffer. The values are added to the
; end of the buffer in left to right order. If the list contains more vals
; than the buffer can hold, then the operation is aborted and
; `report-buffer-full-error` is called.
; buffer-add-lst : List(schemeVal) * buffer -> buffer | error
(define buffer-add-lst
  (lambda (lst buf)
    (if (> (+ (length lst) (buffer-size buf)) (buffer-max buf))
        (report-buffer-full-error)
        (buffer
         (append (reverse lst) (buffer-lst buf))
         (+ (buffer-size buf) (length lst))
         (buffer-max buf)))))

; Remove the first value in a buffer, returning the first value and a buffer
; containing the remaining values. If the buffer is empty then
; `report-buffer-empty-error` is called.
; In practice the buffer's internal list is reversed and the first value
; cons-ed on to the empty list is pulled off. Then the remaining list
; elements are reversed back as the new buffer list.
; buffer-get : buffer -> (schemeVal * buffer) | error
(define buffer-get
  (lambda (buf)
    (if (buffer-empty? buf)
        (report-buffer-empty-error)
        (let ((rev-buf (reverse (buffer-lst buf))))
          (cons (car rev-buf) (buffer
                               (reverse (cdr rev-buf))
                               (- (buffer-size buf) 1)
                               (buffer-max buf)))))))

; Look at the next value in the buffer without removing it from the buffer.
; If the buffer is empty then `report-buffer-empty-error` is called.
; buffer-peek : buffer -> schemeVal | error
(define buffer-peek
  (lambda (buf)
    (if (buffer-empty? buf)
        (report-buffer-empty-error)
        (car (reverse (buffer-lst buf))))))

; Discard the first val in the buffer and make a new buffer with the remaining
; buffer elements. If the buffer is empty then `report-buffer-empty-error` is
; called.
; buffer-skip : buffer -> buffer | error
(define buffer-skip
  (lambda (buf)   
    (cdr (buffer-get buf))))

; Report that the buffer is full, so no more elements can be added.
; report-buffer-full-error : () -> error
(define report-buffer-full-error
  (lambda ()
    (error "The buffer is full, can not add elements")))

; Report that the buffer is empty, so there are no elements to remove.
; report-buffer-empty-error : () -> error
(define report-buffer-empty-error
  (lambda ()
    (error "The buffer is empty, no elements to retrieve")))

; Implement the buffer ADT
(provide buffer-new
         buffer-empty?
         buffer-full?
         buffer-add
         buffer-add-lst
         buffer-get
         buffer-peek
         buffer-skip)
