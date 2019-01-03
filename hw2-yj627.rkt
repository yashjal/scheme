;;Yash Jalan
;;HW2

(define (foldr func seed lis)
  (cond ((null? lis) seed)
        (else (func (car lis) (foldr func seed (cdr lis)))))) 

#|
This function, paramreverse assumes that the given arg func
takes at least two arguments, i.e. list has two elements.
If the list has only one element then this function returns
that element without applying the given func to it.
|#

(define (paramreverse func lis)
  (cond ((= (length lis) 1) (car lis))
        (else (func (paramreverse func (cdr lis)) (car lis)))))

#|
The highest function is implemented using two helper functions,
maxvalue and remove. maxvalue returns the highest element of a
list and remove returns a list with a specific element removed
from it.
|#

(define (maxvalue lis)
  (if (= (length lis) 1) (car lis)
      (if (>= (car lis) (maxvalue (cdr lis))) (car lis) (maxvalue (cdr lis)))))

(define (remove lis ele)
  (cond ((null? lis) '())
        ((= (car lis) ele) (cdr lis))
        (else (cons (car lis) (remove (cdr lis) ele)))))

(define (highest lis k)
  (cond ((= k 0) '())
        (else (cons (maxvalue lis) (highest (remove lis (maxvalue lis)) (- k 1))))))


(define (mapfun FL L)
  (cond ((null? FL) '())
        ((null? L) '())
        (else (cons ((car FL) (car L)) (mapfun (cdr FL) (cdr L))))))


(define (filter pred L)
  (if (null? L) '()
      (if (pred (car L)) (cons (car L) (filter pred (cdr L))) (filter pred (cdr L))))) 