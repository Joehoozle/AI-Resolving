
; Make sure to deal with only one being able to resolve 

(define (negate a)
    (cond
        ((list? a) (car (cdr a)))
        (#t (list 'NOT a))
    )
)

(define (res-search pos neg list2)
    (cond
        ((null? list2) (list pos))
        ((equal? pos (car list2)) "same")
        ((equal? neg (car list2)) "negative")
        (#t (res-search pos neg (cdr list2)))
    )
)

(define (res-delete-helper alist chosen newlist)
    (cond
        ((null? alist) newlist)
        ((equal? chosen (car alist)) (res-delete-helper (cdr alist) chosen newlist))
        ;((list? (car alist)) (res-delete-helper (cdr alist) chosen (append newlist (car alist))))
        (#t (res-delete-helper (cdr alist) chosen (append newlist (list (car alist)))))                
    )
)

(define (res-delete alist chosen) (res-delete-helper alist chosen '()))


(define (res-helper list1 list2 sol chosen)
    (cond     
        ((null? list1) (append sol (res-delete list2 chosen)))
        (#t (let ((value (res-search (car list1) (negate (car list1)) list2))) 
            (cond 
                ((equal? value "negative") (res-helper (cdr list1) list2 sol (negate (car list1))))
                ((equal? value "same") 'cool)
                ;((equal? value "positive") (res-helper (cdr list1) list2 (append sol (car list1)) chosen (append (car list1) duplicates)))
                (#t (res-helper (cdr list1) list2 (append sol value) chosen))
            )
        )) 
    )    
)

(define (res list1 list2) (res-helper list1 list2 '() 0))
