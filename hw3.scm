

; Negates an element to check for opposite in other list
(define (negate a)
    (cond
        ((list? a) (car (cdr a)))
        (#t (list 'NOT a))
    )
)

; searches for the opposite (or the same element) in the other list to mark it as chosen
(define (res-search pos neg list2)
    (cond
        ((null? list2) (list pos))
        ((equal? pos (car list2)) "same")
        ((equal? neg (car list2)) "negative")
        (#t (res-search pos neg (cdr list2)))
    )
)

; goes through the second list to delete the opposite element of the second list belonging to the chosen pair
(define (res-delete-helper alist chosen newlist)
    (cond
        ((null? alist) newlist)
        ((equal? chosen (car alist)) (res-delete-helper (cdr alist) chosen newlist))
        ;((list? (car alist)) (res-delete-helper (cdr alist) chosen (append newlist (car alist))))
        (#t (res-delete-helper (cdr alist) chosen (append newlist (list (car alist)))))                
    )
)

; wrapper funtion for deleting chosen pair
(define (res-delete alist chosen) (res-delete-helper alist chosen '()))

; function to find if a list has more than one element
(define (multiple-elements alist) 
    (cond 
        ((null? (cdr alist)) #f)
        (#t #t)
    )
)

; main funciton for performing resolve
(define (res-helper list1 list2 sol chosen)
    (cond
        
        ; if there are no chosen pairs, the stamements cannot be resolved
        ((and (null? list1) (null? chosen)) #f)     
        
        ; if there are more than one chosen pairs, the statements cannot be resolved
        ((and (null? list1) (multiple-elements chosen)) #f)

        ; if there is a contradiction
        ((and (null? list1) (null? sol)) "CONTRADICTION")

        ; if one chosen pair has been found and the process is over
        ((null? list1) (append sol (res-delete list2 (car chosen))))

        ; normal case for checking for pairs
        (#t (let ((value (res-search (car list1) (negate (car list1)) list2))) 
            (cond 
                ((equal? value "negative") (res-helper (cdr list1) list2 sol (append (list (negate (car list1))) chosen)))
                ((equal? value "same") 'cool)
                ;((equal? value "positive") (res-helper (cdr list1) list2 (append sol (car list1)) chosen (append (car list1) duplicates)))
                (#t (res-helper (cdr list1) list2 (append sol value) chosen))
            )
        )) 
    )    
)

; wrapper function for resolve
(define (res list1 list2) (res-helper list1 list2 '() '()))
