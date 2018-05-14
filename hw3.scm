

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
    
    (display list1)
    (display list2)
    (display sol)
    (display chosen)
    (newline)
    (newline)
    (cond

        ; if there are no chosen pairs, the stamements cannot be resolved
        ((and (null? list1) (null? chosen)) #f)     
        
        ; if there are more than one chosen pairs, the statements cannot be resolved
        ((and (null? list1) (multiple-elements chosen)) #f)

        ; if there is a contradiction, meaning 
        ((and (and (null? list1) (null? sol)) (null? (res-delete list2 (car chosen)))) 'CONTRADICTION)

        ; if one chosen pair has been found and the process is over
        ((null? list1) (append sol (res-delete list2 (car chosen))))

        ; normal case for checking for pairs
        (#t (let ((value (res-search (car list1) (negate (car list1)) list2))) 
            (cond 
                ; if you found the negative of current, add it to chosen to be removed from the list 
                ((equal? value "negative") (res-helper (cdr list1) list2 sol (append (list (negate (car list1))) chosen)))
                ; ignore the first of two duplicates
                ((equal? value "same") (res-helper (cdr list1) list2 sol chosen))
                (#t (res-helper (cdr list1) list2 (append sol value) chosen))
            )
        )) 
    )    
)

; wrapper function for resolve
(define (res list1 list2) (res-helper list1 list2 '() '()))


;--------------------------------------------------------------------------------------------------------------------------------------------------

; INTERMEDIATE ASSIGNMENT

; define the knowledge base
(define kb '())

; adds a statement to the knoweledge base
(define (tell stmt)
    (display "telling")
    (newline)
    (begin
        (set! kb (append kb (list stmt)))
        'OK
    )
)

; initial run through the knowledge base to see if the stament is there
(define (knowledge-check-helper stmt kb-cur)
    (display "knowledge check helper")
    (newline)
    (cond
        ((null? kb-cur) #f)
        ((equal? stmt (car kb-cur)) #t)
        (#t (knowledge-check-helper stmt (cdr kb-cur)))    
    )
)


(define (knowledge-check stmt) (knowledge-check-helper stmt kb))


; actually resolving the statement
(define (resolve-stmt-helper stmt kb-cur-stmt)
    (display "resolving stmt helper")
    (newline)
    (display "...............")
    (newline)
    (display stmt)
    (newline)
    (display kb-cur-stmt)
    (newline)
    (display kb)
    (newline)
    (newline)
    (let ((new (res stmt kb-cur-stmt)))
        (cond
            ((and (not (equal? new #f)) (not (knowledge-check new))) 
                (begin 
                    (tell new) 
                    new
                )              
            )
        )
    ) 
)

; resolve every statment against current one
(define (resolve-stmt ask-stmt stmt kb-cur)
    (display "resolving stmt")
    (newline)
    (cond
        ((null? kb-cur) #f)
        ((equal? ask-stmt (resolve-stmt-helper stmt (car kb-cur))) #t)
        (#t (resolve-stmt ask-stmt stmt (cdr kb-cur)))    
    )
)

; because we kb is changing as we are resolving things, we want an updated knowledege base every time we
; start exhaustively resolving for the next element
(define (kb-cur-update stmt kb-cur)
    (display "updating current")
    (newline)
    (cond 
        ((equal? stmt (car kb-cur)) (cdr kb-cur))
        (#t (kb-cur-update stmt (cdr kb-cur)))
    )
)

; attempt to resolve the first element that we have of kb with all other elements and add those new resolutions to kb
(define (resolve-stmts-helper ask-stmt kb-cur)
    (display ask-stmt)
    (newline)
    (cond 
        ((null? kb-cur) #f)
        (#t (begin
                (cond 
                    ((resolve-stmt ask-stmt (car kb-cur) (cdr kb-cur)) #t)
                    (#t (resolve-stmts-helper ask-stmt (kb-cur-update (car kb-cur) kb)))
                )
            )
        )
    )
)

(define (resolve-stmts ask-stmt) (resolve-stmts-helper ask-stmt kb))

; return true if you can resolve to the statement, or unknown
(define (ask ask-stmt) 
    (cond 
        ((knowledge-check ask-stmt) #t)
        ((resolve-stmts ask-stmt) #t)
        (#t 'UNKNOWN)
    )    
)
