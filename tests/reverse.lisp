(begin
	(define concat
		(lambda (listA listB)
			(if (eqv? listA '()) listB
				(cons (car listA) (concat (cdr listA) listB))
				)
			)
		)
	(define reverse
		(lambda (list)
			(if (eqv? list '()) '()
				(concat (reverse (cdr list)) (cons (car list) '()))
				)
			)
		)
	(define rev (reverse '(1 2 3 4 5)))
	)