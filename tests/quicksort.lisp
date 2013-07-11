(begin
	(define concat
		(lambda (listA listB)
			(if (eqv? listA '()) listB
				(cons (car listA) (concat (cdr listA) listB))
				)
			)
		)

	(define filterSmaller
		(lambda 
			(listA x)
			(if (eqv? listA '()) '()
				(if (lt? (car listA) x) (cons (car listA) (filterSmaller (cdr listA) x))
					(filterSmaller (cdr listA) x)
					)
				)
			)
		)

	(define filterNonSmaller
		(lambda
			(listA x)
			(if (eqv? listA '()) '()
				(if (lt? (car listA) x) (filterNonSmaller (cdr listA) x)
					(cons (car listA) (filterNonSmaller (cdr listA) x))
					)
				)
			)
		)

	(define quickSort
		(lambda (list)
			(if (eqv? list '()) '()
				(concat
					(quickSort (filterSmaller (cdr list) (car list)))
					(cons (car list)
						(quickSort (filterNonSmaller (cdr list) (car list)))
						)
					)
				)
			)
		)

	(quickSort '(4 5 2 6 4 1 9))
)