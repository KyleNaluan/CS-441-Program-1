#lang racket

; List Generation
(define (generate-random-integers count min-value max-value)
  (define (generate n)
    (if (zero? n)
        '()
        (cons (random min-value max-value)
              (generate (- n 1)))))
  (generate count))

; Utility Functions
(define (find-min lst)
  (if (null? lst)
      (error "The list is empty")
      (foldl min (first lst) (rest lst))))

(define (filter-by-comparison comp-fn value lst)
  (filter (lambda (x) (comp-fn x value)) lst))

(define (selection-sort lst)
  (define (sort-helper unsorted sorted)
    (if (null? unsorted)
        (reverse sorted)
        (let ([min-val (find-min unsorted)])
          (sort-helper (filter-by-comparison > min-val unsorted) 
                       (cons min-val sorted)))))
  (sort-helper lst '()))

(define (is-sorted lst)
  (let loop ((lst lst))
    (cond
      [(or (null? lst) (null? (rest lst))) #t]
      [(> (first lst) (second lst)) #f]
      [else (loop (rest lst))])))

(define (more-than-five-items? lst)
  (and (pair? lst)
       (pair? (rest lst))
       (pair? (rest (rest lst)))
       (pair? (rest (rest (rest lst))))
       (pair? (rest (rest (rest (rest lst)))))
       (pair? (rest (rest (rest (rest (rest lst)))))))
  )

; Median of Medians Functions
;; Function to split the list into chunks of 5 or fewer elements
(define (split-into-sublists lst)
  (define (split-helper lst sublists)
    (if (null? lst)                         ;; Base case: If the list is empty
        (reverse sublists)                  ;; Return the reversed list of sublists
        (let* ([has-more-than-five (more-than-five-items? lst)]  ;; Check if there are more than 5 items left
               [chunk (if has-more-than-five (take lst 5) lst)]  ;; Take 5 or remaining items
               [remaining (if has-more-than-five (drop lst 5) '())])  ;; Drop 5 or stop if fewer
          (split-helper remaining (cons chunk sublists)))))  ;; Recurse with remaining items
  (split-helper lst '()))  ;; Initial call with an empty sublist accumulator

;; Function to find the median of a list (assumes list has been sorted & is at most 5 elements)
(define (median lst)
  (let ([n (length lst)])
    (list-ref lst (quotient (- n 1) 2))))

;; Median of medians partitioning
(define (median-of-medians lst)
  ;; Step 1: Split the list into sublists of 5 or fewer elements
  (let ([sublists (split-into-sublists lst)])
    ;; Step 2: Sort each sublist and find its median
    (define medians
      (map (lambda (sublist) (median (selection-sort sublist))) sublists))
    ;; Step 3: If the list of medians has more than 5 items, recursively apply median-of-medians
    (if (more-than-five-items? medians)
        (median-of-medians medians)
        ;; Otherwise, return the median of the medians
        (median (selection-sort medians)))))

; Quick Sort
(define (quicksort lst)
  (if (null? lst)  ;; Base case: an empty list is already sorted
      '()
      (let* ([pivot (median-of-medians lst)]  ;; Step 1: Choose pivot using median of medians
             [less (filter (lambda (x) (< x pivot)) lst)]  ;; Step 2: Partition list
             [equal (filter (lambda (x) (= x pivot)) lst)]
             [greater (filter (lambda (x) (> x pivot)) lst)])
        ;; Step 3: Recursively sort the "less" and "greater" partitions and concatenate results
        (append (quicksort less) equal (quicksort greater)))))



; Testing Functions

"Utility Function Tests"

(generate-random-integers 43 1 20)

(filter-by-comparison < 5 '(1 2 3 4 5 5 6 7 25 0))

(filter-by-comparison > 5 '(1 2 3 4 5 5 6 7 25 0))

(filter-by-comparison = 5 '(1 2 3 4 5 5 6 7 25 0))

(selection-sort '(0 9 1 5 4 25 3 7))

"Is Sorted:"

(is-sorted '(-1 2 3 4 5))

(is-sorted '(1 2 3 5 4))

"More than five items:"

(more-than-five-items? '(1 2 3 4 5 2))

(more-than-five-items? '(1 2 3 4 5))

(more-than-five-items? '(0 5 1))

"Median of Medians Tests"

(median-of-medians '(1 5 2 3 4 9 7 6 8 10 14 13 15 11 12 20 21 45))

(median-of-medians '(3 8 13 21 45 10 12 46 1 5 2))

(median-of-medians '(1 2 3 5 8 10 12 13 21 45 46 1 2 4 5 7))

(median '(1 2 3 4))

(median '(2 4 6 8 10 15))

"Quick Sort Tests"

(quicksort '(13 12 11 10 9 8 7 6 5 4 3 2 1))

(quicksort (generate-random-integers 4 1 100))

(quicksort (generate-random-integers 43 1 100))

(is-sorted (quicksort (generate-random-integers 403 1 100)))

(is-sorted (quicksort (generate-random-integers 40003 1 10)))

(is-sorted (quicksort (generate-random-integers 400003 1 10)))

"start timer"

(is-sorted (quicksort (generate-random-integers 10000003 1 100)))

"done running"