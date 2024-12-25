;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; readLineFromFile(aPort) --> line (as list of characters)
;;
;; Read one line from a port, not including the return or newline
;© R. Heise 4
;; but eliminating them. This is a wrapper for the recursive method
;; that does the work (readLoopFromFile).
;;
(define (readLineFromFile aPort)
  (readLoopFromFile aPort (read-char aPort) '())) ;do wait for one char
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; readLoopFromFile(aPort currentCharacter line) --> line
;; (as list of characters)
;;
;; This recursive method reads a character at a time from the
;; port until it finds the return + newline (i.e. enter). It builds the
;; characters into a list which is returned at the end. Carriage
;; returns, newlines, and end of files are removed.
;;
;; If EOF needs to be flagged, the first condition could be changed
;; to return (cons '_EOF_ line)
;;
(define (readLoopFromFile aPort curChar line)
  (cond
    ((eof-object? curChar) line)
    ((char=? #\newline curChar) (reverse line))
    ((char=? #\return curChar) (read-char aPort) line) ;lines on Windows end
    ;with #\return followed by #\newline
    (#t (readLoopFromFile aPort (read-char aPort)
                          (append line (list curChar))))))

;Stores the maze into a 2D list by recusively calling
;readLineFromFile and combines each line into a list
(define (listMaze aPort)
  (cond
    (#t (listMazeWrap aPort '()))))
;listMaze wrapper 
(define (listMazeWrap aPort maze)
  (cond
    ((eof-object? (peek-char aPort)) (close-input-port aPort)
                                      (reverse maze))
    (#t (listMazeWrap aPort (cons (readLineFromFile aPort) maze)))))
;listMaze

;Takes a two digit list and turns it into a number
(define (list->ten list)
  (cond
    ((null? (cdr list)) (car list))
    (#t (+ (* (car list) 10)(cadr list)))))
;list->ten

;Converts char to an integer by using char->integer
;and subtracting the decimal value of char '0 which is 48
(define (char->int chars)
  (cond
   ((null? (cdr chars)) (list (- (char->integer (car chars)) 48)))
   (#t (list (- (char->integer (car chars)) 48) (- (char->integer (cadr chars)) 48)))))

;Takes a 2D list and recursively go through maze to find
;the player starting position and return the x y values
(define (findPosition maze)
  (cond
    ((null? maze) cons(x y))
    (#t (findPositionRec maze (car maze) 0 0))))
;FindPosition recursive function
(define (findPositionRec maze row x y)
  (cond
    ((equal? (car row) '#\<) (cons x y))
    ((equal? (car row) '#\>) (cons x y))
    ((equal? (car row) '#\^) (cons x y))
    ((equal? (car row) '#\V) (cons x y))
    ((null? (car maze)) (cons x y));if at the end of the list
    ((null? (cdr row)) (findPositionRec (cdr maze) (car (cdr maze)) 0 (+ y 1)))
    ;if at the end of the row go to the next one
    (#t (findPositionRec maze (cdr row) (+ x 1) y))))

;Updates the element at the given location 
(define (updateDirection maze row col direction)
  (cond
    ((null? maze) '())
    ((= row 0)
     (cons (replace (car maze) col direction) (cdr maze))) ; Replace in the row
    (#t (cons (car maze) (updateDirection (cdr maze) (- row 1) col direction))))) ; Recurse on rows

; Helper function to replace an element in a 1D list (same as before)
(define (replace maze index direction)
  (cond
    ((= index 0) (cons direction (cdr maze))) ; Replace element at index 0
    ((null? maze) '())
    (#t (cons (car maze) (replace (cdr maze) (- index 1) direction)))))

;
;Uses given x y values and find the element at that location
(define (findLocation maze x y)
  (cond
    ((= 0 (apply + (list x y))) (car maze));if at the location return the element
    ((> y 0) (findLocation (cdr maze) x (- y 1)));go to the next row
    ((= y 0) (findLocation (car maze) x (- y 1)))
    ((>= x 0) (findLocation (cdr maze) (- x 1) y));go to the next element in the row
    (#t (display '(wrong)))));if element coul dnot be found

;Moves the character depending on the user input
(define (playerInput maze input positionY positionX)
  (cond
    ;If input is 'u, go to the row above
    ((equal? input 'u ) (cons (updateDirection (updateDirection maze positionY positionX '#\space)
                                         (- positionY 1) positionX '#\V) (cons (- positionY 1) positionX)))
    ;If input is 'd, go to the row below
    ((equal? input 'd ) (cons (updateDirection (updateDirection maze positionY positionX '#\space)
                                         (+ positionY 1) positionX '#\^) (cons (+ positionY 1) positionX)))
    ;If input is 'r, go to the place right
    ((equal? input 'r ) (cons (updateDirection (updateDirection maze positionY positionX '#\space)
                                         positionY (+ positionX 1) '#\<) (cons positionY (+ positionX 1))))
    ;If input is 'l, go to the position left
    ((equal? input 'l ) (cons (updateDirection (updateDirection maze positionY positionX '#\space)
                                         positionY (- positionX 1) '#\>) (cons positionY (- positionX 1))))
    ;If no a valid input, just return unchanged maze
    (#t (cons maze (cons positionY PositionX)))))

;Prints each line of the maze by converting the list to a string              
(define (printMaze maze)
  (newline)
  (cond
    ((null? (cdr maze)) (newline))
    (#t (display (list->string (cadr maze))) (printMaze (cdr maze)))))

;Checks the next position the player is moving to
;Checks what the character is moving into
(define (checkNext maze input x y)
  (cond
    ((equal? input 'u ) (findLocation maze x (- y 1)))
    ((equal? input 'd ) (findLocation maze x (+ y 1)))
    ((equal? input 'r ) (findLocation maze (+ x 1) y))
    ((equal? input 'l ) (findLocation maze (- x 1) y))))

;Adds score according to what the player lands on
(define (returnScore char)
  (cond
    ((equal? char '#\C) 10)
    ((equal? char '#\.) 2)
    ((equal? char '#\space) -1)
    (#t '0)))

;Checks the next player position depending on input
(define (checkNextPosition input x y)
  (cond
    ((equal? input 'u ) (cons x (- y 1)))
    ((equal? input 'd ) (cons x (+ y 1)))
    ((equal? input 'r ) (cons (+ x 1) y))
    ((equal? input 'l ) (cons (- x 1) y))))

;Returns true if valid false otherwise
(define (checkValidInput input)
  (cond
    ((equal? input 'u ) #t)
    ((equal? input 'd ) #t)
    ((equal? input 'r ) #t)
    ((equal? input 'l ) #t)
    (#t #f)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Print functions
(define (welcome)
  (display "======================================================\n")
  (display "Maze Miner \n")
  (display "Cookie Crumb = +2; Cherry = +10; Space = -1\n")
  (display "======================================================\n"))

(define (printScore score)
  (display "Current score : ")
  (display score))

(define (gameOver maze score)
  (printMaze (cdr maze))
  (printScore score)
  (display "\n======================================================\n")
  (display "Game Over! \nScore: ")
  (display score)
  (display "\n======================================================\n"))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  


;;Game loop
(define (game aPort)
  (welcome)
  (printMaze (cdr aPort))
  (gameRec aPort (car (findposition (cddr aPort))) (cdr (findposition (cddr aPort))) 0 (read)))

(define (gameRec maze positionX positionY score input)
  (printMaze (cdr (car (playerInput maze input (+ positionY 2) positionX))))
  (printScore (+ score (returnScore (checkNext (cddr maze) input (+ positionX 1) positionY))))
  (cond
    ;;Checks if input is valid
    ((equal? #f (checkValidInput input)) (gameRec maze positionX positionY score (read)))
    ;;Checks if player has reached the end of the maze
    ((equal? input 'q) (gameOver (car (playerInput maze input positionY positionX)) score))
    ((= (car (checkNextPosition input positionX positionY)) 0) (gameOver
                      (car (playerInput maze input positionY positionX)) score))
    ((= (cdr (checkNextPosition input positionX positionY)) 0) (gameOver
                      maze score))
    ((>= (car (checkNextPosition input positionX positionY)) (- (list->ten (char->int (cadr maze))) 1)) (gameOver
              (car (playerInput (cddr maze) input positionY positionX))
              (+ score (returnScore (checkNext (cddr maze) input (+ positionX 1) positionY)))))
    ((>= (cdr (checkNextPosition input positionX positionY)) (- (list->ten (char->int (car maze))) 1)) (gameOver
              (car (playerInput (cddr maze) input positionY positionX))
              (+ score (returnScore (checkNext (cddr maze) input (+ positionX 1) positionY)))))
    ;;Checks if next player position is a wall
    ((equal? '#\- (checkNext (cddr maze) input (+ positionX 1) positionY)) (gameRec maze positionX positionY score (read)))
    ;;updates next position
    (#t (gameRec  (car (playerInput maze input (+ positionY 2) positionX))
                  (cddr (playerInput (cddr maze) input positionY positionX))
                  (cadr (playerInput (cddr maze) input positionY positionX))
                  (+ score (returnScore (checkNext (cddr maze) input (+ positionX 1) positionY)))
                  (read)))))

(game (listMaze(open-input-file "Maze1.txt")))
