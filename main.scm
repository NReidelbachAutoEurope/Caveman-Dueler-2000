;;; Caveman Dueler 2000

;; A program built to teach young people to program
;; Idea shamelessly stolen from this Stack Overflow question 
;; https://codegolf.stackexchange.com/questions/34968/caveman-duels-or-me-poke-you-with-sharp-stick

;; Two cavemen face off. Each can choose 1 of the folowing actions
(define poke-action "P")
(define sharpen-action "S")
(define defend-action "D")

(define poke-action-char #\P)
(define sharpen-action-char #\S)
(define defend-action-char #\D)

;; If one pokes while the other sharpens, the poker wins
;; However, the poker must have a sharp stick
;; Sharpening increases his stick's sharpness by 1
;; Poking decreases his stick's sharpness by 1
;; Defending stops a poke but doesn't decrease the stick's sharpness

;; If a caveman pokes with a super sharp stick (sharpness > 5)
;; Then unless the other caveman is also poking with a super sharp stick
;; the super sharp poker wins

;;Returns the winner's name
(define caveman-duel
  (lambda
    (
;; Caveman brains are functions 
    caveman-one-brain 
    caveman-two-brain
;; Player names
    caveman-one-name
    caveman-two-name
;; Titles
    caveman-one-title
    caveman-two-title
    )
    
    (describe-introduction caveman-one-name caveman-two-name 
      caveman-one-title caveman-two-title)

    ;; Call a recursive function that has the cavemen duel until there is a winner
    (caveman-duel-with-state 
      caveman-one-brain 
      caveman-two-brain
      0
      0
      ""
      ""
      caveman-one-name
      caveman-two-name
      0
    )
  )
)

(define caveman-duel-with-state
  (lambda
    (      
      caveman-one-brain 
      caveman-two-brain
      ;;inputs defining state
      caveman-one-sharpness
      caveman-two-sharpness
      caveman-one-history
      caveman-two-history
      ;; Player names
      caveman-one-name
      caveman-two-name
      timeout-counter
    )
    ;; Life expectency is short in pre-history if the fight lasts 100 rounds both cavemen lose
    (if ( > timeout-counter 100)
      (time-out)
      (begin
        (display (string-append "Round " (number->string timeout-counter) " begins!"))
        (newline)
      ;; determine what each caveman will do
        (define caveman-one-action (caveman-one-brain caveman-one-history caveman-two-history))
        (define caveman-two-action (caveman-two-brain caveman-two-history caveman-one-history))
      ;; describe what's happening
        (describe-action-sequence caveman-one-name caveman-one-action caveman-one-sharpness)
        (describe-action-sequence caveman-two-name caveman-two-action caveman-two-sharpness)

        ;; Two base cases that happen if a player wins otherwise go to the recursive case
        (if 
        ;;requirements to win for player 1
          (has-won? caveman-one-action caveman-two-action caveman-one-sharpness caveman-two-sharpness)
          ;;Display who won and end when we have a winner
          (win caveman-one-name)
          ;;otherwise check if the other player won
          (if 
        ;;requirements to win for player 2
            (has-won? caveman-two-action caveman-one-action caveman-two-sharpness caveman-one-sharpness)
          ;;Display who won and end when we have a winner
            (win caveman-two-name)
          ;;otherwise update the state and fight again
            (caveman-duel-with-state
              caveman-one-brain 
              caveman-two-brain
              ;; Update the sharpness for the next round
              (determine-next-sharpness caveman-one-sharpness caveman-one-action)
              (determine-next-sharpness caveman-two-sharpness caveman-two-action)
              ;; Update the history for the next round
              (string-append caveman-one-history caveman-one-action)
              (string-append caveman-two-history caveman-two-action)
              ;; Player names
              caveman-one-name
              caveman-two-name
              (+ timeout-counter 1)
            )
          )
        )
      )
    )
  )
)

;;End state functions


;; describe wining and return the winner
(define win
  (lambda (winner-name)
    (describe-winning winner-name)
    winner-name
  )
)

;; define timeout return empty string  for no winner
(define time-out
  (lambda ()
    (describe-death-by-old-age)
    ""
  )
)

;; Functions to describe what's going on

(define describe-introduction
  (lambda (caveman-one-name caveman-two-name caveman-one-title caveman-two-title)
    (display "Oooga booga Ooooga booga!")
    (newline)
    (display 
      (string-append "It's a fight between " 
        caveman-one-name " " caveman-one-title " and " caveman-two-name 
        " " caveman-two-title "!")
    )
    (newline)
  )
)

(define describe-death-by-old-age
  (lambda ()
    (display "Both cavemen died of old age.")
    (newline)
  )
)

(define describe-winning 
  (lambda (winner-name)
    (display (string-append winner-name " successfully pokes and is victorious!"))
    (newline)
  )
)

(define describe-action-sequence
  (lambda 
    (caveman-name action sharpness)
    (define pretty-action-sentence 
      (string-append caveman-name " decides to " 
      (describe-action action) " a "
      (describe-sharpness sharpness) " stick." )
    )
    (display pretty-action-sentence)
    (newline)
  )
)

(define describe-sharpness
  (lambda
    (sharpness)
    (if (< 0 sharpness)
      (if (< 4 sharpness)
        "razor sharp"
        "pretty sharp"
      )      
      "completely dull"
    )
  )
)

(define describe-action
  (lambda
    (action)
    (if 
      (equal? action poke-action)
      "poke with"
      (if
        (equal? action defend-action)
        "defend with"
        (if 
          (equal? action sharpen-action)
          "sharpen"
          ;;Unexpected actions are just printed 
          ;; with some hacky text to make the rest make sense
          (string-append action ". He has")
        )
      )
    )
  )
)

;; returns #t if the caveman with the winner-action has won 
;; otherwise returns #f
(define has-won?
  (lambda 
    (winner-action loser-action winner-sharpness loser-sharpness)
    
    (begin
    ;; to win you must poke
    (and 
      (equal? winner-action poke-action)
      (or
        ;; if you poke with something sharp while the loser sharpens, you win
        (and           
          (> winner-sharpness 0)
          (not
            (or
              (equal? loser-action poke-action)
              (equal? loser-action defend-action)
            )
          )
        )
        ;; if you poke with something super sharp, 
        ;; unless the loser is also poking with something super sharp, 
        ;; you win
        (and
          (> winner-sharpness 4)
          ( or
            (< loser-sharpness 5)
            (not 
              (equal? loser-action poke)
            )
          )
        )      
      )
    )
  )
  )
)

;; returns the news sharpness as a number based on what action the caveman did
(define determine-next-sharpness
  (lambda
    (sharpness action)
    (if
      (equal? action poke-action)
      (- sharpness 1)
      (if
        (equal? action sharpen-action)
        (+ sharpness 1)
        ;;If you didn't attack or defend, your sharpness stays the same
        sharpness
      )
    )
  )
)

;; returns #t if the stick is sharp based on the history
(define is-sharp
  (lambda
    (history)
    (< 0 (determine-sharpness history))
  )
)

;; returns #t if the stick is super sharp (sharp enough to get through a block)
;; based on the history
(define is-super-sharp
  (lambda
    (history)
    (< 4 (determine-sharpness history))
  )
)

;; returns the sharpness of a stick based on the history input
(define determine-sharpness
  (lambda
    (history)
    (if 
      (equal? "" history)
      0
      (begin
        (define first-char (string-ref history 0))
        (define rest-of-history (substring history 1 (string-length history)))
        (if 
          (eqv? first-char poke-action-char)
          (- (determine-sharpness rest-of-history) 1)
          (if 
            (eqv? first-char sharpen-action-char)
            (+ 1 (determine-sharpness rest-of-history))
            (determine-sharpness rest-of-history)
          )
        )
      )
    )
  )

)

;; simplified function for testing to avoid having to fill as many parameters
(define caveman-duel-no-titles
  (lambda
    (caveman-one-brain caveman-two-brain 
    caveman-one-name caveman-two-name)
    (caveman-duel-no-titles caveman-one-brain 
    caveman-two-brain caveman-one-name caveman-two-name
    )
  )
)


;; Testing functions to test against the player-decision-function
(define duel-super-easy
  (lambda () 
    (caveman-duel player-decision-function always-sharpen-strategy 
    player-caveman-name "Ook" player-caveman-title "the not so sharp")
  )
)

(define duel-easy
  (lambda () 
    (caveman-duel player-decision-function sharpen-then-poke-strategy 
    player-caveman-name "Zook" player-caveman-title "the Odd Poker")
  )
)

(define duel-normal
  (lambda ()
    (caveman-duel player-decision-function block-when-vulnerable-strategy 
    player-caveman-name "Conan" player-caveman-title "the cautious")
  )
)

;;TODO finish this to build and execute a tournament among various functions
;; for figuring out what to do
(define run-tournament
  (lambda
    ;; list of triples where element 1 is name 2 is title and 3 is function
    (list-of-cavemen)
    (run-)
  )
)

;; Various strategies for determining what to do next
;; Each takes in histories and output which action to do
;; Players can use these to help define their functions

(define always-sharpen-strategy
  (lambda (my-history opponent-history)
    sharpen-action
  )
)

(define always-defend-strategy
  (lambda (my-history opponent-history)
    defend-action
  )
)

(define sharpen-then-poke-strategy
  (lambda (my-history oppoent-history)
    (if 
      ;; If the length of the history is an even number, 
      ;; sharpen, otherwise poke
      (even? (string-length my-history))
      sharpen-action
      poke-action
    )  
  )
)

(define block-when-vulnerable-strategy
  (lambda (my-history opponent-history)
    ;; if the opponent looks to have a sharp stick based on his history, defend
    (if (is-sharp opponent-history)
      defend-action
      ;; if your caveman has a sharp stick, poke, otherwise sharpen
      (if (is-sharp my-history)
        poke-action
        sharpen-action
      )
    )
  )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;MODIFY HERE:::::::::::::::::::::::::::::::::::::::::::::::::::::::
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;Give your caveman a name by replacing the string
(define player-caveman-name "Ooog")
;;Give your caveman a title by replacing the string
(define player-caveman-title "the Nasal Excavator")

;;This is where you decide how your caveman duels
(define player-decision-function
  (lambda 
    (
      ;;There are two inputs the first is what your caveman has been doing      
      ;; The second is what your opponent's caveman has been doing
      ;; These inputs are strings like "SSBBPP" 
      ;; "S" means sharpen "P" means poke "B" means block
      ;; So "SPSB" would mean round 1 you sharpend, round 2 you poked
      ;; round 3 you sharpend and round 4 you blocked
      my-history 
      opponent-history)
      ;; here is where you decide what to do next
      ;; "S" would sharpen "P" would poke and "B" would block
      ;; You can also have your caveman do something else (any string), 
      ;; but it won't help you win.
    "pick his nose"
  )
)

;; To test your decision function, you can manually pass in inputs like
;; (player-decision-function "" "")
;; or
;; (player-decision-function "SSSP" "SSSB")
;; You can also use the pre-built functions: duel-super-easy, duel-easy, and duel-normal like
;; (duel-easy)
;; these will automatically call your caveman against one I've built










