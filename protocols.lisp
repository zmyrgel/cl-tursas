;;;; protocols lists generic functions available when working with
;;;; different chess board representations.

(in-package :tursas)

;; Move protocol

(defgeneric move->coord (move)
  (:method (move) (error "Can't use generic move->coord!"))
  (:documentation "Takes a move and returns its coordinate representation."))

(defgeneric from (move)
  (:method (move) (error "Can't use generic from!"))
  (:documentation "Returns moves originating squares coordinate."))

(defgeneric to (move)
  (:method (move) (error "Can't use generic to!"))
  (:documentation "Returns moves destination squares coordinate."))

(defgeneric promotion (move)
  (:method (move) (error "Can't use generic promotion!"))
  (:documentation "Returns possible promotion character from a move"))

;; State protocol

(defgeneric allowedp (state move)
  (:method (state move) (error "Can't use generic function!"))
  (:documentation "Predicate to see if move is allowed in given state."))

(defgeneric occupiedp (state index)
  (:method (state index) (error "Can't use generic function!"))
  (:documentation "Predicate to see if given index is occupied in the state."))

(defgeneric blackp (state index)
  (:method (state index) (error "Can't use generic function!"))
  (:documentation "Predicate to see if given index is occupied by black player in the state."))

(defgeneric whitep (state index)
  (:method (state index) (error "Can't use generic function!"))
  (:documentation "Predicate to see if given index is occupied by white player in the state."))

(defgeneric checkp (state)
  (:method (state) (error "Can't use generic function!"))
  (:documentation "Predicate to see if given state is in check."))

(defgeneric matep (state)
  (:method (state) (error "Can't use generic function!"))
  (:documentation "Predicate to see if given state is in mate."))

(defgeneric drawp (state)
  (:method (state) (error "Can't use generic function!"))
  (:documentation "Predicate to see if given state is in draw."))

(defgeneric state->fen (state)
  (:method (state) (error "Can't use generic function!"))
  (:documentation "Returns the FEN representation of given state."))

(defgeneric result (state)
  (:method (state) (error "Can't use generic function!"))
  (:documentation "Returns the game result from the given state."))

(defgeneric legal-states (state)
  (:method (state) (error "Can't use generic function!"))
  (:documentation "Returns all the legal states from given state."))

(defgeneric legal-moves (state)
  (:method (state) (error "Can't use generic function!"))
  (:documentation "Returns all the legal moves from given state."))

(defgeneric apply-move (state move)
  (:method (state move) (error "Can't use generic function!"))
  (:documentation "Returns new state with with given move applied to state."))

(defgeneric turn (state)
  (:method (state) (error "Can't use generic function!"))
  (:documentation "Returns which players turn it is."))

(defgeneric perft (state depth)
  (:method (state depth) (error "Can't use generic function!"))
  (:documentation "Calculates perft score to given depth from state."))

(defgeneric dynamicp (state)
  (:method (state) (error "Can't use generic function!"))
  (:documentation "Predicate to see if state is 'dynamic'."))

(defgeneric full-moves (state)
  (:method (state) (error "Can't use generic function!"))
  (:documentation "Returns count of full moves the game has lasted."))

(defgeneric evaluate (state)
  (:method (state) (error "Can't use generic function!"))
  (:documentation "Returns heuristic score for given state."))

(defgeneric last-move (state)
  (:method (state) (error "Can't use generic function!"))
  (:documentation "Returns the last move which lead to state."))

(defgeneric game-score (state)
  (:method (state) (error "Can't use generic function!"))
  (:documentation "Returns the game's end score."))

(defgeneric game-end-p (state)
  (:method (state) (error "Can't use generic function!"))
  (:documentation "Predicate to see if game has ended"))
