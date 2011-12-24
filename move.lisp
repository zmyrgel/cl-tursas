(in-package :tursas)

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
