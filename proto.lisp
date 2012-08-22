(defclass creature ()
  ((life :initarg :life :reader creature-life)
   (damages :initform 0 :accessor creature-dmg)
   (name :initarg :name :reader creature-name)))

(defmethod print-object ((object creature) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~a: ~a/~a" (creature-name object) (creature-dmg object) (creature-life object))))

(defclass wizard (creature)
  ((right-hand :initform nil :accessor wiz-right)
   (left-hand :initform nil :accessor wiz-left)
   (effects :initform nil :accessor wiz-effects)
   (control :initform nil :accessor wiz-ctrl))
  (:default-initargs :life 15))

(defgeneric creature-element (object))

(defmethod creature-element (object)
  nil)

(defclass elemental (creature)
  ((element :initarg :element :reader creature-element)))


(define-test element
  (assert-false (creature-element (make-instance 'creature)))
  (assert-false (creature-element (make-instance 'wizard)))
  (assert-equal "Foo" (creature-element (make-instance 'elemental :element "Foo"))))


(defclass game ()
  ((players :initarg :players :accessor players)
   (spells :initarg :spells :reader spells)))
