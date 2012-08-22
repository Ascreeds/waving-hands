(defclass monster ()
  ((life :initarg :life :reader monster-life)
   (damages :initform 0 :accessor monster-dmg)
   (name :initarg :name :reader monster-name)))

(defmethod print-object ((object monster) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (format stream "~a: ~a/~a" (monster-name object) (monster-dmg object) (monster-life object))))
