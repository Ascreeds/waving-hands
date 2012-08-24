#|

Trie data structure

|#

(defclass trie ()
  ((children :initform nil :accessor trie-children)
   (value :initform nil :accessor trie-value)))

(defun trie-init ()
  (make-instance 'trie))

(defun trie-insert (trie key value) 
  (if (zerop (length key))
      (setf (trie-value trie) value)
      (let ((sub-trie (if-let (pair (assoc (elt key 0) (trie-children trie)))
			(cdr pair)
			(let ((new-trie (make-instance 'trie)))
			  (push (cons (elt key 0) new-trie) (trie-children trie))
			  new-trie))))
	(trie-insert sub-trie (subseq key 1) value))))
