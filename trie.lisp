#|

Trie data structure

TODO: This version is not able to differentiate when NIL has been
stored and when no value has ever been stored for a given key.

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

(defun trie->dot (trie)
  (with-output-to-string (dot)
    (princ "digraph trie { node [ label=\"\" ] ; " dot)
    (let ((nodes (make-hash-table :test #'eq))
	  (node-counter 0))
      (labels ((node-number (node)
		 (if-let (number (gethash node nodes))
		   number
		   (setf (gethash node nodes) (incf node-counter)))))
	(let@ rec ((current-trie trie)
		   (remaining-tries nil))
	  (dolist (child (trie-children current-trie))
	    (format dot "n~a -> n~a [label=\"~a\"]; " (node-number current-trie) (node-number (cdr child)) (car child))
	    (if-let (value (trie-value (cdr child)))
	      (format dot "n~a [label=\"~a\"]; " (node-number (cdr child)) value)))
	  (if-let (children (trie-children current-trie))
	    (rec (cdr (first children)) (append (mapcar #'cdr (rest children)) remaining-tries))
	    (if remaining-tries
		(rec (first remaining-tries) (rest remaining-tries)))))))
    (princ "}" dot)))

(defun assoc->trie (alist)
  (let ((trie (trie-init)))
    (let@ rec ((alist alist))
      (if alist
	  (progn
	    (trie-insert trie (caar alist) (cdar alist))
	    (rec (rest alist)))
	  trie))))

(defun hash->trie (hash-table)
  (let ((trie (trie-init)))
    (maphash (lambda (key value) (trie-insert trie key value))
	     hash-table)
    trie))

(defun trie-locate (trie key)
  (let@ rec ((trie trie)
	     (key (map 'list #'identity key)))
    (if key
	(cif next (find (first key) (trie-children trie) :key #'car)
	     (rec (cdr next) (rest key))
	     nil)
	trie)))

(defun trie-get (trie key)
  (if-let (sub-trie (trie-locate trie key))
    (trie-value sub-trie)))
