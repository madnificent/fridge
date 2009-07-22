(declaim (optimize (speed 0) (safety 3) (debug 3)))

(in-package :fridge)

(defgeneric build-hash-id (class &optional id)
  (:documentation "Builds the identifier used to fetch the object from the hash, if it is there"))
(defgeneric build-hash-identifier (class slot &optional value)
  (:documentation "Builds a finder that can be used to fetch the object from the hash, if it is there"))
(defgeneric quickstore (object &optional slot value)
  (:documentation "Stores the object in the quickstore"))

(defgeneric quickfetch (identifier)
  (:documentation "Fetches the first object that matches the identifier from the quickstore"))
(defgeneric quickfetch-all (identifier)
  (:documentation "Fetches all the objects that match the identifier from the quickstore"))
(defgeneric quickrm (object &optional slot value)
  (:documentation "Removes the object from the quickstore"))
(defgeneric quickstore-again (object)
  (:documentation "This is a way to store objects which have been stored in the quickstore previously, but which have been removed in the mean time"))

(defgeneric notify-id-changed (notified-object notifier)
  (:documentation "This method is called for any registered object when the id of the current object has changed."))
(defgeneric register-for-id-change (notifier notified-object)
  (:documentation "Registers <notified-object> to be notified by <notifier> whenever its id changed"))
(defgeneric deregister-for-id-change (notifier notified-object)
  (:documentation "Deregisters <notified-object> to be notified by <notifier> whenever its id changed"))
(defgeneric id-slot (object)
  (:documentation "Returns the slot which contains the id of the current object"))
(defgeneric id-slot-name (object)
  (:documentation "Returns the slot-name which contains the id of the requested object"))
(defgeneric id (object)
  (:documentation "Returns the id of the given object"))

(defparameter *known-objects* (make-hash-table :test 'equal)
  "Hash that will store the known objects by their class and id")

(defun save-quickstore ()
  "Saves all objects known to the quickstore to the database"
  (let ((objects-to-save (remove-duplicates (loop for object being the hash-value in *known-objects* collect (first object)))))
    (save-objects objects-to-save)
    objects-to-save))

(defun quickclear ()
  "Clears the quickstore database"
  (setf *known-objects* (make-hash-table :test 'equal)))

(defclass quicksearch-support-metaclass (db-support-metaclass)
  ()
  (:documentation "This metaclass allows you to do quick searches for objects and choose a failsafe method when they couldn't be found"))
(defclass quicksearch-support-class (db-support-class)
  ((notifiers :initform nil
	      :writer (setf notifiers)))
  (:metaclass quicksearch-support-metaclass)
  (:documentation "This should be the superclass of the quicksearch-support-metaclass"))

(defmethod notifiers ((object quicksearch-support-class))
  (and (slot-boundp object 'notifiers) (slot-value object 'notifiers)))

(defmethod closer-mop:validate-superclass ((a quicksearch-support-metaclass) (b standard-class))
  T)

(defclass quicksearch-id-direct-slot (column-direct-slot)
  ()
  (:documentation "Quicksearch direct slot for the ID of the current object"))
(defclass quicksearch-id-effective-slot (direct-effective-slot)
  () (:documentation "Quicksearch effective slot for the ID of the current object"))

(defmethod id-slot ((class symbol))
  (id-slot (find-class class)))
(defmethod id-slot ((object quicksearch-support-class))
  (id-slot (class-of object)))
(defmethod id-slot ((class quicksearch-support-metaclass))
  (loop for slot in (class-direct-slots class)
     when (typep slot (find-class 'quicksearch-id-direct-slot))
     return slot))
(defmethod id-slot-name (class)
  (slot-name (id-slot class)))

(defmethod id ((object quicksearch-support-class))
  (slot-value object (id-slot-name object)))

;; behavior changes
(defmethod effective-slot-definition-class :around ((class quicksearch-support-metaclass) &rest args)
  (declare (ignore args))
  (cond ((find (find-class 'quicksearch-id-direct-slot) *direct-slots* :key #'class-of)
	 (find-class 'quicksearch-id-effective-slot))
	(T
	 (call-next-method))))
(defmethod direct-slot-definition-class :around ((class quicksearch-support-metaclass) &rest initargs &key arg &allow-other-keys)
  (if (find :id initargs)
      (find-class 'quicksearch-id-direct-slot)
      (call-next-method)))

(defmethod slot-value-using-class :around ((class quicksearch-support-metaclass) (object quicksearch-support-class) (slot quicksearch-id-effective-slot))
  (let ((slot-name (slot-name (direct-slot slot)))) 
    (or (and (slot-boundp object slot-name) (call-next-method))
       (progn (save object)
	      (slot-value object slot-name)))))
(defmethod (setf slot-value-using-class) :around (value (class quicksearch-support-metaclass) (object quicksearch-support-class) (slot quicksearch-id-effective-slot))
  (when (slot-boundp-using-class class object slot)
    (quickrm object))
  (call-next-method)
  (dolist (notified-object (notifiers object))
    (notify-id-changed notified-object object))
  (quickstore object))

(defmethod register-for-id-change ((notifier quicksearch-support-class) notified-object)
  (unless (find notifier (notifiers notifier))
    (push notified-object (notifiers notifier)))
  notifier)
(defmethod deregister-for-id-change ((notifier quicksearch-support-class) notified-object)
  (setf (notifiers notifier) (delete notified-object notifier))
  notifier)

(defmethod where-clause ((object quicksearch-support-class))
  (list := 
	(column (find (id-slot-name object) (class-direct-slots (class-of object)) :key #'slot-name))
	(snapshot-value object +db-snapshot+ (id-slot-name object))))

(defmethod delete-instance :before ((object quicksearch-support-class))
  (quickrm object))

;; hashes
(defmethod build-hash-id ((class symbol) &optional id)
  (build-hash-id (find-class class) id))
(defmethod build-hash-id ((object quicksearch-support-class) &optional id)
  (declare (ignore id))
  (build-hash-id (class-of object) (id object)))
(defmethod build-hash-id ((class quicksearch-support-metaclass) &optional id)
  (list class (id-slot-name class) id))

(defmethod build-hash-identifier ((class quicksearch-support-metaclass) (slot-name symbol) &optional id)
  (list class slot-name id))
(defmethod build-hash-identifier ((class symbol) (slot-name symbol) &optional id)
  (build-hash-identifier (find-class class) slot-name id))
(defmethod build-hash-identifier ((object quicksearch-support-class) (slot-name symbol) &optional (id nil id-p))
  (unless id-p (setf id (slot-value object slot-name)))
  (build-hash-identifier (class-of object) slot-name id))

(defun hash-identifier-slot (hash)
  (second hash))

;; fetching of stored slots
(defmethod quickstore ((object quicksearch-support-class) &optional (key nil key-p) (value nil value-p))
  (let ((hash (cond ((and key-p value-p) (build-hash-identifier object key value))
		    (key-p (build-hash-identifier object key))
		    (T (build-hash-id object)))))
    (if (eql (hash-identifier-slot hash)
	     (id-slot-name object))
	(setf (gethash hash *known-objects*) (list object))
	(setf (gethash hash *known-objects*) (cons object (gethash hash *known-objects*))))))
(defmethod quickstore-again ((object quicksearch-support-class))
  (quickstore object))
(defmethod quickfetch (identifier)
  (first (gethash identifier *known-objects*)))
(defmethod quickfetch-all (identifier)
  (gethash identifier *known-objects*))
(defmethod quickrm ((object quicksearch-support-class) &optional (key nil key-p) (value nil value-p))
  (let ((hash (cond ((and key-p value-p) (build-hash-identifier object key value))
		    (key-p (build-hash-identifier object key))
		    (T (build-hash-id object)))))
    (if (eql (hash-identifier-slot hash)
	     (id-slot-name object))
	(setf (gethash hash *known-objects*) nil)
	(setf (gethash hash *known-objects*) (delete object (gethash hash *known-objects*))))))
(defmethod save :after ((object quicksearch-support-class))
  (quickstore object))

;;; fetching thecorrect slots
(defmethod object-matches-initargs ((object quicksearch-support-class) initargs)
  (loop for (key value) on initargs by #'cddr
     unless (equal value (slot-value object (slot-name (find-slot-by-initarg (class-of object) key))))
     do (return-from object-matches-initargs nil))
  object)
(defmethod object-matches-columns ((object quicksearch-support-class) initcols)
  (loop for (key . value) in initcols
     unless (equal value (slot-value object (find-slot-name-by-column (class-of object) key)))
     do (return-from object-matches-columns nil))
  object)
(defmethod load-instance :around ((class quicksearch-support-metaclass) &rest initargs &key id &allow-other-keys)
  (declare (ignore initargs))
  (or (and id (object-matches-initargs (quickfetch (build-hash-id class id))))
      (quickstore (call-next-method))))

(defmethod load-instance-from-column-alist :around ((class quicksearch-support-metaclass) &rest column-alist)
  (let ((column (find-column-by-slot class (id-slot-name class))))
    (let ((fetched-value (when (find column column-alist :key #'car)
			   (quickfetch (build-hash-id class (cdr (assoc column column-alist)))))))
      (or fetched-value (call-next-method)))))

(defmethod load-instances :around ((class quicksearch-support-metaclass) &rest initargs)
  (remove-if (lambda (object)
	       (block object-validator
		 (loop for (column value) on initargs by #'cddr
		    when (not (equal value
				     (slot-value object (slot-name (find-slot-by-column class column)))))
		    do (return-from object-validator T)))
	       nil)
	     (call-next-method)))

;;; loading of instances by cached slots
(defmethod load-instances-by-slot-names :around ((class quicksearch-support-metaclass) &rest initargs)
  (remove-if (lambda (object)
	       (block object-validator
		 (loop for (slot value) on initargs by #'cddr
		    when (not (equal value (slot-value object slot)))
		    do (return-from object-validator T)))
	       nil)
	     (call-next-method)))

(defmethod load-instance-from-slot ((class symbol) slot value)
  (load-instance-from-slot (find-class class) slot value))
(defmethod load-instance-from-slot ((class quicksearch-support-metaclass) slot value)
  (first (load-instances-from-slot class slot value)))
(defmethod load-instances-from-slot ((class symbol) slot value)
  (load-instances-from-slot (find-class class) slot value))
(defmethod load-instances-from-slot ((class quicksearch-support-metaclass) slot value)
  (let ((fetched (quickfetch-all (build-hash-identifier class slot value)))
	(loaded (load-instances-by-slot-names class slot value)))
    (remove-duplicates (concatenate 'list fetched loaded) :key #'id)))
