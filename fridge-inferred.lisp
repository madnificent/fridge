(in-package :fridge)

(defclass dbi-metaclass (linkable-metaclass validatable-class)
  ()
  (:documentation "This metaclass will infer most data from the database"))
(defmethod validate-superclass ((a dbi-metaclass) (b standard-class))
  T)
(defclass dbi-class (linkable-class validatable-object)
  ()
  (:metaclass dbi-metaclass)
  (:documentation "This class is supposed to be used with the dbi-metaclass"))

(defmethod slot-value-using-class :around ((class dbi-metaclass) (object dbi-class) (slot quicksearch-id-effective-slot))
  (let ((slot-name (slot-name (direct-slot slot))))
    (or (and (slot-boundp object slot-name) (call-next-method))
	(progn (setf (slot-value object slot-name) (with-ensured-connection
						     (query (:raw (format nil "select nextval(pg_get_serial_sequence('~A','~A'));"
									  (s-sql:to-sql-name (database-table class))
									  (s-sql:to-sql-name (column (direct-slot slot)))))
							    :single)))))))

(defmacro add-unless-existant (place getter value)
  "Simple macro to skip typing when assigning to plists"
  `(unless (getf ,place ,getter)
     (setf ,place (concatenate 'list ,place (list ,getter ,value)))))

(defun complete-slot-definition (class-name column slot-definition direct-slots package)
  (if (cl-ppcre:scan-to-strings "_ids?$" (s-sql:to-sql-name column))
      (let ((new-slot-definition slot-definition))
	(unless (getf new-slot-definition :id)
	  (setf new-slot-definition (concatenate 'list new-slot-definition (list :id T))))
	(list new-slot-definition (build-accompanying-linked-slot class-name new-slot-definition direct-slots package)))
      (list slot-definition)))
  
(defun build-accompanying-linked-slot (class-name slot-definition direct-slots package)
  (declare (ignore class-name))
  (let* ((name (intern (format nil "~A" (s-sql:from-sql-name
					 (elt (second 
					       (multiple-value-list 
						(cl-ppcre:scan-to-strings 
						 "(.*)_ids?"
						 (s-sql:to-sql-name (getf slot-definition :column)))))
					      0)))
		       package))
	 (accompanying-slot (or (find-if (lambda (direct-slot) (eql (getf direct-slot :internal-slot) (getf slot-definition :name)))
					 direct-slots)
				(list :name name))))
    (add-unless-existant accompanying-slot :external-class name)
    (add-unless-existant accompanying-slot :slot (getf slot-definition :name))           ;;
    (add-unless-existant accompanying-slot :readers (list name))
    (add-unless-existant accompanying-slot :writers (list (list 'setf name)))
    (add-unless-existant accompanying-slot :initargs (list (getf slot-definition :column)))
    (add-unless-existant accompanying-slot :i-am-obediant-p T)
    accompanying-slot))
				      
(defun create-slot-definition (class-name column direct-slots package)
  (let* ((name (intern (format nil "~A" column) package)))
    (let ((statements (list :name name 
			    :column column 
			    :initargs (list column)
			    :readers (list name)
			    :writers (list (list 'setf name)))))
      (if (cl-ppcre:scan "_ids?$" (s-sql:to-sql-name column))
	  (let ((current (concatenate 'list statements (list :id T))))
	    (list current (build-accompanying-linked-slot class-name current direct-slots package)))
	  (list statements)))))

(defun build-linked-slot-description (table table-description name direct-slots package)
  (let ((my-single-link-name (concatenate 'string (s-sql:to-sql-name name) "_id"))
	(my-multi-link-name (concatenate 'string (s-sql:to-sql-name name) "_ids")))
    (let* ((assumed-class-name (intern (format nil "~A" table) package))
	   (single-spec (find-if (lambda (column-spec) (string= (first column-spec) my-single-link-name)) table-description))
	   (multi-spec (find-if (lambda (column-spec) (string= (first column-spec) my-multi-link-name)) table-description))
	   (new-slot-definition (find assumed-class-name direct-slots :key (lambda (slot-description) (getf slot-description :external-class)))))
      (when (or single-spec multi-spec)
	(add-unless-existant new-slot-definition :name assumed-class-name)
	(add-unless-existant new-slot-definition :external-class assumed-class-name)
	(add-unless-existant new-slot-definition :i-am-master-p T)
	(add-unless-existant new-slot-definition :readers (list assumed-class-name))
	(add-unless-existant new-slot-definition :writers (list (list 'setf assumed-class-name))))
      (cond (multi-spec
	     (add-unless-existant new-slot-definition :amount :many)
	     (add-unless-existant new-slot-definition :slot (intern (format nil "~A" (s-sql:from-sql-name my-multi-link-name)) package)))
	    (single-spec
	     (add-unless-existant new-slot-definition :amount :one)
	     (add-unless-existant new-slot-definition :slot (intern (format nil "~A" (s-sql:from-sql-name my-single-link-name)) package))))
      new-slot-definition)))

(defmethod shared-initialize :around ((dsm dbi-metaclass) slot-names &rest args &key package name table direct-slots &allow-other-keys)
  (with-ensured-connection
    (let* ((package (or (symbol-package  (first package)) (symbol-package name)))
	   (name (or name (class-name dsm)))
	   (table (or (first table) (intern (format nil "~A" name) (symbol-package :table)))))
      (format T "~&oh, we've set the table to ~A~%" table)
      (format T "we saw ~A pass by as the name of the class~%" name)
      (format T "before I forget to tell you, everything will be interned in ~A~%" package)
      (if (table-description table)
	  (let ((updated-slot-definitions
		 (loop for column in (map 'list (lambda (description) (s-sql:from-sql-name (first description))) (table-description table))
		    for slot-definition = (find column direct-slots :key (lambda (direct-slot) (getf direct-slot :column)))
		    append (if slot-definition
			       (complete-slot-definition name column slot-definition direct-slots package)
			       (create-slot-definition name column direct-slots package)))))
	    (loop for slot-definition in direct-slots
	       for column = (getf slot-definition :column)
	       when (and column (not (find column updated-slot-definitions :key (lambda (slot-definition) (getf slot-definition :column)))))
	       do (push slot-definition updated-slot-definitions))
	    (loop for table in (list-tables nil)
	       for table-description = (table-description table)
	       for slot-description = (build-linked-slot-description table table-description name direct-slots package)
	       when slot-description
	       do (push slot-description updated-slot-definitions))
	    (loop for slot-definition in direct-slots
	       do (unless (find (getf slot-definition :name)
				updated-slot-definitions
				:key (lambda (x) (getf x :name)))
		    (push slot-definition updated-slot-definitions)))
	    (let ((new-args (concatenate 'list (loop for (key value) on args by #'cddr
						  append (cond ((eql key :table)
								nil)
							       ((eql key :direct-slots)
								(list :direct-slots updated-slot-definitions))
							       (T (list key value))))
					 (list :table (list table)))))
	      (progn (format T "~&I've generated the following from the table:~%")
		     (princ new-args T)
		     (format T "~&You can extract the needed variables from there, if you'd need to tweak or debug~%"))
	      (apply #'call-next-method dsm slot-names new-args)))
	  (call-next-method)))))

(defmethod save :around ((object dbi-class))
  (when (valid-p object)
    (call-next-method)))

(defmethod can-save-object-in-list-p :around ((object dbi-class) (other-objects list))
  (and (valid-p object)
       (call-next-method)))