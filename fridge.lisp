(defpackage :fridge
  (:use :common-lisp :postmodern :closer-mop :versioned-objects :validations)
  (:export :load-instance
	   :load-instances
	   :find-or-create-instance
	   :save :save-objects :save-quickstore
	   :delete-instance
	   :db=
	   :quickclear
	   :quickstore-again
	   :dbi-class :dbi-metaclass
	   :with-quickstore
	   :complete-ring
	   :complete-stored-ring))

(defpackage :fridge-user
  (:use :common-lisp :fridge))

(in-package :fridge)
(declaim (optimize (speed 0) (safety 3) (debug 3)))

(defconstant +db-snapshot+ '+db-snapshot+ "Snapshot of the state of the object in the database")

(defgeneric load-instance (class &key &allow-other-keys)
  (:documentation "Loads an object, its superclasses and its subclasses from the database.
The order in which the objects are loaded is unspecified."))
(defgeneric reload-instance (object)
  (:documentation "Reloads the instance from the database.  This can help in setting the ids of certain objects"))
(defgeneric load-instances (class &key s-sql-rewriter &allow-other-keys)
  (:documentation "Loads a series of objects matching the given initargs, their superclasses and their subclasses from the database.
The order in which the objects are loaded internally and externally, is unspecified.
If no objects in the database match the initargs, the empty list is returned."))
(defgeneric load-instance-by-slot-names (class &rest initargs)
  (:documentation "Loads the first instance that matches the given slot names"))
(defgeneric save (object)
  (:documentation "Updates the correct row(s) in the database, so it represents the values that are currently set."))
(defgeneric save-objects (objects)
  (:documentation "Saves the objects in the list of objects"))
(defgeneric can-save-object-in-list-p (object objects)
  (:documentation "Checks if <object> can be saved when the list of other objects to be saved is <objects>.  Returns T when this is ok for <object>, nil otherwise."))
(defgeneric delete-instance (object)
  (:documentation "Removes the rows from the database that represented this object."))
(defgeneric db= (a &rest args)
  (:documentation "Wether or not the objects have the same state in the database.  It assumes that the objects have been loaded from the database and will not reload them.  This effectively allows you to test if an object would receive another state in the database."))

(defgeneric db-backed-slot-p (class slot)
  (:documentation "Returns true iff this slot is backed by the database"))
(defgeneric db-backed-slot-names (class)
  (:documentation "Returns the names of the slots that are backed by the database, for the given class"))
(defgeneric db-backed-slots (class)
  (:documentation "Returns the slots that are backed by a database, for the given class"))
(defgeneric slot-name (slot)
  (:documentation "Returns the name of the given slot"))
(defgeneric find-slot-by-column (class column-name)
  (:documentation "Finds the slot that is related to the given column name for the given class"))
(defgeneric find-column-by-slot (class slot-name)
  (:documentation "Finds the column that is related to the given slot name for the given class"))
(defgeneric find-slot-name-by-column (class column-name)
  (:documentation "Finds the slot name that is related to the given column name for the given class"))
(defgeneric find-column-by-initarg (class initarg)
  (:documentation "Finds the column that is specified by the given initarg"))
(defgeneric find-slot-by-initarg (class initarg)
  (:documentation "Finds the slot that has the given initarg."))
(defgeneric object-in-database-p (object)
  (:documentation "Whether or not <object> has a database representation (which may be out-of-date)"))
(defgeneric object-up-to-date-p (object)
  (:documentation "Whether or not <object> has changed its representation since its last load or save.  If the object has no known database-representation, nil is returned."))
(defgeneric object-updated-database-state (object)
  (:documentation "A plist of the columns and their values within the object, which represents the state the object currently has with respect to the database."))
(defgeneric object-current-database-state (object)
  (:documentation " A plist of the colemns and their values within the object, which represents the last known state the object had in the database.  This is the state it had its last load or save."))
(defgeneric and-query-from-initargs (class &rest initargs)
  (:documentation "Returns an s-sql and-query that represents the way the initargs would be interpreted in the database"))
(defgeneric set-slots-from-column-alist (class object &rest column-alist)
  (:documentation "Sets the slot-values from the given column alist. After setting the slots, the snapshot is set to the current values in the object."))
(defgeneric load-instance-from-column-alist (class &rest column-alist)
  (:documentation "Creates a new instance and sets the values from the column alist as its standard values."))
(defgeneric and-query-from-initslots (class &rest initslots)
  (:documentation "Creates an and-query from the slots and the values they should have"))


(defclass db-support-metaclass (standard-class)
  ((database-table :initarg :database-table
		   :accessor database-table))
  (:documentation "This is the most basic form of a class that has access to the database.  There are a range of slots that have a database backing. The slots must be specified manually and can be set/retrieved through save and load.

eg: (defclass user ()
      ((name :column :name
             :initarg :name
             :accessor :name)
       (email :column :email-address
              :initarg :email
              :accessor email))
      (:table :users)
      (:metaclass db-support-metaclass))"))
(defmethod shared-initialize :after ((dsm db-support-metaclass) slot-names &rest args &key table &allow-other-keys)
  (declare (ignore slot-names args))
  (setf (database-table dsm) (first table)))

(defclass column-direct-slot (standard-direct-slot-definition)
  ((column :initarg :column
	   :accessor column))
  (:documentation "A direct slot that is linked to the column in the object's table"))
(defclass direct-effective-slot (standard-effective-slot-definition)
  ((direct-slot :initarg :direct-slot
		:accessor direct-slot))
  (:documentation "An effective slot that contains its linked direct slot"))

(defmethod closer-mop:validate-superclass ((a db-support-metaclass) (b standard-class))
  T)

(defclass db-support-class (versioned-object)
  ()
  (:documentation "The superclass for all classes that have the db-support-metaclass")
  (:metaclass db-support-metaclass))

(defmethod shared-initialize :after ((cds column-direct-slot) slot-names &rest args &key column &allow-other-keys)
  (declare (ignore slot-names args))
  (setf (column cds) column))

(defmethod direct-slot-definition-class :around ((class db-support-metaclass) &key column &allow-other-keys)
  (if column
      (find-class 'column-direct-slot)
      (call-next-method)))

(defmethod database-table ((class symbol))
  (database-table (find-class class)))

;; begin effective-slot-definition-hacks
(defparameter *direct-slots* nil "Postmodern trick to send the direct slots to the effective-slot-definition-class method")
(defgeneric initialize-extra-info (slot args)
  (:documentation "Initializes the extra information that an effective slot could get from the direct slots"))
(defmethod initialize-extra-info ((slot direct-effective-slot) args)
  (setf (direct-slot slot) (first args))
  slot)
(defmethod initialize-extra-info (a b)
  (declare (ignore b))
  a)
(defmethod compute-effective-slot-definition :around ((class db-support-metaclass) name args)
  (let ((*direct-slots* args))
    (initialize-extra-info (call-next-method) args)))
(defmethod effective-slot-definition-class :around ((class db-support-metaclass) &rest args)
  (declare (ignore args))
  (cond ((find (find-class 'column-direct-slot) *direct-slots* :key #'class-of)
	 (find-class 'direct-effective-slot))
	(T
	 (call-next-method))))

;; end effective-slot-definition-hacks
(defmethod db-backed-slot-p (class slot)
  (declare (ignore class slot))
  nil)
(defmethod db-backed-slot-p (class (slot column-direct-slot))
  (declare (ignore class))
  T)

(defmethod slot-name (slot)
  (slot-value slot 'sb-pcl::name))
(defmethod db-backed-slots ((class standard-class))
  (remove-if-not (lambda (slot) (db-backed-slot-p class slot))
		 (closer-mop:class-direct-slots class)))
(defmethod db-backed-slot-names ((class standard-class))
  (map 'list #'slot-name (db-backed-slots class)))

(defmethod db= ((object db-support-class) &rest args)
  (if args
      (and (not (find-if-not 
		 (lambda (slot-name)
		   (equalp (slot-value object slot-name)
			   (slot-value (first args) slot-name)))
		 (db-backed-slot-names (class-of object))))
	   (apply #'db= args))
      T))

(defmethod object-in-database-p ((object db-support-class))
  (snapshot-p object +db-snapshot+))

(defmethod object-up-to-date-p ((object db-support-class))
  (and (object-in-database-p object)
       (not (changed-p object +db-snapshot+))))

(defmethod object-updated-database-state ((object db-support-class))
  (loop for slot in (db-backed-slots (class-of object))
     when (slot-boundp object (slot-name slot))
     append (list (column slot)
		  (slot-value object (slot-name slot)))))
(defmethod object-current-database-state ((object db-support-class))
  (when (object-in-database-p object) 
    (loop for slot in (db-backed-slots (class-of object))
       when (slot-boundp object (slot-name slot))
       append (list (column slot)
		    (snapshot-value object +db-snapshot+ (slot-name slot))))))

(defmethod find-slot-by-initarg ((class standard-class) (initarg symbol))
  (flet ((slot-has-initarg-p (slot initarg)
	   (find initarg (slot-value slot 'sb-pcl::initargs))))
    (find-if (lambda (slot) (slot-has-initarg-p slot initarg))
	     (class-direct-slots class))))

(defmethod find-column-by-slot ((class db-support-metaclass) (slot-name symbol))
  (let ((slot (find slot-name (db-backed-slots class) :key #'slot-name)))
    (unless slot
      (error "Could not find slot for ~A and ~A" class slot-name))
    (when slot
      (column slot))))

(defmethod find-column-by-initarg ((class db-support-metaclass) (initarg symbol))
  (column (find-slot-by-initarg class initarg)))

(defmethod find-slot-by-column ((class db-support-metaclass) (column symbol))
  (find column (db-backed-slots class) :key #'column))
(defmethod find-slot-name-by-column ((class db-support-metaclass) (column symbol))
  (slot-name (find-slot-by-column class column)))

(define-condition record-not-found-error (error)
  ((db-table :initarg :table :reader db-table)
   (where :initarg :where :reader where-clause)
   (query :initarg :query :reader db-query))
  (:documentation "This error is thrown when a certain record could not be found.  This can happen whilst trying to load a certain instance."))

(defun get-query-alist (query &optional (rewriter (lambda (query) query)))
  (let ((query-result (query (sql-compile (funcall rewriter query)) :alists)))
    (restart-case (unless query-result
		    (error 'record-not-found-error :query query))
       (provide-alist (alist) (return-from get-query-alist alist))
      (retry () (get-query-alist query)))
    query-result))

(defmethod and-query-from-initargs ((class symbol) &rest initargs)
  (apply #'and-query-from-initargs (find-class class) initargs))
(defmethod and-query-from-initargs ((class db-support-metaclass) &rest initargs)
  (let ((initcolumns (loop for initarg on initargs by #'cddr append
			  (list (find-column-by-initarg class (first initarg)) (second initarg)))))
    (when initcolumns
      (cons :and
	   (loop for initcol on initcolumns by #'cddr collect
		(list := (first initcol) (second initcol)))))))

(defmethod and-query-from-initslots ((class symbol) &rest initslots)
  (apply #'and-query-from-initslots (find-class class) initslots))
(defmethod and-query-from-initslots ((class db-support-metaclass) &rest initargs)
  (let ((initcolumns (loop for initarg on initargs by #'cddr append
			  (list (find-column-by-slot class (first initarg))
				(second initarg)))))
    (when initcolumns
      (cons :and
	   (loop for initcol on initcolumns by #'cddr collect
		(list := (first initcol) (second initcol)))))))


(defmethod set-slots-from-column-alist ((class db-support-metaclass) (object db-support-class) &rest column-alist)
  (loop for (column-name . value) in column-alist
     do (setf (slot-value object (find-slot-name-by-column (class-of object) column-name)) value))
  (setf (versioned-objects::versioned-variables object)
	(loop for slot in (class-direct-slots class) when (db-backed-slot-p class slot) collect (slot-name slot)))
  (snapshot object +db-snapshot+)
  object)

(defmethod load-instance-from-column-alist ((class symbol) &rest column-alist)
  (apply #'load-instance-from-column-alist (find-class class) column-alist))
(defmethod load-instance-from-column-alist ((class db-support-metaclass) &rest column-alist)
  (apply #'set-slots-from-column-alist class (make-instance class) column-alist))

(defmethod load-instance ((class symbol) &rest initargs)
  (apply #'load-instance (find-class class) initargs))
(defmethod load-instance ((class db-support-metaclass) &rest initargs)
  (first (apply #'load-instances class initargs)))

(defmethod load-instances ((class symbol) &rest initargs)
  (apply #'load-instances (find-class class) initargs))
(defmethod load-instances ((class db-support-metaclass) &rest initargs &key (s-sql-rewriter (lambda (x) x) s-sql-rewriter-p))
  (when s-sql-rewriter-p
    (setf initargs (cddr initargs)))
  (let* ((where-clause (apply #'and-query-from-initargs class initargs))
	 (complete-query `(:select '* :from ,(database-table class) ,@(when where-clause `(:where ,where-clause)))))
    (handler-case (loop for alist in (get-query-alist complete-query s-sql-rewriter)
		     collect (apply #'load-instance-from-column-alist class alist))
      (record-not-found-error () nil))))
(defmethod load-instances-by-slot-names ((class symbol) &rest initargs)
  (apply #'load-instances-by-slot-names (find-class class) initargs))
(defmethod load-instances-by-slot-names ((class db-support-metaclass) &rest initargs)
  (let* ((where-clause (apply #'and-query-from-initslots class initargs))
	 (complete-query `(:select '* :from ,(database-table class) ,@(when where-clause `(:where ,where-clause)))))
    (handler-case (loop for alist in (get-query-alist complete-query)
		     collect (apply #'load-instance-from-column-alist class alist))
      (record-not-found-error () nil))))
(defmethod load-instance-by-slot-names ((class symbol) &rest initargs)
  (first (apply #'load-instances-by-slot-names (find-class class) initargs)))
(defmethod load-instance-by-slot-names ((class db-support-metaclass) &rest initargs)
  (first (apply #'load-instances-by-slot-names class initargs)))

(define-condition inconsistent-database-error (error)
  ((object :initarg :object :reader object)
   (where :initarg :where :reader where-clause)
   (set :initarg :set :reader set-clause :initform nil))
  (:documentation "Error that will be thrown when an action can not be fulfilled due to a changed database schema."))

(defgeneric where-clause (object)
  (:documentation "The where-clause that can be used to find the given object in the database"))
(defmethod where-clause ((object db-support-class))
  (cons :and 
	(loop for (column value) on (object-current-database-state object) by #'cddr
	   collect (list := column value))))
(defgeneric set-clause (object)
  (:documentation "The set-clause that can be used to set the values of the given object in the database"))
(defmethod set-clause ((object db-support-class))
  (object-updated-database-state object))

(defmethod save ((object db-support-class))
  (unless (and (object-in-database-p object)
	       (object-up-to-date-p object))
    ;; we need to save our inner class
    (flet ((update-database (object set &optional where)
	     (apply #'set-slots-from-column-alist
		    (class-of object) object 
		    (first
		     (if where
			 (query (sql-compile `(:update ,(database-table (class-of object)) :set ,@set :where ,where :returning '*)) :alists)
			 (query (sql-compile `(:insert-into ,(database-table (class-of object)) :set ,@set :returning '*)) :alists))))))
      (cond ((and (object-in-database-p object) (not (object-up-to-date-p object)))
	     (update-database object (set-clause object) (where-clause object)))
	    ((not (object-in-database-p object))
	     (update-database object (set-clause object)))))
    (snapshot object +db-snapshot+)))

(defmethod delete-instance (ignored)
  (declare (ignore ignored)) nil)
(defmethod delete-instance ((object db-support-class))
  (let* ((query `(:delete-from ,(database-table (class-of object)) :where ,(where-clause object))))
    (multiple-value-bind (unimportant updated-rows)
	(query (sql-compile query))
      (declare (ignore unimportant))
      (restart-case (progn 
		      (unless (> updated-rows 0)
			(error 'record-not-found-error
			       :table (database-table (class-of object))
			       :where (where-clause object)
			       :query query))
		      (rm-snapshot object +db-snapshot+))
	(retry () (delete-instance object))
	(continue () nil)))))

(defmethod reload-instance ((object db-support-class))
  (let ((class (class-of object)))
    (set-slots-from-column-alist class object (query (sql-compile `(:select * :from ,(database-table class) :where ,(where-clause object))))))
  object)

(defmethod can-save-object-in-list-p (a b)
  (declare (ignore a b))
  T)
(defmethod can-save-object-in-list-p ((object db-support-class) (objects list))
  T)
(defmethod save-objects ((objects list))
  (let ((invalid-objects (loop for object in objects unless (can-save-object-in-list-p object objects) collect object)))
    (if invalid-objects
	invalid-objects
	(loop for object in objects do (save object)))))
