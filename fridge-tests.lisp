(declaim (optimize (speed 0) (safety 3) (debug 3)))

(defpackage :fridge-tests
  (:use :common-lisp :fiveam :fridge :postmodern :database-migrations))

(in-package :fridge-tests)

;; connection
(setf *db-connection-parameters* '("fridge-tests" "tester" "password" "localhost"))
(connect-toplevel "fridge-tests" "tester" "password" "localhost")

;; suites
(def-suite manual :description "Tests for the manual system")
(def-suite only-columns :description "Test case that contains only basic saving and loading through the columns." :in manual)
(def-suite master-link :description "Test case that contains links in which the linked class is the master of the current class." :in manual)

;; migrations
(def-queries-migration 1247013639 "Creating the table for the books"
  :execute ("CREATE TABLE books ( revision integer , title text , description text , author text );"
	    "ALTER TABLE books OWNER TO tester;"
	    "COMMENT ON COLUMN books.revision IS 'number representing the revision of the book';"
	    "COMMENT ON COLUMN books.title IS 'the title on the cover of the book';"
	    "COMMENT ON COLUMN books.description IS 'a short description about the book.';"
	    "COMMENT ON COLUMN books.author IS 'the name of the main author of the book.';")
  :revert ("DROP TABLE books"))

(def-queries-migration 1247420820 "Creating a table for the master class"
  :execute ("CREATE TABLE master ( id SERIAL PRIMARY KEY, name TEXT );"
	    "ALTER TABLE master OWNER TO tester;"
	    "COMMENT ON COLUMN master.id IS 'id is the standard column for the identifier of the object'"
	    "COMMENT ON COLUMN master.name IS 'something to show that the object is still the same'")
  :revert ("DROP TABLE master"))
(def-queries-migration 1247421052 "Dependent for the master table"
  :execute ("CREATE TABLE dependent ( id SERIAL PRIMARY KEY, main_master_id INTEGER, master_id INTEGER, name TEXT );"
	    "ALTER TABLE dependent OWNER TO tester;"
	    "COMMENT ON COLUMN dependent.id IS 'standard identifier';"
	    "COMMENT ON COLUMN dependent.master_id IS 'link to a master object (there may be multiple objects with this link)';"
	    "COMMENT ON COLUMN dependent.main_master_id IS 'link to a master object (and I am the only one with that link)';"
	    "COMMENT ON COLUMN dependent.name IS 'something to manually see which object we roally have.'")
  :revert ("DROP TABLE dependent"))

(downgrade)
(upgrade)

(in-suite master-link)

(defclass master (fridge::linkable-class)
  ((id :column :id
       :reader id
       :id T)
   (name :column :name
	 :accessor name
	 :initarg :name)
   (dependent :i-am-master-p T
	      :external-class dependent
	      :slot main-master-id
	      :amount :one
	      :accessor dependent)
   (dependents :i-am-master-p T
	       :external-class dependent
	       :slot master-id
	       :amount :many
	       :accessor dependents))
  (:table :master)
  (:optimize-slot-access nil)
  (:metaclass fridge::linkable-metaclass))

(defclass dependent (fridge::linkable-class)
  ((id :column :id
       :reader id
       :id T)
   (name :accessor name
	 :initarg :name
	 :column :name)
   (main-master-id :column :main-master-id
		   :linkable-p T)
   (master-id :column :master-id
	      :linkable-p T)
   (master :i-am-obediant-p T
	   :external-class master
	   :slot master-id
	   :accessor master)
   (main-master :i-am-obediant-p T
		:external-class master
		:slot main-master-id
		:accessor main-master))
  (:optimize-slot-access nil)
  (:table :dependent)
  (:metaclass fridge::linkable-metaclass))

(defparameter *anja* (make-instance 'dependent :name "anja"))
(defparameter *eefje* (make-instance 'dependent :name "eefje"))
(defparameter *foofie* (make-instance 'master :name "foofie"))
(save *anja*)
(save *eefje*)
(save *foofie*)

(test assignments
  (is-false (dependents *foofie*) "No dependents have been assigned")
  (is-false (master *eefje*) "eefje knows of no master")
  (is-false (master *anja*) "anja knows of no master")
  (is-false (main-master *eefje*) "eefje has no main master")
  (is-false (main-master *anja*) "anja has no main master")
  (setf (dependents *foofie*) (list *eefje*))
  (is (= (length (dependents *foofie*)) 1) "foofie must have one dependent now")
  (is (eql (first (dependents *foofie*)) *eefje*) "foofie must have eefje as the first (and only) dependent")
  (is (eql (master *eefje*) *foofie*) "eefje must have foofie as standard master")
  (is-false (master *anja*) "anja still has no master set")
  (is-false (main-master *eefje*) "eefje still has no main-master set")
  (is-false (main-master *anja*) "anja still has no main-master set")
  (setf (dependents *foofie*) (list *anja* *eefje*))
  (is (= (length (dependents *foofie*)) 2) "foofie has both anja and eefje")
  (is (find *anja* (dependents *foofie*)) "foofie knows both ANJA and eefje")
  (is (find *eefje* (dependents *foofie*)) "foofie knows both anja and EEFJE")
  (is-false (or (main-master *anja*) (main-master *eefje*)) "neither anja, nor eefje have a main master")
  (setf (main-master *anja*) *foofie*)
  (is (eql (main-master *anja*) *foofie*) "Anja has foofie as her main master now")
  (is-false (main-master *eefje*) "eefje hasn't received a main master")
  (setf (dependents *foofie*) nil)
  (quickstore-again *anja*)
  (quickstore-again *eefje*)
  (is (eql (dependents *foofie*) nil) "All dependents have been removed")
  (is-false (master *eefje*) "All master should be removed, allso for eefje")
  (is-false (master *anja*) "All master should be removed, allso for anja")
  (is (eql (main-master *anja*) *foofie*) "Main master mustn't have been touched"))

(in-suite only-columns)

(defclass book (fridge::db-support-class)
  ((rev :initarg :rev
	:accessor revision
	:column :revision)
   (title :initarg :title
	  :accessor title
	  :column :title)
   (desc :initarg :description
	 :accessor description
	 :column :description)
   (author :initarg :author
	   :column :author
	   :accessor author))
  (:table :books)
  (:metaclass fridge::db-support-metaclass))

(defparameter *book-bv* (make-instance 'book :rev 0 :title "boison vitaminee" :description "Shake before consuming. Keep cool after opening and consume within three to four days." :author "ACE"))
(defparameter *book-river* (make-instance 'book :rev 0 :title "river" :description "Natuurlijk mineraalwater" :author "Fiorelino"))
(defparameter *book-reine* (make-instance 'book :rev 1 :title "reine" :description "Natuurijk mineraalwater" :author "SPA"))
(defparameter *book-frisdrank* (make-instance 'book :rev 2 :title "frisdrank" :description "Fristdrank met plantenextracten" :author "Coca-Cola"))

(save *book-bv*)
(save *book-river*)
(save *book-reine*)
(save *book-frisdrank*)

(test db=-test
      (is (db= *book-bv* (load-instance 'book :title "boison vitaminee")) "The saved book must be loaded now")
      (is (db= *book-river* (load-instance 'book :title "river")) "The saved book must be loaded now")
      (is (db= *book-reine* (load-instance 'book :title "reine")) "The saved book must be loaded now")
      (is (db= *book-frisdrank* (load-instance 'book :rev 2)) "We can also load through rev")
      (let ((other-book (load-instance 'book :title "river")))
	(is (db= *book-river* other-book) "The books are still equal")
	(setf (revision other-book) 20)
	(is-false (db= *book-river* other-book) "Changed the rev of the other book")
	(setf (revision *book-river*) 20)
	(is (db= *book-river* other-book) "Both books look the same again")
	(setf *book-river* (load-instance 'book :title "river"))))

(test update-test
      (let ((other-book (load-instance 'book :title "river")))
	(is (db= *book-river* other-book) "Starting from the same book")
	(setf (revision other-book) 20)
	(is-false (db= *book-river* other-book) "Changed other-book")
	(is (db= *book-river* (load-instance 'book :title "river")) "The database still has the old book")
	(is-false (db= other-book (load-instance 'book :title "river")) "The new version is not in the database yet")
	(save other-book)
	(is-false (db= *book-river* (load-instance 'book :title "river")) "Saved the changes, this is not the original anymore")
	(is (db= other-book (load-instance 'book :title "river")) "other-book is in the database right now")
	(setf (revision other-book) (revision *book-river*))
	(save other-book)
	(is (db= other-book *book-river*) "books have been changed to be the same again")
	(is (db= *book-river* (load-instance 'book :title "river")) "*book-river* is the same as the current database representation again")))

(test delete-instance-test
  (is (db= *book-river* (load-instance 'book :title "river")))
  (delete-instance *book-river*)
  (is-false (load-instance 'book :title "river") "can not load the book")
  (save *book-river*)
  (is (db= *book-river* (load-instance 'book :title "river"))))

(test load-instances-test
      (is (eql nil (load-instances 'book :rev 20)) "There are no books with revision 20, and that doesn't throw an error, but returns nil")
      (is (= 2 (length (load-instances 'book :rev 0))) "There are two books with revesion 0")
      (is (find *book-bv* (load-instances 'book :rev 0) :test #'db=) "*book-bv* is one of the books with rev 0")
      (is (find *book-river* (load-instances 'book :rev 0) :test #'db=) "*book-river* is the other book with rev 0"))
