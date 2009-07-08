(defpackage :fridge-tests
  (:use :common-lisp :fiveam :fridge :postmodern :database-migrations))

(in-package :fridge-tests)

;; connection
(setf *db-connection-parameters* '("fridge-tests" "tester" "password" "localhost"))
(connect-toplevel "fridge-tests" "tester" "password" "localhost")

;; suites
(def-suite manual :description "Tests for the manual system")
(def-suite only-columns :description "Test case that contains only basic saving and loading through the columns." :in manual)

;; migrations
(def-queries-migration 1247013639 "Creating the table for the books"
  :execute ("CREATE TABLE books ( revision integer , title text , description text , author text );"
	    "ALTER TABLE books OWNER TO tester;"
	    "COMMENT ON COLUMN books.revision IS 'number representing the revision of the book';"
	    "COMMENT ON COLUMN books.title IS 'the title on the cover of the book';"
	    "COMMENT ON COLUMN books.description IS 'a short description about the book.';"
	    "COMMENT ON COLUMN books.author IS 'the name of the main author of the book.';")
  :revert ("DROP TABLE books"))

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

(downgrade)
(upgrade)
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
      (let ((threw nil))
	(handler-case (load-instance 'book :title "river")
	  (record-not-found-error ()
	    (setf threw T)))
	(is (equal threw T) "must have thrown, can not load the book"))
      (save *book-river*)
      (is (db= *book-river* (load-instance 'book :title "river"))))

(test load-instances-test
      (is (eql nil (load-instances 'book :rev 20)) "There are no books with revision 20, and that doesn't throw an error, but returns nil")
      (is (= 2 (length (load-instances 'book :rev 0))) "There are two books with revesion 0")
      (is (find *book-bv* (load-instances 'book :rev 0) :test #'db=) "*book-bv* is one of the books with rev 0")
      (is (find *book-river* (load-instances 'book :rev 0) :test #'db=) "*book-river* is teh other book with rev 0"))