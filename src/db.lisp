(in-package :situated-program-challenge)

;; cd resources
;; cat migrations/*up.sql > psql-v1.sql
;; psql -f psql-v1.sql -U meetup -d meetup -h localhost

(defun connect ()
  (connect-toplevel :postgres :database-name "meetup" :username "meetup" :password "password123"))

(defmacro deftable (table-name superclass-list &body column-type-pairs)
  `(defclass ,table-name (,@superclass-list)
     ,(mapcar (lambda (col)
		(let* ((col-symbol (if (listp col) (car col) col))
		       (col-name (symbol-name col-symbol))
                       (col-type (if (listp col) (cadr col)))
                       (col-primary (if (find :primary-key col) t nil)))
		  (list col-symbol
			:accessor (intern (concatenate 'string (symbol-name table-name) "-" col-name))
			:initarg (intern col-name :keyword)
                        :col-type col-type
                        :primary-key col-primary)))
       column-type-pairs)
     (:metaclass dao-table-class)))

;;; Define tables

(deftable groups ()
  (name :text))

(deftable groups-members ()
  (group-id  :integer)
  (member-id :integer)
  (admin     :boolean))

(deftable meetups ()
  (title    :text)
  (start-at :timestamp)
  (end-at   :timestamp)
  (venue-id :integer)
  (group-id :integer))

(deftable meetups-members ()
  (meetup-id :integer)
  (member-id :integer))

(deftable members ()
  (first-name :text)
  (last-name  :text)
  (email      :text))

(deftable venues ()
  (name        :text)
  (postal-code :text)
  (prefecture  :text)
  (city        :text)
  (street1     :text)
  (street2     :text)
  (group-id    :integer))

(defun create-all-table ()
  (mapc (lambda (table)
          (execute-sql (car (table-definition table))))
        '(groups groups-members meetups meetups-members members venues)))

(defun clear-all-table ()
  (mapc (lambda (table)
          (mapcar #'delete-dao (select-dao table)))
        '(groups groups-members meetups meetups-members members venues)))
