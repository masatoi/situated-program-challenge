(in-package :situated-program-challenge)

(defmacro with-protect-to-json (&body body)
  `(handler-case
       `(200 (:content-type "application/json")
             (,(jojo:to-json (progn ,@body))))
     (error (e)
       `(500 (:content-type "application/json")
             (,(jojo:to-json (list :|error| (format nil "~A" e))))))))

(defun ambiguous-eq (key1 key2)
  (etypecase key2
    (string  (string= (symbol-name key1) (string-upcase key2)))
    (keyword (string= (symbol-name key1) (symbol-name key2)))
    (symbol  (eq key1 key2))))

(defmacro bind-alist (alist (&rest key-type-bindings) &body body)
  (macrolet ((with-gensyms ((&rest names) &body body)
               `(let ,(loop for n in names collect `(,n (gensym)))
                  ,@body)))
    (with-gensyms (alist-form)
      `(let ((,alist-form ,alist))
         (declare (ignorable ,alist-form))
         (let ,(loop for l in key-type-bindings
                     collect `(,(if (listp l) (car l) l)
                               (cdr (assoc ',(if (listp l) (car l) l) ,alist-form
                                           :test #'ambiguous-eq))))
           (declare ,@(remove-if #'null
                                 (mapcar (lambda (l)
                                           (if (listp l)
                                               (list 'type (cadr l) (car l))))
                                         key-type-bindings)))
           ,@body)))))

(defmacro define-api (url method (&rest param-type-bindings) &body body)
  (let ((params (gensym)))
    `(defroute ,url (,params :method ,method)
       (with-protect-to-json
         (bind-alist ,params (,@param-type-bindings) ,@body)))))
 
;;; Members ;;;

(defun members-dao->plist (dao)
  (list :|member-id|  (object-id dao)
        :|first-name| (members-first-name dao)
        :|last-name|  (members-last-name dao)
        :|email|      (members-email dao)))

(define-api "/members" :get ()
  (mapcar #'members-dao->plist (select-dao 'members)))

(define-api "/members" :post ((first-name string) (last-name string) (email string))
  (let ((dao (make-instance 'members
                            :first-name first-name
                            :last-name  last-name
                            :email      email)))
    (insert-dao dao)
    (members-dao->plist dao)))

(define-api "/members/:member-id" :get (member-id)
  (let ((dao (efind-dao 'members :id (parse-integer member-id))))
    (members-dao->plist dao)))

(define-api "/members/:member-id/meetups/:event-id" :post (member-id event-id)
  (let* ((member (efind-dao 'members :id (parse-integer member-id)))
         (meetup (efind-dao 'meetups :id (parse-integer event-id)))
         (meetups-members-dao
           (make-instance 'meetups-members :member-ref member :meetup-ref meetup)))
    (insert-dao meetups-members-dao)
    (meetups-dao->plist meetup)))

(define-api "/members/:member-id/groups/:group-id" :post ((admin boolean) member-id group-id)
  (let* ((member-dao (efind-dao 'members :id (parse-integer member-id)))
         (group-dao  (efind-dao 'groups  :id (parse-integer group-id)))
         (groups-members-dao
           (make-instance 'groups-members
                          :admin admin :member-ref member-dao :group-ref group-dao)))
    (insert-dao groups-members-dao)
    (groups-dao->plist* group-dao)))

;;; Groups ;;;

(defun groups-dao->plist* (groups-dao)
  (let* ((group-id (object-id groups-dao))
         (groups-members-list
           (select-dao 'groups-members (includes 'members)
             (where (:= :group-ref-id group-id))))
         (members-list (mapcar #'groups-members-member-ref groups-members-list))
         (admin-members-list
           (mapcar #'groups-members-member-ref
                   (remove-if-not #'groups-members-admin groups-members-list)))
         (venues-list  (select-dao 'venues  (where (:= :group-id group-id))))
         (meetups-list (select-dao 'meetups (where (:= :group-id group-id)))))
    (list :|group-id|   group-id
          :|group-name| (groups-name groups-dao)
          :|admin|      (mapcar #'members-dao->plist admin-members-list)
          :|venues|     (mapcar #'venues-dao->plist  venues-list)
          :|meetups|    (mapcar #'meetups-dao->plist meetups-list)
          :|members|    (mapcar #'members-dao->plist members-list))))

(define-api "/groups" :get ()
  (mapcar #'groups-dao->plist* (select-dao 'groups)))

(defun groups-dao->plist (groups-dao admin-members-dao-list)
  (list :|group-id|   (object-id groups-dao)
        :|group-name| (groups-name groups-dao)
        :|admin|      (mapcar #'members-dao->plist admin-members-dao-list)))

(define-api "/groups" :post ((admin-member-ids list) (group-name string))
  (let* ((group-dao (make-instance 'groups :name group-name))
         (members (if admin-member-ids (select-dao 'members (where (:in :id admin-member-ids)))))
         (groups-members-dao-list
           (mapcar (lambda (member-ref)
                     (make-instance 'groups-members :admin t :member-ref member-ref))
                   members)))
    (when (set-difference admin-member-ids (mapcar #'object-id members))
      (error "Not exist member"))
    (insert-dao group-dao)
    (dolist (dao groups-members-dao-list)
      (setf (groups-members-group-ref dao) group-dao)
      (insert-dao dao))
    (groups-dao->plist group-dao members)))

;;; Venues ;;;

(defun venues-dao->plist (dao)
  (list :|venue-id|   (object-id dao)
        :|venue-name| (venues-name dao)
        :|address| (list :|postal-code| (venues-postal-code dao)
                         :|prefecture|  (venues-prefecture dao)
                         :|city|        (venues-city dao)
                         :|address1|    (venues-street1 dao)
                         :|address2|    (venues-street2 dao))))

(define-api "/groups/:group-id/venues" :get (group-id)
  (mapcar #'venues-dao->plist
          (select-dao 'venues
            (where (:= :group-id (parse-integer group-id))))))

(define-api "/groups/:group-id/venues" :post ((venue-name string) (address list) group-id)
  (bind-alist address ((postal-code string) (prefecture string) (city string)
                       (address1 string) (address2 string))
    (let ((dao (make-instance 'venues
                              :name        venue-name
                              :venue-type  "physical"
                              :postal-code postal-code
                              :prefecture  prefecture
                              :city        city
                              :street1     address1
                              :street2     address2
                              :url         ""
                              :group-id    (parse-integer group-id))))
      (insert-dao dao)
      (venues-dao->plist dao))))

(defun online-venues-dao->plist (dao)
  (list :|online-venue-id| (object-id dao)
        :|venue-name|      (venues-name dao)
        :|url|             (venues-url  dao)))

(define-api "/groups/:group-id/online-venues" :get (group-id)
  (mapcar #'online-venues-dao->plist
          (select-dao 'venues
            (where (:= :group-id (parse-integer group-id))))))

;; (dex:get "http://localhost:5000/groups/1/online-venues")

(define-api "/groups/:group-id/online-venues" :post ((venue-name string) (url string) group-id)
  (let ((dao (make-instance 'venues
                            :name        venue-name
                            :venue-type  "online"
                            :postal-code ""
                            :prefecture  ""
                            :city        ""
                            :street1     ""
                            :street2     ""
                            :url         url
                            :group-id    (parse-integer group-id))))
    (insert-dao dao)
    (online-venues-dao->plist dao)))

;; (dex:post "http://localhost:5000/groups/1/online-venues"
;;           :content (jojo:to-json '(:venue-name "hoge" :url "http://hogehoge.com/"))
;;           :headers '(("content-type" . "application/json")))

;;; Meetups ;;;

(defun meetups-dao->plist (meetups-dao)
  (let* ((venues-dao (find-dao 'venues :id (meetups-venue-id meetups-dao)))
         (online-venue-dao (find-dao 'venues :id (meetups-online-venue-id meetups-dao)))
         (meetups-members-dao-list
           (select-dao 'meetups-members (includes 'members)
             (where (:= :meetup-ref-id (object-id meetups-dao)))))
         (members-dao-list
           (mapcar #'meetups-members-member-ref meetups-members-dao-list)))
    (list :|event-id| (object-id   meetups-dao)
          :|title|    (meetups-title    meetups-dao)
          :|start-at| (local-time:format-rfc3339-timestring nil (meetups-start-at meetups-dao))
          :|end-at|   (local-time:format-rfc3339-timestring nil (meetups-end-at meetups-dao))
          :|venue|    (venues-dao->plist venues-dao)
          :|online-venue| (online-venues-dao->plist online-venue-dao)
          :|members|  (mapcar #'members-dao->plist members-dao-list))))

(define-api "/groups/:group-id/meetups" :get (group-id)
  (mapcar #'meetups-dao->plist
          (select-dao 'meetups
            (where (:= :group-id (parse-integer group-id))))))


(define-api "/groups/:group-id/meetups" :post
    (group-id (venue-id integer) (online-venue-id integer) (end-at string) (start-at string) (title string))
  (let ((dao (make-instance 'meetups
                            :group-id (parse-integer group-id)
                            :venue-id venue-id
                            :online-venue-id online-venue-id
                            :end-at   (local-time:parse-rfc3339-timestring end-at)
                            :start-at (local-time:parse-rfc3339-timestring start-at)
                            :title    title)))
    (efind-dao 'venues :id (meetups-venue-id dao))
    (insert-dao dao)
    (meetups-dao->plist dao)))

(define-api "/groups/:group-id/meetups/:event-id" :get (event-id)
  (meetups-dao->plist
   (efind-dao 'meetups :id (parse-integer event-id))))
