(in-package :situated-program-challenge)

(defmacro defroute (name (params &rest route-args) &body body)
  `(setf (ningle:route *app* ,name ,@route-args)
         (lambda (,params)
           (declare (ignorable ,params))
           ,@body)))

(defparameter *app* (make-instance 'ningle:<app>))
(defparameter *handler* nil)

(defun start (&key (port 5000))
  (setf *handler*
	(clack:clackup *app*
                       :server :woo
                       :use-default-middlewares nil
                       :port port)))

(defun stop () (clack:stop *handler*))

(defun re-start () (stop) (start))
