(ql:quickload '(cl-who hunchentoot parenscript cl-mongo))

(defpackage :gestao-eventos
  (:use :cl :cl-who :hunchentoot :parenscript :cl-mongo))

(in-package :gestao-eventos)

;; Domain model
;; ============

(defvar *member-ids* 0)

(defclass member ()
  ((member-id
    :initarg :member-id
    :reader member-id
    :initform (incf *member-ids*))
   (name
    :initarg :name
    :reader name)
   (admin
    :initarg :admin
    :reader admin
    :initform false)
   (plafond
    :initarg :plafond
    :reader plafond)
   (my-events
    :initarg :my-events
    :reader my-events)
   (username
    :initarg :username
    :reader username)
   (password
    :initarg :password
    :reader password)
   (email
    :initarg :email
    :reader email)
   (contact
    :initarg :contact
    :reader contact)
   (birth-date
    :initarg :birth-date
    :reader birth-date)
   (tshirt-size
    :initarg :tshirt-size
    :reader tshit-size)
   (cc
    :initarg :cc
    :reader cc)))

(defvar *event-ids* 0)

(defclass event ()
  ((event-id
    :initarg :event-id
    :reader event-id
    :initform (incf *event-ids*))
   (event-date
    :initarg :event-date
    :reader event-date)
   (created-by
    :initarg :created-by
    :reader created-by)
   (address
    :initarg :address
    :reader address)
   (postal-code
    :initarg :postal-code
    :reader postal-code)
   (limit-inscription-date
    :initarg :limit-inscription-date
    :reader limit-inscription-date)
   (web-url
    :initarg :web-url
    :reader web-url)
   (responsible
    :initarg :responsible
    :reader responsible)
   (participants
    :initarg :participants
    :reader participants)
   (activities
    :initarg :activities
    :reader activities)))

(defclass activity ()
  ((type
    :initarg :type
    :reader type)
   (distance
    :initarg :distance
    :reader distance)
   (duration
    :initarg :duration
    :reader duration)
   (cost
    :initarg :cost
    :reader cost)))

(defclass plafond ()
  ((year
    :initarg :year
    :reader year)
   (initialPlafond
    :initarg :initialPlafond
    :reader initialPlafond)))

(defclass activity-sign-up ()
  ((event
    :initarg :event
    :reader event)
   (traveled-distance
    :initarg :traveled-distance
    :reader traveled-distance)
   (was-present
    :initarg :was-present
    :reader was-present)
   (ended-event
    :initarg :ended-event
    :reader ended-event)
   (winner
    :initarg :winner
    :reader winner)
   (activity
    :initarg :activity
    :reader activity)))


;; Backend
;; =======

;; Pre-requisite: a mongod daemon process runs on localhost
;; using the default port.
;; Here we establish a connection to the database games that
;; we'll use for all storage:
(cl-mongo:db.use "members")
(cl-mongo:db.use "events")
(cl-mongo:db.use "activities")
(cl-mongo:db.use "activities-sign-up")
(cl-mongo:db.use "plafonds")

;; We store all game documents in the following collection:
(defparameter *member-collection* "member")
(defparameter *event-collection* "event")
(defparameter *event-collection* "activities")
(defparameter *event-collection* "activities-sign-up")
(defparameter *event-collection* "plafonds")

;; We encapsulate all knowledge of the concrete storage
;; medium in the following functions:

(defun member->doc (member)
  ($ ($ "member-id" (member-id member))
     ($ "name" (name member))
     ($ "admin" (admin member))
     ($ "plafond" (plafond member))
     ($ "my-events" (my-events member))
     ($ "username" (username member))
     ($ "password" (password member))
     ($ "email" (email member))
     ($ "contact" (contact member))
     ($ "birth-date" (birth-date member))
     ($ "tshirt-size" (tshirt-size member))
     ($ "cc" (cc member))))

(defun event->doc (event)
  ($ ($ "event-id" (event-id member))
     ($ "event-date" (event-date member))
     ($ "created-by" (created-by member))
     ($ "address" (address member))
     ($ "postal-code" (postal-code member))
     ($ "limit-inscription-date" (limit-inscription-date member))
     ($ "web-url" (web-url member))
     ($ "responsible" (responsible member))
     ($ "participants" (participants member))
     ($ "activities" (activities member))
     ($ "tshirt-size" (tshirt-size member))))

(defun activity->doc (activity)
  ($ ($ "type" (type activity))
     ($ "ditance" (distance activity))
     ($ "duration" (duration activity))
     ($ "cost" (cost activity))))

(defun plafond->doc (plafond)
  ($ ($ "year" (type activity))
     ($ "initial-plafond" (initial-plafond activity))))

(defun activity-sign-up->doc (activity-sign-up)
  ($ ($ "event" (event activity-sign-up))
     ($ "traveled-distance" (activity-sign-up activity-sign-up))
     ($ "was-present" (was-present activity-sign-up))
     ($ "ended-event" (ended-event memberactivity-sign-up))
     ($ "winner" (winner activity-sign-up))
     ($ "activity" (activity activity-sign-up))))


(defun doc->member (member-doc)
  (make-instance 'member :member-id (get-element "member-id" member-doc)
                         :name (get-element "name" member-doc)
                         :admin (get-element "admin" member-doc)
                         :plafond (get-element "plafond" member-doc)
                         :my-events (get-element "my-events" member-doc)
                         :username (get-element "username" member-doc)
                         :password (get-element "password" member-doc)
                         :email (get-element "email" member-doc)
                         :contact (get-element "contact" member-doc)
                         :birth-date (get-element "birth-date" member-doc)
                         :tshirt-size (get-element "tshirt-size" member-doc)
                         :cc (get-element "cc" member-doc)))

(defun doc->event (event-doc)
  (make-instance 'event :event-id (get-element "event-id" event-doc)
                        :name (get-element "name" event-doc)
                        :admin (get-element "admin" event-doc)
                        :plafond (get-element "plafond" event-doc)
                        :my-events (get-element "my-events" event-doc)
                        :username (get-element "username" event-doc)
                        :password (get-element "password" event-doc)
                        :email (get-element "email" event-doc)
                        :contact (get-element "contact" event-doc)
                        :birth-date (get-element "birth-date" event-doc)
                        :tshirt-size (get-element "tshirt-size" event-doc)
                        :cc (get-element "cc" event-doc)))

(defun doc->activity-sign-up (activity-sign-up-doc)
  (make-instance 'activity-sign-up :event (get-element "event" eventactivity-sign-up-doc)
                                   :traveled-distance (get-element "traveled-distance" activity-sign-up-doc)
                                   :was-present (get-element "was-present" activity-sign-up-doc)
                                   :ended-event (get-element "ended-event" activity-sign-up-doc)
                                   :winner (get-element "winner" activity-sign-up-doc)
                                   :activity (get-element "activity" activity-sign-up-doc)))

(defun doc->plafond (plafond-doc)
  (make-instance 'plafond :year (get-element "year" plafond-doc)
                          :initial-plafond (get-element "initial-plafond" plafond-doc)))

(defun activity->plafond (activity-doc)
  (make-instance 'activity :type (get-element "type" activity-doc)
                           :distance (get-element "distance" activity-doc)
                           :duration (get-element "duration" activity-doc)
                           :cost (get-element "cost" activity-doc)))


