(ql:quickload '(cl-who hunchentoot parenscript cl-mongo))

(defpackage :gestao-eventos
  (:use :cl :cl-who :hunchentoot :parenscript :cl-mongo))

(in-package :gestao-eventos)

;; Domain model
;; ============

(defclass person ()
  ((name  :reader   name
          :initarg  :name)
   (races :accessor races
          :initarg :races ; when read from persistent storage
          :initform 0)))

(defmethod print-object ((object person) stream)
  (print-unreadable-object (object stream :type t)
    (with-slots (name races) object
      (format stream "name: ~s with ~d races" name races))))

(defmethod add-race (person)
  (incf (races person)))

;; Backend
;; =======

;; Pre-requisite: a mongod daemon process runs on localhost
;; using the default port.
;; Here we establish a connection to the database games that
;; we'll use for all storage:
(cl-mongo:db.use "persons")

;; We store all game documents in the following collection:
(defparameter *person-collection* "person")

;; We encapsulate all knowledge of the concrete storage
;; medium in the following functions:

(defun person->doc (person)
  ($ ($ "name" (name person))
     ($ "races" (races person))))

(defun doc->person (person-doc)
  (make-instance 'person :name (get-element "name" person-doc)
                       :races (get-element "races" person-doc)))

(defmethod add-race :after (person)
  "In this method we update the races in the persistent storage.
   An after method in CLOS gives us an Observer-like behaviour;
   once the primary method has run, CLOS invokes our after method."
  (let ((person-doc (person->doc person)))
    (db.update *person-collection* ($ "name" (name person)) person-doc)))

(defun person-from-name (name)  
  (let ((found-persons (docs (db.find *person-collection* ($ "name" name)))))
    (when found-persons
      (doc->person (first found-persons)))))

(defun person-stored? (name)
  (person-from-name name))

(defun persons ()
  "Returns a sequence of all persons, sorted on
   their number of races in descending order.
   The implementation is straightforwards since
   cl-mongo provides a db.sort macro. We just need
   to remember that we get a lazy sequence back and
   have to realize it (iterate to the end) using iter."
  (mapcar #'doc->person
          (docs (iter (db.sort *person-collection* :all
                               :field "races"
                               :asc nil)))))

(defun unique-index-on (field)
  (db.ensure-index *person-collection*
                   ($ field 1)
                   :unique t))

;; We want to avoid duplicates. In the current version with
;; a limited domain model, the name alone is used for uniqueness.
;; As we evolve the domain we probably want to modify this constraint too.
(unique-index-on "name")

(defun add-person (name)
  "Add a person with the given name to the database.
   In this version we don't check for duplicates."
  (let ((person (make-instance 'person :name name)))
    (db.insert *person-collection* (person->doc person))))

;; Web Server - Hunchentoot

(defun start-server (port)
  (start (make-instance 'easy-acceptor :port port)))

(defun publish-static-content ()
  (push (create-static-file-dispatcher-and-handler
         "/corrida.jpg" "static/corrida.jpg") *dispatch-table*)
  (push (create-static-file-dispatcher-and-handler
         "/eventos.css" "static/eventos.css") *dispatch-table*))

;; DSL for our web pages
;; =====================

;; Here we grow a small domain-specific language for
;; creating dynamic web pages.

                                        ; Control the cl-who output format (default is XHTML, we
                                        ; want HTML5):
(setf (html-mode) :html5)

(defmacro standard-page ((&key title script) &body body)

  `(with-html-output-to-string
       (*standard-output* nil :prologue t :indent t)
     (:html :lang "en"
            (:head
             (:meta :charset "utf-8")
             (:title ,title)
             (:link :type "text/css"
                    :rel "stylesheet"
                    :href "/eventos.css")
             ,(when script
                `(:script :type "text/javascript"
                          (str ,script))))
            (:body
             (:div :id "header" ; gestao eventos header
                   (:img :src "/corrida.jpg"
                         :alt "corrida"
                         :class "logo")
                   (:span :class "strapline"
                          "Corridas de Membros!"))
             ,@body))))

;; HTML
;; ====

;; The functions responsible for generating the actual pages of our app go here.
;; We use the Hunchentoot macro define-easy-handler to automatically
;; push our uri to the dispatch table of the server and associate the
;; request with a function that will handle it.

(define-easy-handler (gestao-eventos :uri "/gestao-eventos") ()
  (standard-page (:title "Top Corridas Membros")
    (:h1 "Adicione corridas aos membros.")
    (:p "Membro não existe? Adicione" (:a :href "new-person" "aqui"))
    (:h2 "Ranking atual")
    (:div :id "chart" ; Used for CSS styling of the links.
          (:ol
	   (dolist (person (persons))
	     (htm
	      (:li (:a :href (format nil "race?name=~a" (url-encode ; avoid injection attacks
                                                         (name person))) "Adicione corrida.")
	           (fmt "~A with ~d races" (escape-string (name person))
                        (races person)))))))))

(define-easy-handler (new-person :uri "/new-person") ()
  (standard-page (:title "Adicione um membro novo."
                  :script (ps  ; client side validation
                            (defvar add-form nil)
                            (defun validate-person-name (evt)
                              "For a more robust event handling
                                     mechanism you may want to consider
                                     a library (e.g. jQuery) that encapsulates
                                     all browser-specific quirks."
                              (when (= (@ add-form name value) "")
                                (chain evt (prevent-default))
                                (alert "Introduza o nome.")))
                            (defun init ()
                              (setf add-form (chain document
                                                    (get-element-by-id "addform")))
                              (chain add-form
                                     (add-event-listener "submit" validate-person-name false)))
                            (setf (chain window onload) init)))
    (:h1 "Adicione um membro novo.")
    (:form :action "/person-added" :method "post" :id "addform"
           (:p "Qual é o nome da pessoa?" (:br)
               (:input :type "text" :name "name" :class "txt"))
           (:p (:input :type "submit" :value "Add" :class "btn")))))

(define-easy-handler (person-added :uri "/person-added") (name)
  (unless (or (null name) (zerop (length name))) ; In case JavaScript is turned off.
    (add-person name))
  (redirect "/gestao-eventos")) ; back to the front page

(define-easy-handler (race :uri "/race") (name)
  (when (person-stored? name)
    (add-race (person-from-name name)))
  (redirect "/gestao-eventos")) ; back to the front page

;; Alright, everything has been defined - launch Hunchentoot and have it
;; listen to incoming requests:
(publish-static-content)
(start-server 8080)