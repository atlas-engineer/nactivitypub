;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package #:nactivitypub)

(defclass base ()
  ((id :initform nil
       :initarg :id
       :accessor id
       :type (or string null))
   (object-type
    :initform "Object"
    :initarg :object-type
    :accessor object-type
    :type string)
   (original-object
    :initform nil
    :initarg :original-object
    :accessor original-object
    :type (maybe hash-table)
    :documentation "The de-serialized JSON this object was created from.
Possibly contains some Lisp-inaccessible properties."))
  (:documentation "The base class for all the ActivityStreams types."))

(defun base-p (object)
  (typep object 'base))

;; TODO: Wipe/clean it periodically to allow feed updates?
(defvar *url->object* (make-hash-table :test 'equal)
  "A memoization table from URL string to the fetched objects.")

(defgeneric fetch-object (object &key &allow-other-keys)
  (:method ((object list) &key)
    (lparallel:pmapcar #'fetch-object object))
  (:method :around ((object string) &key)
    (alex:ensure-gethash object *url->object* (call-next-method)))
  (:method ((object string) &key auth-token)
    (ignore-errors
     (parse-object
      (j:decode
       (dex:get object :headers
                `(("Accept" . "application/ld+json; profile=\"https://www.w3.org/ns/activitystreams\"")
                  ,@(serapeum:and-let* ((auth auth-token)
                                        (full-token (str:concat "Bearer " auth)))
                      `(("Authorization" . ,full-token)))))))))
  (:method ((object quri:uri) &key)
    (fetch-object (quri:render-uri object)))
  (:documentation "Fetch the object from the provided OBJECT URL."))

(defgeneric send-object (object &key auth-token &allow-other-keys)
  (:method ((object list) &key)
    (lparallel:pmapcar #'send-object object))
  (:method ((object base) &key outbox auth-token)
    (multiple-value-bind (content code headers)
        (dex:post outbox
                  :headers `(("Content-Type" . "application/ld+json; profile=\"https://www.w3.org/ns/activitystreams\"")
                             ,@(alex:when-let* ((auth auth-token)
                                                (full-token (str:concat "Bearer " auth)))
                                 `(("Authorization" . ,full-token))))
                  :content (unparse-object object))
      (declare (ignore content))
      (cond
        ((and (= code 201) headers (hash-table-p headers))
         (or (gethash "Location" headers) t))
        ((= code 201) t)
        (t nil))))
  (:documentation "Send the OBJECT to the OUTBOX (should be a valid profile outbox link).
Returns the URL for the newly created object or T in case of success,
NIL otherwise"))

(defvar *classes* (make-hash-table :test 'equalp)
  "A map from ActivityStreams/ActivityPub type name to the Lisp-side class symbol.")

(defmethod initialize-instance :after ((object base) &key original-object)
  (let ((class-name (sera:class-name-of object)))
    (setf (object-type object)
          (or (when (hash-table-p original-object)
                (gethash "type" original-object))
              (block find-type
                (maphash (lambda (type class)
                           (when (eq class class-name)
                             (return-from find-type type)))
                         *classes*))
              "Object"))
    (when (hash-table-p original-object)
      (setf (id object) (gethash "id" original-object)))))

(defgeneric parse-object (object)
  (:method ((object t))
    object)
  (:method ((object list))
    (lparallel:pmapcar #'parse-object object))
  (:documentation "Parse the object from the provided JSON data.
Possibly recurse to the nested sub-objects."))

(defmethod parse-object ((object hash-table))
  (or (sera:and-let* ((type (gethash "type" object))
                      (class (gethash type *classes*)))
        (make-instance class :original-object object))
      object))

(defmethod parse-object ((object sequence))
  (lparallel:pmap (serapeum:class-name-of object) #'parse-object object))

(defmethod parse-object ((object string))
  (or (sera:and-let* ((fetched (fetch-object object)))
        (parse-object fetched))
      object))

(defgeneric unparse-object (object &optional hash)
  (:method :around (object &optional hash)
    (call-next-method object (or hash (make-hash-table :test 'equal))))
  (:method ((object t) &optional hash)
    hash)
  (:method ((object base) &optional (hash (make-hash-table :test 'equal)))
    (j:encode
     (or (original-object object)
         (sera:lret ((hash hash))
           (when (id object)
             (setf (gethash "id" hash) (id object)))
           (setf (gethash "type" hash) (object-type object))
           (setf (gethash "@context" hash) "https://www.w3.org/ns/activitystreams")))))
  (:documentation "Produce the JSON for the OBJECT.
Should always CALL-NEXT-METHOD with the half-filled HASH so that superclass methods fill it too."))

(defmacro define-json-type (name type (&rest superclasses) &body names-and-slots)
  "Define a JSON-serializable ActivityPub type with a Lisp class mirroring it.

NAME is an unquoted symbol naming the Lisp-side class.
TYPE is a string with a ActivityPub object type.
SUPERCLASSES are Lisp superclasses to fill slots from in addition to the direct ones.

NAMES-AND-SLOTS is (JSON-NAME &key LISP-NAME PROCESSOR DEPROCESSOR LITERAL-P)
forms list or just JSON-NAMEs as strings, where

- JSON-NAME is the string matching the JSON name of the property to fill the
  slot from.
- LISP-NAME is the (unquoted) symbol for the Lisp-side slot storing the information.
- PROCESSOR is a form evaluating to a function object. The function designated
  by this object should take the raw string value of a JSON-NAMEd property and
  produce a Lisp value matching it.
- DEPROCESSOR is a form evaluating to a function object. The resulting function
  takes a non-nil object and encodes it to a valid Lisp value serializable to
  JSON. DEPROCESSOR should ideally perfectly reverse the effect of the
  PROCESSOR.
- LITERAL-P denotes whether the object is in its final form when accessed. If
  LITERAL-P is false (it is by default, unless PROCESSOR is set), slot accessor
  might try to fetch the object from the remote URL and/or parse the slot
  value."
  (let ((normalized-slots
          (mapcar (lambda (slot)
                    (destructuring-bind
                        (json-name
                         ;; FIXME: Is there a better way to translate from CaMelCAsE?
                         &key (lisp-name (cffi:translate-camelcase-name json-name))
                           processor deprocessor (literal-p (sera:true processor)))
                        (uiop:ensure-list slot)
                      (list json-name lisp-name processor deprocessor literal-p)))
                  names-and-slots)))
    `(progn
       (setf (gethash ,type *classes*) (quote ,name))
       (defclass ,name (,@superclasses)
         (,@(mapcar (lambda (slot)
                      `(,(second slot) :initform nil
                                       :initarg ,(intern (symbol-name (second slot)) :keyword)))
                    normalized-slots)))
       (defun ,(alex:symbolicate name :-p) (object)
         (typep object (quote ,name)))
       ,@(loop for slot in normalized-slots
               for lisp-name = (second slot)
               for literal-p = (fifth slot)
               collect `(defmethod ,lisp-name ((object ,name))
                          ,(if literal-p
                               `(slot-value object (quote ,lisp-name))
                               `(when (slot-value object (quote ,lisp-name))
                                  (parse-object (slot-value object (quote ,lisp-name)))))))
       (defmethod unparse-object ((object ,name) &optional hash)
         ,@(loop for (json-name lisp-name . rest) in normalized-slots
                 for deprocessor = (second rest)
                 collect `(j:when (slot-value object (quote ,lisp-name))
                            (setf (j:get ,json-name hash)
                                  ,(if deprocessor
                                       `(funcall ,deprocessor (slot-value object (quote ,lisp-name)))
                                       `(slot-value object (quote ,lisp-name))))))
         (call-next-method object hash))
       (defmethod initialize-instance :after ((object ,name) &key original-object)
         (when (hash-table-p original-object)
           ,@(loop for (json-name lisp-name processor) in normalized-slots
                   collect `(j:when (j:get ,json-name original-object)
                              (setf (slot-value object (quote ,lisp-name))
                                    ,(if processor
                                         `(funcall ,processor (j:get ,json-name original-object))
                                         `(j:get ,json-name original-object))))))))))

(defun unparse-timestring (timestamp)
  (when timestamp
    (local-time:format-timestring nil timestamp :format local-time:+rfc3339-format+)))

(define-json-type object "Object" (base)
  ("name" :literal-p t)
  ("nameMap" :literal-p t)
  "attachment" ; nested
  "attributedTo" ; nested
  "audience" ; nested
  ("content" :literal-p t)
  ("contentMap" :literal-p t)
  ("source" :literal-p t)
  ("context" :literal-p t)
  ("startTime" :processor #'local-time:parse-timestring :deprocessor #'unparse-timestring)
  ("endTime" :processor #'local-time:parse-timestring :deprocessor #'unparse-timestring)
  ("published" :processor #'local-time:parse-timestring :deprocessor #'unparse-timestring)
  ("updated" :processor #'local-time:parse-timestring :deprocessor #'unparse-timestring)
  ("duration" :literal-p t) ; time period
  "generator" ; nested
  "icon" ; nested
  "image" ; nested
  "location" ; nested
  "preview" ; nested
  "replies" ; nested
  ("summary" :literal-p t)
  ("summaryMap" :literal-p t)
  "tag" ; nested
  "inReplyTo" ; nested
  "url" ; nested
  "to" ; nested
  "bto" ; nested
  "cc" ; nested
  "bcc" ; nested
  "mediaType"
  "likes" ; nested
  "shares" ; nested
  ("width" :literal-p t) ;; Mastodon adds this
  ("height" :literal-p t)  ;; Mastodon adds this
  )

(define-json-type link "Link" (base)
  ("href" :processor #'quri:uri :deprocessor #'quri:render-uri)
  ("rel" :literal-p t)
  ("mediaType" :literal-p t)
  ("name" :literal-p t)
  ("hreflang" :literal-p t)
  ("height" :literal-p t)
  ("width" :literal-p t)
  "preview" ; nested
  )

(define-json-type base-activity "" (object)
  "actor" ; nested
  "target" ; nested
  "result" ; nested
  "origin" ; nested
  "instrument" ; nested
  )

(define-json-type activity "Activity" (base-activity)
  "object" ; nested
  )

(define-json-type intransitive-activity "IntransitiveActivity" (base-activity))

(define-json-type actor "" (object)
  "inbox" ; nested
  "outbox" ; nested
  "following" ; nested
  "followers" ; nested
  "liked" ; nested
  "streams" ; nested
  ("preferredUsername" :literal-p t)
  "endpoints" ; nested
  "featured" ; Mastodon-specific
  )

(define-json-type base-collection "" (object)
  ("totalItems" :literal-p t)
  ("first" :lisp-name first-item) ; object
  ("last" :lisp-name last-item) ; object
  ("current" :lisp-name current-item) ; object
  )

(define-json-type collection "Collection" (base-collection)
  "items" ; nested
  )

(define-json-type ordered-collection "OrderedCollection" (collection)
  "orderedItems" ; nested
  )

(define-json-type collection-page "CollectionPage" (collection)
  "partOf" ; nested
  "next" ; nested
  "prev" ; nested
  )

(define-json-type ordered-collection-page "OrderedCollectionPage" (collection-page ordered-collection)
  ("startIndex" :literal-p t))

;;; Activity Vocabulary Actor Types (https://www.w3.org/TR/activitystreams-vocabulary/#actor-types)

(define-json-type application "Application" (actor))
(define-json-type group "Group" (actor))
(define-json-type organization "Organization" (actor))
(define-json-type person "Person" (actor))
(define-json-type service "Service" (actor))

;;; Activity Vocabulary Activity Types (https://www.w3.org/TR/activitystreams-vocabulary/#activity-types)

(define-json-type accept-activity "Accept" (activity))
(define-json-type tentative-accept-activity "TentativeAccept" (accept-activity))
(define-json-type add-activity "Add" (activity))
(define-json-type arrive-activity "Arrive" (intransitive-activity))
(define-json-type create-activity "Create" (activity))
(define-json-type delete-activity "Delete" (activity))
(define-json-type follow-activity "Follow" (activity))
(define-json-type ignore-activity "Ignore" (activity))
(define-json-type block-activity "Block" (ignore-activity))
(define-json-type join-activity "Join" (activity))
(define-json-type leave-activity "Leave" (activity))
(define-json-type like-activity "Like" (activity))
(define-json-type offer-activity "Offer" (activity))
(define-json-type invite-activity "Invite" (offer-activity))
(define-json-type reject-activity "Reject" (activity))
(define-json-type tentative-reject-activity "TentativeReject" (reject-activity))
(define-json-type remove-activity "Remove" (activity))
(define-json-type undo-activity "Undo" (activity))
(define-json-type update-activity "Update" (activity))
(define-json-type view-activity "View" (activity))
(define-json-type listen-activity "Listen" (activity))
(define-json-type read-activity "Read" (activity))
(define-json-type move-activity "Move" (activity))
(define-json-type travel-activity "Travel" (intransitive-activity))
(define-json-type announce-activity "Announce" (activity))
(define-json-type flag-activity "Flag" (activity))
(define-json-type dislike-activity "Dislike" (activity))
(define-json-type question-activity "Question" (intransitive-activity)
  "oneOf" ; nested
  "anyOf" ; nested
  ("closed" :processor #'local-time:parse-timestring))

;;; Activity Vocabulary Activity Types (https://www.w3.org/TR/activitystreams-vocabulary/#object-types)

(define-json-type relationship "Relationship" (object)
  "subject" ; nested
  "object" ; nested
  "relationship" ; nested
  )
(define-json-type article "Article" (object))
(define-json-type document "Document" (object))
(define-json-type audio "Audio" (document))
(define-json-type image "Image" (document))
(define-json-type video "Video" (document))
(define-json-type note "Note" (object))
(define-json-type page "Page" (document))
(define-json-type event "Event" (object))
(define-json-type place "Place" (object)
  ("accuracy" :literal-p t)
  ("altitude" :literal-p t)
  ("latitude" :literal-p t)
  ("longitude" :literal-p t)
  ("radius" :literal-p t)
  ("units" :literal-p t))
(define-json-type mention "Mention" (link))
(define-json-type profile "Profile" (object)
  "describes" ; nested
  )
(define-json-type tombstone "Tombstone" (object)
  "formerType" ; nested
  ("deleted" :processor #'local-time:parse-timestring :deprocessor #'unparse-timestring))

(defgeneric name* (object)
  ;; FIXME: This should not exists! Strong typing should be strong!
  (:method ((object t)) "")
  (:method :around ((object t))
    (first (uiop:ensure-list (call-next-method))))
  (:method ((object sequence))
    (lparallel:pmap (serapeum:class-name-of object) #'name* object))
  (:documentation "Return a human-readable name for the object."))

(defmethod name* ((object actor))
  (j:or (name object) (preferred-username object) (id object)))

(defmethod name* ((object object))
  (j:or (name object) (id object)))

(defmethod name* ((object link))
  (j:or (name object) (id object) (quri:render-uri (href object))))

(defgeneric author* (object)
  (:method :around ((object t))
    (first (uiop:ensure-list (call-next-method))))
  (:method ((object sequence))
    (lparallel:pmap (serapeum:class-name-of object) #'author* object))
  (:documentation "Return the supposed original author of the OBJECT."))

(defmethod author* ((object object))
  (j:or (attributed-to object)
        (generator object)))

(defmethod author* ((object activity))
  (j:or (origin object)
        (and (object object)
             (author* (object object)))))

(defgeneric url* (object)
  (:method ((object string)) object)
  (:method :around ((object t))
    (let* ((urls (uiop:ensure-list (call-next-method)))
           (suitable-url (or (find #'quri:uri-https-p urls)
                             (find #'quri:uri-http-p urls)
                             (first urls))))
      (j:when suitable-url
        (quri:render-uri (quri:uri suitable-url)))))
  (:method ((object sequence))
    (lparallel:pmap (serapeum:class-name-of object) #'url* object))
  (:method ((object hash-table))
    (url* (or (j:get "href" object)
              (j:get "url" object))))
  (:documentation "Get the URL to the OBJECT that it can be referred to by."))

(defmethod url* ((object link))
  (href object))

(defmethod url* ((object object))
  (cond
    ((stringp (slot-value object 'url)) (slot-value object 'url))
    ((j:true (url object))
     (if (link-p (first (uiop:ensure-list (url object))))
         (url* (url object))
         (slot-value object 'url)))
    (t (id object))))

(defmethod published* ((object object))
  "Get the publication date of the OBJECT, checking all the date-related fields."
  (alex:if-let ((time (j:or (published object) (updated object) (start-time object))))
    (local-time:format-timestring nil time :format local-time:+asctime-format+)
    "sometime"))

(defgeneric items* (collection)
  (:method ((collection object))
    nil)
  (:method ((collection base-collection))
    (loop for item = (first-item collection) then (next item)
          while (j:true item)
          if (collection-page-p item)
            append (j:or (items item)
                         (and (ordered-collection-p item)
                              (ordered-items item)))
          else if (base-p item)
                 collect item))
  (:documentation "Get all the items from the collection.

Beware: the number of items in any ActivityStreams collection can be
arbitrarily large, so first check `total-items' to make sure you've
got enough space for it."))
