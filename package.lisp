;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(defpackage #:nactivitypub
  (:use #:cl)
  (:local-nicknames
   (:j :njson)
   (:sera :serapeum)
   (:alex :alexandria))
  (:export
   ;; Generic API:
   #:parse-object #:unparse-object #:fetch-object #:send-object
   ;; Base and its slots:
   #:base #:base-p #:id #:object-type #:original-object
   ;; Derived fundamental classes:
   #:object #:object-p #:link #:link-p #:activity #:activity-p #:intransitive-activity
   #:intransitive-activity-p #:actor #:actor-p #:collection #:collection-p #:ordered-collection
   #:ordered-collection-p #:collection-page #:collection-page-p
   #:ordered-collection-page #:ordered-collection-page-p
   ;; Object slots:
   #:name #:name-map #:attachment #:attributed-to #:audience #:content #:content-map #:source
   #:context #:start-time #:end-time #:published #:updated #:duration #:generator #:icon #:image
   #:location #:preview #:replies #:summary #:summary-map #:tag #:in-reply-to #:url #:to #:bto #:cc
   #:bcc #:media-type #:likes #:shares #:width #:height
   ;; Link slots:
   #:href #:rel #:hreflang #:preview
   ;; Actors and their slots:
   #:application #:application-p #:group #:group-p #:organization
   #:organization-p #:person #:person-p #:service #:service-p
   #:inbox #:outbox #:following #:followers #:liked #:streams
   #:preferred-username #:endpoints #:featured
   ;; Activity slots:
   #:actor #:target #:result #:origin #:instrument #:object
   ;; Question slots:
   #:one-of #:any-of #:closed
   ;; Activities:
   #:accept-activity #:accept-activity-p #:tentative-accept-activity #:tentative-accept-activity-p
   #:add-activity #:add-activity-p #:arrive-activity #:arrive-activity-p
   #:create-activity #:create-activity-p #:delete-activity #:delete-activity-p
   #:follow-activity #:follow-activity-p #:ignore-activity #:ignore-activity-p
   #:block-activity #:block-activity-p #:join-activity #:join-activity-p
   #:leave-activity #:leave-activity-p #:like-activity #:like-activity-p
   #:offer-activity #:offer-activity-p #:invite-activity #:invite-activity-p
   #:reject-activity #:reject-activity-p #:tentative-reject-activity #:tentative-reject-activity-p
   #:remove-activity #:remove-activity-p #:undo-activity #:undo-activity-p
   #:update-activity #:update-activity-p #:view-activity #:view-activity-p
   #:listen-activity #:listen-activity-p #:read-activity #:read-activity-p
   #:move-activity #:move-activity-p #:travel-activity #:travel-activity-p
   #:announce-activity #:announce-activity-p #:flag-activity #:flag-activity-p
   #:dislike-activity #:dislike-activity-p #:question-activity #:question-activity-p
   ;; Object types
   #:relationship #:relationship-p #:subject #:object #:relationship
   #:place #:place-p #:accuracy #:altitude #:latitude #:longitude #:radius #:units
   #:article #:article-p #:document #:document-p #:audio #:audio-p #:image #:image-p
   #:video #:video-p #:note #:note-p #:page #:page-p #:event #:event-p #:mention #:mention-p
   #:profile #:profile-p #:describes
   #:tombstone #:tombstone-p #:former-type #:deleted
   ;; Collections and pages slots
   #:total-items #:first-item #:last-item #:current-item #:items #:ordered-items
   #:part-of #:next #:prev #:start-index
   ;; Helpers.
   #:name* #:author* #:url* #:published*)
  (:documentation "ActivityPub parsing and transfer library.

Implements most ActivityStreams and ActivityPub types of objects as
CLOS classes with convenient accessors.

There is a number of helper functions to extract semantic data from ActivityPub
objects: `name*', `url*', `author*', `published*'.

Important methods to be aware of:
- `parse-object' ensures that the data one gets from the page is a proper
  `object', `link', `activity' etc.
- `unparse-object' serializes the object to the ActivityPub-friendly JSON.
- `fetch-object' fetches the object from the HTTP(S) URL and parses it to
  primitive Lisp form.
- `send-object' sends the objects to the account outbox (given you are logged
  in)."))
