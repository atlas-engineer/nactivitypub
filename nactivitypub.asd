;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(asdf:defsystem #:nactivitypub
  :description "An extensible ActivityPub support library for Common Lisp."
  :author "Atlas Engineer LLC"
  :license  "BSD 3-Clause"
  :version "0.0.1"
  :serial t
  ;; FIXME: Depending on njson/core makes us lighter on dependencies
  ;; users of the library don't need, but forces us to load the actual
  ;; NJSON back-end, like njson/cl-json, elsewhere. Is that a
  ;; reasonable trade-off?
  :depends-on (#:njson/core #:serapeum #:str #:local-time #:lparallel #:dexador #:quri)
  :components ((:file "package")
               (:file "nactivitypub")))