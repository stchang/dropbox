#lang setup/infotab
(define name "dropbox")
(define scribblings '(("dropbox.scrbl")))
(define categories '(misc))
(define repositories (list "4.x"))
(define primary-file '("dropbox.rkt"))
(define required-core-version "5.3.1.12")
(define version "2.0")
(define blurb (list '(div "Racket bindings for Dropbox API.")))
(define release-notes  
  (list '(div "2.0: Remove upload-file and rename upload-large-file to upload-file.")
        '(div "Combine obtain-request-token and get-authorization-url and remove obtain-request-token.")))