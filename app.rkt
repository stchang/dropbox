#lang racket

(require "dropbox.rkt")

(set!-APP-KEY "3ysfqt0flcbex2t")
(set!-APP-SECRET "hia6gkco347zczj")
(obtain-request-token)
(get-authorization-url)

;; after granting access at url returned by get-authorization-url:
#;(obtain-access-token)
#;(get-account-info)