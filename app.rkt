#lang racket

(require "dropbox.rkt")

;(set!-APP-KEY "3ysfqt0flcbex2t")
;(set!-APP-SECRET "hia6gkco347zczj")

;(obtain-request-token)
;(get-authorization-url #:callback "http://yahoo.com")

;; after granting access at url returned by get-authorization-url:
#;(obtain-access-token)
;(get-account-info)
;(get-metadata "lazyinf.pdf")
#;(upload-file "test-files/lazyinf.pdf" "lazyinf.pdf"
;             #:locale "pt-BR"
             #:overwrite? "false"
;             #:parent-rev "20ce7e933"
             )
