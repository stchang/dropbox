#lang racket

(require "dropbox.rkt")

;(set!-APP-KEY "3ysfqt0flcbex2t")
;(set!-APP-SECRET "hia6gkco347zczj")

;(obtain-request-token)
;(get-authorization-url #:callback "http://yahoo.com")

;; after granting access at url returned by get-authorization-url:
#;(obtain-access-token)
;(get-account-info)
#;(get-metadata "lazyinf.pdf")
#;(upload-file "test-files/lazyinf.pdf" "lazyinf.pdf"
;             #:locale "pt-BR"
             #:overwrite? "false"
;             #:parent-rev "20ce7e933"
             )

#;(download-file "lazyinf.pdf" "lazyinf.pdf" #:exists 'replace)

#;(upload-file-post "test-files/lazyinf.pdf" "lazyinf.pdf"
;             #:locale "pt-BR"
             #:overwrite? "false"
;             #:parent-rev "20ce7e933"
             )
#;(get-delta)
#;(get-revisions "testing-revisions.txt")
#;(restore-file "testing-revisions.txt" "110ce7e933")

#;(upload-file "testing-revisions.txt" "testing-revisions.txt")

;(search "" "test")
;(search "" "txt")
;(search "test" "pdf")

;(get-share-url "test" #:short-url "false")
;(get-media-url "lazyinf.pdf")

#;(get-copy-ref "lazyinf.pdf")

;(upload-file "test-files/maine.jpg" "maine.jpg")
#;(get-image-thumbnail "maine.jpg" "maine-thumb.jpg" 
                     #:size "m" #:exists 'replace)

;(upload-large-file "test-files/xmas.jpg" "xmas.jpg")
#;(download-file "xmas.jpg" "xmas.jpg")
;(get-copy-ref "xmas.jpg")
;(copy "xmas.jpg" "xmas2.jpg")
;(copy "xmas.jpg" "xmas2.jpg" #:copy-ref "DL38wjBoYW4xdnh2cGhmcA")
;(copy "test" "test2")

;(create-folder "new-folder")
;(delete "new-folder")
;(delete "test2")
;(delete "xmas2.jpg")

;(move "xmas.jpg" "xmasss.jpg")
(define res
  (upload-large-file "test-files/xmas.jpg" "xmas.jpg" 
                     #:verbose? #t 
;                   #:resume? #t 
;                   #:resume-id "izFM0xvusanQhT7Sn4BAxA"
;                   #:resume-offset 8388608
                     ))
