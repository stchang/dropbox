#lang racket

(require "dropbox.rkt")
(require rackunit)
(require json)
(require file/sha1)

;; ----------------------------------------------------------------------------
;; These tests use the Dropbox app "Racket Test App"
;; (created at https://www.dropbox.com/developers/apps),
;; with the following app-key and app-secret:

;(set!-APP-KEY "3ysfqt0flcbex2t")
;(set!-APP-SECRET "hia6gkco347zczj")

;; ----------------------------------------------------------------------------
;; App has been granted access using the recommended OAuth authentication process:
;; https://www.dropbox.com/developers/reference/api#request-token
;; https://www.dropbox.com/developers/blog/20
;;
;; The authenication tokens are hardcoded as the default values in dropbox.rkt
;; and subsequent API calls will use these credentials

(define OAUTH-REQUEST-TOKEN "uk264rf6wc0lyte")

;(obtain-request-token)

(check-equal?
 (get-authorization-url #:callback "http://yahoo.com")
 (string-append "https://www.dropbox.com/1/oauth/authorize"
                "?"
                "oauth_token="
                OAUTH-REQUEST-TOKEN
                "&"
                "oauth_callback=http://yahoo.com&locale=en"))

;; after granting access at url returned by get-authorization-url:
;(obtain-access-token)

;; ----------------------------------------------------------------------------
;; test account info
(define account-info (get-account-info))
(check-equal? (hash-ref account-info 'display_name)
              "Stephen Chang")
(check-equal? (hash-ref account-info 'uid)
              132816807)
            
;; ----------------------------------------------------------------------------
;; test upload/download/delete
;;
;; sample meta data:
;; '#hasheq((bytes . 127748)
;;         (path . "/dropbox.pdf")
;;         (modified . "Thu, 10 Jan 2013 19:11:26 +0000")
;;         (icon . "page_white_acrobat")
;;         (size . "124.8 KB")
;;         (root . "app_folder")
;;         (revision . 86)
;;         (rev . "560ce7e933")
;;         (thumb_exists . #f)
;;         (client_mtime . "Thu, 10 Jan 2013 19:11:26 +0000")
;;         (is_dir . #f)
;;         (mime_type . "application/pdf"))

(define PDF-FILE "dropbox.pdf")
(define PNG-FILE "dropbox.png")
(define BIG-FILE "xmas.jpg")

(define TEST-DIR "test-files")

(define PDF-PATH (string-append TEST-DIR "/" PDF-FILE))
(define PNG-PATH (string-append TEST-DIR "/" PNG-FILE))
(define BIG-PATH (string-append TEST-DIR "/" BIG-FILE))

;; multi-arity equal?
;; must have at least 1 argument
(define (equa? . args)
  (define x (first args))
  (for/and ([y (rest args)]) (equal? x y)))

(define-syntax (check-field: stx)
  (syntax-case stx (in-metas: vals:)
    [(_ fld in-metas: m ... equal-to: v)
     #'(check-true (equa? (hash-ref m (quote fld)) ... v))]))
(define-syntax (check-field-equal: stx)
  (syntax-case stx (in-metas:)
    [(_ fld in-metas: m ...)
     #'(check-true (equa? (hash-ref m (quote fld)) ...))]))
(define-syntax (check-field-not-equal: stx)
  (syntax-case stx (in-metas:)
    [(_ fld in-metas: m1 m2)
     #'(check-false (equa? (hash-ref m1 (quote fld))
                           (hash-ref m2 (quote fld))))]))

(define (do-upload/download-test localfile remotefile)
  ;; upload
  (define uploaded-meta (upload-file localfile remotefile #:overwrite? "true"))
  (define uploaded-rev-meta (first (get-revisions remotefile)))
  (define meta (get-metadata remotefile))
  
  ;; delete locally, but record size and sha1 hash
  (define old-size (file-size localfile))
  (define old-sha1 (call-with-input-file localfile sha1))
  (check-true (file-exists? localfile))
  (delete-directory/files localfile)
  (check-false (file-exists? localfile))
  
  ;; re-download file
  (download-file remotefile localfile #:exists 'replace)
  (define new-size (file-size localfile))
  (define new-sha1 (call-with-input-file localfile sha1))
  
  ;; delete remotely
  (define deleted-meta (delete remotefile))
  (define deleted-rev-meta (first (get-revisions remotefile)))

  (check-field-equal: rev 
   in-metas:          uploaded-meta uploaded-rev-meta meta)

  (check-field-not-equal: rev
   in-metas:              meta deleted-meta)
  
  (check-field-equal: rev
   in-metas:          deleted-meta deleted-rev-meta)
  
  (check-field: bytes
   in-metas:    uploaded-meta uploaded-rev-meta meta
   equal-to:    old-size)

  (check-field: bytes
   in-metas:    deleted-meta deleted-rev-meta
   equal-to:    0)
  
  (check-field: path
   in-metas: uploaded-meta uploaded-rev-meta meta deleted-meta deleted-rev-meta
   equal-to:    (string-append "/" remotefile))
  
  (check-equal? old-size new-size)
  (check-equal? old-sha1 new-sha1)
  
  #;(values uploaded-meta meta deleted-meta))

(do-upload/download-test PDF-PATH PDF-FILE)
(do-upload/download-test PNG-PATH PNG-FILE)
(do-upload/download-test BIG-PATH BIG-FILE)

#;(get-metadata "lazyinf.pdf")

#;(download-file "lazyinf.pdf" "lazyinf.pdf" #:exists 'replace)

#;(upload-file-post "test-files/lazyinf.pdf" "lazyinf.pdf"
;             #:locale "pt-BR"
             #:overwrite? "false"
;             #:parent-rev "20ce7e933"
             )

;; not sure how to test this
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
#;(define res
  #;(upload-large-file "test-files/xmas.jpg" "xmas.jpg" 
                     #:verbose? #t 
;                   #:resume? #t 
;                   #:resume-id "izFM0xvusanQhT7Sn4BAxA"
;                   #:resume-offset 8388608
                     ))
