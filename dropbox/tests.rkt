#lang racket

(require "dropbox.rkt"
         rackunit
         json
         file/sha1
         net/url)

;; ----------------------------------------------------------------------------
;; These tests use the Dropbox app "Racket Test App"
;; (created at https://www.dropbox.com/developers/apps),
;; with the following app-key and app-secret:

;(set!-APP-KEY "3ysfqt0flcbex2t")
;(set!-APP-SECRET "hia6gkco347zczj")

;; Also, these tests are only for "app_folder" (ie, limited) access type
;; and not "dropbox" (ie, full) access.

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

;; ----------------------------------------
;; utility functions/macros

;; multi-arity equal?
;; must have at least 1 argument
(define (equa? . args)
  (define x (first args))
  (for/and ([y (rest args)]) (equal? x y)))

(define-syntax (check-field: stx)
  (syntax-case stx (in-metas: vals:)
    [(_ fld in-metas: m ... equal-to: v)
     (syntax/loc stx (check-true (equa? (hash-ref m (quote fld)) ... v)))]))
(define-syntax (check-field-equal: stx)
  (syntax-case stx (in-metas:)
    [(_ fld in-metas: m ...)
     (syntax/loc stx (check-true (equa? (hash-ref m (quote fld)) ...)))]))
(define-syntax (check-field-not-equal: stx)
  (syntax-case stx (in-metas:)
    [(_ fld in-metas: m1 m2)
     (syntax/loc stx (check-false (equal? (hash-ref m1 (quote fld))
                                          (hash-ref m2 (quote fld)))))]))

;; filters a search result by exact filename
(define (filter-search-by-exact-match filename search-res)
  (filter (λ (m) (string=? (hash-ref m 'path)
                           (string-append "/" filename)))
              search-res))

;; ----------------------------------------
;; upload/download test fn
(define (do-upload/download-test localfile remote-file
                                 #:remote-dir [remote-dir ""]
                                 #:large-file? [large-file? #f])
  (define remotefile (if (string=? remote-dir "")
                         remote-file
                         (string-append remote-dir "/" remote-file)))
  (define orig-size (file-size localfile))
  (define orig-sha1 (call-with-input-file localfile sha1))

  (unless (string=? remote-dir "")
    (if (null? (search "" remote-dir))
        (create-folder remote-dir)
        (begin
          (delete remote-dir)
          (create-folder remote-dir))))
  
  ;; uploading ------------------------------
  ;; save revision number
  (define uploaded-meta 
    (if large-file?
        (upload-large-file localfile remotefile #:overwrite? "true")
        (upload-file localfile remotefile #:overwrite? "true")))
  (check-true (jsexpr? uploaded-meta)) ;; check not thunk (ie upload completed)
  (define uploaded-meta-from-revlst (first (get-revisions remotefile)))
  (define up-rev (hash-ref uploaded-meta 'rev))
  (define meta (get-metadata remotefile))
  
  (check-field-equal: rev 
   in-metas:          uploaded-meta uploaded-meta-from-revlst meta)
  
  (check-field: bytes
   in-metas:    uploaded-meta uploaded-meta-from-revlst meta
   equal-to:    orig-size)

  ;; searching ------------------------------
  (define search-meta 
    (filter-search-by-exact-match remotefile (search remote-dir remote-file)))
  
  (check-false (null? search-meta))
  
  (check-field-equal: rev
   in-metas: (first search-meta) uploaded-meta)
  
  ;; copying ------------------------------
  (define copied-file (string-append "COPIED" remote-file))
  (define copiedfile 
    (if (string=? remote-dir "")
        copied-file
        (string-append remote-dir "/" copied-file)))

  ;; delete copy target if it's already there
;  (define copied-check
;    (filter-search-by-exact-match copiedfile (search remote-dir copied-file)))
;  (unless (null? copied-check) (delete copiedfile))
  (when (exists? remote-dir copied-file) 
    (delete copiedfile))
  
  (define copied-meta (copy remotefile copiedfile))
  (define copied-search-meta 
    (filter-search-by-exact-match copiedfile (search remote-dir copied-file)))
  
  (check-false (null? copied-search-meta))
  
  (define copyref (hash-ref (get-copy-ref remotefile) 'copy_ref))
  (define copyref-copied-file (string-append "COPYREF" remote-file))
  (define copyref-copiedfile
    (if (string=? remote-dir "")
        copyref-copied-file
        (string-append remote-dir "/" copyref-copied-file)))

  ;; delete copy target if it's already there
;  (define copyref-copied-check
;    (filter-search-by-exact-match copyref-copiedfile (search remote-dir copyref-copied-file)))
;  (unless (null? copyref-copied-check) (delete copyref-copiedfile))
  (when (exists? remote-dir copyref-copied-file) 
    (delete copyref-copiedfile))
  
  (define copyref-copied-meta (copy "" copyref-copiedfile #:copy-ref copyref))
  (define copyref-copied-search-meta 
    (filter-search-by-exact-match copyref-copiedfile
                                  (search remote-dir copyref-copied-file)))
  
  (check-false (null? copyref-copied-search-meta))
  
  (check-field: bytes
   in-metas:    copied-meta (first copied-search-meta)
                copyref-copied-meta (first copyref-copied-search-meta)
   equal-to:    orig-size)
  
  ;; moving ------------------------------
  (define moved-file (string-append "MOVED" remote-file))
  (define movedfile 
    (if (string=? remote-dir "")
        moved-file
        (string-append remote-dir "/" moved-file)))

  ;; delete move target if it's already there
;  (define moved-check
;    (filter-search-by-exact-match movedfile (search remote-dir moved-file)))
;  (unless (null? moved-check) (delete movedfile))
  (when (exists? remote-dir moved-file)
    (delete movedfile))

  (define moved-meta (move copiedfile movedfile))
  (define moved-search-meta 
    (filter-search-by-exact-match movedfile (search remote-dir moved-file)))
  
  (check-false (null? moved-search-meta))
  
  (check-field: bytes
   in-metas:    moved-meta (first moved-search-meta)
   equal-to:    orig-size)
  
  ;; cleanup
  (delete copyref-copiedfile)
  (delete movedfile)
  
  ;; check share urls ------------------------------
  ;; example: http://db.tt/JST2Phny
  (define share-url-short 
    (string->url (hash-ref (get-share-url remotefile) 'url)))
  ;; https://www.dropbox.com/s/4ny073s2zeomkw7/dropbox.pdf
  (define share-url 
    (string->url (hash-ref (get-share-url remotefile #:short-url "false") 'url)))
  ;; https://dl.dropbox.com/0/view/znoemp4dlzoyb1n/Apps/Racket%20Test%20App/dropbox.pdf
  (define media-url 
    (string->url (hash-ref (get-media-url remotefile) 'url)))
  
  (check-equal? (url-scheme share-url-short) "http")
  (check-equal? (url-scheme share-url) "https")
  (check-equal? (url-scheme media-url) "https")
  (check-equal? (url-host share-url-short) "db.tt")
  (check-equal? (url-host share-url) "www.dropbox.com")
  (check-equal? (url-host media-url) "dl.dropbox.com")
  ;; url-path returns list of path/param structs, where each one is a subdir
  ;; The path field is the path string
  (check-equal? (last (map path/param-path (url-path share-url)))
                remote-file)
  (check-equal? (last (map path/param-path (url-path media-url)))
                remote-file)
  
  ;; downloading ------------------------------
  ;; delete locally first, but record size and sha1 hash
  (check-true (file-exists? localfile))
  (delete-file localfile)
  (check-false (file-exists? localfile))
  
  ;; now download file
  (download-file remotefile localfile #:exists 'replace)
  (define new-size (file-size localfile))
  (define new-sha1 (call-with-input-file localfile sha1))
  
  (check-equal? orig-size new-size)
  (check-equal? orig-sha1 new-sha1)

  ;; deleting ------------------------------
  (define deleted-meta (delete remotefile))
  (define deleted-meta-from-revlst (first (get-revisions remotefile)))
  
  (check-field-equal: rev
   in-metas:          deleted-meta deleted-meta-from-revlst)

  (check-field: bytes
   in-metas:    deleted-meta deleted-meta-from-revlst
   equal-to:    0)
  
  ;; revisions should have changed
  (check-field-not-equal: rev
   in-metas:              meta deleted-meta)
 
  ;; restoring ------------------------------
  (define restored-meta (restore-file remotefile up-rev))
  (define restored-meta-from-revlst (first (get-revisions remotefile)))

  (check-field-not-equal: rev
   in-metas:              restored-meta deleted-meta)
  (check-field-not-equal: rev
   in-metas:              restored-meta uploaded-meta)
  (check-field-equal: rev
   in-metas:          restored-meta restored-meta-from-revlst)
  (check-field: bytes
   in-metas:    restored-meta restored-meta-from-revlst
   equal-to:    orig-size)
  
  ;; download again after restoring
  (check-true (file-exists? localfile))
  (delete-file localfile)
  (check-false (file-exists? localfile))
  
  (download-file remotefile localfile #:exists 'replace)
  (define new-size2 (file-size localfile))
  (define new-sha12 (call-with-input-file localfile sha1))
  
  (check-equal? orig-size new-size2)
  (check-equal? orig-sha1 new-sha12)
  
  ;; delete again
  (delete remotefile)

  ;; other checks ------------------------------
  (check-field: path
   in-metas: uploaded-meta uploaded-meta-from-revlst meta 
             deleted-meta deleted-meta-from-revlst
             restored-meta restored-meta-from-revlst
   equal-to: (string-append "/" remotefile))
  

  )

;; test upload to app root dir
;(do-upload/download-test PDF-PATH PDF-FILE)
;(do-upload/download-test PNG-PATH PNG-FILE)
;(do-upload/download-test BIG-PATH BIG-FILE)
#;(do-upload/download-test BIG-PATH BIG-FILE #:large-file? #t)

;; test upload to app subdir
(do-upload/download-test PDF-PATH PDF-FILE #:remote-dir TEST-DIR)
(do-upload/download-test PNG-PATH PNG-FILE #:remote-dir TEST-DIR)
;(do-upload/download-test BIG-PATH BIG-FILE #:remote-dir TEST-DIR)
#;(do-upload/download-test BIG-PATH BIG-FILE #:remote-dir TEST-DIR
                                            #:large-file? #t)

;; not sure how to test these
#;(get-delta)
#;(get-image-thumbnail "xmas.jpg" "xmas-thumb.jpg" 
                       #:size "s" #:exists 'replace)
;; interrupted chunk upload



#;(get-copy-ref "lazyinf.pdf")

;(upload-file "test-files/maine.jpg" "maine.jpg")

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
