#lang racket

;; Racket bindings for Dropbox API:
;; https://www.dropbox.com/developers/reference/api

;; Requires Racket 5.3.1.12 or later

;; TODO:
;; [o] = open, [x] = closed
;;
;; [x] 2013-01-15: combine obtain-request-token and get-authorization-url into
;;                 one step and don't provide obtain-request-token
;;  - DONE 2013-01-15
;; [x] 2013-01-07: support locale
;; Add an optional parameter to functions of API calls that support
;; a locale parameter.
;;  - DONE 2013-01-07
;; [o] 2013-01-07: fix POST file uploading (ie upload-file-post function)
;;     This is low priority since POST uploading is not recommended by
;;     dropbox.
;; [o] 2013-01-07: test full dropbox mode (ie - non-sandbox)
;; [o] 2013-01-07: abstract post-pure-port, get-pure-port, etc
;;                 so AUTHORIZATION-HEADER is not repeated
;; [o] 2013-01-07: create utility fn to build remote path
;; [o] 2013-01-07: fix POST calls to not include params in url
;; [o] 2014-01-08: fix download-file and get-image-thumbnail to return the
;;                 metadata of downloaded file (instead of void)

(require net/url
         net/uri-codec ; form-urlencoded->alist
         json
         )

(provide set!-APP-KEY
         set!-APP-SECRET
         set!-ACCESS-TYPE
         
         ;; oauth authentication
         ;obtain-request-token
         get-authorization-url
         obtain-access-token
         
         ;; account info
         get-account-info
         
         ;; files and metadata
         get-metadata
         upload-file
         ;;upload-file-post
         download-file
         get-delta
         get-revisions
         restore-file
         search
         get-share-url
         get-media-url
         get-copy-ref
         get-image-thumbnail
         upload-large-file
         
         ;; file ops
         copy
         create-folder
         delete
         move
         exists?
         )

;; ----------------------------------------------------------------------------
;; Constants for app key and app secret.
;; These must be set before starting OAuth authentication.
;; Default values are used for testing purposes only.
;; ----------------------------------------------------------------------------
(define APP-KEY "3ysfqt0flcbex2t")
(define APP-SECRET "hia6gkco347zczj")
;; ACCESS-TYPE = "app_folder" (limited access)
;;            or "dropbox"    (full access)
(define ACCESS-TYPE "app_folder")
(define (set!-APP-KEY akey) (set! APP-KEY akey))
(define (set!-APP-SECRET asec) (set! APP-SECRET asec))
(define (set!-ACCESS-TYPE atype) (set! ACCESS-TYPE atype))

;; ----------------------------------------------------------------------------
;; Utility functions
;; ----------------------------------------------------------------------------

;; Returns a url to the specified dropbox api call
;; mk-api-url : string -> url
(define (mk-api-url api-call [params #f])
  (string->url 
   (string-append "https://api.dropbox.com/1/" api-call 
                  (if params
                      (string-append "?" params)
                      ""))))

;; Returns a url to the specified dropbox api call
;; uses api-content.dropbox.com instead of api.dropbox.com
;; mk-api-content-url : string -> url
(define (mk-api-content-url api-call [params #f])
  (string->url 
   (string-append "https://api-content.dropbox.com/1/" api-call 
                  (if params
                      (string-append "?" params)
                      ""))))

;; Returns the root path corresponding to the current access type.
;; Possible return values are "dropbox" or "sandbox".
;; This is needed by many of the Dropbox api calls.
;; - access type "dropbox" returns root "dropbox"
;; - otherwise, the returned root is "sandbox"
(define (get-root)
  (if (string=? ACCESS-TYPE "dropbox")
      "dropbox"
      "sandbox"))

;; Returns params formatted with = and &
;; ie (format-params "param1" "val1" "param2" "val2")
;;     => "param1=val1&param2=val2"
;; format-params : -> string
(define (format-params . args)
  (cond [(null? args) ""]
        [(null? (cddr args)) (string-append (car args) "=" (cadr args))]
        [else (string-append (car args) "=" (cadr args) "&" 
                             (apply format-params (cddr args)))]))
      

;; ----------------------------------------------------------------------------
;; other constants
;; ----------------------------------------------------------------------------
(define DEFAULT-LOCALE "en")
(define AUTH-URL-BASE "https://www.dropbox.com/1/oauth/authorize")

;; ----------------------------------------------------------------------------
;; OAUTH authentication functions
;; Step 1) Go to url returned by get-authorization-url and grant access.
;;         get-authorization-url first gets a request token based on the 
;;         current app-key and app-secret (set via set!-APP-KEY and 
;;                                                 set!-APP-SECRET)
;; Step 2) Call obtain-access-token.
;; ----------------------------------------------------------------------------

;; User should ignore these initial tokens/secrets and get their own.
;; Default values are used for testing purposes only.
(define OAUTH-REQUEST-TOKEN "uk264rf6wc0lyte")
(define OAUTH-REQUEST-SECRET "8vfhlfahxd8xfxp")
(define OAUTH-ACCESS-TOKEN "ws51ylwe4geys4c")
(define OAUTH-ACCESS-SECRET "in75kn1ci9fskt4")
(define AUTHORIZATION-HEADER 
  (list (string-append "Authorization: OAuth oauth_version=\"1.0\","
                       "oauth_signature_method=\"PLAINTEXT\","
                       "oauth_consumer_key=\"" APP-KEY "\","
                       "oauth_token=\"" OAUTH-ACCESS-TOKEN "\","
                       "oauth_signature=\"" APP-SECRET "&" 
                                            OAUTH-ACCESS-SECRET"\"")))

;; obtain request token from Dropbox API
;; Step 1 of OAuth authentication process
(define (obtain-request-token)
  (define p 
    (post-pure-port 
     (mk-api-url "oauth/request_token")
     (bytes)
     (list (string-append "Authorization: OAuth oauth_version=\"1.0\","
                          "oauth_signature_method=\"PLAINTEXT\","
                          "oauth_consumer_key=\"" APP-KEY "\","
                          "oauth_signature=\"" APP-SECRET "&\""))))
  
  (define response-alist (form-urlencoded->alist (port->string p)))
  
  (close-input-port p)
    
  (set! OAUTH-REQUEST-SECRET (cdr (assq 'oauth_token_secret response-alist)))
  (set! OAUTH-REQUEST-TOKEN (cdr (assq 'oauth_token response-alist)))
  
  (values OAUTH-REQUEST-TOKEN OAUTH-REQUEST-SECRET))

;; get authorization url
;; user should go to returned url to grant access
;; Step 2 of OAuth authentication process
(define (get-authorization-url #:locale [locale DEFAULT-LOCALE] 
                               #:callback [callback-url AUTH-URL-BASE])
  ;; First, get a request token. This sets OAUTH-REQUEST-TOKEN and 
  ;;                                       OAUTH-REQUEST-SECRET.
  (obtain-request-token)
  (define params (format-params "oauth_token" OAUTH-REQUEST-TOKEN
                                "oauth_callback" callback-url
                                "locale" locale))
  (string-append AUTH-URL-BASE "?" params))

;; obtain access token from Dropbox API
;; Step 3 of OAuth authentication process
;; sets ACCESS-HEADER
(define (obtain-access-token)
  (define p 
    (post-pure-port 
     (mk-api-url "oauth/access_token")
     (bytes)
     (list (string-append "Authorization: OAuth oauth_version=\"1.0\","
                          "oauth_signature_method=\"PLAINTEXT\","
                          "oauth_consumer_key=\"" APP-KEY "\","
                          "oauth_token=\"" OAUTH-REQUEST-TOKEN "\","
                          "oauth_signature=\"" APP-SECRET "&" 
                                               OAUTH-REQUEST-SECRET"\""))))
  
  (define response-alist (form-urlencoded->alist (port->string p)))
  
  (close-input-port p)
  
  (set! OAUTH-ACCESS-SECRET (cdr (assq 'oauth_token_secret response-alist)))
  (set! OAUTH-ACCESS-TOKEN (cdr (assq 'oauth_token response-alist)))
  
  (set! AUTHORIZATION-HEADER
        (list (string-append "Authorization: OAuth oauth_version=\"1.0\","
                             "oauth_signature_method=\"PLAINTEXT\","
                             "oauth_consumer_key=\"" APP-KEY "\","
                             "oauth_token=\"" OAUTH-ACCESS-TOKEN "\","
                             "oauth_signature=\"" APP-SECRET "&" 
                                                  OAUTH-ACCESS-SECRET"\"")))
  
  (values OAUTH-ACCESS-SECRET OAUTH-ACCESS-TOKEN))


;; ----------------------------------------------------------------------------
;; functions to get account information
;; ----------------------------------------------------------------------------
;; get-account-info : (void) -> jsexpr
;; returns a JSON expression (ie hash table) with account info
(define (get-account-info #:locale [locale DEFAULT-LOCALE])
  (define params (format-params "locale" locale))
  (define p (get-pure-port 
             (mk-api-url "account/info" params) 
             AUTHORIZATION-HEADER))
  (define jsexp (read-json p))
  (close-input-port p)
  jsexp)

;; ----------------------------------------------------------------------------
;; functions to manipulate upload/download files
;; and get metadata, revisions history, etc
;; ----------------------------------------------------------------------------
(define (get-metadata path 
                      #:file-limit [file-limit 10000]
                      #:hash [hash ""]
                      #:list [lst "true"]
                      #:inc-del [inc-del "false"]
                      #:rev [rev ""]
                      #:locale [locale DEFAULT-LOCALE])
  (define params (format-params "file_limit" (number->string file-limit)
                                "hash" hash
                                "list" lst
                                "include_deleted" inc-del
                                "rev" rev
                                "locale" locale))
  (define p
    (get-pure-port
     (mk-api-url (string-append "metadata/" (get-root) "/" path)
                 params)
     AUTHORIZATION-HEADER))
  (define jsexp (read-json p))
  (close-input-port p)
  jsexp)

;; uses PUT to upload a file (recommended over upload-file-post)
;; remote-filepath should be a remote file name and NOT a directory
;; max filesize = 150mb
(define (upload-file local-filepath remote-filepath 
                     #:locale [locale DEFAULT-LOCALE] 
                     #:overwrite? [overwrite? "true"]
                     #:parent-rev [parent-rev ""])
  (define params (format-params "locale" locale
                                "overwrite" overwrite?
                                "parent_rev" parent-rev))
  (define p
    (put-pure-port
     (mk-api-content-url
      (string-append "files_put/" (get-root) "/" remote-filepath)
      params)
     (file->bytes local-filepath)
     AUTHORIZATION-HEADER))
  (define jsexp (read-json p))
  (close-input-port p)
  jsexp)

;; uploads a file using POST 
;; (not recommended -- use upload-file instead)
;; remote-filepath should be a remote file name and NOT a directory
;; max filesize = 150mb (use upload-large-file for > 150mb)
;; TODO: this isn't working -- how to include parameters as part of request url
;;       (must be signed like OAuth request URL)
(define (upload-file-post local-filepath remote-filepath 
                          #:locale [locale DEFAULT-LOCALE] 
                          #:overwrite? [overwrite? "true"]
                          #:parent-rev [parent-rev ""])
  (define params (format-params 
                  "locale" locale
                  "overwrite" overwrite?
;                  "parent_rev" parent-rev
                  ))
  (define p
    (post-pure-port
     (mk-api-content-url
      (string-append "files/" (get-root) "/" remote-filepath)
      params)
     (file->bytes local-filepath)
     AUTHORIZATION-HEADER))
  (define jsexp (read-json p))
  (close-input-port p)
  jsexp)

(define (download-file remote-filepath
                       local-filepath
                       #:rev [rev ""]
                       #:exists [exists 'error])
  (define params (format-params "rev" rev))
  (define p
    (get-pure-port
     (mk-api-content-url
      (string-append "files/" (get-root) "/" remote-filepath)
                     params)
     AUTHORIZATION-HEADER))
  (define out (open-output-file local-filepath 
                                #:mode 'binary
                                #:exists exists))
  (write-bytes (port->bytes p) out)
  (close-output-port out)
  (close-input-port p))

(define (get-delta #:cursor [cursor ""]
                   #:locale [locale DEFAULT-LOCALE])
  (define params (format-params "locale" locale "cursor" cursor))
  (define p
    (post-pure-port
     (mk-api-url "delta" params)
     (bytes)
     AUTHORIZATION-HEADER))
  (define jsexp (read-json p))
  (close-input-port p)
  jsexp)

(define (get-revisions filepath #:rev-limit [rev-limit 10]
                                #:locale [locale DEFAULT-LOCALE])
  (define params (format-params "rev_limit" (number->string rev-limit)
                                "locale" locale))
  (define p
    (get-pure-port
     (mk-api-url (string-append "revisions/" (get-root) "/" filepath)
                 params)
     AUTHORIZATION-HEADER))
  (define jsexp (read-json p))
  (close-input-port p)
  jsexp)

;; restores given remote file to specified revision
;; must use "rev" metadata field
;; ("revision" metadata field is deprecated)
(define (restore-file filepath rev #:locale [locale DEFAULT-LOCALE])
  (define params (format-params "rev" rev
                                "locale" locale))
  (define p
    (post-pure-port
     (mk-api-url (string-append "restore/" (get-root) "/" filepath)
                 params)
     (bytes)
     AUTHORIZATION-HEADER))
  (define jsexp (read-json p))
  (close-input-port p)
  jsexp)

;; searches for files whose name includes the query string
;; recursively searches subdirectories
(define (search remote-dir query
                #:file-limit [file-limit 1000]
                #:inc-del [inc-del "false"]
                #:locale [locale DEFAULT-LOCALE])
  (define params (format-params "query" query
                                "file_limit" (number->string file-limit)
                                "include_deleted" inc-del
                                "locale" locale))
  (define p
    (get-pure-port
     (mk-api-url (string-append "search/" (get-root) "/" remote-dir)
                 params)
     AUTHORIZATION-HEADER))
  (define jsexp (read-json p))
  (close-input-port p)
  jsexp)

;; publically shares specified file or dir at returned url
(define (get-share-url remote-path
                       #:locale [locale DEFAULT-LOCALE]
                       #:short-url [short-url "true"])
  (define params (format-params "locale" locale
                                "short_url" short-url))
  (define p
    (post-pure-port
     (mk-api-url (string-append "shares/" (get-root) "/" remote-path)
                 params)
     (bytes)
     AUTHORIZATION-HEADER))
  (define jsexp (read-json p))
  (close-input-port p)
  jsexp)

;; use this instead of get-share-url to stream media files
;; input must be a file (cannot stream a dir)
(define (get-media-url remote-file
                       #:locale [locale DEFAULT-LOCALE])
  (define params (format-params "locale" locale))
  (define p
    (post-pure-port
     (mk-api-url (string-append "media/" (get-root) "/" remote-file)
                 params)
     (bytes)
     AUTHORIZATION-HEADER))
  (define jsexp (read-json p))
  (close-input-port p)
  jsexp)

;; gets a copy_ref string, to use with copy-file
(define (get-copy-ref remote-file)
  (define p
    (get-pure-port
     (mk-api-url (string-append "copy_ref/" (get-root) "/" remote-file))
     AUTHORIZATION-HEADER))
  (define jsexp (read-json p))
  (close-input-port p)
  jsexp)

;; remote-file must be an image
;; #:format = "jpeg" (default) or "png"
;; #:size = "xs", "s", "m", "l", or "xl"
;; #:exists = 'error 'append 'update 'can-update 'replace 'truncate
;;            'must-truncate 'truncate/replace
(define (get-image-thumbnail remote-file local-file
                             #:format [format "jpeg"]
                             #:size [size "s"]
                             #:exists [exists 'error])
  (define params (format-params "format" format
                                "size" size))
  (define p
    (get-pure-port
     (mk-api-content-url 
      (string-append "thumbnails/" (get-root) "/" remote-file)
      params)
     AUTHORIZATION-HEADER))
  (define out (open-output-file local-file 
                                #:mode 'binary
                                #:exists exists))
  (write-bytes (port->bytes p) out)
  (close-output-port out)
  (close-input-port p))

;; uploads a file larger than 150mb
;; remote-filepath should be a remote file name and NOT a directory
;; #:chunk-size is in bytes (default = 4mb chunks)
(define (upload-large-file local-filepath remote-filepath 
                           #:locale [locale DEFAULT-LOCALE] 
                           #:overwrite? [overwrite? "true"]
                           #:parent-rev [parent-rev ""]
                           #:chunk-size [chunk-size 4194304]
                           #:verbose? [verbose? #f]
                           #:resume? [resume? #f]
                           #:resume-id [resume-id ""]
                           #:resume-offset [resume-offset 0])
  (define upload-id #f)
  (define offset 0)
  (define in (open-input-file local-filepath))
  (when resume?
    (when verbose?
      (printf "Resuming chunk upload, id = ~a\n" resume-id))
    (set! upload-id resume-id)
    (set! offset resume-offset)
    (read-bytes offset in)) ; skip these bytes
  (let LOOP ([chunk (read-bytes chunk-size in)])
    (with-handlers 
        ([exn:fail:network:errno? 
          (λ _ 
            (when verbose?
              (printf "Network connection lost. Returning resume thunk.\n"))
            (thunk 
             (upload-large-file local-filepath remote-filepath
                                #:locale locale
                                #:overwrite? overwrite?
                                #:parent-rev parent-rev
                                #:chunk-size chunk-size
                                #:verbose? verbose?
                                #:resume? #t
                                #:resume-id upload-id
                                #:resume-offset offset)))])
    (if (eof-object? chunk)
        (let ([params (format-params "locale" locale
                                     "overwrite" overwrite?
                                     "parent_rev" parent-rev
                                     "upload_id" upload-id)])
          (close-input-port in)
          (define p
            (post-pure-port
             (mk-api-content-url
              (string-append "commit_chunked_upload/" (get-root) "/" 
                             remote-filepath)
              params)
             (bytes)
             AUTHORIZATION-HEADER))
          (define jsexp (read-json p))
          (when verbose? 
            (printf "Chunk upload completed, id = ~a\n" upload-id))
          (close-input-port p)
          jsexp)
        (let ([params (if upload-id
                          (format-params "upload_id" upload-id
                                         "offset" (number->string offset))
                          (format-params "offset" "0"))])
          (define p
            (put-pure-port
             (mk-api-content-url "chunked_upload" params)
             chunk
             AUTHORIZATION-HEADER))
          (define jsexp (read-json p))
          (close-input-port p)
          (unless upload-id
            (when verbose?
              (printf "Chunk upload started, id = ~a\n" 
                      (hash-ref jsexp 'upload_id))))
          (set! upload-id (hash-ref jsexp 'upload_id))
          (when verbose?
            (printf "Chunk uploaded: bytes ~a to ~a\n" 
                    offset (hash-ref jsexp 'offset)))
          (set! offset (hash-ref jsexp 'offset))
          (LOOP (read-bytes chunk-size in))))))
  )

;; ----------------------------------------------------------------------------
;; file operations, ie copy, delete, move, etc
;; ----------------------------------------------------------------------------

(define (filter-search-by-exact-match filename search-res)
  (filter (λ (m) (string=? (hash-ref m 'path)
                           (string-append "/" filename)))
              search-res))

;; function to check if dropbox file already exists
(define (exists? dirname filename)
  (define filepath
    (if (string=? dirname "")
        filename
        (string-append dirname "/" filename)))
  (not
   (null? (filter-search-by-exact-match filepath (search dirname filename)))))
  
;; copy file or folder
(define (copy from to
                   #:locale [locale DEFAULT-LOCALE]
                   #:copy-ref [copy-ref #f])
  (define params 
    (if copy-ref
        (format-params "root" (get-root)
                                "from_path" ""
                                "to_path" to
                                "locale" locale
                                "from_copy_ref" copy-ref)
        (format-params "root" (get-root)
                       "from_path" from
                       "to_path" to
                       "locale" locale)))
  (define p
    (post-pure-port
     (mk-api-url "fileops/copy" params)
     (bytes)
     AUTHORIZATION-HEADER))
  (define jsexp (read-json p))
  (close-input-port p)
  jsexp)

(define (create-folder path #:locale [locale DEFAULT-LOCALE])
  (define params (format-params "root" (get-root)
                                "path" path
                                "locale" locale))
  (define p
    (post-pure-port
     (mk-api-url "fileops/create_folder" params)
     (bytes)
     AUTHORIZATION-HEADER))
  (define jsexp (read-json p))
  (close-input-port p)
  jsexp)

(define (delete path #:locale [locale DEFAULT-LOCALE])
  (define params (format-params "root" (get-root)
                                "path" path
                                "locale" locale))
  (define p 
    (post-pure-port
     (mk-api-url "fileops/delete" params)
     (bytes)
     AUTHORIZATION-HEADER))
  (define jsexp (read-json p))
  (close-input-port p)
  jsexp)

(define (move from to #:locale [locale DEFAULT-LOCALE])
  (define params (format-params "root" (get-root)
                                "from_path" from
                                "to_path" to
                                "locale" locale))
  (define p
    (post-pure-port
     (mk-api-url "fileops/move" params)
     (bytes)
     AUTHORIZATION-HEADER))
  (define jsexp (read-json p))
  (close-input-port p)
  jsexp)