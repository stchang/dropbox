#lang racket

;; Racket bindings for Dropbox API:
;; https://www.dropbox.com/developers/reference/api

;; TODO:
;; [o] = open, [x] = closed
;; [o] 2013-01-07: support locale
;; Add an optional parameter to functions of API calls that support
;; a locale parameter.

(require net/url
         net/uri-codec ; form-urlencoded->alist
         json
         )

(provide set!-APP-KEY
         set!-APP-SECRET
         set!-ACCESS-TYPE
         
         ;; oauth authentication
         obtain-request-token
         get-authorization-url
         obtain-access-token
         
         ;; account info
         get-account-info
         
         ;; files and metadata
         get-metadata
         upload-file
         )

;; ----------------------------------------------------------------------------
;; Constants for app key and app secret.
;; These must be set before starting OAuth authentication
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
;; returns a url to the specified dropbox api call
;; mk-api-url : string -> url
(define (mk-api-url api-call)
  (string->url (string-append "https://api.dropbox.com/1/" api-call)))

;; returns a url to the specified dropbox api call
;; uses api-content.dropbox.com instead of api.dropbox
;; mk-api-content-url : string -> url
(define (mk-api-content-url api-call)
  (string->url (string-append "https://api-content.dropbox.com/1/" api-call)))

(define (get-root)
  (if (string=? ACCESS-TYPE "dropbox")
      "dropbox"
      "sandbox"))

;; ----------------------------------------------------------------------------
;; other constants
;; ----------------------------------------------------------------------------
(define DEFAULT-LOCALE "en")
(define AUTH-URL-BASE "https://www.dropbox.com/1/oauth/authorize")

;; ----------------------------------------------------------------------------
;; OAUTH authentication functions
;; Step 1) call obtain-request-token
;; Step 2) go to url returned by get-authorization-url and grant access
;; Step 3) call obtain-access-token
;; ----------------------------------------------------------------------------

;; user should ignore these initial tokens/secrets and get their own
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
  
  (values OAUTH-REQUEST-SECRET OAUTH-REQUEST-TOKEN))

;; get authorization url
;; user should go to returned url to grant access
;; Step 2 of OAuth authentication process
(define (get-authorization-url #:locale [locale DEFAULT-LOCALE] 
                               #:callback [callback-url AUTH-URL-BASE])
  (string-append AUTH-URL-BASE
                 "?"
                 "oauth_token=" OAUTH-REQUEST-TOKEN "&"
                 "oauth_callback=" callback-url "&"
                 "locale=" locale
                 ))

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
  (define p (get-pure-port 
             (mk-api-url 
              (string-append "account/info"
                             "?"
                             "locale=" locale)) 
             AUTHORIZATION-HEADER))
  (define jsexp (read-json p))
  (close-input-port p)
  jsexp)

;; ----------------------------------------------------------------------------
;; functions to manipulate files
;; ----------------------------------------------------------------------------
(define (get-metadata path)
  (define p
    (get-pure-port
     (mk-api-url (string-append "metadata/" (get-root) "/" path))
     AUTHORIZATION-HEADER))
  (define jsexp (read-json p))
  (close-input-port p)
  jsexp)

;; remote-filepath should be a remote file name and NOT a directory
;; max filesize = 150mb
(define (upload-file local-filepath remote-filepath 
                     #:locale [locale DEFAULT-LOCALE] 
                     #:overwrite? [overwrite? "true"]
                     #:parent-rev [parent-rev ""])
  (define params (string-append 
                  "locale=" locale "&"
                  "overwrite=" overwrite? "&"
                  "parent_rev=" parent-rev))
  (define p
    (put-pure-port
     (mk-api-content-url
      (string-append "files_put/" (get-root) "/" remote-filepath
                     "?" params))
     (file->bytes local-filepath)
     AUTHORIZATION-HEADER))
  (define jsexp (read-json p))
  (close-input-port p)
  jsexp)