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
         obtain-request-token
         get-authorization-url
         obtain-access-token
         get-account-info)
         
;; ----------------------------------------------------------------------------
;; Constants for app key and app secret.
;; These must be set before starting OAuth authentication
;; ----------------------------------------------------------------------------
(define APP-KEY "")
(define APP-SECRET "")
;; ACCESS-TYPE = "app_folder" (limited access)
;;            or "dropbox"    (full access)
(define ACCESS-TYPE "app_folder")
(define (set!-APP-KEY akey) (set! APP-KEY akey))
(define (set!-APP-SECRET asec) (set! APP-SECRET asec))
(define (set!-ACCESS-TYPE atype) (set! ACCESS-TYPE atype))

;; ----------------------------------------------------------------------------
;; OAUTH authentication functions
;; Step 1) call obtain-request-token
;; Step 2) go to url returned by get-authorization-url and grant access
;; Step 3) call obtain-access-token
;; ----------------------------------------------------------------------------

(define OAUTH-REQUEST-TOKEN "")
(define OAUTH-REQUEST-SECRET "")
(define OAUTH-ACCESS-TOKEN "")
(define OAUTH-ACCESS-SECRET "")
(define AUTHORIZATION-HEADER null)

;; returns a url to the specified dropbox api call
;; mk-dropbox-api-url : string -> url
(define (mk-api-url api-call)
  (string->url (string-append "https://api.dropbox.com/1/" api-call)))

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
  
  #;(values OAUTH-REQUEST-SECRET OAUTH-REQUEST-TOKEN))

;; get authorization url
;; user should go to returned url to grant access
;; Step 2 of OAuth authentication process
(define (get-authorization-url)
  (string-append "https://www.dropbox.com/1/oauth/authorize"
                 "?"
                 "oauth_token="
                 OAUTH-REQUEST-TOKEN))

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
  
  #;(values OAUTH-ACCESS-SECRET OAUTH-ACCESS-TOKEN))


;; ----------------------------------------------------------------------------
;; functions to get account information
;; ----------------------------------------------------------------------------
;; get-account-info : (void) -> jsexpr
;; returns a JSON expression (ie hash table) with account info
(define (get-account-info)
  (define p (get-pure-port (mk-api-url "account/info") AUTHORIZATION-HEADER))
  (define jsexp (read-json p)#;(form-urlencoded->alist (port->string p)))
  (close-input-port p)
  jsexp)