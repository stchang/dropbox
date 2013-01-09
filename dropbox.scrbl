#lang scribble/manual
@(require scribble/eval
          (for-label "dropbox.rkt"
                     racket/contract
                     racket/base
                     json))

@title{Racket Dropbox SDK}

@(define the-eval (make-base-eval))
@(the-eval '(require "dropbox.rkt"))

@defmodule["dropbox.rkt"]

@author[@author+email["Stephen Chang" "stchang@racket-lang.org"]]

Dropbox SDK for Racket. Requires Racket version 5.3.1.12 or later.

@section[#:tag "authentication"]{OAuth authentication}

A Dropbox app must first get authentication to access a user's files. This is done in several steps.

@#reader scribble/comment-reader
(racketblock
  ;; 1) Set the app's key and key secret. 
  ;;    These will be used when obtaining request and access tokens.
  (set!-APP-KEY "3ysfqt0flcbex2t")
  (set!-APP-SECRET "hia6gkco347zczj")
  
  ;; 2) Get a request token. 
  ;;    The obtained request token and token secret are automatically saved
  ;;    internally and used to generate the authorization url and obtain the
  ;;    access token.
  (obtain-request-token)
  
  ;; 3) Direct the user to a browser pointed at the authorization url returned
  ;;    by get-authorization-url.
  (get-authorization-url)
  
  ;; 4) Get an access token. 
  ;;    This can only be done after the user grants access at the authorization
  ;;    url returned by get-authorization-url. The obtained access token and 
  ;;    token secret are automatically saved internally and sent as 
  ;;    authorization in the header of subsequent API calls.
  (obtain-access-token)
                    )

@defproc[(set!-APP-KEY [akey string?]) void?]{
  Sets the app key for your Dropbox app. App keys and secrets can be obtained @hyperlink["https://www.dropbox.com/developers/apps"]{here}.
}

@defproc[(set!-APP-SECRET [asecret string?]) void?]{
  Sets the app secret for your Dropbox app. App keys and secrets can be obtained @hyperlink["https://www.dropbox.com/developers/apps"]{here}.
}

@defproc[(set!-ACCESS-TYPE [atype (or/c "app_folder" "dropbox")]) void?]{
  Sets the access level type for your Dropbox app. This is set when creating the app @hyperlink["https://www.dropbox.com/developers/apps"]{here}. Possible values are @racket["app_folder"] (limited access) or @racket["dropbox"] (full access). More info on access levels @hyperlink["https://www.dropbox.com/developers/start/core"]{here}.
}

@defproc[(obtain-request-token) (values string? string?)]{
  Step 1 of OAuth authentication. Gets a request token and request token secret and saves it internally. Also returns the obtained request token and request token secret.
}

@defproc[(get-authorization-url [#:locale locale string? "en"]
                                [#:callback callback-url string? "https://www.dropbox.com/1/oauth/authorize"]) string?]{
  Step 2 of OAuth authentication. Returns a url in string form. Direct the app user to this page to grant the app access to the user's files. Takes an optional @hyperlink["https://www.dropbox.com/developers/reference/api#param.locale"]{locale} parameter and callback url to display after the user grants access.
}
                                                                           
@defproc[(obtain-access-token) (values string? string?)]{
  Step 3 of OAuth authentication. Gets an access token and access token secret and saves it internally. These will be automatically used in the authorization on subsequent API calls. Also returns the obtained access token and access token secret.
}

@section[#:tag "account"]{Account info}

@defproc[(get-account-info [#:locale locale string? "en"]) jsexpr?]{
  Returns information about the user's account as a @tech{jsexpr}.
}
          
@section[#:tag "updown"]{Uploading, downloading, and metadata}

@defproc[(get-metadata [path string?]
                        [#:file-limit file-limit number? 10000]
                        [#:hash hash string? ""]
                        [#:list lst (or/c "true" "false") "true"]
                        [#:inc-del inc-del (or/c "true" "false") "false"]
                        [#:rev rev string? ""]
                        [#:locale locale string? "en"]) jsexpr?]{
  Returns metadata of specified remote file or folder as a @tech{jsexpr}. See @hyperlink["https://www.dropbox.com/developers/reference/api#metadata"]{here} for more info about metadata fields.
  }
                                                                
@defproc[(upload-file [local-filepath string?]
                      [remote-filepath string?]
                      [#:locale locale string? "en"]
                      [#:overwrite? overwrite? (or/c "true" "false") "true"]
                      [#:parent-rev parent-rev string? ""]) jsexpr?]{
  Uploads a file less than 150MB. Use @racket[upload-large-file] for larger files. Both @racket[local-filepath] and @racket[remote-filepath] must be files and not directories. When @racket[parent-rev] is specified, the file will be replaced only if the latest version matches. Otherwise, the file will be renamed.
}
                                                                    
@defproc[(upload-large-file [local-filepath string?]
                            [remote-filepath string?]
                            [#:locale locale string? "en"]
                            [#:overwrite? overwrite? (or/c "true" "false") "true"]
                            [#:parent-rev parent-rev string? ""]
                            [#:chunk-size chunk-size number? 4194304]
                            [#:verbose? verbose? boolean? #f]
                            [#:resume? resume? boolean? #f]
                            [#:resume-id resume-id string? ""]
                            [#:resume-offset resume-offset number? 0]) (or/c jsexpr? thunk?)]{
  Uploads a file in chunks. Default @racket[chunk-size] is 4MB. Use this function to upload files greater than 150MB. When @racket[verbose?] is @racket[#t], the progress is printed as each chunk completes.
                                    
If an upload is interrupted due to network outage, a thunk is returned that resumes the upload when evaluated. Alternatively, an upload can be manually resumed by setting @racket[resumed?] to @racket[#t] and giving the appropriate @racket[resume-id] and @racket[resume-offset] (set @racket[verbose?] to @racket[#t] to get this information).

When the upload completes successfully, a @tech{jsexpr} is returned with the metadata of the uploaded file.
}
                                                                                             
@defproc[(download-file [remote-filepath string?]
                        [local-filepath string?]
                        [#:rev rev string? ""]
                        [#:exists exists 
                                  (or/c 'error 'append 'update 'can-update
                                        'replace 'truncate
                                        'must-truncate 'truncate/replace)
                                  'error]) void?]{
  Downloads specified file to specified local path. The @racket[exists] parameter is the same as in @racket[open-output-file].
}
                                                 
@defproc[(get-delta [#:cursor cursor string? ""]
                    [#:locale locale string? "en"]) jsexpr?]{
  Returns a @tech{jsexpr} with delta information between a user's local state and the server's state. More information @hyperlink["https://www.dropbox.com/developers/reference/api#delta"]{here}.
}
                                                            
@defproc[(get-revisions [filepath string?]
                        [#:rev-limit rev-limit number? 10]
                        [#:locale locale string? "en"]) jsexpr?]{
  Returns revision information about specified file as a @tech{jsexpr}.
}
                                                                
@defproc[(restore-file [filepath string?]
                       [rev string?]
                       [#:locale locale string? "en"]) jsexpr?]{
  Restore specified file to specified revision.
  }
                                                               
@defproc[(search [remote-dir string?]
                 [query string?]
                 [#:file-limit file-limit number? 1000]
                 [#:inc-del inc-del (or/c "true" "false") "false"]
                 [#:locale locale string? "en"]) jsexpr?]{
  Searches speficied directory for paths containing the specified query. Subdirectories are recursively searched. Results are returned as a @tech{jsexpr}.
}
                                                         
@defproc[(get-share-url [remote-path string?]
                        [#:locale locale string? "en"]
                        [#:short-url short-url (or/c "true" "false") "true"])
         jsexpr?]{
  Publicly shares the specified path (ie file or directory) and at the returned url in a @tech{jsexpr}. When @racket[short-url] is @racket[#t], a shortened url is used.
}
                 
@defproc[(get-media-url [remote-file string?]
                        [#:locale locale string? "en"])
         jsexpr?]{
  Publicly shares the specified file at the returned url in a @tech{jsexpr}. This function is better for streaming media because it bypasses the Dropbox webserver.
}
                 
@defproc[(get-image-thumbnail [remote-file string?]
                              [local-file string?]
                              [#:format format (or/c "jpeg" "png") "jpeg"]
                              [#:size size (or/c "xs" "s" "m" "l" "xl") "s"]
                              [#:exists exists 
                                  (or/c 'error 'append 'update 'can-update
                                        'replace 'truncate
                                        'must-truncate 'truncate/replace)
                                  'error]) void?]{
  Downloads a thumbnail for the specified image file to the specified local file. Image file must be jpg or png. The @racket[exists] parameter is the same as in @racket[open-output-file].
}
                                                 
@section[#:tag "fileops"]{File Operations: Copy, delete, move}

@defproc[(get-copy-ref [remote-file string?]) jsexpr?]{
  Returns a copy-ref in a @tech{jsexpr}. Can be used with @racket[copy]. More info @hyperlink["https://www.dropbox.com/developers/reference/api#copy_ref"]{here}.
                          }

@defproc[(copy [from string?]
               [to string?]
               [#:locale locale string? "en"]
               [#:copy-ref copy-ref (or/c #f string?) #f]) jsexpr?]{
  Copies specified file or folder to specified destination. When a non-false @racket[copy-ref] is specified, it is used instead of the @racket[from] path.
}
                                                                   
@defproc[(create-folder [path string?]
                        [#:locale locale string? "en"]) jsexpr?]{
  Tries to create the specified folder. If successful, returns a @tech{jsexpr} with the folder's metadata. Otherwise (ie, the folder already exists), the @tech{jsexpr} contains the error.
}
                                                                
@defproc[(delete [path string?]
                 [#:locale locale string? "en"]) jsexpr?]{
  Deletes the specified file or folder. Returns metadata for deleted file or folder as a @tech{jsexpr}.
  }
                                                         
@defproc[(move [from string?]
               [to string?]
               [#:locale locale string? "en"]) jsexpr?]{
  Moves specified file or folder to specified destination. Returns metadata for moved file or folder in a @tech{jsexpr}.
}                                                      