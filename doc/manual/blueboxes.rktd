565
((3) 0 () 2 ((c (? . 1) q application%) (q lib "appy/app.rkt")) () (h ! (equal) ((c meth c (c (? . 0) q open)) q (2847 . 2)) ((c meth c (c (? . 0) q shutdown)) q (3260 . 3)) ((c meth c (c (? . 0) q default-command-line-handler)) q (3084 . 4)) ((c meth c (c (? . 0) q get-full-name)) q (3202 . 2)) ((c def c (? . 0)) q (0 . 3)) ((c meth c (c (? . 0) q close)) q (2936 . 2)) ((c constructor c (? . 0)) q (55 . 45)) ((c def c (c (? . 1) q the-application)) q (3346 . 4)) ((c meth c (c (? . 0) q run)) q (2892 . 2)) ((c meth c (c (? . 0) q open-files)) q (2982 . 3))))
class
application% : class?
  superclass: object%
constructor
(new application%                                                    
                   [name name]                                       
                   [short-name short-name]                           
                   [version version]                                 
                   [open-procs open-procs]                           
                   [run-procs run-procs]                             
                   [close-procs close-procs]                         
                  [[exception-handler exception-handler]             
                   [shutdown-handler shutdown-handler]               
                   [command-line-handler command-line-handler]       
                   [start-empty-handler start-empty-handler]         
                   [open-file-handler open-file-handler]             
                   [pref-dir pref-dir]                               
                   [maintenance-start-proc maintenance-start-proc]   
                   [maintenance-end-proc maintenance-end-proc]       
                   [language language]                               
                   [hardcoded-translations hardcoded-translations]]) 
 -> (is-a?/c application%)
  name : string?
  short-name : string?
  version : string?
  open-procs : (listof (-> application%/c any))
  run-procs : (listof (-> application%/c any))
  close-procs : (listof (-> application%/c any))
  exception-handler : (->  application%/c exception? any)
                    = (appy-default-exception-handler)
  shutdown-handler : (-> application%/c
                         (or/c exception? boolean?) any)
                   = (lambda (app maybe-exn) (send app close) #t)
  command-line-handler : (-> application%/c   any)
                       = (lambda (app) (default-command-line-handler app))
  start-empty-handler : (-> application%/c   any)
                      = (lambda (app) (void))
  open-file-handler : (-> application%/c  (or/c path? path-string?) any)
                    = (lambda (app file) (void))
  pref-dir : (or/c path? path-string?)
           = (system-prefs-dir short-name)
  maintenance-start-proc : (-> string? any)
                         = (lambda (msg) (info 'application:open -200 name msg))
  maintenance-end-proc : (-> string? any)
                       = (lambda (msg) (info 'application:open -201 name msg))
  language : (or/c symbol? boolean?) = #f
  hardcoded-translations : (listof (or/c path? path-string?))
                         = '()
method
(send an-application open) -> any
method
(send an-application run) -> any
method
(send an-application close) -> any
method
(send an-application open-files listof) -> any
  listof : (or/c path? path-string?)
method
(send an-application default-command-line-handler application)
 -> any
  application : application%/c
method
(send an-application get-full-name) -> string?
method
(send an-application shutdown [exn]) -> any
  exn : exception? = #f
procedure
(the-application) -> application%/c
(the-application application) -> any
  application : application%/c
