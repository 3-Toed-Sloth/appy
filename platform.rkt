#lang racket
;; Platform - platform-based abstractions

(require net/sendurl
         net/url
         racket/gui/base
         racket/path)

(provide open-file open-url open-folder guess-file-purpose standard-file-open-command system-prefs-dir)

(define (standard-file-open-command)
  (case (system-type 'os)
    ((unix) #"xdg-open")
    ((osx) #"open")
    (else #"powershell -c")))
    
(define/contract (open-file p #:file-open-command [file-open-command (standard-file-open-command)] #:file-open-parameters [file-open-parameters #""])
  (->* ((or/c path? path-string?)) (#:file-open-command bytes? #:file-open-parameters bytes?) any)
  (unless (file-exists? p)
    (raise-argument-error 'open-file "file does not exist ~s" p))
  (system (bytes-append file-open-command
                        #" '"
                        (path->bytes (simplify-path p))
                        #"' "
                        file-open-parameters)))

(define/contract (open-url u [browser (external-browser)])
  (->* (url?) (browser-preference?) any)
  (parameterize ([external-browser browser])
    (send-url (url->string u))))

(define/contract (open-folder f)
  (-> (or/c path? path-string?) any)
  (open-file f))

(define/contract (guess-file-purpose path)
  (-> (or/c path-string? path?) any)
  (case (path-get-extension (if (string? path) (string-downcase path) path))
    ((#".webm" #".mkv" #".flv" #".vob" #".ogg" #".ogv" #".avi" #".mov" #".qt" #".wmv" #".yuv"
               #".rm" #".rmvb" #".asf" #".mp4" #".m4p" #".m4v" #".mpg" #".mp2" #".mpeg" #".mpe" #".mpv" #".m2v" #".m4v" #".svi" #".3g2" #".mxf" #".roq" #".nsv" #".f4v" #".f4p" #".f4a" #".f4b" #".h264" #".swf" #".vob" #".3gp" #".3g2")
     'video)
    ((#".icns" #".ico" #".ai" #".bmp" #".gif" #".jpeg" #".jpg" #".png" #".svg" #".ps" #".psd" #".pict" #".tif" #".tiff")
     'image)
    ((#".3gp" #".aa" #".aac" #".aax" #".act" #".aiff" #".amr" #".ape" #".au" #".awb" #".dct" #".dss" #".dvf" #".flac" #".gsm" #".iklax" #".ivs" #".m4a" #".m4b" #".mmf" #".mp3" #".mpc" #".msv" #".ogg" #".oga" #".mogg" #".opus" #".ra" #".rm" #".raw" #".sln" #".tta" #".vox" #".wav" #".wma" #".wv" #".webm" #".8svx")
     'audio)
    ((#".txt" #".text" #".tex" #".cfg" #".config" #".org" #".md" #".markdown")
     'plaintext)
    ((#".doc" #".docx" #".odt" #".pap" #".rtf" #".wks" #".wps" #".wpd" #".scriv" #".pdf")
     'wordprocessor)
    ((#".dll" #".exe" #".msi" #".apk" #".bat" #".bin" #".cgi" #".com" #".gadget" #".app" #".jar" #".py" #".wsf" #".desktop")
     'executable)
    ((#".bak" #".cab" #".cpl" #".cur" #".dmp" #".dump" #".drv" #".ini" #".pref" #".lnk" #".sys" #".tmp" #".deb" #".rpm" #".log")
     'system)
    ((#".7z" #".arj" #".pkg" #".rar" #".gz" #".zip" #".z")
     'compressed)
    ((#".bin" #".dmg" #".iso" #".toast" #"vcd")
     'disc-image)
    ((#".csv" #".dat" #".db" #".dbf" #".mdb" #".sql" #".sqlite" #".sqlite3" #".xml")
     'database)
    ((#".key" #".odp" #".pps" #".ppt" #".pptx")
     'presentation)
    ((#".c" #".class" #".cpp" #".ada" #".nim" #".rb" #".cs" #".html" #".h" #".java" #".sh"
            #".swift" #".vb" #".lsp" #".scm" #".lisp" #".ruby" #".rust")
     'programming)
    ((#".ods" #".xlr" #".xls" #".xlsx" #"xac")
     'office)
    ((#".fnt" #".fon" #".otf" #".ttf")
     'font)
    (else 'unknown)
    ))

(define/contract (system-prefs-dir appname)
  (-> path-string? path?)
  (case (system-type 'os)
    ((unix) (build-path (find-system-path 'home-dir)(string-append "." appname)))
    (else (build-path (find-system-path 'pref-dir) appname))))
  
(module+ test
  (require rackunit)

  (check-true (bytes? (standard-file-open-command)))
  (check-exn exn:fail:contract? (lambda () (open-file "no-such-file")))
  (check-equal? (guess-file-purpose "test.exe") 'executable)
  (check-equal? (guess-file-purpose "hello.exe/compressed.csv.7z") 'compressed)
  (check-exn exn:fail:contract? (lambda () (guess-file-purpose #"hello world")))
  ;; unfortunately the rest is difficult to test without user interaction, so automated tests are left out
  )