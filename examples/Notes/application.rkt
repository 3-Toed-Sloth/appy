#lang racket
(require racket/gui
         racket/runtime-path
         (prefix-in gregor: gregor)
         images/icons/misc
         "../../gui.rkt")

(provide run)

;; Localization strings need to be recorded here:
(define-localizations
  (loc:notes-file "Appy Notes.appynote")
  (loc:notes-directory "Appy Notes Folder")
  (loc:application-title "Notes")
  (loc:button-delete "Delete Note")
  (loc:button-new "New Note")
  (loc:title-label "Title: ")
  (loc:tags-label "Tags: ")
  (loc:notes "Notes")
  (loc:all-notes "List of Notes")
  (loc:selected-note "Selected Note")
  (loc:new-note "New Note")
  (loc:no-note-displayed "No note displayed.")
  (loc:note-displayed "Displaying note ~s."))

(init-language-system #:application-name "Notes")
(load-language 'de_DE)

;; Runtime paths
(define-runtime-path icon-windows (build-path "assets" "NotesIcon.ico"))
(define-runtime-path icon-linux (build-path "assets" "NotesIcon.png"))
(define-runtime-path icon-mac (build-path "assets" "NotesIcon.icns"))
(define-runtime-path translation-de (build-path "assets" "de_DE.loc"))

;; Convenience wrappers for datetime to string conversions.
(define (text->time str)
  (gregor:iso8601->date str))

(define (time->text t)
  (gregor:datetime->iso8601 t))

(define (now-text)
  (time->text (gregor:now/utc)))

;; -----
;; MODEL
;; -----
;; We define a model as a subclass of storage%. Directly basing the model on a database
;; may not be desirable for larger applications but in this small example it will suffice.
(define model%
  (class storage%
    (inherit set get set-component delete-item new-item with-transaction search-text)
    
    ;; The public API for our model:
    (define/public (new-note title content tags)
      (define note (new-item 'Notes))
      (with-transaction
          (lambda ()
            (set 'Notes note 'Title title)
            (set 'Notes note 'Content content)
            (set 'Notes note 'Created (now-text))
            (set 'Notes note 'Modified (now-text))
            (set 'Notes note 'Tags tags)))
      note)

    (define/public (update-title note title)
      (update note 'Title title))

    (define/public (update-content note content)
      (update note 'Content content))
     
    (define/public (update-tags note tags)
      (update note 'Tags tags))

    (define/public (delete-note note)
      (delete-item 'Notes note))

    (define/public (get-title note)
      (get 'Notes note 'Title))

    (define/public (get-content note)
      (get 'Notes note 'Content))

    (define/public (get-tags note)
      (get 'Notes note 'Tags))

    (define/override (open)
      (super open)
      (set-component 'Notes '(Title text) '(Content text) '(Created text) '(Modified text) '(Tags list-of text)))

    (define/public (search str)
      (search-text 'Notes 'Title str #:search-mode 'substring))

    ;; Private part 
    (define (update note selector datum)
      (with-transaction
          (lambda ()
            (set 'Notes note selector datum)
            (set 'Notes note 'Modified (now-text)))))
    
    ;; It's just a 'single document application', so we initialize the storage class with a hard-coded path.
    ;; Notice that the file name is translated.
    (define notes-folder (build-path (find-system-path 'doc-dir) (loc:notes-directory)))
    (make-directory* notes-folder) ; The full path to the notes database needs to exist
    (super-new                     ; before we can hand the database path to the superclass.
     [file (build-path notes-folder (loc:notes-file))])))

;; ----
;; VIEW
;; ----
;; Next, we define the view which is responsible for displaying notes, the contents of a selected note,
;; and allows for searching, deleting, and creating notes.
(define notify? (make-parameter #t))

(define view%
  (class frame%
    (inherit create-status-line set-status-text get-height get-width get-x get-y)
    
    (define search #f)
    (define noteslist #f)
    (define title #f)
    (define content #f)
    (define tags #f)
    (define new-button #f)
    (define delete-button #f)
    (define gp2 #f)
    
    ;; Private part
    (define (init-display)
      (define vp0 (new vertical-panel% [parent this]))
      (define hp3 (new horizontal-panel% [parent vp0][stretchable-height #f][alignment '(left bottom)]
                       [horiz-margin 8]))
      (set! delete-button (new button% [label (loc:button-delete)]
                               [parent hp3] [callback (lambda (b evt) (maybe-notify 'delete this #f))]))
      (set! new-button (new button% [label (loc:button-new)]
                            [parent hp3] [callback (lambda (b evt)
                                                     (maybe-notify 'new this #f))]))
      (define hp1 (new horizontal-panel% [parent vp0] [vert-margin 8] [horiz-margin 8]))
      (define gp1 (new group-box-panel% [parent hp1] [stretchable-width #f] [label (loc:all-notes)]))
      (define vp1 (new vertical-panel% [parent gp1] [vert-margin 8][horiz-margin 8]))
      (define hp2 (new horizontal-panel% [parent vp1][min-width 200][stretchable-height #f]))
      (set! gp2 (new group-box-panel% [parent hp1] [label (loc:selected-note)]))
      (define vp2 (new vertical-panel% [parent gp2][vert-margin 8][horiz-margin 8]))
      ;; left side of window
      (define search-icon (new message% [parent hp2] [label (left-magnifying-glass-icon #:height 24)]))
      (set! search (new text-field%
                        [parent hp2]
                        [label #f]
                        [callback  (lambda (field evt)
                                     (maybe-notify 'search-change field (send field get-value)))]))
      (set! noteslist (new list-box% [parent vp1] [label ""] [choices '()][stretchable-height #t]
                           [callback (lambda (box evt)
                                       (if (send box get-selection)
                                           (maybe-notify 'selection-change box (send box get-data (send box get-selection)))
                                           (maybe-notify 'no-selection box #f)))]))
    
      ;; right side of window
      (set! title (new text-field% [parent vp2] [label (loc:title-label)]
                       [callback (lambda (field evt) (maybe-notify 'title-change field (send field get-value)))]))
      (set! content (new text-field% [parent vp2] [label #f] [style '(multiple)]
                         [callback (lambda (field evt) (maybe-notify 'content-change this (send field get-value)))]))
      (set! tags (new text-field% [parent vp2] [label (loc:tags-label)]
                      [callback (lambda (field evt) (maybe-notify 'tags-change this (send field get-value)))]))
      (note-display-enable #f))

    ;; It is important to notify the controller of this event, because it is a single window application.
    (define/augment (on-close)
      (pref 'window-height (get-height)) ; save window coordinates and size
      (pref 'window-width (get-width))   
      (pref 'window-x (get-x))
      (pref 'window-y (get-y))
      (notify 'application-shutdown this #f))

    ;; The view also handles enabling and disabling the input fields.
    ;; When note is #f, then they are disabled, otherwise enabled.
    (define/public (display-note note title-text content-text tag-text)
      (without-notification
       (lambda ()
         (select-note note)
         (cond
           (note
            (set-status-text (loc:note-displayed note))
            (note-display-enable #t)
            (send title set-value title-text)
            (send content set-value content-text)
            (send tags set-value tag-text))
           (else (note-display-enable #f) (set-status-text (loc:no-note-displayed)))))))

    ;; Unfortunately, set does not support the data field of a list-box%, which we use to store the note id.
    ;; So the data has to be set in a second loop.
    (define/public (display-search-results results)
      (without-notification
       (lambda ()
         (note-display-enable #f)
         (send noteslist set (map first results))
         (for ([datum (in-list results)]
               [i (in-naturals)])
           (send noteslist set-data i (second datum))))))

    (define/public (update-noteslist note title)
      (define idx (if (> (send noteslist get-number) 0)
                      (for/or ([i (in-range (send noteslist get-first-visible-item)
                                            (+ (send noteslist get-first-visible-item) (send noteslist number-of-visible-items)))])
                        (if (equal? (send noteslist get-data i) note) i #f))
                      #f))
      (when idx (send noteslist set-string idx title)))

    ;; Private
    (define (without-notification thunk)
      (parameterize ([notify? #f])
        (thunk)))

    (define (maybe-notify evt notifier data)
      (when (notify?) (notify evt notifier data)))

    
    (define (note-display-enable enable?)
      (without-notification
       (lambda ()
         (send gp2 enable enable?)
         (send title enable enable?)
         (send content enable enable?)
         (send tags enable enable?)
         (unless enable? (send title set-value "") (send content set-value "") (send tags set-value "")))))

    (define (select-note note)
      (define (deselect-all)
        (for ([i (in-list (send noteslist get-selections))])
          (send noteslist select i #f)))
      (when (> (send noteslist number-of-visible-items) 0)
        (deselect-all)
        (for/first ([i (in-range 0 (send noteslist get-number))]
                    #:when (equal? note (send noteslist get-data i)))
          (send noteslist select i))))

    (super-new
     [label (loc:notes)]
     [min-width 800]
     [min-height 600])
    
    (init-display)
    (create-status-line)))

;; ----------
;; CONTROLLER
;; ----------
;; Now a controller is defined that takes a view and a model and controls updating of the view and model.
;; We make this an instance of gui-application% to store the metadata and control startup and shutdown.
(define app%
  (class gui-application%
    (inherit close)
    
    [field (model (make-object model%))]
    [field (view (make-object view%))]
    [field (current-note #f)]

    ;; We don't need the following methods in this simple single-document application,
    ;; so they are set to (void).
    (define/override (open-file file)
      (void))

    (define/override (new-document)
      (void))

    (define/override (edit-preferences)
      (void))

    ;; Private
    (define (update-note-view)
      (if current-note
          (send view display-note
                current-note
                (send model get-title current-note)
                (send model get-content current-note)
                (tags->text (send model get-tags current-note)))
          (send view display-note #f "" "" '())))

    (define (tags->text tags)
      (if (empty? tags)
          ""
          (let ((first-tag (car tags)))
            (apply string-append
                   first-tag
                   (for/list [(tag (in-list (cdr tags)))]
                     (string-append "; " tag))))))

    (define (text->tags t)
      (map string-trim (string-split t ";")))

    (define (search str)
      (send view display-search-results
            (map (lambda (note) (list (send model get-title note) note))
                 (send model search str))))

    (define (init-prefs)
      ;; Default preferences
      (defpref 'window-height 600)
      (defpref 'window-width 800)
      (defpref 'window-x (send view get-x))
      (defpref 'window-y (send view get-y))
      (defpref 'app-launches 0)
      ;; Adjust the view according to prefs or center it on first launch.
      ;; This is not best practise. In a more realistic setting we have to check for screen size changes and
      ;; need to add a way for the user to revert these 'volatile' preferences to their defaults.
      (cond
        ((equal? (pref 'app-launches) 0)
         (send view center))
        (else
         (send view resize (pref 'window-width) (pref 'window-height))
              (send view move (pref 'window-x) (pref 'window-y))))
       (pref 'app-launches (add1 (pref 'app-launches))))
      
    ;; The gui-application% super class is instantiated with our defaults.
    (super-new
     [name "Notes"]
     [short-name "Notes"]
     [version "1.0"]
     [open-procs (list (lambda (app) (send model open)))]
     [close-procs (list (lambda (app) (send model close) (send view show #f)))]
     [run-procs (list (lambda (app)  (init-prefs) (send view show #t) (search "")))]
     [icons (list icon-linux icon-windows icon-mac)]
     [hardcoded-translations (list translation-de)])

    ;; Following are subscriptions to deal with the view events. We could of course also just
    ;; use native methods from Racket's class system, but this way we avoid troubles and gain some flexibility.
    ;; Still, the controller needs to make sure that event loops cannot occur. Event loops
    ;; are the bane of GUI frameworks and it would be nice to have some automatic way of preventing them, but its
    ;; not clear whether this is possible in general. A reactive GUI that maintains hidden state would be nicer but these
    ;; are rare and one would have to build one on top of Racket's.
    (subscribe this 'application-shutdown
               (lambda (evt notifier data)
                 (close)))

    (subscribe this 'new
               (lambda (evt notifier data)
                 (set-field! current-note this (send model new-note (loc:new-note) "" '()))
                 (search "") ; selects all notes, but does not clear the view's search field - a slight inconsistency
                 (update-note-view)))

    (subscribe this 'delete
               (lambda (evt notifier data)
                 (when current-note
                   (send model delete-note current-note)
                   (set-field! current-note this #f)
                   (search "")
                   (update-note-view))))

    (subscribe this 'title-change
               (lambda (evt notifier data)
                 (send model update-title current-note data)
                 (send view update-noteslist current-note data)))
 
    (subscribe this 'content-change
               (lambda (evt notifier data)
                 (send model update-content current-note data)))

    (subscribe this 'tags-change
               (lambda (evt notifier data)
                 (send model update-tags current-note (text->tags data))))

    (subscribe this 'search-change
               (lambda (evt notifier data)
                 (search (string-trim data))))

    (subscribe this 'selection-change
               (lambda (evt notifier data)
                 (set-field! current-note this data)
                 (update-note-view)))))

;; The following will be used by (deploy) for deployment:
(the-application (make-object app%)) 

(define (run)
  (send (the-application) open)
  (send (the-application) run))

;; The submodule main is started automatically by DrRacket, so you can run the application by runing this module in the IDE.
(module+ main
  (run))

;; Here should be the tests in a test submodule. These are left as an excercise to the reader.