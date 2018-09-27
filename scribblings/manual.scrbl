#lang scribble/manual

@(require
   (for-label net/url)
   (for-label racket/class)
   (for-label net/sendurl)
   (for-label racket/serialize)
   (for-label racket/gui)
   (for-label appy/gui))

@title{@emph{Appy}: An Application Framework for Racket}
@author[(author+email "Erich Rast" "erich@snafu.de")]

@emph{Appy} is an application framework for writing GUI and commandline application in Racket. It provides an object-oriented API for
dealing with all kinds of mundane issues like preferences, storing data in simple key-value stores, internationalization,
command and undo management, and easy deployment. While it is still in its early development stages, the long-term goal of this
package is to make writing cross-platform end-user applications in Racket as seamless as possible.

@section{Overview}

@margin-note{This is the Reference Manual which lists the API in Section @secref["Reference"]. The @seclink["Guide" #:doc '(lib "appy/scribblings/guide.scrbl")]{guide document} provides an introduction and tutorials.}

@emph{Appy} is an object-oriented framework whose main classes are @racket[application%] and @racket[gui-application%] for building command line and GUI applications respectively.

@local-table-of-contents[]

@subsection{Features}

Creating and packaging a self-contained end-user application in Racket requires a lot of boring detail that varies only slighly between applications. @\emph{Appy} helps you with the following tasks:

@itemlist[@item{@bold{Application metadata:} The section on @seclink["Applications"]{applications} describes the two main application classes @racket[application%] and @racket[gui-application%] that deal with application name, localization, preferences, icons, exception handlers, and other issues. Once you have defined an instance of @racket[gui-application%] or a subclass thereof, you can @seclink["Deployment"]{compile and package} it automatically.}
          @item{@bold{Internationalization:} An @seclink["Localization"]{internationalization system} allows you to associate English format strings with symbolic keys using the @racket[defloc] macro, which defines translation functions and keys for the function @racket[tr]. Localizations can be added at runtime and at compile-time. Templates can be created and there is support for checking the integrity and completeness  of all translations before deployment.}
          @item{@bold{Error Handling:} The @seclink["Errors"]{error support} allows for localized error messages with numeric keys. While writing a module, you register numeric keys and corresponding translations for the error messages using @racket[register-errors] and use one of the error functions with the numeric key to raise the error. It is your job to ensure that each module has its own range of numeric error keys. While the system is a bit cumbersome, it has the advantage that error messages are localizable and therefore end-user readable by default and that numeric error codes can be used for further reference in a manual.}
          @item{@bold{Preferences:} The @seclink["Preferences"]{preferences system} provides functions for associating symbolic keys with preferences and saving and loading them from the place where they usually reside according to operating system conventions. Once the preference system has been initialized and default preferences have been defined with @racket{defpref}, serializable Racket values can be stored and retrieved with the @racket{pref} function. Preferences are stored in an Sqlite database that is handled automatically behind the scenes.}
          @item{@bold{Commands & Undo Management:} The @seclink["Undo"]{commands} and undo system handles a database-driven undo manager with disk-based, unlimited undo and redo functionality and provides functions and classes for defining commands that perform some function and are automatically managed by the Undo Manager. The disk-based method is slow and not suitable for every type of application but if you need the functionality it's there.}
          @item{@bold{An Easy-to-use Database:} The @seclink["Storage"]{storage} module provides a @racket[storage%] class that wraps an Sqlite database into an easy-to-use object-oriented API. If you need a simple key-value store or some way to store a few tables with columns without complicated queries, this class might come in handy. For more complex data-driven models it might be more advisable to use Racket's full database capabilities and SQL.}
          @item{@bold{Event-Subscriber System:} The @seclink["Notifications"]{notifications system} provides the means to implement the notify-subscriber pattern for easy communication between distant system components. Depending on how you use it, this may be a curse or blessing, but it greatly simplifies implementing the model-view-controller GUI pattern. Check out the @emph{Notes} example for a use of this module.}
          @item{@bold{Easy Deployment:} The @racket[deploy] function of the @seclink["Deployment"]{deployment} allows you to easily compile and package an application once it has been set up. It expects an instance of @racket[application%] or @racket[gui-application%], from which @racket[(deploy)] pulls all the necessary meta-information to compile the application and create a packed standalone distribution file. Pre and post compilation scripts can be provided to e.g. upload the distribution to a server.}]

@subsection{Importing the Library}

@defmodule[appy]{This imports the bindings for applications, storage, preferences, custom error handling, and internationalization but not any GUI-related functionality.}

@defmodule[appy/gui]{This imports all the bindings from @tt{appy} and in addition the support for building GUI desktop applications using Racket's GUI framework. For cross-platform graphical end-consumer applications, this is the right choice.}

@section{Reference}

@local-table-of-contents[]

@subsection{Applications}

You only need to import one of the two base modules and none of the specific modules listed below:

@itemlist[@item{Use @racket[appy] for the framework without any GUI-related classes and functions.}
          @item{Use @racket[appy/gui] for the framework with all GUI-related classes and functions.}]

As of the time of this writing, the two master modules do not differ much but in the future @racket{appy/gui} might become much larger than what @racket{appy} alone imports.

@subsubsection{The Base Application Class}

@defmodule[appy/app]{This module exports the basic application class, but not any classes or functions related to building gui-applications. It is re-exported by both @code{appy} and @code{appy/gui}.}

An instance of @racket{application%} is used for a basic application without any kind of graphical user interface, for example for a simple command line application that is to be run on the console.

@defclass[application% object% ()]{An instance of this class holds the meta-data of a non-gui application and procedures that are called when an application is opened, run, and closed.

 @defconstructor[([name valid-application-name?]
                  [short-name valid-application-name?]
                  [version valid-version-string?]
                  [open-procs (listof (-> application%/c any))]
                  [run-procs (listof (-> application%/c any))]
                  [close-procs (listof (-> application%/c any))]
                  [exception-handler (->  application%/c exception? any) (appy-default-exception-handler)]
                  [shutdown-handler (-> application%/c 
                                        (or/c exception? boolean?) any) (lambda (app maybe-exn) (send app close) #t)]
                  [command-line-handler (-> application%/c   any) (lambda (app) (default-command-line-handler app))]
                  [start-empty-handler (-> application%/c   any) (lambda (app) (void))]
                  [open-file-handler (-> application%/c  (or/c path? path-string?) any) (lambda (app file) (void))]
                  [pref-dir (or/c path? path-string?) (system-prefs-dir short-name)]
                  [maintenance-start-proc (-> string? any) (lambda (msg) (info 'application:open -200 name msg))]
                  [maintenance-end-proc (-> string? any) (lambda (msg) (info 'application:open -201 name msg))]
                  [language (or/c symbol? boolean?) #f]
                  [hardcoded-translations (listof (or/c path? path-string?)) '()])] 
                                                                                   
                 
 The @racket[name] is the verbose name of the application as a string. A valid name cannot be longer than 64 unicode glyphs and may not contain any of "?*\/" or ASCII 0. The @racket[short-name] has to fulfill the same contract and is used in messages, for filename, and menus. It should be much shorter and displayable as a menu entry. (This is not checked, since the requirements for such strings vary from platform to platform.) The @racket[version] string represents the version of the application in semantic version format as it is described at @url["https://semver.org/"].

 The @racket[open-procs], @racket[run-procs], and @racket[close-procs] arguments are lists of procedures that take the application object. When the corresponding @racket[open], @racket[run], and @racket[close] methods are called they are executed in the order they were specified. They are executed with the exception handler set to @racket[exception-handler] to provide control over the reported error messages. When the @racket[open-procs] are executed, the language and preferences subsystems have already been initialized.

 Applications install custom exception handlers in order to capture errors that weren't handled by the program. The @racket[exception-handler] init field takes a procedure that takes an application and an exception and handles the exception without returning control to the main program. The default exception handler raises a new exception with the original error message embedded.

 The @racket[shutdown-handler] is called when the application is shutdown. For a non-gui application this happens when a fatal exception has occurred. If the shutdown handler returns false, then this signals that the application does not want to quit. However, under normal circumstances this will not prevent the application from closing and you should finish all operations and close all open files in this handler. The default handler closes the application and returns true. Since @racket[application-quit-handler] is only available in @racket[racket/gui], it is not set for this handler. If you want the application to install an @racket[application-quit-handler], use @racket[gui-application%] instead.

 The @racket[command-line-handler] processes commandline arguments when the application is opened. The handler is called after the preference and localization systems have started but before the open procedures are called.

 The @racket[start-empty-handler] is called after the application has been opened if the field @racket[command-line-files] is empty. This field is populated by the -o open argument of the command line handler but subclasses may also populate it in other ways (e.g. in @racket[gui-application%] by opening documents associated with the application.

 The @racket[open-file-handler] is called for every file in the list of field @racket[command-line-files] after the application has been opened if the field @racket[command-line-files] is not empty.

 The @racket[pref-dir] init field is used to specify the location of the application preferences. By default, it is based on a platform-dependend system preferences directory in combination with the application's short name as in @racket[(system-prefs-dir short-name)] from @racket[appy/platform].

 The @racket[maintenance-start-proc] is called with a localized maintenance message when the application starts to perform automatic maintenance routines (database cleanup, etc.) and can be used to further customize the maintenance message. Since the maintenance depends on sqlite3 procedures of varying duration (as it depends on the state of the database), a time estimate cannot be given, but maintenance is usually short. The user may not interrupt this process and the application ought not quit, or otherwise data loss may occur.

 The @racket[maintenance-end-proc] is called with a localized message when the maintenance routine ends.

 If the @racket[language] init field is set to a valid language flag such as @racket['de_DE], the application checks during the open stage whether a localization for that language is available. If yes, the application switches to that language. If no, the application prints a warning to the error output and switches to the default @racket['en_US].

 The purpose of the @racket[hardcoded-translations] init field is to provide a list of paths or path strings to valid translation files for the application. You should use @racket[define-runtime-path] in order to ensure that these files are included in the binary by the @racket[deploy] procedure in @racket[appy/deploy].
   
 @defmethod[(open) any]{Opens the preferences, undo system, and language system, and then calls the open procedures in the order they are stored in the @racket[open-procs] init field.
 }

 @defmethod[(run) any]{
  Runs the @racket[run-procs] in the order in which they are specified in the list of the init field.
 }

 @defmethod[(close) any]{
  Runs the @racket[close-procs] in the order in which they are specified in the list of the init field, and then closes the preferences, undo system and language system.
 }

 @defmethod[(open-files (files (listof (or/c path? path-string?)))) any]{
  Calls the open file handler on each of the files in the list.
 }

 @defmethod[(default-command-line-handler (application application%/c)) any]{
  Calls the application's default command line handler that handle's file arguments and language settings. This ought not be called directly and instead the installed @racket[command-line-handler] ought to be used.
 }

 @defmethod[(get-full-name) string?]{
  Returns the full application name consisting of the name and version string.
 }

 @defmethod[(shutdown [exn (or/c exception? #f)]) void]{
  Shutdown the application. This calls the application close procedures and closes the preference, language, and undo systems.
 }
}

@defproc*[([(the-application) application%/c]
           [(the-application [application application%/c]) any])]{
 A parameter to get and set the application object. This is used by deployment procedures.
}

@defproc[(valid-application-name? (name string?)) boolean?]{
 Return true if the given name is admissible for applications. This procedure only performs a rough check and care should be taken not to include unusual characters in application names that the platform might process in a special way.}

@defproc[(valid-version-string? (version string?)) boolean?]{Return true if the given version string conforms with the semantic version specification, false otherwise. A semantic version must consist at least of three numbers separated by dots, as in "1.0.2". All three mandatory version numbers have to be provided and only these in the scheme are checked, whereas anything after a minus "-" like in "0.1.1-alpha.2" is not checked. Note that the full semantic version specification also has certain requirements for the optional alpha/beta/release and build numbers after "-".}

@subsubsection{GUI Applications}

@defmodule[appy/gui-app]{This module exports the gui application class. It is re-exported by @code{appy/gui}.}

An instance of @racket{gui-application%} is used for applications with graphical user interfaces. @emph{Appy} imports @racket{racket/gui} and automatically creates pre-fabricated dialogs for errors and messages whose look & feel might be improved in later versions.

@defclass[gui-application% application% ()]{An instance of this class holds the meta-data of a gui application and procedures that are called when an application is opened, run, and closed.
 @defconstructor[([name string?]
                  [short-name string?]
                  [version string?]
                  [open-procs (listof (-> application%/c any))]
                  [run-procs (listof (-> application%/c any))]
                  [close-procs (listof (-> application%/c any))]
                  [exception-handler (->  application%/c exception? any) (appy-default-exception-handler)]
                  [shutdown-handler (-> application%/c 
                                        (or/c exception? boolean?) any) (lambda (app maybe-exn) (send app close) #t)]
                  [command-line-handler (-> application%/c   any) (lambda (app) (default-command-line-handler app))]
                  [start-empty-handler (-> application%/c   any) (lambda (app) (send this new-document))]
                  [open-file-handler (-> application%/c  (or/c path? path-string?) any) (lambda (app file) (send app open-file file))]
                  [pref-dir (or/c path? path-string?) (system-prefs-dir short-name)]
                  [maintenance-start-proc (-> string? any) (lambda (msg) (info 'application:open -200 name msg))]
                  [maintenance-end-proc (-> string? any) (lambda (msg) (info 'application:open -201 name msg))]
                  [language (or/c symbol? boolean?) #f]
                  [hardcoded-translations (listof (or/c path? path-string?)) '()]
                  [icons (or/c (listof (or/c path? path-string?))
                               (->* (positive?) ((or/c string? #f)) bitmap%)) make-generic-icon-bitmap]
                  [preferences-handler (-> any) (lambda () (send this edit-preferences))]
                  [about-handler (-> any) (lambda ()
                                            (message-box (appy-about-title short-name)
                                                         "An application made with APPY, the ectlectic application framework for Racket."
                                                         '(ok)))])]

 See @racket[application%] for a description of the inherited superclass init fields and note that some of the handlers have new default values. Some of the new handlers such as the maintenance start proc open localized default dialogs instead of displaying text messages on the console and @emph{Appy} generally tries to provide reasonable GUI defaults.

 @bold{Additional init fields:}
 
 The @racket[icons] init field is either a list consisting of a .png file, an .ico file, and an .icns file for the Linux, Windows, and MacOS icon respectively, or it must be a procedure that takes a requested size and an optional name and returns a rectangular bitmap% of the requested size. The icon files should adhere to the platform recommendations of target operating system and provide at least a size of 128x128 pixels on Linux, sizes 16x16, 20x20, 24x24, 32x32, 40x40, 48x48, 64x64, 96x96, 128x128, 256x256 within the Windows .ico file, and an icon of 512x512 or even 1024x1024 on MacOS. The default procedure @racket[make-generic-icon-bitmap] creates a generic "Appy" icon. Note that using the generic procedure will take a very long time during deployment because the rendering is not cached.

 The @racket[preferences-handler] init field takes a procedure of no arguments that is called when the user requests to edit the preferences. This happens on MacOS, for instance, when the @emph{Preferences} item of the application menu is selected. The default preferences handler calls @racket[edit-preferences] in @racket[gui-application%], which should be overriden.

 The @racket[about-handler] init field takes a procedure of no arguments that is called when the user selects the "About <short-name>" menu entry of the application. By default it shows a dialog displaying that the application was made with @emph{Appy}.

                                                                   
 @bold{Additional methods:}
 
 The following methods need to be overridden to do be of any use, but have not been made abstract to make debugging easier.
 
 @defmethod[(open-file (file (or/c path? path-string?))) any]{ This method is called by the default @racket[open-file-handler] when an application document has been double-clicked or an @racket[--open] command line argument was provided. Override this method to deal with files that are provided to the application at startup. The default implementation displays a console message.}

 @defmethod[(new-document) any]{This method is called by the default @racket[start-empty-handler] when the application is started without any files provided (e.g. by directly clicking on the application icon.) Override this method to create and display an empty document in a multi-document application. The default implementation displays a console message.}

 @defmethod[(edit-preferences) any]{This method is called by the default @racket[preferences-handler] when the user selects the Preferences menu item of an application. Override this method to display a settings dialog that allows the user to adjust the relevant application preferences. The default implementation displays a console message.}
}

@subsection[#:tag "Storage"]{Storing Data}

@subsubsection{Imports}

@defmodule[appy/storage]{This module exports the @racket[storage%] database wrapper.}

@defmodule[appy/storage-interface]{Exports @racket[storage<%>] separately from the storage module in order to support implementations of other storage implementations.}

Both modules are re-exported by @racket{appy} and @racket{appy/gui}.

@subsubsection{The Storage Class}

The @racket[storage%] class provides a simplified, object-oriented database interface that may be used for application documents, for example. The database schemas it creates are opaque to the programmer and not normalized, hence it ought not be used for applications that require access to all SQL features or high performance. You should regard it as a quick & dirty solution that is better than rolling your own document format for storing complex data structures.

The storage stores entities as @emph{items} which are represented as exact positive integer numbers in Racket. Once you have obtained an item, you can use it to store and retrieve values in a schema that has been set by @racket[set-component]. 

@defclass[storage% object% (storage<%>)]{A simplified, object-oriented wrapper around an sqlite3 database.
 @defconstructor[([file (or/c path? path-string?)])]{

  The @racket[file] init field is a path to the filesystem location where the database file is to be stored. If the file exists, then the @racket[open] method will open it, otherwise a new empty database file is created at that location. The @racket[storage%] instance needs full write access to the containing directory, since Sqlite3 sometimes writes additional files such as locks and write-ahead logs next to the main database file. If the permissions to read & write files at the location and its containing directory are not sufficient, an exception is raised.
 }

 @defmethod[(set-component (component symbol?)  (attribute attribute-spec/c) ...) void]{
  Creates a new component with given symbol as name and a non-empty list of attributes as additional arguments. Each attribute spec is either of the form @racket[(name type)] where @racket[name] is a symbol for the attribute name (case sensitive) and type is one of the allowed types @racket['(text integer blob)] or of the form @racket[(name list-of type)] where the @racket[list-of] symbol signifies that attributes of that name provide lists of values of the given type.

  For example, @codeblock{(send a-storage set-component
   'Person
   '(Name text)
   '(Age integer)
   '(Emails list-of text))} creates a component with "Person" as table, "Name" as a text (VARCHAR) column, "Age" as an integer column, and a list of "Emails" entries of type text. In the latter case, a @racket[get] operation on the emails table will return a list of strings. This is internally implemented as relational link to a separate table.

  It is safe to set the component every time the storage instance has been opened, since the method checks whether the respective tables already exist.
 }

 @defmethod[(index (component symbol?) (attribute symbol?) ...) void]{
  Indexes the given attributes for faster search access.
 }

 @defmethod[(search-text (component symbol?) (attribute symbol?) (search-term search-string/c)
                         (#:search-mode search-mode (one-of/c '(starting-with substring exact) 'starting-with))
                         (#:sort-by sort-by (or/c symbol? #f) #f)
                         (#:sort-order sort-order (one-of/c 'asc 'desc) 'asc))
            item-list]{
  Search for a string in a given text attribute. The search term may be any string that does not contain "_" and "%". Search is case-insensitive and the optional @racket[#:search-mode] keyword argument may be one of:

  @itemlist[@item{starting-with: the search term must occur at the beginning of the attribute text.}
            @item{substring: the search term may occur anywhere in the attribute text.}
            @item{exact: the attribute text must be exactly like the search term except for case.}
            ]
  The optional @racket[#:sort-by] keyword argument must be the symbol for an attribute by which the results are sorted.

  The option @racket[#:sort-order] keyword argument specifies whether the search results are sorted in ascending order @racket['asc] or descending order @racket['desc].
 }

 @defmethod[(get (component symbol?) (item exact-positive-integer?) (attribute symbol?)
                 (#:on-fail failure-thunk (or/c (-> any) #f) #f)
                 (#:deleted? deleted? boolean? #f)) value-or-list-of-values]{
  Obtain the attribute value for the given item in the given component. The optional @racket[#:on-fail] keyword argument specifies a failure procedure that is called when the item does not exist. If the @racket[#:deleted?] keyword argument is true, then the attribute value of an item that has already been deleted may be returned. If the attribute is a list attribute, then a list of values is returned.
 }

 @defmethod[(get-latest-item (component symbol?)
                             (#:on-fail failure-thunk (or/c (-> any) #f) #f)
                             (#:deleted? deleted? boolean? #f)) exact-integer?]{
  Return the item that was last added in the given component. See the @racket[get] method for an explanation of the optional keyword arguments.
 }

 @defmethod[(set (component symbol?)
                 (item exact-positive-integer?)
                 (attribute symbol?)
                 (value (or/c string? bytes? exact?))) void]{
  Set the value of the attribute for the given item to the given value. The value must have the right type, as it was defined by @racket[set-component], or else an exception is may be raised or an unexpected result occurs. (This module does not check types and Sqlite is internally untyped. Racket converts the types automatically, so you can in theory store values of the wrong type without error. However, this is not recommended.)}

 @defmethod[(new-item (component symbol?)) exact-positive-integer?]{
  Create a new item in the component and return it. An item is just an exact positive integer number.}

 @defmethod[(contains? (component symbol?)
                       (item exact-positive-integer?)
                       (attribute symbol?))
            boolean?]{
  Return true if a value for the attribute and item in the given component exists (has been set), false otherwise.}

 @defmethod[(is-list-attribute? (component symbol?)
                                (attribute symbol?)) boolean?]{
  Return true if the given attribute in the component is a list attribute, false otherwise.}

 @defmethod[(open) void]{Open the storage database. If no associated database file is found, then a new empty database is created.}

 @defmethod[(close) void]{Close the storage database. A storage instance must always be opened before first using it and closed after the last operation has been performed on it (e.g. when the application shuts down).}

 @defmethod[(is-open?) boolean?]{Return true if the database is open, false otherwise. You may never open a storage instance twice without closing it first.}

 @defmethod[(needs-maintenance?) boolean?]{Return true if the database requires maintenance, false otherwise. See @racket[start-maintenance] to perform the maintenance.}

 @defmethod[(get-info) list?]{Return a list with information about the database. The first element represents @racket[needs-maintenance?], the second is the number of times the database was opened, and the third element is the number of objects that have been internally marked as being deleted.}

 @defmethod[(start-maintenance (done-callback (-> any))) void]{Start the maintenance, which consists in deleting items marked as deleted and vacuuming the database. After maintenance has finished, the @racket{done-callback} is called with no arguments.}

 @defmethod[(kv-set (key symbol?) (value serializable?)) void]{Set a value for the given key. While keys are symbols, values can be any values that are serializable according to module @racket[racket/serialize].}

 @defmethod[(kv-get (key symbol?) (failure-thunk (-> any))) any]{Return the value for key as a Racket value or call @racket[failure-thunk] with no arguments if no value was found.}

 @defmethod[(kv-has-key? (key symbol?)) boolean?]{Return true if the internal key-value story contains the key, i.e., when a value for the key has been set at least once; false otherwise.}

 @defmethod[(kv-delete (key symbol?)) void]{Delete the key-value pair for the given key if it exists.}

 @defmethod[(with-transaction (proc (-> any))) any]{Call @racket[proc] within a database transaction. This should be used whenever several storage operations are bundled in order to increase performance and failure safety.}
}

@defproc[(is-valid-search-term? (term string?)) boolean?]{Return true if the given string is an admissible search term. Search terms may not contain the characters "_" and "%".}

@subsection[#:tag "Undo"]{Commands & Undo Management}

Commands represent arbitrary operations that can be applied to (link-)targets. An undo manager automatically stores these operations and allows them to be undone or redone. Undo and redo information is persistent accorss program launches and only limited by disk space.

@emph{Important note: The current implementation is relatively slow in terms of storing undo information, so commands should correspond to high-level operations that bundle several smaller operations. In particular, commands should never be executed in tight loops when performance is needed. Since every command is undoable, you should generally bundle operations into one command rather than storing large sequences of repetitive commmands. For example, when the user types text into a text field, not every keystroke should be recorded but only the edit sequence as a whole after some amount of time such as every 10-20 seconds.}

@subsubsection[#:tag "links"]{Links}

@defmodule[appy/links]{This module provides ways to define links within the application. The Undo/Redo manager uses links to identify targets to which commands are applied. However, links may also be used independently to provide a way to address arbitrary instances of objects of both GUI subclasses or ordinary classes directly. This can sometimes be convenient to circumvent widget hierarchies that might change or are complicated to traverse.}

@defproc*[([(current-links) hash?]
           [(current-links [links hash?]) (void)])]{
 A parameter to get and set the current links, which are stored in a mutable hash table.
}

@defclass[link% object% ()]{Represents a link.
 @defconstructor[([linkpath (list-of symbol?)])]{
  Every link stores a list of symbols that represents a path.}
}

Links implement the @racket[equal<%>] interface and two links are equal if and only if their linkpaths are equal. Apart from that the API is a bit unconventional, as links themselves provide no methods but are used in the ordinary function calls listed below.

@defproc[(make-link (pathlist (listof any/c))) any]{Creates a link with given pathlist.}

@defproc[(new-link-for (obj any/c) (pathlist (listof any/c))) any]{Return a link that is associated with @racket[obj].}

@defproc[(clear-links) any]{Clear the link associations stored in parameter @racket[(current-links)].}

@defproc[(link->list (link (is-a?/c link%))) any]{Convert a link into a list.}

@defproc[(list->link (pathlist (listof any/c))) any]{Convert a pathlist to a (new) link.}

@defproc[(associate (link (is-a?/c link%)) (obj any/c)) any]{Associate the given link with the given object @racket[obj].}

@defproc*[([(resolve (link (is-a?/c link%))) any]
           [(resolve (link (is-a?/c link%)) (on-fail? any/c)) any])]{Resolve the link, returning the associated object. If the link is not associated with any object, then @racket[on-failure?] is returned if it is a value and called if it is a procedure that takes no arguments. By default, false is returned if no associated object is found.}

@defproc[(disassociate (link (is-a?/c link%))) any]{Remove the associatiation of the link to an object.}

@defproc[(stale? (link (is-a?/c link%))) any]{Return true if the link has no associated object, false otherwise.}

@definterface[link-addressable<%>  ()]{An interface that represents an object to which a link may lead, i.e., that may be associated with a link.

 @defmethod[(get-link) (or/c (is-a? link%) #f)]{Return the link that addresses this object, false if no link is available}}

@defmixin[link-mixin () (link-addressable<%>)]{An object that statically stores a link and automatically associates itself with that link. This mixin is used for objects whose link association does not change during runtime, such as particular instances of GUI elements that need to be addressed with a link.
 @defconstructor[([link (is-a?/c link%)])]{The link is provided as an initialization argument and automatically associated with the instance of the link-addressable class.}

 @defmethod[(get-link) link]{Return the link associated with this object.}
}                            
              
@subsubsection{The Undo Manager}

@defmodule[appy/commands]{This module provides functions and classes to define generic commands that are automatically managed by an Undo/Redo manager.}

Under normal circumstances, the Undo Manager is used by calling @racket[open-undo-system] first. Define @racket[command%] instances using @racket[make-command] and ensure that the objects that process a command return suitable @racket[result%] instances. These objects will usually be instances of @racket[undo-mixin] or implement the @racket[undo-target<%>] interface manually. Then you can use the functional interface defined below, which implicitly calls the parameter @racket[(current-undo-manager)] to retrieve the default undo manager instanciated by @racket[open-undo-system]. When everything is done, call @racket[close-undo-system] to close the system and make all changes on disk permanent. The underlying @racket[undo-manager%] class needs not be instanciated manually when the system is used that way.

@defproc*[([(current-undo-manager) (or/c (is-a?/c undo-manager%) #f)]
           [(current-undo-manager [undo-manager  (or/c (is-a?/c undo-manager%) #f)]) any])]{
 A parameter to get and set the undo manager.
}

@defproc*[([(command-table) hash?]
           [(command-table [table hash?]) (void)])]{
 A parameter to get and set the current command-table, which is a mutable hash table that holds all registered commands.
}

@defproc[(open-undo-system (undo-file (or/c path? path-string?))
                           (#:maintenance-start-proc start-proc (-> string? any))
                           (#:maintenance-end-proc end-proc (-> string? any))
                           (#:maintenance-start-message maintenance-start-message string?)
                           (#:maintenance-end-message maintenance-end-message string?)) any]{Create an undo manager instance with given undo file and store it in the @racket[current-undo-manager] parameter. This undo manager is then called by the procedures below. When the Undo Manager needs to start maintenance it does so automatically. When maintenance starts the @racket[start-proc] is called with the @racket[maintenance-start-message] and when maintenance has stopped the @racket[end-proc] is called with the @racket[maintenance-end-message].}

@defproc[(close-undo-system) any]{Closes the current undo manager and makes changes permanent. After this procedure has been called, no operations can be undone or redone any longer and attempting to do so will result in an error.}


@defproc[(can-undo?) boolean?]{Return true if the current undo manager can undo a command, false otherwise.}

@defproc[(undo) any]{Undo the last command, or return false if no command can be undone.}

@defproc[(can-redo?) boolean?]{Return true if the current undo manager can redo a command, false otherwise.}

@defproc[(redo) any]{Redo the last undone command, or return false if nothing can be redone.}

@defproc[(get-undo-command) (is-a?/c command%)]{Return the command to be undone, or false if there is no command to be undone.}

@definterface[undo-target<%> ()]{An interface that represents the possible target object of an undo or redo operation.

 @defmethod[(execute (command (is-a?/c command%)) (args (listof any/c))) any]{Executes the command.}

 @defmethod[(can-execute? (command (is-a?/c command%)) (args (listof any/c))) boolean?]{Return true if the command can be executed with the given argument, false otherwise.}

 @defmethod[(can-undo? (result (is-a?/c result%))) boolean?]{Return true if the given command that yielded the given result can be undone, false otherwise.}

 @defmethod[(can-redo? (result (is-a?/c result%))) boolean?]{Return true if the command that yielded the given result and was previously undone can be redone, false otherwise.}

 @defmethod[(undo (result (is-a?/c result%))) any]{Undo the command that yielded the given result.}

 @defmethod[(redo (result (is-a?/c result%))) any]{Redo the command that yielded the given result and was previously undone.}

}

@defmixin[undo-mixin () (undo-target<%> link-addressable<%>)]{An object that can be the target of undo/redo operations via @racket[command%] instances and that is addressed in commands via a link.

 @defconstructor[([link (is-a?/c link%)])]{The link that points to this instance. See Section @secref["links"] for more information about links.}

 @defmethod[(get-link) any]{Return the link associated with this instance unless the automatic association has been changed directly by the programmer.}
}

@defclass[command% object% (equal<%>)]{A generic command. Two commands are equal iff. their numeric Ids are equal.
 @defconstructor[([numeric exact-integer?]
                  [symbolic symbol?]
                  [name string?]
                  [help (or/c (->* () () #:rest (listof string?) string?)
                              string?)]
                  [shortcuts (listof (or/c symbol?
                                           string?
                                           char?))])]{
  The @racket[numeric] init-field represents the numeric command Id. It is used for determining command equality and there should only be one command per Id.

  The @racket[symbolic] init-field represents a symbolic command Id and may be used for various interfaces such as commandline interfaces.

  The @racket[name] init-field is the user-readable name of the command and is usually localized. It should adhere to platform limitations for menu entries in GUI applications, which is not checked, however.

  The @racket[help] init-field is either a help string or a procedure that takes a variable number of strings and yields a string that is constructed out of these arguments - usually, the procedure will use format to create a help string out of a fixed number of arguments for e.g. the application name or name of the command.

  The @racket[shortcuts] init-field is a list of symbols, strings, or characters that represents the menu shortcuts for the command.}

 @defmethod[(execute (target-link (is-a?/c link%)) (arg any/c) ...) any]{Execute the command to the object associated with @racket[target-link] and with zero or more arguments given.}

 @defmethod[(can-execute? (target-link (is-a?/c link%)) (arg any/c) ...) boolean?]{Return true if the object associated with @racket[target-link] can execute the command with the given arguments, false otherwise. This may be used by a menu handler to check whether the menu should be active, for instance.}
}

@defproc[(make-command (symbolic symbol?) (name string?)
                       (help (or/c (->* () () #:rest (listof string?) string?)
                                   string?))
                       (keyboard-shortcuts (listof (or/c symbol?
                                                         string?
                                                         char?)))) any]{Create a command with arguments like in the init-fields of @racket[command%], where a unique numeric Id is created automatically by increasing an internal counter.

 @bold{Caveats:}

 @itemlist[@item{This convenience method is not safe for concurrency and should only be used in a single thread.}
           @item{If you spread uses of this method accross modules or make changes in the source code, bear in mind that two commands are equal if they have the same Id, not equal otherwise. You need to make sure that the order of calling this method is kept accross program versions and executions so that the same Id is assigned to the same command in future versions and after program changes. Otherwise, undo/redo information may become inconsistent and the persistent Undo Manager will no longer work as expected.}]}

@defclass[result% object% ()]{Represents the result of a command execution and is used by the Undo Manager to store the command and associated data.
 @defconstructor[([command (is-a?/c command%)]
                  [result serializable?]
                  [target-link (is-a?/c link%)]
                  (undo-data serializable?)
                  [redo-data serializable?])]{The @racket[command] contains the command that was executed.

  The @racket[result] stores the result of the particular command execution, which must be a serializable data.

  The @racket[target-link] is the link to the associated target object to which the command was applied.

  The @racket[undo-data] and @racket[redo-data] init-fields are used to store serializable data that the respective command target uses to undo and redo the operation when the Undo Manager calls it with the given result object.}

 @defmethod[(can-undo?) boolean?]{Resolves the target link and calls the target object's @racket[can-undo?] method, returns false if the link cannot be resolved or the command cannot be undone.}

 @defmethod[(undo) any]{Undo the command that resulted in this result by resolving the target link and calling the target object's @racket[undo] method.}
 
 @defmethod[(can-redo?) boolean?]{Resolves the target link and calls the target object's @racket[can-redo?] method, returns false if the link cannot be resolved or the command cannot be undone.}

 @defmethod[(redo) any]{Redo the command that resulted in this result by resolving the target link and calling the target object's @racket[redo] method.}
}

@defclass[undo-manager% storage% ()]{A database that persistently stores undo and redo information based on @racket[result%] instances and can undo and redo commands. The default undo manager is stored in parameter @racket{(current-undo-manager)}.
 @defconstructor[([file (or/c path? path-string?)])]{Create an undo manager.

  The @racket[file] init-field is inherited from @racket[storage%] and is used to store the undo/redo data. Like in case of the storage superclass, the directory in which this file resides must be generally writeable and the database engine might write additional files such as locks or writeahead logs in this directory.}
                                                                                                            
 @defmethod[(init) (void)]{This method always needs to be called before the undo manager can be used.}

 @defmethod[(add-result (result (is-a?/c result%))) any]{Add the result to the undo stack.}

 @defmethod[(can-undo?) boolean?]{Return true if the undo stack is not empty (i.e, a result is available to be undone), false otherwise.}

 @defmethod[(get-undo-command) (is-a/c command%)]{Get the command associated with the latest result on the undo stack.}

 @defmethod[(undo) any]{Undo the latest result from the undo stack, removing it from the undo stack and putting it on the redo stack.}

 @defmethod[(can-redo?) boolean?]{Return true if the redo stack is not empty (i.e., a previously undone result can be redone), false if the redo stack is empty.}

 @defmethod[(redo) any]{Redo the latest result from the redo stack, removing it from the redo stack.}

 @defmethod[(clear-redo-chain) any]{Remove all redo items from the redo stack. Nothing can be redone after this command.}

}

@subsection{Localization}

@defmodule[appy/lang]{@emph{Appy}'s localization and internationalization system. This is re-exported and used by both @racket[appy] and @racket[appy/gui].}

@subsubsection{Overview}

The localization module is a bit more complicated than the other ones and provides a trade-off between static and dynamic (runtime) translations by allowing one to define translations at compile time, which are later loaded from modifiable files at runtime. This allows end users of an application to provide their own translations as long as the programmer provides a way to obtain a translation template.

Before using the module at runtime, it first has to be initialized with @racket[init-language-system]. However, even before this initialization step it is possible to define default localizations using @racket[define-localizations] at compile time. This macro defines a procedure for each key, localization string pair. The localizations are defined for @racket['en_US] as the default language. Uses of @racket[define-localization] can be spread accross several modules. Translations are read at runtime from the current localization folder by using @racket[load-language]. Entries can be translated explicitly by the @racket[tr] function or the procedures defined by @racket[define-translations] can be used. To get a translation template for some language, use @racket[write-translation-template] and existing translations may be checked with @racket[check-translations] before deployment. If in an @racket[application%] instance hardcoded translations are provided  - usually by defining the paths using @racket[racket/runtime-path] -, these translations will nevertheless be written to the current translations directory so they can be modified, including the @racket['en_US] default translation file.

@subsubsection{Localization API}

@defproc[(check-translations) any]{Checks the installed translations for consistency and missing keys and reports the results to the console. This procedure should be used as a last step before deployment.}

@defproc*[([(current-language) symbol?]
           [(current-language [lang symbol?]) any])]{
 A parameter for the current language. Use @racket[load-language] to change the language instead of attempting to set the language by this parameter directly.
}

@defproc*[([(current-localization-folder) (or/c path? path-string?)]
           [(current-localization-folder [folder (or/c path? path-string?)]) any])]{
 A parameter that contains the current localization folder. Use it to query the folder during the development if you can't find it. The parameter should not be used for anything else.
}


@defform[(define-localizations [key translation-string] ...)
         #:contracts ([key identifier?] (translation-string string?))]{Define a list of translations for the default language @racket['en_US]. Every key,translation pair defines a translation with @racket[key] and the translation string @racket[translation-string], as well as a procedure with name @racket[key] that takes an arbitrary number of format string arguments and provides the translation and can be exported (e.g. @racket[all-defined-out] in a provide clause). It is strongly recommended to use the procedures and not the keys directly, since using @racket[tr] with a key will produce an error only at runtime, whereas a spelling error with a procedure name is detected at compile time. The number of format string arguments in a translation string must match the number of arguments in the corresponding procedure or additional arguments in @racket[tr], or otherwise a runtime error is produced. Procedure @racket[check-localizations] checks that format strings of translations have the same number of argument places, though without taking into account any escape sequences.}

@defproc[(init-language-system (#:application-name application-name valid-application-name?)
                               (#:hardcoded-paths hardcoded-paths (listof (or/c path? path-string?)) '())
                               (#:write-templates? write-templates? boolean? #f)) any]{Initializes the translation system for runtime use. The application name is used to build the localization directory, which is located at the OS-specific preference directory plus a directory made from the application name and the directories @tt{app-data} and @tt{localizations}. If hardcoded paths are provided, the corresponding files are copied into the localization directory unless they already exist. If @racket[write-templates?] is true, then a template is created for each of the provided hardcoded files. Such a template can be used to translate to another language.}

@defproc[(language-available? (lang symbol?)) boolean?]{Return true if the language is available, false otherwise. By convention, names like @racket['en_US], @racket['de_DE], and @racket['de_AT] should be used, where the first part specifies the language and the second part a regional variation.}

@defproc[(list-available-languages) (listof symbol?)]{Return a list of all available language varieties as symbols.}

@defproc[(load-language (lang symbol?)) any]{Activates the given language, displays a warning on the console error port and switches to the default English if the language is not available.}

@defproc[(tr (key symbol?) (arg any/c) ...) string?]{Translate by key with given format string arguments (or none).}

@defproc[(write-translation-template (lang symbol?)) any]{Writes an empty translation template based on the current default translations and stores the file in the current localization folder. The usual procedure is to edit this file, move it into an assets directory, define a runtime path to it, and provide it as a hardcoded translation. Of course, this should only be done once all default translations are fixed and no longer change.}

@subsection{Preferences}

@defmodule[appy/pref]{A simple dynamic preference system.}

The preferences are stored in a @racket[storage%] instance using the built-in key-value methods that can be used to store serializable data. Notice that before using @racket[pref] you need to define a default value by using @racket[defpref]. These default values are stored in memory and the database is only changed when a default is changed.

@defproc*[([(current-prefs-storage) (is-a?/c storage%)]
           [(current-prefs-storage [storage (is-a?/c storage%)]) any])]{
 A parameter that controls the @racket[storage%] instance used for storing preferences.
}

@defproc[(prefs-open (file (or/c path? path-string?))
                     (#:maintenance-start-proc start-proc (-> string? any) (lambda (msg) (void)))
                     (#:maintenance-end-proc end-proc (-> string? any) (lambda (msg) (void)))
                     (#:maintenance-start-message start-message string? "Compacting preferences...")
                     (#:maintenance-end-message end-message string? "Compacting preferences... done.")) any]{Initialize the preference system. This function must be called before other preference functions except @racket[prefs-open?] can be used. The file argument is a path to a database file. The same caveat as for the @racket[storage%] class applies: The underlying Sqlite database may write additional files within the directory in which the database file resides.

 If the underlying storage instance requires maintenance, this is called automatically. The optional maintenance start and end procedures and the start and end messages are used like in @racket[storage%] to inform the user about the delay. (Preference databases rarely exceed a size that would make maintenance noticeable slow, so an automatic cleanup was chosen for convenience.)}

@defproc[(prefs-open?) boolean?]{Return true if the preferences have been opened, false otherwise.}

@defproc[(pref? (key symbol?)) boolean]{Return true if there is a preference for the given key, false otherwise.}

@defproc[(defpref (key symbol?) (default any/c)) any]{Define the default preference for the given key. This is a function not a macro, so you have to quote the key.}

@defproc[(prefs-close) any]{Close the preference system. This function should be called once when the application is shutdown, and afterwards the preference system can no longer be used.}

@defproc*[([(pref (key symbol?)) any]
           [(pref (key symbol?) (value serializable?)) (void)])]{The first form obtains the preference for a given key or raises an exception when no preference has been found for the key. The second form sets the preference for the given key; it does not "pass through" the value.}

@subsection[#:tag "Errors"]{Error Handling}

The error handling module uses integer error numbers as keys for localized error messages. Error numbers have the advantage of easy automatic processing and allow for easy lookup of error numbers in the application's manual. It is left to you to ensure that error numbers do not overlap. A simple convention may e.g. be that every module registers errors in its own number range and the range leaves enough space for later extensions. For example, Module A might use error numbers from 100-199, Module B from 200-299, and so on.

@defmodule[appy/err]{A functional error handling interface that is also used by @emph{Appy} internally. This module imports the @racket[appy/lang] module and assumes that error messages should be lozalized, i.e., it is intended for error messages that are read by end users.}

@defproc[(register-errors (listof (list/c exact-integer? (or/c string? symbol?)))) any]{Add a list of error messages to the system by numeric keys. By convention, @emph{Appy} uses negative integers and user keys need to be positive integers. Duplicate numbers raise an exception. The value associated with the numeric key may be a format string or it may be a symbol. If it is a symbol, then the error module uses @racket[tr] from @racket[appy/lang] to translate the error message with the value as key. If it is a string, the string is used directly to produce the error message.}

@defproc[(warn (source 'symbol) (num exact-integer?) (arg any/c) ...) any]{Emit a warning message which will be logged if the current @racket[app-logger] level is set to @racket['warning] or higher.}

@defproc[(fatal (source 'symbol) (num exact-integer?) (arg any/c) ...) any]{A fatal error. The error is first logged and then an exception is raised.}

@defproc[(info (source 'symbol) (num exact-integer?) (arg any/c) ...) any]{Emit an info message which will be logged if the current @racket[app-logger] level is set to @racket['info] or higher.}

@defproc[(fail (source 'symbol) (num exact-integer?) (arg any/c) ...) any]{Like @racket[fatal] but at log level @racket['error].}

@defproc[(debug (source 'symbol) (num exact-integer?) (arg any/c) ...) any]{Emit a debug message which will be logged if the current @racket[app-logger] level is set to @racket['debug] or higher.}

@defproc*[([(current-error-messages) hash?]
           [(current-error-messages (messages hash?)) any])]{
 A parameter that stores the current error messages in a hash table.
}

@defproc*[([(current-logger) logger?]
           [(current-logger (logger logger?)) any])]{
 A parameter that stores the current logger, which is created as @racket[(define-logger app)]. 
}

The module also exports procedures @racket[log-fatal], @racket[log-error], @racket[log-info], @racket[log-debug], @racket[log-warning] and the logger @racket[app-logger] to which @racket[current-logger] is set by default. See Section @secref["logging" #:doc '(lib "scribblings/reference/reference.scrbl")] of the Racket manual for more information about loggers.

The framework defines a few localizable error messages whose localization procedures start with the prefix @racket[appy-] and whose error keys are negative integers. These need to be translated into the target language as well if all error messages are to be user-readable.

@subsection{Notifications}

@defmodule[appy/notify]{Provides functions for the one-to-many @\emph{notifier-subscriber} programming pattern where an entity sends notifications that are received by one or more subscribers.}

@defproc[(notify (event any/c) (notifier any/c) (data any/c)) any]{Send a notification from the notifier to any subscribers for the given event with the given data.}

@defproc[(subscribe (subscriber any/c) (event any/c) (callback (-> any/c any/c any/c any)) (exclusions (listof any/c) '())) any]{For the given subscriber, subscribe to the given event with a callback that takes an event, a notifier, and some data as arguments. The optional exclusions work as follows: If the receiver attempts to send an event in the exclusion list via @racket[notify] within @racket[(notify-prune-delta-msec)] milliseconds since the original event was received, then the new notification is blocked. The exclusion list is pruned when @racket[(notify-prune-frequency-msec)] milliseconds is reached since the last notification to prevent it from growing indefinitely.}

The exclusion list functionality can be used to prevent event loops but it is rather crude. The problem is that it doesn't allow the definition of event hierarchies for mutual exclusion, and is therefore only useful for simple cases.

@emph{Caution: The API for exclusions is currently not stable and may change in future versions.}

@defproc[(unsubscribe (subscriber any/c) (event any/c)) any]{Unsubscribe a subscriber from an event. The function does not change anything and does not raise an exception if the subscriber has not subscribed to the event.}

@defproc[(remove-subscriber (subscriber any/c)) any]{Remove all subscriptions of the subscriber.}

@defproc[(remove-all-subscribers) any]{Remove all subscriptions and subscribers from receiving any notifications. The function should be called during application shutdown when notifications ought no longer have any effect.}
                                                                                                                            
@defproc*[([(notify-prune-delta-msec) real?]
           [(notify-prune-delta-msec [msec real?]) any])]{
 A parameter for the number of milliseconds during which an event on an exclusion list is blocked. After that time is passed, the event is no longer blocked, so an event loop might occur. Default value is 8000.
}

@defproc*[([(notify-prune-frequency-msec) real?]
           [(notify-prune-frequency-msec [msec real?]) any])]{
 A parameter for the number of milliseconds that have to pass since the last pruning of the global event exclusion buffer before it is pruned again.  Default value is 5000. Note that entries are only pruned when in addition @racket[(notify-prune-delta-msec)] have passed since the excluding event was received.
}

@subsection{Deployment}

@defmodule[appy/deploy]{High-level support for deploying instances of @racket[application%] and @racket[gui-application%].}

The usual way to deploy an application is to create the application instance in some module, say, in "application.rkt". When this module is required, the application does not start yet, but an instance of @racket[application%] or @racket[gui-application%] is stored in parameter @racket[the-application]. Then create another file named "main.rkt" as a module that loads the necessary application modules and starts the application. In a separate file "deploy-application.rkt", require "application.rkt". You can then deploy the application simply by calling @racket[(deploy)].

@emph{@bold{Important} When deploying an application from DrRacket's IDE, you @emph{have} to switch the language to option "No debugging or profiling" in the advanced language settings (Choose Language -> Show Details -> Dynamic properties section). Packaging will fail with an error message if debugging or profiling is on!}

@defproc[(deploy (#:source-file source-file (or/c path-string? path?) (build-path (current-directory) "main.rkt"))
                 (#:application app (is-a?/c application%) (the-application))
                 (#:built-dir built-dir (or/c path-string? path?) (build-path (current-directory) "bin"))
                 (#:distributions-dir distributions-dir (or/c path-string? path?) (build-path (current-directory) "distributions"))
                 (#:pre-proc pre-proc (-> any) (lambda () (void)))
                 ( #:post-proc post-proc (-> (or/c path-string? path?) (or/c path-string? path?) any) (lambda (distribution-dir packed-distribution-file) (void)))) any]{Deploy an application. If no @racket[app] is provided explicitly, then @racket[(the-application)] is used. The @racket[source-file] must contain a module that starts the application. The @racket[built-dir] is a directory in which the application is built and contains build information that may be discarded later. The @racket[distributions-dir] is the directory in which the distribution is stored. The name of the distribution files is derived from the application's short name and version. Before deployment starts, @racket[pre-proc] is called with no arguments. After deployment has finished, @racket[post-proc] is called with the dsitribution directory as first argument and the packed distribution file path as second argument.

 During deployment the function provides feedback on the console about the process.}

@subsection{Platform Helpers}

@defmodule[appy/platform]{Contains helper functions to abstract away from platform-specific issues.}

@defproc[(open-file (file-path path?) (#:file-open-command file-open-command bytes? standard-file-open-command)
                    (#:file-open-parameters file-open-parameters bytes #"")) any]{Attempt to open a file in a platform-compliant way so the user can edit or view it. The procedure escapes the file name and uses a platform-specific command line tool to open the file. On MacOS the default is currenty "open", on linux "xdg-open", and on windows "powershell -c". If the command is not available or the file does not exist, then an exception is raised. The procedure blocks an indefinite time while opening the file. A @racket[file-open-command] and additional parameters @racket[file-open-parameters] can be supplied as optional keyword arguments to override the defaults.}

@defproc[(open-url (url url?) (browser browser-preferences? (external-browser))) any]{Opens an url in a web browser. The optional @racket[browser] parameter must be a valid browser-preference, as defined in module @racket[net/sendurl].}

@defproc[(open-folder (folder (or/c path? path-string?))) any]{Open the given folder in file browser or MacOS Finder.}

@defproc[(guess-file-purpose (file-path (or/c path? file-path?))) symbol?]{Attempts to guess the purpose of a file based on common file suffixes and returns one symbol from the list @racket['(video image audio plaintext wordprocessor executable system compressed disc-image presentation programming office font unknown)] that represents the purpose. The hardcoded list of file suffixes used by this function is not complete but it recognizes the most common file suffixes and may e.g. be used to graphically depict the file's purpose if no other information or icons are available.}

@defproc[(system-prefs-dir (appname path-string?)) path?]{Return a preference folder path constructed from the given application name.}
