
# Appy: an Application Framework for Racket

*Appy* is framework for writing GUI and commandline applications in [Racket](https://racket-lang.org/). It provides an object-oriented API for dealing with all kinds of mundane issues like preferences, storing data in simple key-value stores, internationalization, command and undo management, and easy deployment. While it is still in its early development stages, the long-term goal of this package is to make writing cross-platform end-user applications in Racket as seamless as possible.

:construction: *Note: Appy is currently in experimental status. Please try it out if you want, but be aware that the API will change.*

## Installation

The framework is not yet available on Racket's package server. To test it, you need to download the master branch and compile the documentation in 'scribblings/manual.scrbl' (e.g. by loading it and clicking Scribble HTML in DrRacket). You can install it manually using `raco`.

## Usage

Once the framework has been installed locally, `(require appy/gui)` will import all functionality. Importing by `(require "path/to/appy/gui")` should also work if you don't want to install it locally. If you only `(require appy)` the gui-related classes will not be imported.

## Features :tada:

The main classes are `application%` and `gui-application%` for building command line and GUI applications respectively. *Appy*'s main features are:

- Application metadata: The two main application classes `application%` and `gui-application%` manage the application name(s), version, localization, preferences, icons, exception handlers, and other issues. Once you have defined an instance of `gui-application%` or a subclass thereof, you can compile and package it automatically.

- Internationalization: An internationalization system allows you to associate English format strings with symbolic keys using the defloc macro, which defines translation functions and keys for the function `tr`. Localizations can be added at runtime and at compile-time. Templates can be created and there is support for checking the integrity and completeness of all translations before deployment.

- Error Handling: The error support allows for localized error messages with numeric keys. While writing a module, you register numeric keys and corresponding translations for the error messages using `register-errors` and use one of the error functions with the numeric key to raise the error. It is your job to ensure that each module has its own range of numeric error keys. While the system is a bit cumbersome, it has the advantage that error messages are localizable and therefore end-user readable by default and that numeric error codes can be used for further reference in a manual.

- Preferences: The preferences system provides functions for associating symbolic keys with preferences and saving and loading them from the place where they usually reside according to operating system conventions. Once the preference system has been initialized and default preferences have been defined with `defpref`, serializable Racket values can be stored and retrieved with the `pref` function. Preferences are stored in an Sqlite database that is handled automatically behind the scenes.

- Commands & Undo Management: The commands and undo system handles a database-driven undo manager with disk-based, unlimited undo and redo functionality and provides functions and classes for defining commands that perform some function and are automatically managed by the Undo Manager. The disk-based method is slow and not suitable for every type of application but if you need the functionality it’s there.

- An Easy-to-use Database: The storage module provides a `storage%` class that wraps an Sqlite database into an easy-to-use object-oriented API. If you need a simple key-value store or some way to store a few tables with columns without complicated queries, this class might come in handy. For more complex data-driven models it might be more advisable to use Racket’s full database capabilities and SQL.

- Event-Subscriber System: The notifications system provides the means to implement the notify-subscriber pattern for easy communication between distant system components. Depending on how you use it, this may be a curse or blessing, but it greatly simplifies implementing the model-view-controller GUI pattern. Check out the Notes example for a use of this module.

- Easy Deployment: A `deploy` function allows you to easily compile and package an application once it has been set up. It expects an instance of `application%` or `gui-application%`, from which `(deploy)` pulls all the necessary meta-information to compile the application and create a packed standalone distribution file. Pre and post compilation scripts can be provided to e.g. upload the distribution to a server.
