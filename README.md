# haskell-todos

A simple web application for experimenting with Haskell development.

## Technology choices

Let me explain why I selected the dependencies:

* __scotty__: friendly, basic web framework. Selected to experiment with the REST API interface. With HTTP protocol experience it is easy to grasp the Scotty way of implementing handlers.
* __hasql__: postgresql client, easy to read and understand its documentation, it is a nice excersice to integrate pool and migrations for it
* __mtl__: the monad transformers stack seems to be the most stable way to develop applications, also scottyT is integrating well with a custom monad transformers stack
* __random__, __MonadRandom__: I wanted to generate some UUIDs in the application and this library looked the most straigthforward to integrate with my mtl stack.
* __co-log__: it was not obvious which logging library should be used, I have read nice things about contravairant functors and logging, so I made this choise. Integrating to my custom type was a nice experience too
* __ekg__: I wanted to look on the metrics inside haskell runtime, ekg was simple to set up and also gave a chance to look into thread handling in haskell

## Design choices

There are plenty of ways to construct an application, here I would like to emphasise the decisions behind my design choices.

### Structure

Main structure is driven by the stack generated skeleton, usual _./app_, _./src_, _./test_ folders. One additional folder is the _./migrations_ folder, which contains database migration script(s).

_./src_ folder has 3 main modules, which represents the logical units of the application:

* _App.hs_: application initialization logic, this contains the glue code for the application, knows about most of the other modules
* _Health.hs_: a well separated health check endpoint, which triggers a simple database query to check DB connectivity
* _Todo.hs_: the glue code for an anticipated __Todo__ domain

### Configuration and initialization

In my view it is the application responsibility to gather and interpret options from environment. This is the main reason command line parsing is placed into the app folder.
Relevant modules expose their Options type, what can be constructed from command line options, and then pass to the App module.

The App module (within the library) is coordinating how the application is initialized, how many threads are started, when to do database migration.

The configurable items and other reusable functions and types can be found within App submodules. Each have their own __runWith\*__ functions which help the initialization, and other reusable types and functions.

Modules:

* _App.Monad_: The type behind the application.
* _App.DB_: Reusable utilities for DB access.
* _App.Web_: Scotty specific configuration, starts up scotty using a customized default error handler.
* _App.Ekg_: Ekg setup, guarding it with bracket.
* _App.Log_: co-log settings.
* _App.Random_: configuration for random numbers.
* _App.Error_: a little utility module for error handling.
* _App.Time_: hiding time behind this module (this seems an overkill however)

