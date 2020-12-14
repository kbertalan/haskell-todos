# haskell-todos

A simple web application for experimenting with Haskell development.

## Technology choices

Let me explain why I selected the dependencies:

* scotty: friendly, basic web framework. Selected to experiment with the REST API interface. With HTTP protocol experience it is easy to grasp the Scotty way of implementing handlers.
* hasql: postgresql client, easy to read and understand its documentation, it is a nice excersice to integrate pool and migrations for it
* mtl: the monad transformers stack seems to be the most stable way to develop applications, also scottyT is integrating well with a custom monad transformers stack
* random, MonadRandom: I wanted to generate some UUIDs in the application and this library looked the most straigthforward to integrate with my mtl stack.
* co-log: it was not obvious which logging library should be used, I have read nice things about contravairant functors and logging, so I made this choise. Integrating to my custom type was a nice experience too
* ekg: I wanted to look on the metrics inside haskell runtime, ekg was simple to set up and also gave a chance to look into thread handling in haskell

## Design choices

There are plenty of ways to construct an application, here I would like to emphasise the decisions behind my design choices.

### Configuration and initialization

In my view it is the application responsibility to gather and interpret options from environment. This is the main reason command line parsing is placed into the app folder.
Relevant modules expose their Options type, what can be constructed from command line options, and then pass to the App module.
The App module (within the library) is coordinating how the application is initialized, how many threads are started, when to do database migration.

The configurable items and other reusable functions and types can be found within App submodules. Each have their own runWith\* functions which help the initialization, and other reusable types and functions.


