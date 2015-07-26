# bst-with-pattern-matching

Binary Search Tree implemented using **[defun](https://github.com/killme2008/defun)** a macro to define functions with pattern matching like in Erlang or Elixir.

## How to run the tests

The project uses [Midje](https://github.com/marick/Midje/).

`lein midje` will run all tests.

`lein midje namespace.*` will run only tests beginning with "namespace.".

`lein midje :autotest` will run all the tests indefinitely. It sets up a
watcher on the code files. If they change, only the relevant tests will be
run again.