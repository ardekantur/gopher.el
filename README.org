#+TITLE:   About =gopher.el=
#+AUTHOR:  Matthew Snyder and [[./AUTHORS.org][the gopher.el authors]]
#+DATE:    2011
#+STARTUP: align
#+OPTIONS: toc:nil

=gopher.el= is an Emacs mode to browse Gopher.

* Features

  + GIF and auto-detected image support.
  + Quick navigation.
  + Gopher search.

* Keys

  | Chord        | Meaning                                                      |
  |--------------+--------------------------------------------------------------|
  |              | <60>                                                         |
  | =M-x gopher= | Enter Gopher, and provide an address to browse to.           |
  | =ENTER=      | Open the link at the cursor's position, or choose the search to enter a query into. |
  | =TAB=        | Move to the next directory listing link.                     |
  | =M-TAB=      | Move to the previous directory listing link.                 |
  | =]=          | Move to the next text file link.                             |
  | =[=          | Move to the previous text file link.                         |
  | =u=          | Navigate to the parent of the current address.               |
  | =r=          | Reload the current address.                                  |
  | =B=          | Navigate backwards through the history                       |
  | =F=          | Navigate forward through the history                         |

* Miscellaneous

  It includes a function, =w3m-open-this-url-in-gopher=, which you may
  use to open Gopher links in w3m.

* Contributing

 The source code may be forked, and issues may be filed, by visiting
  =gopher.el='s [[http://github.com/ardekantur/gopher.el][GitHub page]].
