_idris-js_
==========

Toy JavaScript backend for Idris.

Based on Edwin Brady’s [PHP backend](https://github.com/edwinb/idris-php).  Written in order to arrive at a [GNU _bash_ backend](https://github.com/mietek/idris-bash).

Barely functional.


Example
-------

    $ idris pythag.idr --codegen js -o pythag.js
    $ node pythag.js
    [(3, (4, 5)), (6, (8, 10)), (5, (12, 13)), (9, (12, 15)), (8, (15, 17)), (12, (16, 20)), (15, (20, 25)), (7, (24, 25)), (10, (24, 26)), (20, (21, 29)), (18, (24, 30)), (16, (30, 34)), (21, (28, 35)), (12, (35, 37)), (15, (36, 39)), (24, (32, 40)), (9, (40, 41)), (27, (36, 45)), (30, (40, 50)), (14, (48, 50))]


About
-----

Made by [Miëtek Bak](https://mietek.io/).  Published under the [MIT X11 license](LICENSE.md).
