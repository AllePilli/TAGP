TAGP Taak
=========

Clean
-----

    $ rebar3 clean

Compile
-------

    $ rebar3 compile

Eunit Test
----------

    $ rebar3 eunit
    
    or

    $ rebar3 do clean, eunit

    For unknown reasons there will sometimes be errors when inserting data into the 'logboek' ets table. if we modify the survivor
    module to not create an ets table all eunit test pass.

Build PLT file
--------------

    dialyzer --build_plt --apps erts kernel stdlib crypto mnesia sasl common_test eunit --output_plt my.plt

TypEr Annotations
-----------------

    $ typer --annotate -r src

Dialyzer 
--------

    $ dialyzer --src src
