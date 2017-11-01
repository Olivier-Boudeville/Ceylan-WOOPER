.. _Top:


.. comment stylesheet ../../../common/css/Ceylan.css


.. role:: raw-html(raw)
   :format: html

.. role:: raw-latex(raw)
   :format: latex




==========
WOOPER 1.0
==========

.. comment Note: this is the latest, current version of the WOOPER 1.x documentation. As for WOOPER 2.0, all the available relevant bits are here as well, commented-out.


---------------------------------------------------
*Wrapper for Object-Oriented Programming in Erlang*
---------------------------------------------------


:Organisation: Copyright (C) 2008-2017 Olivier Boudeville
:Contact: <first name> (dot) <last name> (at) esperide (dot) com
:Creation Date: Thursday, February 25, 2008
:Lastly Updated: Wednesday, November 1, 2017


The latest version of this documentation is to be found at the `official WOOPER website <http://wooper.esperide.org>`_ (``http://wooper.esperide.org``).

:raw-html:`<p>This WOOPER documentation is also available in the PDF format: see <a href="wooper-1.0.pdf">wooper-1.0.pdf</a> (and mirrored <a href="http://olivier-boudeville.github.io/Ceylan-WOOPER/wooper-1.0.html>here<>/a>.</p>`

:raw-latex:`The documentation is also mirrored \href{https://olivier-boudeville.github.io/Ceylan-WOOPER/wooper-1.0.html}{here}.`


.. comment Latest stable WOOPER archives are:

.. - `wooper-1.0.tar.bz2 <http://downloads.sourceforge.net/ceylan/wooper-1.0.tar.bz2>`_
.. - `wooper-1.0.zip <http://downloads.sourceforge.net/ceylan/wooper-1.0.zip>`_



:raw-latex:`\pagebreak`




.. _`table of contents`:


.. comment contents:: Table of Contents

.. comment	:depth: 3


:raw-latex:`\pagebreak`


Overview
========

WOOPER, which stands for *Wrapper for Object-Oriented Programming in Erlang*, is a `free software`_ lightweight layer on top of the `Erlang <http://erlang.org>`_ language that provides constructs dedicated to `Object-Oriented Programming <http://en.wikipedia.org/wiki/Object-oriented_programming>`_ (OOP).

WOOPER is a rather autonomous part of the `Ceylan <https://github.com/Olivier-Boudeville/Ceylan>`_ project.

At least a basic knowledge of Erlang is expected in order to use WOOPER.


Motivations & Purpose
---------------------

Some problems may almost only be tackled efficiently thanks to an object-oriented modelling.

The set of code and conventions proposed here allows to benefit from all the main OOP features (including polymorphism, life cycle management, state management and multiple inheritance) directly from Erlang (which natively does not rely on the OOP paradigm), so that - in the cases where it makes sense - an object-oriented approach at the implementation level can be easily achieved.




The WOOPER Mode of Operation in a Nutshell
------------------------------------------

The WOOPER OOP concepts translate into Erlang constructs according to the following mapping:

======================  =================================================================
WOOPER base concept     Corresponding mapping to Erlang
======================  =================================================================
class definition        module (typically compiled in a ``.beam`` file)
instance                process
instance reference      process identifier (PID)
new operators           WOOPER-provided functions, making use of user-defined ``construct/N`` functions (a.k.a. the constructors)
delete operator         WOOPER-provided function, making use of any user-defined ``destruct/1`` (a.k.a. the destructor)
method definition       module function that respects some conventions
method invocation       sending of an appropriate inter-process message
method look-up          class-specific virtual table taking into account inheritance transparently
instance state          instance-specific datastructure storing its attributes, and kept by the instance-specific WOOPER tail-recursive infinite loop
instance attributes     key/value pairs stored in the instance state
class (static) method   exported module function
======================  =================================================================

In practice, developing a class with WOOPER mostly involves including the `wooper.hrl <https://github.com/Olivier-Boudeville/Ceylan-WOOPER/blob/master/src/wooper.hrl>`_ header file and respecting the WOOPER conventions detailed below.


.. _example:

Example
-------

Here is a simple example of how a WOOPER class can be defined and used.

It shows ``new/delete`` operators, method calling (both request and oneway), and inheritance.

A cat is here a viviparous mammal, as defined below (this is a variation of our more complete `class_Cat.erl <https://github.com/Olivier-Boudeville/Ceylan-WOOPER/blob/master/examples/class_Cat.erl>`_ example)::

 -module(class_Cat).

 % Determines what are the mother classes of this class (if any):
 -define(wooper_superclasses,[class_Mammal,class_ViviparousBeing]).

 % Parameters taken by the constructor ('construct').
 % They are here the ones of the Mammal mother class (the viviparous being
 % constructor does not need any parameter) plus whisker color.
 % These are class-specific data needing to be set in the constructor:
 -define(wooper_construct_parameters,Age,Gender,FurColor,WhiskerColor).

 % Declaring all variations of WOOPER standard life-cycle operations:
 % (this is just a pasted template, with updated arities)
 -define( wooper_construct_export, new/4, new_link/4,
	synchronous_new/4, synchronous_new_link/4,
	synchronous_timed_new/4, synchronous_timed_new_link/4,
	remote_new/5, remote_new_link/5, remote_synchronous_new/5,
	remote_synchronous_new_link/5, remote_synchronous_timed_new/5,
	remote_synchronous_timed_new_link/5, construct/5, destruct/1 ).

 % Member method declarations:
 -define( wooper_method_export,getWhiskerColor/1,setWhiskerColor/2,
	canEat/2 ).

 % Static method declarations:
 -define( wooper_static_method_export, get_default_whisker_color()/0 ).

 % Allows to define WOOPER base variables and methods for that class:
 -include("wooper.hrl").

 % Constructs a new Cat.
 construct( State, ?wooper_construct_parameters ) ->
	% First the direct mother classes:
	MammalState = class_Mammal:construct( State, Age, Gender, FurColor ),
	ViviparousMammalState = class_ViviparousBeing:construct(MammalState),
	% Then the class-specific attributes; returns an updated state:
	setAttributes( ViviparousMammalState, whisker_color, WhiskerColor ).

 destruct(State) ->
	io:format( "Deleting cat ~w! (overridden destructor)~n", [self()] ),
	State.

 % Member methods.

 % A cat-specific const request:
 getWhiskerColor(State)->
	?wooper_return_state_result( State, ?getAttr(whisker_color) ).

 % A (non-const) oneway:
 setWhiskerColor(State,NewColor)->
	NewState = setAttribute( State, whisker_color, NewColor ),
	?wooper_return_state_only( NewState ).

 % Overrides any request method defined in the Mammal class:
 % (const request)
 canEat(State,soup) ->
	?wooper_return_state_result( State, true );

 canEat(State,croquette) ->
	?wooper_return_state_result( State, true );

 canEat(State,meat) ->
	?wooper_return_state_result( State, true );

 canEat(State,_OtherFood) ->
	?wooper_return_state_result( State, false ).

 % Static method:
 get_default_whisker_color() ->
	white.


Straightforward, isn't it? We will discuss it in-depth, though.

To test this class (provided that ``GNU make`` and ``Erlang 20.0`` or more recent are available in one's environment), one can easily install ``Ceylan-WOOPER``, which depends on ``Ceylan-Myriad``, hence to be installed first::

 $ git clone https://github.com/Olivier-Boudeville/Ceylan-Myriad.git
 $ cd Ceylan-Myriad && make all && cd ..


Then, as ``Ceylan-Myriad`` is known by WOOPER as the ``common`` layer::

 $ ln -s Ceylan-Myriad common
 $ git clone https://github.com/Olivier-Boudeville/Ceylan-WOOPER.git
 $ cd Ceylan-WOOPER && make all


Running the cat-related example just boils down to::

 $ cd examples && make class_Cat_run

In the ``examples`` directory, the test defined in `class_Cat_test.erl <https://github.com/Olivier-Boudeville/Ceylan-WOOPER/blob/master/examples/class_Cat_test.erl>`_ should run against the class defined in `class_Cat.erl <https://github.com/Olivier-Boudeville/Ceylan-WOOPER/blob/master/examples/class_Cat.erl>`_, and no error should be detected::

 Running unitary test class_Cat_run (second form)
 Erlang/OTP 20 [erts-9.0.1] [source] [64-bit] [smp:8:8] [..]
 --> Testing module class_Cat_test.
 [..]
 Deleting cat <0.70.0>! (overridden destructor)
 Deleting mammal <0.68.0>! (overridden destructor)
 Actual class from destructor: class_Cat.
 Deleting mammal <0.70.0>! (overridden destructor)
 This cat could be created and be synchronously deleted, as expected.
 --> Successful end of test.
 (test finished, interpreter halted)

That's it!

Now, more in-depth explanations.

:raw-latex:`\pagebreak`



Why Adding Object-Oriented Capabilities To Erlang?
==================================================

Although applying blindly an OOP approach while using languages based on other paradigms (Erlang ones are functional and concurrent; the language is not specifically targeting OOP) is a common mistake, there are some problems that may be deemed inherently "object-oriented", i.e. that cannot be effectively modelled without encapsulated abstractions sharing behaviours.

Examples of this kind of systems are multi-agent simulations. If they often need massive concurrency, robustness, distribution, etc. (Erlang is particularly suitable for that), the various types of agents have also often to largely share states and behaviours, while still being able to be further specialised on a per-type basis.

The example_ mentioned in this document is an illustration [#]_ of the interacting lives of numerous animals of various species. Obviously, they have to share behaviours (ex: all ovoviviparous beings may lay eggs, all creatures can live and die, all have an age, etc.), which cannot be mapped easily (read: automatically) to Erlang concepts without adding some generic constructs.

.. [#] This example is not a *simulation*, it is just a multi-agent system. For real, massive, discrete-time simulations of complex systems in Erlang (using WOOPER), one may refer to `Sim-Diasca <http://www.sim-diasca.com>`_.


WOOPER, which stands for *Wrapper for OOP in Erlang*, is a lightweight yet effective (performance-wise, but also regarding the overall developing efforts) means of making these constructs available, notably in terms of state management and multiple inheritance.

The same programs could certainly be implemented without such OOP constructs, but at the expense of way too much manually-crafted, specific (per-class) code. This process would be tedious, error-prone, and most often the result could hardly be maintained.


:raw-latex:`\pagebreak`

How to Use WOOPER: Detailed Description & Concept Mappings
==========================================================

.. comment May trigger following error: 'LaTeX Error: File `minitoc.sty' not found.':
		   Use: 'pacman -S texlive-latexextra' then.

.. contents::
 :local:
 :depth: 2



Classes
-------


Classes & Names
...............

A class is a blueprint to create objects, a common scheme describing the state and behaviour of its instances, i.e. the attributes and methods that the created objects for that class all have.

With WOOPER, each class has a unique name, such as ``class_Cat``.

To allow for **encapsulation**, a WOOPER class is mapped to an Erlang module, whose name is by convention made from the ``class_`` prefix followed by the class name, in the so-called `CamelCase <http://en.wikipedia.org/wiki/CamelCase>`_: all words are spelled in lower-case except their first letter, and there are no separators between words, like in: *ThisIsAnExample*.

For example, a class modeling a cat should translate into an Erlang module named ``class_Cat``, thus in a file named ``class_Cat.erl``. At the top of this file, the corresponding module would be therefore declared with: ``-module(class_Cat).``.

Similarly, a pink flamingo class could be declared as ``class_PinkFlamingo``, in ``class_PinkFlamingo.erl``, which would include a ``-module(class_PinkFlamingo).`` declaration.


The class name can be obtained through its ``get_class_name/0`` static method [#]_ (automatically defined by WOOPER)::

  > class_Cat:get_class_name().
  class_Cat

.. [#] The ``get_class_name/0`` static method has no real interest of its own, it is defined mostly for explanation purpose.

Note that a static method (i.e. a class method that does not apply to any specific instance) of a class X is nothing more than an Erlang function exported from the corresponding ``class_X`` module: all exported functions could be seen as static methods.



Inheritance & Superclasses
..........................

A WOOPER class can inherit from other classes, in which case the state and behaviour defined in the mother classes are readily available to this child class.

Being in a **multiple inheritance** context, a given class can have any number (``[0..n]``) of direct mother classes, which themselves may have mother classes, and so on. This leads to a class hierarchy that forms a graph.

This is declared in WOOPER thanks to the ``wooper_superclasses`` define. For example, a class with no mother class should specify, once having declared its module::

 -define(wooper_superclasses,[]).


.. comment This is declared in WOOPER thanks to the ``get_superclasses/0`` function. For example, a class with no mother class should specify, once having declared its module, ``get_superclasses() -> [].`` [#]_.

.. comment .. [#] Such WOOPER-related functions are already automatically exported by WOOPER. As an added bonus, this allows the class developer to be notified whenever he forgets to define them.

As for our cat, this superb animal could be modelled both as a mammal (itself a specialised creature) and a viviparous being [#]_. Hence its direct inheritance could be defined as::

 -define(wooper_superclasses,[class_Mammal,class_ViviparousBeing]).

.. [#] Neither of them is a subset of the other, these are mostly unrelated concepts, at least in the context of that example! (ex: a platypus is a mammal, but not a viviparous being).


The superclasses (direct mother classes) of a given class can be known thanks to its ``get_superclasses/0`` static method::

 > class_Cat:get_superclasses().
 [class_Mammal,class_ViviparousBeing]

.. comment Note::  We will discuss here mostly the WOOPER versions 2.x and higher, originating from a development branch that is sometimes codenamed the "*Zero-Overhead WOOPER*", as opposed to the legacy versions (prior to 2.x), codenamed "*Hashtable-based WOOPER*".



Instances
---------


Instance Mapping
................

With WOOPER, which focuses on multi-agent systems, all **instances** of a class are mapped to Erlang processes (one WOOPER instance is exactly one Erlang process).

They are therefore, in UML parlance, *active objects* (each has its own thread of execution, they may apparently "live" simultaneously [#]_).

.. [#] For some uses, such a concurrent feature (with *active* instances) may not be needed, in which case one may deal also with purely *passive* instances (as Erlang terms, not Erlang processes).

	   To anticipate a bit, instead of using ``new/n`` (returning the PID of a new process instance looping over its state), one may rely on ``construct/n+1`` (returning directly to the caller process that corresponding initial state, that can be then stored and interacted upon at will).


Instance State
..............

Another common OOP need is to rely on **state management** and **encapsulation**: each instance should be stateful, have its state fully private, and be able to inherit automatically the data members defined by its mother classes.

In WOOPER, this is obtained thanks to a per-instance associative table, whose keys are the names of attributes and whose values are the attribute values. This will be detailed in the `state management`_ section.




:raw-latex:`\pagebreak`


Methods
-------

They can be either:

- **member methods**: to be applied to a specific *instance* (of a given class), like in: ``MyCat ! declareBirthday``

- or **static methods**: general to a *class*, not targeting specifically an instance, like: ``class_Cat:get_default_mew_duration()``


Unless specified otherwise, just mentioning *method* by itself refers to a *member method*. Static methods are discussed into their specific subsection.

**Member methods** can be publicly called by any process (be it WOOPER-based or not - provided of course it knows the PID of that instance), whether locally or remotely (i.e. on other networked computers, like with RMI or with CORBA, or directly from the same Erlang node), distribution (and parallelism) being seamlessly managed thanks to Erlang.

Member methods (either inherited or defined directly in the class) are mapped to specific Erlang functions, triggered by Erlang messages.

For example, our cat class may define, among others, following member methods (actual arities to be discussed later):

- ``canEat``, taking one parameter specifying the type of food, and returning whether the corresponding cat can eat that kind of food; here the implementation should be cat-specific (i.e. specific to cats and also, possibly, specific to this very single cat), whereas the method signature shall be shared by all beings

- ``getWhiskersColor``, taking no parameter, returning the color of its whiskers; this is indeed a purely cat-specific method, and different cats may be different whisker colors; as this method, like the previous one, returns a result to the caller, it is a *request* method

- ``declareBirthday``, incrementing the age of our cat, not taking any parameter nor returning anything; it will be therefore be implemented as a *oneway* method (i.e. not returning any result to the caller, hence not even needing to know it), whose call is only interesting for its effect on the cat state: here, making it one year older

- ``setWhiskerColor``, assigning the specified color to the whiskers of that cat instance, not returning anything (another oneway method, then)

Declaring a birthday is not cat-specific, nor mammal-specific: we can consider it being creature-specific. Cat instances should then inherit this method, preferably indirectly from the ``class_Creature`` class, in all cases without having to specify anything, since the ``wooper_superclasses`` define already implies it (implying one time for all that cats *are* creatures). Of course this inherited method may be overridden at will anywhere in the class hierarchy.

We will discuss the *definition* of these methods later, but for the moment let's determine their signatures and declarations, and how we are expected to *call* them.


Method Declaration
..................

The cat-specific member (i.e. non-static) methods are to be declared:

- in the ``class_Cat`` (defined as mentioned in ``class_Cat.erl``)
- thanks to the ``wooper_method_export`` define (which, as expected, automatically exports these member methods)

Their arity should be equal to the number of parameters they should be called with, plus one that is automatically managed by WOOPER and corresponds to the (private) state of that instance.

This ``State`` variable defined by WOOPER can be somehow compared to the ``self`` parameter of Python, or to the ``this`` hidden pointer of C++. That state is automatically kept by WOOPER instances in their main loop, and automatically prepended, as first element, to the parameters of incoming method calls.

In our example, the declarations could therefore result in::

 -define(wooper_method_export, canEat/2, getWhiskerColor/1,
		 setWhiskerColor/2).


.. Note:: In our example, ``declareBirthday/1`` will be inherited but not overridden (its base implementation being fine for cats as well), so it should not be listed among the ``class_Cat`` methods.


Some method names are reserved for WOOPER; notably no user method should have its name prefixed with ``wooper``.

.. comment In our example, the declarations could therefore result in::
  get_member_methods() ->
	[ {getMewVolume,1}, {canEat,2, [public,final]},
	  {getWhiskerColor,1,[public,const]}, {setWhiskerColor,2,protected} ].
 More generally a member method can be declared with:

 - just its name and full arity (including the ``State`` parameter), ex: ``{getMewVolume,1}``
 - its name, full arity, and one qualifier, ex: ``{getWhiskerColor,1,public}``
 - its name, full arity, and a list of qualifiers, ex: ``{canEat,2, [public,final]}``


 Known method qualifiers are:

 - in terms of accessibility:

  - ``public``: the method can be called from outside the instance as well as from the class itself, i.e. from the body of its own methods (inherited or not), or from its child classes
  - ``protected``: the method can be called only from the body of its own methods (inherited or not), or from its child classes; no call from outside the class
  - ``private``: the method can be called only from the body of its own methods (inherited or not); no call from outside the class or from child classes is allowed

  - in terms of mutability:

   - ``const``: a call to the method on an instance will then never result into a change in the state of that instance

   - ``final``: this method cannot be overridden by child classes

 Unless specified otherwise, a method is public, non-const, non-final.



 .. Note::

  WOOPER allows to *specify* these qualifiers for documentation purposes, but may or may not enforce them.

  For example, to anticipate a bit, all methods could be dispatched into three lists (for public/protected/private), and when an ``execute*`` call is performed, a check, based on the actual class of the instance, could be done.

  On the other hand, method calls, triggered by messages instead, could not have their access controlled (without even mentioning the runtime overhead). For example, protected oneways cannot be checked for accessibility, as the message sender is not known in the context of this kind of method call.


  The complete list of reserved function names that do not start with the ``wooper_`` prefix is:

 - ``get_class_name``
 - ``get_superclasses``
 - ``executeRequest``
 - ``executeOneway``
 - ``delete_any_instance_referenced_in``
 - ``is_wooper_debug``

 They are reserved for all arities.

 Note that functions which must be defined by the class developer are unconditionally exported by the WOOPER header, so that a compile-time error is issued whenever at least one of them is not defined.




Method Invocation
.................

Let's suppose that the ``MyCat`` variable designates an instance of ``class_Cat``. Then this ``MyCat`` reference is actually just the PID of the Erlang process hosting this instance.

All member methods (regardless of whether they are defined directly by the actual class or inherited) are to be called from outside this class thanks to a proper Erlang message, sent to the PID of the targeted instance.

When the method is expected to return a result (i.e. when it is a request method), the caller must specify in the corresponding message its own PID, so that the instance knows to whom the result should be sent.

Therefore the ``self()`` parameter in the call tuples below corresponds to the PID *of the caller*, while ``MyCat`` is bound to the PID *of the target instance*.

The three methods previously discussed would indeed be called that way::

  % Calling the canEat request of our cat instance:
  MyCat ! {canEat,soup,self()},
  receive
	  {wooper_result,true} ->
			   io:format( "This cat likes soup!!!" );

	  {wooper_result,false} ->
			   io:format( "This cat does not seem omnivorous." )
  end,

  % A parameter-less request:
  MyCat ! {getWhiskersColor,[],self()},
  receive
	  {wooper_result,white} ->
			   io:format( "This cat has normal whiskers." );

	  {wooper_result,blue} ->
			   io:format( "What a weird cat..." )
  end,

  % A parameter-less oneway:
  MyCat ! declareBirthday.



Method Name
...........

Methods are designated by their name (as an atom), as specified in the ``wooper_method_export`` define of the class in the inheritance tree that defines them.

The method name is always the first information given in the method call tuple.


Method Parameters
.................

All methods are free to change the state of their instance and possibly trigger any side-effect (ex: sending a message, writing a file, etc.).

As detailed below, there are two kinds of methods:

- *requests* methods: they shall return a result to the caller (obviously they need to know it, i.e. the caller has to specify its PID)

- *oneway* methods: no specific result are expected from them (hence no caller PID is to be specified)

Both can take any number of parameters, including none. As always, the **marshalling** of these parameters and, if relevant, of any returned value is performed automatically by Erlang.

Parameters are to be specified in a (possibly empty) list, as second element of the call tuple.

If only a single, non-list, parameter is needed, the list can be omitted, and the parameter can be directly specified: ``Alfred ! {setAge,31}.`` works just as well as ``Alfred ! {setAge,[31]}.``.


.. _`single method parameter is a list`:

.. Note::
  This cannot apply if the unique parameter is a list, as this would be ambiguous.

  For example: ``Foods = [meat,soup,croquette], MyCat ! {setFavoriteFoods,Foods}`` would result in a call to ``setFavoriteFoods/4``, i.e. a call to ``setFavoriteFoods(State,meat,soup,croquette)``, whereas the intent of the programmer is probably to call a ``setFavoriteFoods/2`` method like ``setFavoriteFoods(State,Foods) when is_list(Foods) -> [..]``.

  The proper call would then be ``MyCat ! {setFavoriteFoods,[Foods]}``, i.e. the parameter list should be used, and it would then contain only one element, the food list, whose content would therefore be doubly enclosed.



Two Kinds of Methods
....................


Request Methods
_______________

A **request** is a method that returns a result to the caller.

For an instance to be able to send an answer to a request triggered by a caller, of course that instance needs to know the caller PID.

Therefore requests have to specify, as the third element of the call tuple, an additional information: the PID to which the answer should be sent, which is almost always the caller (hence the ``self()`` in the actual calls).

So these three potential information (request name, parameters, reference of the sender - i.e. an atom, usually a list, and a PID) are gathered in a triplet (a 3-tuple) sent as a message: ``{request_name,[Arg1,Arg2,..],self()}``.

If only one parameter is to be sent, and if that parameter is not a list, then this can become ``{request_name,Arg,self()}``.

For example::

 MyCat ! {getAge,[],self()}


or::

 Douglas ! {askQuestionWithHint,[{meaning_of,"Life"},{maybe,42}],self()}

or::

 MyCalculator ! {sum,[[1,2,4]],self()}.


The actual result ``R``, as determined by the method, is sent back as an Erlang message, which is a ``{wooper_result,R}`` pair, to help the caller pattern-matching the WOOPER messages in its mailbox.

``receive`` should then be used by the caller to retrieve the request result, like in the case of this example of a 2D point instance::

 MyPoint ! {getCoordinates,[],self()},
 receive
		  {wooper_result,[X,Y]} ->
				  [..]
 end,
 [..]



Oneway Methods
______________

A **oneway** is a method that does not return a result to the caller.

When calling oneway methods, the caller does not have to specify its PID, as no result is expected to be returned back to it.

If ever the caller sends by mistake its PID nevertheless, a warning is sent back to it, the atom ``wooper_method_returns_void``, instead of ``{wooper_result,Result}``.

The proper way of calling a oneway method is to send to it an Erlang message that is:

- either a pair, i.e. a 2-element tuple (therefore with no PID specified): ``{oneway_name,[Arg1,Arg2,..]}`` or ``{oneway_name,Arg}`` if ``Arg`` is not a list; for example: ``MyPoint ! {setCoordinates,[14,6]}`` or ``MyCat ! {setAge,5}``

- or, if the oneway does not take any parameter, just the atom ``oneway_name``. For example: ``MyCat ! declareBirthday``


No return should be expected (the called instance does not even know the PID of the caller), so no receive should be attempted on the caller side, unless wanting to wait until the end of time.

Due to the nature of oneways, if an error occurs instance-side during the call, the caller will never be notified of it.

However, to help the debugging, an error message is then logged (using ``error_logger:error_msg``) and the actual error message, the one that would be sent back to the caller if the method was a request, is given to ``erlang:exit`` instead.




Method Results
..............


Execution Success: ``{wooper_result,ActualResult}``
___________________________________________________

If the execution of a method succeeded, and if the method is a request, then ``{wooper_result,ActualResult}`` will be sent back to the caller (precisely: to the process whose PID was specified in the call triplet).

Otherwise one of the following error messages will be emitted.



Execution Failures
__________________


When the execution of a method fails, three main error results can be output (as a message for requests, as a log for oneways).

A summary could be:

+-----------------------------------+----------------------------+------------------+
| Error Result                      | Interpretation             | Likely Guilty    |
+===================================+============================+==================+
| ``wooper_method_not_found``       | No such method exists in   | Caller           |
|                                   | the target class.          |                  |
+-----------------------------------+----------------------------+------------------+
| ``wooper_method_failed``          | Method triggered a runtime | Called instance  |
|                                   | error (it has a bug).      |                  |
+-----------------------------------+----------------------------+------------------+
| ``wooper_method_faulty_return``   | Method does not respect    | Called instance  |
|                                   | the WOOPER return          |                  |
|                                   | convention.                |                  |
+-----------------------------------+----------------------------+------------------+

.. Note:: More generally, failure detection may better be done through the use of (Erlang) links, either explicitly set (with ``erlang:link/1``) or, preferably (ex: to avoid race conditions), with a linked variation of the ``new`` operator (ex: ``new_link/n``), discussed later in this document.



``wooper_method_not_found``
***************************

The corresponding error message is ``{wooper_method_not_found, InstancePid, Classname, MethodName, MethodArity, ListOfActualParameters}``.

For example ``{wooper_method_not_found, <0.30.0>, class_Cat, layEggs, 2, ...}``.

Note that ``MethodArity`` includes the implied state parameter (that will be discussed later), i.e. here ``layEggs/2`` might be defined as ``layEggs(State,NumberOfNewEggs) -> [..]``.

This error occurs whenever a called method could not be found in the whole inheritance graph of the target class. It means this method is not implemented, at least not with the deduced arity.

More precisely, when a message ``{method_name,[Arg1,Arg2,..,ArgN]...}`` (request or oneway) is received, ``method_name/N+1`` has be to called: WOOPER tries to find ``method_name(State,Arg1,..,ArgN)``, and the method name and arity must match.

If no method could be found, the ``wooper_method_not_found`` atom is returned (if the method is a request, otherwise the error is logged), and the object state will not change, nor the instance will crash, as this error is deemed a caller-side one (i.e. the instance has a priori nothing to do with the error).



``wooper_method_failed``
************************

The corresponding error message is ``{wooper_method_failed, InstancePid, Classname, MethodName, MethodArity, ListOfActualParameters, ErrorTerm}``.

For example, ``{wooper_method_failed, <0.30.0>, class_Cat, myCrashingMethod, 1, [], {{badmatch,create_bug}, [..]]}``.

If the exit message sent by the method specifies a PID, it is prepended to ErrorTerm.

Such a method error means there is a runtime failure, it is generally deemed a instance-side issue (the caller should not be responsible for it, unless it sent incorrect parameters), thus the instance process logs that error, sends an error term to the caller (if and only if it is a request), and then exits with the same error term.



``wooper_method_faulty_return``
*******************************

The corresponding error message is ``{wooper_method_faulty_return, InstancePid, Classname, MethodName, MethodArity, ListOfActualParameters, ActualReturn}``.

For example, ``{wooper_method_faulty_return, <0.30.0>, class_Cat, myFaultyMethod, 1, [], [{{state_holder,..]}``.

This error occurs only when being in debug mode.

The main reason for this to happen is when debug mode is set and when a method implementation did not respect the expected method return convention (neither the ``wooper_return_state_result`` macro nor the ``wooper_return_state_only`` one was used in this method clause).

It means the method is not implemented correctly (it has a bug), or that it was not (re)compiled with the proper debug mode, i.e. the one the caller was compiled with.

This is an instance-side failure (the caller has no responsibility for that), thus the instance process logs that error, sends an error term to the caller (if and only if it is a request), and then exits with the same error term.



Caller-Side Error Management
****************************

As we can see, errors can be better discriminated if needed, on the caller side.
Therefore one could make use of that information, as in::

  MyPoint ! {getCoordinates,[],self()},
  receive
	  {wooper_result, [X,Y] } ->
			   [..];
	  {wooper_method_not_found, Pid, Class, Method, Arity, Params} ->
			   [..];
	  {wooper_method_failed, Pid, Class, Method, Arity, Params, ErrorTerm} ->
			   [..];
	  % Error term can be a tuple {Pid,Error} as well, depending on the exit:
	  {wooper_method_failed, Pid, Class, Method, Arity, Params, {Pid,Error}} ->
			   [..];
	  {wooper_method_faulty_return, Pid, Class, Method, Arity, Params, UnexpectedTerm} ->
			   [..];
	  wooper_method_returns_void ->
			   [..];
	  OtherError ->
			   % Should never happen:
			   [..]
  end.


However defensive development is not really favoured in Erlang, one may let the caller crash on unexpected return instead. Therefore generally one may rely simply on matching the message sent in case of success [#]_::

  MyPoint ! {getCoordinates,[],self()},
  receive
	  {wooper_result, [X,Y] } ->
			   [..]
  end,
  [..]

.. [#] In which case, should a failure happen, the method call will become blocking.




Method Definition
.................

Here we reverse the point of view: instead of **calling** a method, we are in the process of **implementing** a callable one.

A method signature has always for first parameter the state of the instance, for example: ``getAge(State) -> [..]``, or ``getCoordinate(State,Index) -> [..]``.

For the sake of clarity, this variable should preferably always be named ``State``.


A method must always return at least the newer instance state, even if the state did not change.

In this case the initial state parameter is directly returned, as is, like in::

  getWhiskerColor(State) ->
	  ?wooper_return_state_result(State,?getAttr(whisker_color) ).

State is unchanged here.


Note that when a method "returns" the state of the instance, it returns it to the (local, process-wise) private WOOPER-based main loop of that instance: in other words, the state variable is *never* exported/sent/visible outside of its process (unless of course a developer writes a specific method for that).

Encapsulation is ensured, as the instance is the only process able to access its own state. On method ending, the instance then just loops again, with its updated state: that new state will be the base one for the next call, and so on.

One should therefore see each WOOPER instance as primarily a process executing a main loop that keeps the current stat of that instance:

- it is waiting idle for any incoming (WOOPER) message
- when such a message is received, based on the actual class of the instance and on the method name specified in the call, the appropriate function defined in the appropriate module is selected by WOOPER, taking into account the inheritance graph (actually a direct per-class mapping, somewhat akin to the C++ virtual table, was already determined at start-up, for better performances)
- then this function is called with the appropriate parameters (those of the call, in addition to the internally kept current state)
- if the method is a request, the specified result is sent back to the caller
- then the instance loops again, on a state possibly updated by this method call

Thus the caller will only receive the **result** of a method, if it is a request. Otherwise, i.e. with oneways, nothing is sent back (nothing can be, anyway).

More precisely, depending on its returning a specific result, the method signature will correspond either to the one of a request or of a oneway, and will use in its body, respectively, either the ``wooper_return_state_result`` or the ``wooper_return_state_only`` macro to ensure that a state *and* a result are returned, or just a state.

A good practise is to add a comment to each method definition, and to specify whether it is a request or a oneway, if it is a ``const`` method, etc. For example, the previous method could be best written as::

 % Returns the current color of the whiskers of that cat instance.
 % (const request)
 getWhiskerColor(State) ->
	 ?wooper_return_state_result(State, ?getAttr(whisker_color)).


.. Note:: When a constructor or a method determines that a fatal error should be raised (for example because it cannot find a required registered process), it should use ``throw``, like in: ``throw({invalid_value,V})``. Using ``exit`` is supported but not recommended.



For Requests
____________

Requests will use ``?wooper_return_state_result(NewState,Result)``: the new state will be kept by the instance, whereas the result will be sent to the caller. Hence ``wooper_return_state_result`` means that the method returns a state **and** a result.

For example a const request will return an unchanged state, and thus will be just useful for its result (and possible side-effects)::

 getAge(State) ->
	 ?wooper_return_state_result(State,?getAttr(age)).


All methods are of course given the parameters specified at their call.

For example, we can declare::

 giveBirth(State,NumberOfMaleChildren,NumberOfFemaleChildren) ->
		  [..]


And then we may call it, in the case of a cat having 2 male kitten and 3 female ones, with::

  MyCat ! {giveBirth,[2,3],self()}.


Requests can access to one more information than oneways: the PID of the caller that sent the request. As WOOPER takes care automatically of sending back the result to the caller, having the request know explicitly the caller is usually not useful, thus the caller PID does not appear explicitly in request signatures, among the actual parameters.

However WOOPER keeps track of this information, which remains available to requests.

The caller PID can indeed be retrieved from a request body by using the ``getSender`` macro, which is automatically managed by WOOPER::

  giveBirth(State,NumberOfMaleChildren,NumberOfFemaleChildren) ->
	CallerPID = ?getSender(),
	[..]


Thus a request has natively access to its caller PID, i.e. with no need to specify it in the parameters as well as in the third element of the call tuple; so, instead of having to define::

 MyCat ! {giveBirth,[2,3,self()],self()}

one can rely on only::

 MyCat ! {giveBirth,[2,3],self()}

while still letting the possibility for the called request (here ``giveBirth/3``, for a state and two parameters) to access the caller PID thanks to the ``getSender`` macro, and maybe store it for a later use or do anything appropriate with it.

Note that having to handle explicitly the caller PID is rather uncommon, as WOOPER takes care automatically of the sending of the result back to the caller.

The ``getSender`` macro should only be used for requests, as of course the sender PID has no meaning in the case of oneways.

If that macro is called nevertheless from a oneway, then it returns the atom ``undefined``.



For Oneways
___________

Oneway will rely on the ``?wooper_return_state_only(NewState)`` macro: the instance state will be updated, but no result will be returned to the caller, which is not even known.

For example::

  setAge(State,NewAge) ->
	?wooper_return_state_only( setAttribute(State,age,NewAge) ).


This oneway can be called that way::

  MyCat ! {setAge,4}.
  % No result to expect.


Oneways may also be ``const``, i.e. leave the state unchanged, only being called for side-effects, for example::

  displayAge(State) ->
	io:format("My age is ~B~n.",[ ?getAttr(age) ]),
	?wooper_return_state_only(State).



Usefulness Of These Two Return Macros
_____________________________________

The definition of the ``wooper_return_state_result`` and ``wooper_return_state_only`` macros is actually quite simple; they are just here to structure the method implementations (helping the method developer not mixing updated states and results), and to help ensuring, in debug mode, that methods return well-formed information: an atom is then prepended to the returned tuple and WOOPER matches it during post-invocation, before handling the return, for an increased safety.

For example, in debug mode, ``?wooper_return_state_result(AState,AResult)`` will simply translate into ``{wooper_result,AState,AResult}``, and when the execution of the method is over, the WOOPER main loop of this instance will attempt to match the method returned value with that triplet.

Similarly, ``?wooper_return_state_only(AState)`` will translate into ``{wooper_result,AState}``.

If not in debug mode, then the ``wooper_result`` atom will not even be added in the returned tuples; for example ``?wooper_return_state_result(AState,AResult)`` will just be ``{AState,AResult}``.

Performances should increase a bit, at the expense of a less safe checking of the values returned by methods.

The two ``wooper_return_state_*`` macros have been introduced so that the unwary developer does not forget that his requests are not arbitrary functions, that they should not only return a result but also a state, and that the order is always: first the state, then the result, not the other way round.


Type Specifications
___________________

Although doing so is optional, WOOPER strongly recommends declaring type specifications as well (and provides suitable constructs for that), like in::

 % Returns the current color of the whiskers of that cat instance.
 % (const request)
 -spec getWhiskerColor(wooper:state()) -> request_return(color()).
 getWhiskerColor(State) ->
	 ?wooper_return_state_result(State, ?getAttr(whisker_color)).

(of course the developer is responsible for the definition of the ``color()`` type here)

Similarly, the aforementioned ``declareBirthday/1`` oneway could be defined as::

  % Declares the birthday of this creature: increments its age.
  % (oneway)
  -spec declareBirthday(wooper:state()) -> oneway_return().
  declareBirthday(State) ->
	 AgedState = setAttribute(State, age, ?getAttr(age)+1),
	 ?wooper_return_state_ony(AgedState).



Self-Invocation: Calling a Method From The Instance Itself
..........................................................

When implementing a method of a class, one may want to call other methods **of that same class** (have they been overridden or not).

For example, when developing a ``declareBirthday/1`` oneway of ``class_Mammal`` (which, among other things, is expected to increment the mammal age), you may want to perform a call to the ``setAge/2`` oneway (possibly introduced by the ``class_Creature`` ancestor class, or directly defined in ``class_Mammal``) on the current instance.

If you just directly call ``setAge/2`` (i.e. ``class_Mammal:setAge/2``, should it even be defined in that class), then any potentially version of that method that would be overloaded in ``class_Mammal`` child classes would never be called. Indeed, if an instance of child class ``class_Cat`` (which inherited ``declareBirthday/1`` "as is") overloaded ``setAge/2``, as a developer you may desire, if not expect, that, for a cat or for any specialised version thereof, ``declareBirthday/1`` calls automatically ``class_Cat:setAge/2``, and not ``class_Mammal:setAge/2``.

Such a call could be easily performed asynchronously: a classical message-based method auto-call can be used, like in ``self() ! {setAge,10}``. If this approach is useful when not directly needing from the method the result of the call and/or not needing to have it executed at once, there are cases where one wants to have that possibly overridden method be executed *directly*, and to access immediately to the corresponding updated state and, possibly, output result.

In these cases, one should call the WOOPER-defined ``executeRequest/{2,3}`` or ``executeOneway/{2,3}`` functions (or any variation thereof), depending on the type of the method to call.

These two helper functions behave quite similarly to the actual method calls that are based on the operator ``!``, except that no target instance has to be specified (since it is by definition a call made by an instance to itself) and that no message exchange is involved: the method look-up is just performed through the inheritance hierarchy, the correct method is called with the specified parameters and the result is then directly returned.

More precisely, **executeRequest** is ``executeRequest/2`` or ``executeRequest/3``, its parameters being the current state, the name of the request method, and, if specified, the parameters of the called request, either as a list or as a standalone one.

``executeRequest`` returns a pair, made of the new state and of the result.

For example, for a request taking more than one parameter, or one list parameter::

 {NewState,Result} = executeRequest(CurrentState, myRequestName,
									["hello", 42])

For a request taking exactly one (non-list) parameter::

 {NewState,NewCounter} = executeRequest(CurrentState, addToCurrentCounter, 78)

For a request taking no parameter::

 {NewState,Sentence} = executeRequest(CurrentState, getLastSentence)




Regarding now **executeOneway**, it is either ``executeOneway/2`` or ``executeOneway/3``, depending on whether the oneway takes parameters. If yes, they can be specified as a list (if there are more than one) or as a standalone parameter.

``executeOneway`` returns the new state.

For example, a oneway taking more than one parameter, or one list parameter::

 NewState = executeOneway(CurrentState,say,[ "hello", 42 ])


For a oneway taking exactly one (non-list) parameter::

 NewState = executeOneway(CurrentState,setAge,78)


For a oneway taking no parameter::

 NewState = executeOneway(CurrentState,declareBirthday)


.. Note:: As discussed previously, there are caller-side errors that are not expected to crash the instance. If such a call is performed directly from that instance (i.e. with one of the ``execute*`` constructs), then two errors will be output: the first, non-fatal for the instance, due to the method call, then the second, fatal for the instance, due to the failure of the ``execute*`` call. This is the expected behaviour, as here the instance plays both roles, the caller and the callee.


Finally, we can specify explicitly the class defining the version of the method that we want to execute, bypassing the inheritance-aware overriding system.

For example, a method needing to call ``setAge/2`` from its body would be expected to use something like: ``AgeState = executeOneway(State,setAge,NewAge)``.

If ``class_Cat`` overrode ``setAge/2``, any cat instance would then call the overridden ``class_Cat:setAge`` method instead of the original ``class_Creature:setAge``.

What if the method in our cat instance wanted, for any reason, to call the ``class_Creature`` version? In this case a ``execute*With`` function should be used.

These functions, which are ``executeRequestWith/3``, ``executeRequestWith/4``, ``executeOnewayWith/3`` and ``executeOnewayWith/4``, behave exactly as the previous ``execute*`` functions, except that they take an additional parameter (to be specified just after the state) which is the name of the mother class (direct or not) having defined the desired version of the method.

.. Note::

	This mother class does not have to have specifically defined or overridden that method: this method will just be called in the context of that class, as if it was an instance of the mother class rather than one of the actual child class.


In our example, we should thus use simply: ``AgeState = executeOnewayWith(State,class_Creature,setAge,NewAge)``, in order to call the ``class_Creature`` version of the ``setAge`` method.
