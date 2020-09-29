---
title: "How are Lisp REPLs different from Python or Ruby REPLs ? (Hackernews, 2020)"
date: 2020-07-14T16:11:43+02:00
tags: ["python",]
draft: false
---

Mikelevins, https://news.ycombinator.com/item?id=23811382, July 2020

(some more comments on https://www.reddit.com/r/lisp/comments/hqesvp/explaining_the_advantages_of_the_repl/)

(on terminology: we should maybe call Python's "REPL" a *shell*, and put emphasis on *image-based development*, instead of only saying REPL, for Lisp)

---

I've answered similar questions several times over the past few years, but I don't mind repeating myself. It offers me a glimmer of hope that my preferred way of working may not fade away, after all.

Consider the standard Common Lisp generic function `UPDATE-INSTANCE-FOR-REDEFINED-CLASS` (http://clhs.lisp.se/Body/f_upda_1.htm). It reinitializes an object when Lisp detects that the object's class definition has changed.

Ask yourself this: who would call such a function? Why would anyone ever invent it? Not only did someone invent it, a committee of some of the world's smartest and most experienced Lisp programmers wrote it into the ANSI standard for the language. What were they up to?

`UPDATE-INSTANCE-FOR-REDEFINED-CLASS` is not a weird anomaly; it's part of a carefully-considered set of features and protocols designed to support a specific style of programming. The Lisp runtime calls it for you automatically when it touches an object whose class definition has changed.

If you've defined a method specialized for it, then Lisp executes that method to rebuild the touched instance as if it had originally been instantiated from the class's new definition, and then your program goes its merry way. If you didn't specialize `UPDATE-INSTANCE-FOR-REDEFINED-CLASS` for this case, then the Lisp drops you into a breakloop.

A breakloop is an interactive repl with full access to all of the runtime's memory and all of the language's features, including visibility into the whole call stack that landed you in the breakloop. You can wander up and down the call stack, inspect anything in the runtime, edit bindings, redefine types and functions, and resume execution either at the point of control where the breakloop started, or at any other point for which the breakloop exposes a restart.

`UPDATE-INSTANCE-FOR-REDEFINED-CLASS` is not the weird fever dream of a confused eccentric. It's part of a purposeful system design intended to support a style of programming in which you build a program by interacting with a live runtime and teach it, interaction-by-interaction, how to be the program you want, while it runs.

It's a particular example of a general approach to programming best exemplified by these old systems. That general approach is the answer to your question: "Can someone knowledgeable explain how are lisp REPLs different from Python / Ruby REPLs? What is the differentiating point of REPL driven development?"

The differentiating point is that the entire language and system is thoughtfully designed from the ground up with the assumption that you're going to be changing your work in progress while it runs, and that you should be able to change absolutely anything about it as it runs and have a reasonable expectation that it will continue to work while you do it.

I like to call this style of programming "programming as teaching", and distinguish it from the much more widespread "programming as carpentry", in which the programmer is, metaphorically speaking, at a workbench banging together artifacts and assembling them to see how they turn out.

To be clear, I do not claim that the teaching approach is objectively better than the carpentry approach. I claim only that I, personally, am happier and measurably more productive using the teaching approach. I know that some other programmers report the same thing, and I suspect that if the teaching style of programming were more widely known, then there would be more programmers who prefer it.

There are several sibling comments that assert that any language can be made to support repl-driven programming, or that offer various languages and systems as examples of repl-driven programming. I'm sure that's all true, for some relatively restricted version of repl-driven programming, but the gold standard in repl-driven programming is programming as teaching in the style of old-fashioned Lisp and Smalltalk systems. These old systems offer amenities that the younger alternatives touted here do not match. I want more people to be aware of what they're missing.

Starting in the 1980s, I grew accustomed to systems that could start from cold in about a second, presenting to me a complete interactive development environment with all tools preloaded and ready to work, with the whole dynamic environment of my work in progress in the same state it was in the last time I was working with it. Moreover, I was accustomed to being able to take a single file from one machine to another to reproduce that same whole working environment equally quickly and easily on the new machine.

I could save the entire dynamic state of the running system to an image file, a serialized version of the running system's memory. I could later start up the system with that image file and be exactly where I was when I saved the image, right down to the positions and contents of all the open windows. I could save an image showing some bug or some strange behavior and give it to a colleague so that they could see it, too, and interact with the restored dynamic state to debug it.

I enjoyed comprehensive whole-system reflection that enabled me to view and edit absolutely everything in the running system while it ran. I could inspect absolutely everything, including the development environment and all its tools, interactively change any variable or field value, redefine any type or function, and continue to work with the changed system without stopping and restarting. (Obviously, if I made a bad change I might break the system, but remember, I could kill it and get back to where I started in a second or so).

I could start some process running--perhaps a 3D animation in a game, or a discrete-event simulation, or whatever--and change any values or definitions I liked to see what changed in the running process, without stopping the process to rebuild. For example, I could tell a rotating copper cube to become a glass icosahedron and reasonably expect to see my changes immediately reflected in the running program. This property is invaluable not only in games, simulations, and any kind of work with a visual-design component, but also in any kind of exploratory programming, where you're constructing data structures and evaluating expressions interactively to test your ideas.

Similarly, I could build some speculative data structure to explore an idea, and define some functions to operate on it. I could evaluate those expressions to see their results or to change the example structure. I could inspect the structure interactively and edit it in place if I think something different would work better. If I think a problem is caused by some structure or value in it, I could use the inspector to change it and see. If I thought one my my functions was doing something I didn't expect, I could insert a call to break, to activate a repl from inside the function call that would enable me to inspect and edit the data structure, redefine the function, and continue from there.

Anything the development system could do, I could do by typing an expression into the repl. As an example, nowadays you can still rebuild the whole Clozure Common Lisp environment from the ground up by typing (rebuild-ccl :full t).

The point is not that I would want to rebuld my Lisp from the repl all the time. The point is that the repl doesn't impose any aribtrary boundaries on what I can do. If the language and development environment can do it, I can do it from the repl. This is one of the properties that distinguishes the whole-system interactive design of these old tools from the more limited repls offered by newer ones. In pretty much every repl I've used other than old-style Lisps and Smalltalks I'm all the time stumbling over things you can't do from the repl.

I mentioned breakloops above. Their absence in younger languages and tools seem to me like some sort of sin, like we're tragically abandoning some of the best lessons of the past. Few newer development systems have them, but they're incredibly useful--at least if the language runtime is designed to properly support interactive programming.

A breakloop is a repl with all of the same affordances of the normal repl, but extended with all of the dynamic state of the control path that invoked the breakloop. If an error or an intentional call to break triggers a breakloop somewhere deep in a stack of recursive function calls, you get a repl that can see every frame of that stack, and every variable and value lexically accessible from it. You can browse all of that whole, change values, and redefine functions and types. You can resume execution at your leisure, and any changes you made in the breakloop will be visible in the resumed computation just as if that's how things were originally.

Proper breakloops don't just improve error messages; they replace them wholesale with an entire species of programming that lays the whole dynamic state of the system out on the table for you to examine and modify while the program continues to run.

Moreover, everything I just described about breakloops can also be automated. These old systems provide not only interactive tools for rummaging through the dynamic state of a suspended computation, but also APIs for handling them under program control. For example, you can wrap an arbitrary function call in condition handlers that will either drop you into a breakloop and enable you to vivisect the program state, or consult the dynamic state and compute which of several restarts to activate in order to transfer control to a path of your choosing.

I'm banging up against HN's length limit, but the above, I hope, goes some way toward answering to your question.
