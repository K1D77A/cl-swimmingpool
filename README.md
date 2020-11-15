# cl-swimmingpool

This library is a simple implementation of a thread pool.
The purpose is to allow the user to be able to execute tasks concurrently without much
much thought. This libary implements a simple thread pool where each thread has a list of
tasks it has to execute as they are added to the pool. As a task is executed the result is
stored and simply waits for the user to retrieve the results, once retreived the task is
removed from the pool. I decided I would make the library swimming pool themed.

* Swimming pool - This is the primary thread pool which contains swimmers.
* Swimmer - A swimmer is a thread and a list of plastic-floats.
* Plastic float - A plastic float is the name for tasks the user wants to have executed.
These are passed to the swimmer where the swimmer then executes the functions stored
within. The floats store their start and end time, part of the stacktrace and the condition
in the event an unhandled condition is signalled.


```lisp
;make-swimming-pool
SWIM> (make-swimming-pool 10)
#<SWIMMING-POOL {1004ED76A3}>
;dive
SWIM> (dive * (lambda () (sleep 10)(print "abc")))
#<PLASTIC-FLOAT {1003622D83}>
;get-out
SWIM> (get-out *)
"abc"
;bleach (nicely tells the swimmers to stop processing tasks)
SWIM> (make-swimming-pool 10)
#<SWIMMING-POOL {1004FDE793}>
SWIM> (bleach *) ;set dirty to non nil to abort threads
(#<SWIMMER {1004FDE963}> #<SWIMMER {1004FDEF83}> #<SWIMMER {1004FDF573}>
 #<SWIMMER {1004FDFAF3}> #<SWIMMER {1005020083}> #<SWIMMER {1005020693}>
 #<SWIMMER {1005020D33}> #<SWIMMER {1005021343}> #<SWIMMER {10050219E3}>
 #<SWIMMER {1005021F93}>)

;drown
#<SWIMMER {100426C1D3}>
SWIM> (drown *)
T


```
There are two potential conditions when you call 'get-out', 'in-progress' which tells you
the thread is still executing the task and 'function-execution-errored' which tells you
and error occurred in execution and you should inspect the backtrace within the
plastic-float.

A third condition is 'swimmer-has-or-is-trying-to-shutdown', this is signalled when
the user tries to call 'dive' when you have shutdown the swimming pool.

I have exported all of the accessors for the plastic-float class.

## License

MIT
