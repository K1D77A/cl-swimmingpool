# cl-swimmingpool

This library is a simple implementation of a thread poolg (swimming pool) that allows
the user to send functions to a pool (swimmer) to be executed and then grab the result after the task has been evaluated.


todo
* queue for each thread
* add tasks to the shortest queue
* record error output for an armband
* restart/reset broken swimmers
* clean shutdown of swimmers

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
```

## License

MIT

