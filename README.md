# cl-swimmingpool

This library is a simple implementation of a thread poolg (swimming pool) that allows
the user to send functions to a pool (swimmer) to be executed and then grab the result after the task has been evaluated.


todo
* queue for each thread
* add tasks to the shortest queue
* record error output for an armband
* restart/reset broken swimmers
* clean shutdown of swimmers

There are two functions currently, dive (add a function) and get-out (grab the result). 

## License

MIT

