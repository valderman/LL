function Scheduler() {
    // Run queue
    var queue = [];

    // Pick the next thread from the run queue, then run it
    this.runNext = function() {var next = queue.shift()();}

    // Spawn a new thread
    this.spawn = function(f) {queue.push(f);}

    // Create a new synchronization variable
    this.newV = function() {return {empty: true};}

    // Read a synchronization variable and pass its value to a continuation,
    // when it becomes available.
    this.read = function(v, then) {
        if(!v.empty) {
            v.empty = true;
            then(v.value);
        } else {
            var that = this;
            queue.push(function() {that.read(v, then);});
        }
    }

    // Write to a synchronization variable, when it becomes available.
    this.write = function(v, x, then) {
        if(v.empty) {
            v.empty = false;
            v.value = x;
            then();
        } else {
            var that = this;
            queue.push(function() {that.write(v, x, then);});
        }
    }

    // Write to a synchronization variable if it is available. If it is not,
    // do not block but return. The return value (true or false) indicates
    // whether a write took place or not.
    // (See Control.Concurrent.MVar.tryPutMVar.)
    this.tryWrite = function(v, x, then) {
        if(v.empty) {
            v.empty = false;
            v.value = x;
            then(true);
        } else {
            then(false);
        }
    }

    // Run an initial thread and any children it spawns.
    this.run = function(p) {
        this.spawn(p);
        while(queue.length > 0) {
            this.runNext();
        }
    }
}

var s = new Scheduler();
var inVar = s.newV();
var outVar = s.newV();
