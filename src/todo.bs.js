// Generated by ReScript, PLEASE EDIT WITH CARE
'use strict';

var Fs = require("fs");
var Os = require("os");
var Curry = require("bs-platform/lib/js/curry.js");
var Belt_Int = require("bs-platform/lib/js/belt_Int.js");
var Belt_Array = require("bs-platform/lib/js/belt_Array.js");

var getToday = (function() {
  let date = new Date();
  return new Date(date.getTime() - (date.getTimezoneOffset() * 60000))
    .toISOString()
    .split("T")[0];
});

var encoding = "utf8";

function readFromfile(filename) {
  if (!Fs.existsSync(filename)) {
    return [];
  }
  var read_string = Fs.readFileSync(filename, {
          encoding: encoding,
          flag: "r"
        }).trim();
  return read_string.split(Os.EOL);
}

function writeTofile(text, filename) {
  var new_contents = Belt_Array.reduce(text, "", (function (lines, line) {
          return lines + line + Os.EOL;
        }));
  Fs.writeFileSync(filename, new_contents, {
        encoding: encoding,
        flag: "w"
      });
  
}

function appendTofile(line, filename) {
  var line$1 = line + Os.EOL;
  Fs.appendFileSync(filename, line$1, {
        encoding: encoding,
        flag: "a"
      });
  
}

function help(param) {
  console.log("Usage :-\n$ ./todo add \"todo item\"  # Add a new todo\n$ ./todo ls               # Show remaining todos\n$ ./todo del NUMBER       # Delete a todo\n$ ./todo done NUMBER      # Complete a todo\n$ ./todo help             # Show usage\n$ ./todo report           # Statistics");
  
}

function add(todos) {
  var len = todos.length;
  if (len !== 1) {
    if (len === 0) {
      console.log("Error: Missing todo string. Nothing added!");
      return ;
    }
    
  } else {
    var match = todos[0];
    if (match === "") {
      console.log("Error: Missing todo string. Nothing added!");
      return ;
    }
    
  }
  return Belt_Array.forEach(todos, (function (item) {
                appendTofile(item, "todo.txt");
                console.log("Added todo: \"" + item + "\"");
                
              }));
}

function ls(param) {
  var todos = readFromfile("todo.txt");
  var len = todos.length;
  if (len !== 1) {
    if (len === 0) {
      console.log("There are no pending todos!");
      return ;
    }
    
  } else {
    var match = todos[0];
    if (match === "") {
      console.log("There are no pending todos!");
      return ;
    }
    
  }
  var formatted_todos = Belt_Array.mapWithIndex(todos, (function (i, todo) {
          return "[" + String(i + 1 | 0) + "] " + todo;
        }));
  return Belt_Array.forEach(Belt_Array.reverse(formatted_todos), (function (todo) {
                console.log(todo);
                
              }));
}

function getTodo(todoNumber) {
  var todos = readFromfile("todo.txt");
  var index = Belt_Int.fromString(todoNumber);
  if (index !== undefined) {
    return Belt_Array.get(todos, index - 1 | 0);
  }
  
}

function removeTodo(todoToremove) {
  var todos = readFromfile("todo.txt");
  var updated_content = todos.filter(function (todo) {
        return todo !== todoToremove;
      });
  return writeTofile(updated_content, "todo.txt");
}

function delTodo(todoNumber) {
  var todoTodelete = getTodo(todoNumber);
  if (todoTodelete !== undefined) {
    removeTodo(todoTodelete);
    console.log("Deleted todo #" + todoNumber);
  } else {
    console.log("Error: todo #" + todoNumber + " does not exist. Nothing deleted.");
  }
  
}

function del(todoNumber) {
  if (todoNumber.length !== 0) {
    return Belt_Array.forEach(todoNumber, delTodo);
  } else {
    console.log("Error: Missing NUMBER for deleting todo.");
    return ;
  }
}

function done_cmd(todoNumber) {
  if (todoNumber !== undefined) {
    var todoTomark = getTodo(todoNumber);
    if (todoTomark !== undefined) {
      removeTodo(todoTomark);
      var itemDone = "x " + Curry._1(getToday, undefined) + " " + todoTomark;
      appendTofile(itemDone, "done.txt");
      console.log("Marked todo #" + todoNumber + " as done.");
      return ;
    }
    console.log("Error: todo #" + todoNumber + " does not exist.");
    return ;
  }
  console.log("Error: Missing NUMBER for marking todo as done.");
  
}

function report(param) {
  var pending = readFromfile("todo.txt").length;
  var completed = readFromfile("done.txt").length;
  console.log(Curry._1(getToday, undefined) + " Pending : " + String(pending) + " Completed : " + String(completed));
  
}

var $$arguments = Belt_Array.map((process.argv.slice(2)), (function (argument) {
        return argument.trim();
      }));

var command = Belt_Array.get($$arguments, 0);

var inputs = Belt_Array.slice($$arguments, 1, $$arguments.length);

function command_switch(command, inputs) {
  if (command !== undefined) {
    switch (command) {
      case "add" :
          return add(inputs);
      case "del" :
          return del(inputs);
      case "done" :
          return done_cmd(Belt_Array.get(inputs, 0));
      case "help" :
          console.log("Usage :-\n$ ./todo add \"todo item\"  # Add a new todo\n$ ./todo ls               # Show remaining todos\n$ ./todo del NUMBER       # Delete a todo\n$ ./todo done NUMBER      # Complete a todo\n$ ./todo help             # Show usage\n$ ./todo report           # Statistics");
          return ;
      case "ls" :
          return ls(undefined);
      case "report" :
          return report(undefined);
      default:
        console.log("Usage :-\n$ ./todo add \"todo item\"  # Add a new todo\n$ ./todo ls               # Show remaining todos\n$ ./todo del NUMBER       # Delete a todo\n$ ./todo done NUMBER      # Complete a todo\n$ ./todo help             # Show usage\n$ ./todo report           # Statistics");
        return ;
    }
  } else {
    console.log("Usage :-\n$ ./todo add \"todo item\"  # Add a new todo\n$ ./todo ls               # Show remaining todos\n$ ./todo del NUMBER       # Delete a todo\n$ ./todo done NUMBER      # Complete a todo\n$ ./todo help             # Show usage\n$ ./todo report           # Statistics");
    return ;
  }
}

command_switch(command, inputs);

exports.getToday = getToday;
exports.encoding = encoding;
exports.readFromfile = readFromfile;
exports.writeTofile = writeTofile;
exports.appendTofile = appendTofile;
exports.help = help;
exports.add = add;
exports.ls = ls;
exports.getTodo = getTodo;
exports.removeTodo = removeTodo;
exports.delTodo = delTodo;
exports.del = del;
exports.done_cmd = done_cmd;
exports.report = report;
exports.$$arguments = $$arguments;
exports.command = command;
exports.inputs = inputs;
exports.command_switch = command_switch;
/* arguments Not a pure module */
