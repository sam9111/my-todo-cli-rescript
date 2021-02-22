/*
Sample JS implementation of Todo CLI that you can attempt to port:
https://gist.github.com/jasim/99c7b54431c64c0502cfe6f677512a87
*/

/* Returns date with the format: 2021-02-04 */
let getToday: unit => string = %raw(`
function() {
  let date = new Date();
  return new Date(date.getTime() - (date.getTimezoneOffset() * 60000))
    .toISOString()
    .split("T")[0];
}
  `)

type fsConfig = {encoding: string, flag: string}

/* https://nodejs.org/api/fs.html#fs_fs_existssync_path */
@bs.module("fs") external existsSync: string => bool = "existsSync"

/* https://nodejs.org/api/fs.html#fs_fs_readfilesync_path_options */
@bs.module("fs")
external readFileSync: (string, fsConfig) => string = "readFileSync"

/* https://nodejs.org/api/fs.html#fs_fs_writefilesync_file_data_options */
@bs.module("fs")
external appendFileSync: (string, string, fsConfig) => unit = "appendFileSync"

@bs.module("fs")
external writeFileSync: (string, string, fsConfig) => unit = "writeFileSync"

/* https://nodejs.org/api/os.html#os_os_eol */
@bs.module("os") external eol: string = "EOL"

let encoding = "utf8"




//reading file contents

let readFromfile: string => array<string> = filename =>{
  if !existsSync(filename){
    []
  }
  else{
    let read_string: string= Js.String.trim(
      readFileSync(filename,{encoding: encoding,flag: "r"}))
    Js.String.split(eol,read_string)
  }
}

//truncating and overwriting file contents

let writeTofile: (array<string>,string) => unit = (text,filename) =>{
  let new_contents: string=text->Belt.Array.reduce("",
    (lines,line) =>lines++line++eol)
  writeFileSync(filename,new_contents,{encoding: encoding,flag: "w"})
}

//appending to file contents

let appendTofile: (string,string) => unit = (line,filename) =>{
  let line=line++eol
  appendFileSync(filename,line,{encoding: encoding,flag: "a"})
}

//Help

let help: unit => unit=()=>{
Js.log("Usage :-
$ ./todo add \"todo item\"  # Add a new todo
$ ./todo ls               # Show remaining todos
$ ./todo del NUMBER       # Delete a todo
$ ./todo done NUMBER      # Complete a todo
$ ./todo help             # Show usage
$ ./todo report           # Statistics")
}

//Add new single or multiple todos

let add: array<string> => unit= todos =>{
  switch todos {
    |[]|[""] => Js.log("Error: Missing todo string. Nothing added!")
    |items =>{
      items->
      Belt.Array.forEach(
        item=>{
          appendTofile(item,"todo.txt")
          Js.log(`Added todo: "${item}"`)
        }
      )
    }
  }
}

//List all pending todos

let ls: unit => unit=()=>{
  let todos: array<string> =readFromfile("todo.txt")
  switch todos{
    |[]|[""] => Js.log("There are no pending todos!")
    |someTodos =>{
      let formatted_todos: array<string> =someTodos->
        Belt.Array.mapWithIndex(
          (i,todo)=>{
            `[${Belt.Int.toString(i+1)}] ${todo}`
          })

      Belt.Array.forEach(
        Belt.Array.reverse(formatted_todos),
          todo=> Js.log(todo)
      )
    }
  }
}

//get todo from "todo.txt"

let getTodo: string => option<string> =todoNumber=>{
   let todos: array<string> =readFromfile("todo.txt")
   let index: option<int> = Belt.Int.fromString(todoNumber)
   switch index{
    |None => None
    |Some(i) =>Belt.Array.get(todos, i-1)
    }
}
//remove todo from "todo.txt"

let removeTodo: string => unit =todoToremove=>{
  let todos: array<string> =readFromfile("todo.txt")
  let updated_content: array<string> =Js.Array.filter(todo=>todo!=todoToremove,todos)
  writeTofile(updated_content,"todo.txt")
}

//Delete single todo from "todo.txt"

let delTodo: string => unit=todoNumber=>{
  let todoTodelete: option<string> =getTodo(todoNumber)
  switch todoTodelete{
    |None => Js.log(`Error: todo #${todoNumber} does not exist. Nothing deleted.`)
    |Some(deleted_todo)=>{
      removeTodo(deleted_todo)
      Js.log(`Deleted todo #${todoNumber}`)
    }
  }
}
//Delete todos from "todo.txt"
let del: array<string> => unit=todoNumber=>{
  switch todoNumber{
    |[] => Js.log("Error: Missing NUMBER for deleting todo.")
    |someNumbers =>{
      someNumbers->Belt.Array.forEach(
        number=>delTodo(number)
      )
    }
  }
}

//Mark a todo item as completed

let done_cmd: option<string> => unit=todoNumber=>{
  switch todoNumber{
    |None => Js.log(`Error: Missing NUMBER for marking todo as done.`)
    |Some(number)=>{
      let todoTomark: option<string>=getTodo(number)
      switch todoTomark{
          |None => Js.log(`Error: todo #${number} does not exist.`)
          |Some(marked_todo)=>{
            removeTodo(marked_todo)
            let itemDone: string=`x ${getToday()} ${marked_todo}`
            appendTofile(itemDone,"done.txt")
            Js.log(`Marked todo #${number} as done.`)
          }
      }
    }
  }
}

//Generate report
let report: unit => unit=()=>{
  let pending: int=Belt.Array.length(readFromfile("todo.txt"))
  let completed: int=Belt.Array.length(readFromfile("done.txt"))
  Js.log(
    `${getToday()} Pending : ${Belt.Int.toString(pending)} Completed : ${Belt.Int.toString(completed)}`
  )
}

let arguments: array<string> =%raw(`process.argv.slice(2)`)->Belt.Array.map(
  argument=>Js.String.trim(argument)
)
let command: option<string> =Belt.Array.get(arguments,0)
let inputs: array<string> =Belt.Array.slice(arguments,~offset=1,~len=Belt.Array.length(arguments))
let command_switch: (option<string>,array<string>) => unit = (command,inputs)=> {
  switch command {
    |Some("add")=>add(inputs)
    |Some("ls")=>ls()
    |Some("del")=>del(inputs)
    |Some("done")=>done_cmd(Belt.Array.get(inputs,0))
    |Some("report")=>report()
    |Some("help")
    |Some(_)
    |None=> help()
  }
}
command_switch(command,inputs)
