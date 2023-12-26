# erlang chat app
A simple chat program that allows two terminals to communicate with each other. Implemented using Erlang.

# how to run 
### on 2 separate terminals, run this:
`erlang -name <name>@<host>`

`erlang -name <name>@<host>`

### then run on separate terminals:
### terminal 1:
`c(chat).`

`chat:init_chat().`

### terminal 2:
`c(chat).`

`chat:init_chat2('<name1>@<host>')`
