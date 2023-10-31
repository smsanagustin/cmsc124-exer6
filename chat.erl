-module(chat).
-export([init_chat/0, init_chat2/1, receiver1/1, receiver2/2, sendMessage1/2, sendMessage2/3]).

% initializes current node (host node)
init_chat() ->
    UserInput = io:get_line("Enter your name: "),
    % remove \n
    UserName = string:strip(UserInput, both, $\n), 
    register(receiver1, spawn(chat, receiver1, [UserName])).

% ReceiverNode is the node of the user you want to send a message to
% init_chat2 connects current node to the host node
init_chat2(ReceiverNode) ->
    % get user's name
    UserInput2 = io:get_line("Enter your name: "),
    % remove \n
    UserName2 = string:strip(UserInput2, both, $\n), 

    % allow client to start receiving messages
    Receiver2_Pid = spawn(chat, receiver2, [UserName2, ReceiverNode]),

    % connect two nodes
    net_adm:ping(ReceiverNode),
    {receiver1, ReceiverNode} ! {connected, UserName2, Receiver2_Pid},

    % allows client to send messages
    spawn(chat, sendMessage2, [UserName2, ReceiverNode, Receiver2_Pid]).


% called when host is initialized using init_chat
% watches for incoming messages to the host
receiver1(UserName) ->
    receive
        bye ->
            halt();
        % when user receives bye, chat will be terminated
        {"bye", UserName2, Receiver2_Pid} ->
            UserName2 = UserName2,
            io:format("~nYou have disconnected!~n"),

            % also end process of the sender
            Receiver2_Pid ! bye,

            halt();

        {connected, UserName2, Receiver2_Pid} ->
            io:format("You are now connected to ~s~n", [UserName2]),
            % spawn new process to allow host to send messages
            % UserName is the host's while Receiver2_Pid is the client's
            spawn(chat, sendMessage1, [UserName, Receiver2_Pid]);

        {Message, UserName2, Receiver2_Pid} ->
            Receiver2_Pid = Receiver2_Pid,
            % receiver will print on the terminal the message it received
            io:format("~s: ~s~n", [UserName2, Message])
    end,

    % call this function recursively to keep receiving messages
    receiver1(UserName).

% watches for incoming messages to the client
receiver2(UserName2, ReceiverNode) ->
    % check if there are replies from the receiver
    receive
        % used to halt process when the other user receives "bye"
        % see line 19 to understand what this code is for
        bye ->
            % ReceiverNode ! bye,
            halt();

        % halts process when this process itself receives "bye"
        {"bye", UserName} ->
            UserName = UserName,
            io:format("~s: bye~n", [UserName]),
            io:format("~nYour partner has disconnected.~n"),

            % send a bye reply to the receiver as well
            {receiver1, ReceiverNode} ! {"bye", UserName2, self()},

            halt();

        {Reply, UserName} ->
            io:format("~s: ~s~n", [UserName, Reply])
    end,

    % continue receiving messages (call receiver2 recursively)
    receiver2(UserName2, ReceiverNode).

% used to send message to the client from the host
sendMessage1(UserName, Receiver2_Pid) ->
    % io:format("~s", [UserName]),
    Prompt = UserName ++ ": ",
    ReplyInput = io:get_line(Prompt),
    % removes \n
    Reply = string:strip(ReplyInput, both, $\n),

    % send reply to the other user
    Receiver2_Pid ! {Reply, UserName},

    % call this function recursively to ask for another message
    sendMessage1(UserName, Receiver2_Pid).

% used to send message to the host/receiver1
sendMessage2(UserName2, ReceiverNode, Receiver2_Pid) ->
    % get user's input 
    io:format(""),
    io:format(""),
    io:format(""),
    %io:format(":"),

    Prompt = UserName2 ++ ": ",
    NewMessageInput = io:get_line(Prompt),
    
    % removes \n
    NewMessage = string:strip(NewMessageInput, both, $\n),

    % send message to client/receiver1
    {receiver1, ReceiverNode} ! {NewMessage, UserName2, Receiver2_Pid},

    % call this function recusrively to ask for another message
    sendMessage2(UserName2, ReceiverNode, Receiver2_Pid).
