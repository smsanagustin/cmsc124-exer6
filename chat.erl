-module(chat).
-export([init_chat/0, receiver/1, init_chat2/1, sendMessage/3]).

% initializes current node (host node)
init_chat() ->
    UserInput = io:get_line("Enter your name: "),
    % remove \n
    UserName = string:strip(UserInput, both, $\n), 
    register(receiver, spawn(chat, receiver, [UserName])).

% gets reply from the user through user input
getReply(UserName, Sender_Pid) ->
    io:format("~s", [UserName]),
    ReplyInput = io:get_line(": "),
    % removes \n
    Reply = string:strip(ReplyInput, both, $\n),

    % send reply to the other user
    Sender_Pid ! {Reply, UserName},
    receiver(UserName).

% called when host is initialized to allow it to receive messages
receiver(UserName) ->
    receive
        % when user receives bye, chat will be terminated
        {"bye", UserName2, Sender_Pid} ->
            UserName2 = UserName2,
            Sender_Pid = Sender_Pid,
            io:format("You partner has disconnected!~n"),

            % also end process of the sender
            Sender_Pid ! bye,
            halt();

        {"", UserName2, Sender_Pid} ->
            io:format("You are now connected to ~s.~n", [UserName2]),
            % get reply from the current user
            getReply(UserName, Sender_Pid);

        {Message, UserName2, Sender_Pid} ->
            % receiver will print on the terminal the message it received
            io:format("~s: ~s~n", [UserName2, Message]),

            % get reply from the current user when it received a message:
            getReply(UserName, Sender_Pid)
    end.

% ReceiverNode is the node of the user you want to send a message to
% init_chat2 connects current node to the host node
init_chat2(ReceiverNode) ->
    % get user's name
    UserInput2 = io:get_line("Enter your name: "),
    % remove \n
    UserName2 = string:strip(UserInput2, both, $\n), 

    % send an empty string initially to connect two nodes
    spawn(chat, sendMessage, ["", UserName2, ReceiverNode]).

% used to send message to the other user
sendMessage(Message, UserName2, ReceiverNode) ->
    % send Message to receiver
    {receiver, ReceiverNode} ! {Message, UserName2, self()},

    % check if there are replies from the receiver
    receive
        % used to halt process when the other user receives "bye"
        % see line 19 to understand what this code is for
        bye ->
            halt();

        % halts process when this process itself receives "bye"
        {"bye", UserName} ->
            UserName = UserName,
            io:format("Your partner has disconnected.~n"),

            % send a bye reply to the receiver as well
            {receiver, ReceiverNode} ! {"bye", UserName2, self()},
            halt();
        
        {Reply, UserName} ->
            io:format("~s: ~s~n", [UserName, Reply])
    end,

    % after getting a reply from the receiver, get user input to send another message
    io:format("~s", [UserName2]),
    NewMessageInput = io:get_line(": "),
    % removes \n
    NewMessage = string:strip(NewMessageInput, both, $\n),

    sendMessage(NewMessage, UserName2, ReceiverNode).